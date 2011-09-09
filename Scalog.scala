package scalog

import scala.util.parsing.combinator._
import scala.util.parsing.input.Reader
import java.io.{FileReader, FileWriter}

/** Scalog preparser parses prolog language blocks into valid scala source code (with use of the tuProlog API)
	@author Florian Dobener
	@version 0.1*/
class ScalogPreParser extends JavaTokenParsers{
	/** Predefined Scala source code (this is added on top of every file) */
	val scalaPredef = """import alice.tuprolog._
	import scala.util.parsing.combinator._
	import scalog.Predef._
	import scalog._

				"""
	/** Predefined Prolog source code (added above prolog definitions). Here are only things that are absolutly necessary to be written
	into the source code. Everything else is stored in the scalog.Predef object which should be automatically imported in every file*/
	val prologPredef = "val engine = new Prolog\n\n"
	
	val scalaExpr = """[^%]*""".r
	val prologBody = """[^}]*""".r
	
	//Testing for extension of the parser to have more than one prolog block.
	//Problem: Parsers does an infinit loop when using this
	/*def scalaFile:Parser[String] = (opt(scalaExpr) ^^ parseOpt(x=>x)) ~ rep(prologDef | scalaExpr) ^^ {
			case x ~ list => list.foldLeft(x){(conc,a) => conc + a}
	}*/
	
	def scalaFile:Parser[String] = (opt(scalaExpr) ^^ parseOpt(x=>x)) ~ (opt(prologDef) ^^ parseOpt(x=>x)) ~ (opt(scalaExpr) ^^ parseOpt(x=>x)) ^^ {
		case s1 ~ p1 ~ s2 => scalaPredef + s1 + "\n\n//Scalog Definition Part\n" + p1 + "//End of Scalog Definition Part\n" + s2
	}

	def prologDef:Parser[String] = "%prolog" ~> prologFunDef ~ ("{" ~> prologBody <~ "}") ^^ {
		case f ~ b =>	prologPredef + "engine.setTheory(new Theory(\"\"\"" + b + "\"\"\"))" + "\n\n" + f 
	}

	def prologFunDef:Parser[String] = "[" ~> repsep(func,",") <~ "]" ^^ ( _.reduceLeft{(conc,x) => conc + "\n" + x })
	
					
	def func:Parser[String] = (((scalaFunc ~ scalaArgs <~ ":") ~ scalaRetArgs <~ "=>") ~ ident ~ prologFuncArgs) ^^ funcTrafo
					
	def scalaFunc:Parser[String~String] = ident ~ (opt("[" ~> "[A-Z]\\w*".r <~ "]") ^^ parseOpt(x => "[" + x + "]"))
	
	def scalaArgs:Parser[List[(String,String)]] = 	"(" ~> repsep(funArg,",") <~ ")" | 
													"" ^^ (x => List[(String,String)]()) 
	
	def scalaRetArgs:Parser[List[String]] = varRetTyp ^^ (x => List[String](x)) | 
											"(" ~> repsep(varRetTyp,",") <~ ")"
	
	def prologFuncArgs:Parser[List[String]] = ("(" ~> repsep(ident,",") <~ ")")

	
	def funArg:Parser[(String,String)] = (ident <~ ":") ~ varTyp ^^ { case n ~ t => (n,t) }

	def varTyp:Parser[String] = "Option" ~ "[" ~> varTyp <~ "]" | 
								("PrologList" | "List") ~ "[" ~> varTyp <~ "]" ^^ ( "PrologList["+_+"]" ) | 
								litValue 

	def varRetTyp:Parser[String] = 	"Option" ~ "[" ~> varRetTyp <~ "]" | 
									"List" ~ "[" ~> varRetTyp <~ "]" ^^ ( "List["+_+"]" ) | 
									litValue  

	def litValue:Parser[String] = 	"Boolean" | 
									"String" |
									"Int" ^^ (x => "scala.Int") | 
									"Double" ^^ (x => "scala.Double") | 
									"Float" ^^ (x => "scala.Float") |
									"Long" ^^ (x => "scala.Long") |										
									"[A-Z]\\w*".r


	/** Converts a string option into a string. It also applies the function env on the value of a some case. */
	def parseOpt(env:String => String)(o:Option[String]):String = o match {
		case Some(x) => env(x)
		case None => ""
	}

	/** Converts a list of string tupels to a scala variable declaration (e.g. ("name","Int") converts to name:Int).
		@param args List of arguments
		@return Scala variables*/
	private def argConc(args:List[(String,String)]):String = args match{
		case Nil => ""
		case _ => args map(x => x._1 + ":" + x._2) reduceLeft { (conc, x) =>
			conc + ", " + x
		}
	}

	/** Concatenates arguments (with seperator , ). It applies the environment function f to enclose every argument.
		@param args List of arguments
		@param f Encloses every argument
		@return Concatenated arguments*/
	private def argConc[T](args:List[T], f:T => String):String = args match{
		case Nil => ""
		case _ => args map(f) reduceLeft { (conc, x) =>
			conc + ", " + x
		}
	}

	/** Builts the arguments for the prolog function call
		@param pArgs List of prolog arguments (which gives us an expression back)
		@param sArgs List of scala arguments (which are defined in the scala function)
		@return Concatenation of these parameters*/
	private def concPrologArgs[T](pArgs:List[T], sArgs:List[(T,T)], sNum:Int):String = pArgs match {
		case x :: Nil => if(sArgs.exists(y => y._1 == x)){
					if(sNum != 0) throw new ParamException
					"\" + " + x + " + \""
				}else x.toString
		case x :: xs => if(sArgs.exists(y => y._1 == x))
					"\" + " + x + " + \", " + concPrologArgs(xs,sArgs,sNum - 1) 
				else
					x.toString + ", " + concPrologArgs(xs,sArgs,sNum - 1)
		case Nil => ""
	}
			

	/** Builts the scala function which sends the tuProlog call
		@param in Parser output
		@return The scala function */
	def funcTrafo(in: String~String~List[(String,String)]~List[String]~String~List[String]):String = in match{
		case name ~ gen ~ sArgs ~ sRetArgs ~ pName ~ pArgs =>
			println("Parsed " + name + " => " + pName) //Print parsing info...
			val pRetArgs = pArgs.filter(x => !sArgs.exists(y => y._1 == x))
			val pArg = pArgs.filter(x => sArgs.exists(y => y._1 == x))
			
			var res = "def " + name + gen + "(" + argConc(sArgs) + "):("+ argConc(sRetArgs, "Option["+ (_:String) +"]") + ") = { \n"
			res += "\t" + """val result = engine.solve("""" + pName + """(""" + concPrologArgs(pArgs,sArgs,sArgs.length) + """).")""" + "\n\n" 
			
			if(sRetArgs.length != pRetArgs.length) throw new ParamException
			
			var rest = sRetArgs
			var j = 0
			pRetArgs foreach {i =>
				res += "\t var r" + j + ":Option[" + rest.head + "] = None \n"
				res += "\t if(result.isSuccess) r" + j + " = Some("
				res += "result.getVarValue(\"" + i + "\").toString)\n"				
				rest = rest.tail
				j += 1
			}
			res += "\n\n\t(" + argConc(0 until sRetArgs.length toList, "r" + (_:Int) ) + ") \n"
			res += "}"
			
			res
	}
}

/** This object does the external file handling */
object Scalog extends ScalogPreParser{
	/** The main method takes two arguments which are representing the files to read from and to write in. */
	def main(args:Array[String]){
		if(args.length == 2){
			val file = new FileReader(args(0))
			
			parseAll(scalaFile,file) match {
				case Success(parsedOutput, s) => if(s.atEnd){
									val newFile = new FileWriter(args(1), false)
									newFile.write(parsedOutput)
									newFile.close
								}else throw new ParseException("expected end of input", s)
				case NoSuccess(msg,x) => throw new ParseException(msg, x)
			}
			
			file.close
		}else println("Not enough arguments. Try: scalog <SourceFile> <DestFile>")
	}
}
