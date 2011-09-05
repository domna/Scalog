package scalog

import scala.util.parsing.combinator._
import scala.util.parsing.input.Reader
import java.io.{FileReader, FileWriter}

/** Scala PreParser parst die Prolog Konstrukte in valides Scala mit tuProlog API
	@author Florian Dobener
	@version 0.1*/
class ScalogPreParser extends JavaTokenParsers{
	/** Vordefinierte Scala Funktionalität (benötigte imports etc.) */
	val scalaPredef = """import alice.tuprolog._
	import scala.util.parsing.combinator._
	import scalog.Predef._
	import scalog._

				"""
	/** Vordefinierte Prolog Konstrukte (nur das was explizit in der Datei erklärt werden muss => Ansonsten scalog.Predef Objekt)*/
	val predef = "val engine = new Prolog\n\n"
	
	val anyExpr = """(.)*""".r //Match all characters and give them back
	val scalaExpr = """[^%]*""".r
	val prologBody = """[^}]*""".r
	
	/*def scalaFile:Parser[String] = rep(scalaExpr | prologDef) ^^ {
		case Nil => ""
		case x :: xs => 	var res = x
					for(i <- x) res += i
					res
	}*/
	def scalaFile:Parser[String] = (opt(scalaExpr) ^^ optS) ~ (opt(prologDef) ^^ optS) ~ (opt(scalaExpr) ^^ optS) ^^ {
		case s1 ~ p1 ~ s2 => scalaPredef + s1 + "\n\n//Scalog Definition Part\n" + p1 + "//End of Scalog Definition Part\n" + s2
	}

	def prologDef:Parser[String] = "%prolog" ~> prologFunDef ~ ("{" ~> prologBody <~ "}") ^^ {
		case f ~ b =>	predef + "engine.setTheory(new Theory(\"\"\"" + b + "\"\"\"))" + "\n\n" + f 
	}

	def prologFunDef:Parser[String] = "[" ~> repsep(func,",") <~ "]" ^^ funcConc

	def func:Parser[String] = 	(ident ~ (opt("[" ~> "[A-Z]".r <~ "]") ^^ optG) ~ ( "(" ~> repsep(funArg,",") <~ ")"  | "" ^^ (x => List[(String,String)]()))  <~ ":") ~ 
					((varRetTyp <~ "=>") ^^ (x => List[String](x)) | (("(" ~> repsep(varRetTyp,",") <~ ")") <~ "=>")) ~
					ident ~ ("(" ~> repsep(ident,",") <~ ")") ^^ funcTrafo

	def funArg:Parser[(String,String)] = (ident <~ ":") ~ varTyp ^^ { case n ~ t => (n,t) }

	def varTyp:Parser[String] = "Option" ~ "[" ~> varTyp <~ "]" | ("PrologList" | "List") ~ "[" ~> varTyp <~ "]" ^^ ( "PrologList["+_+"]" ) | litValue 

	def varRetTyp:Parser[String] = "List" ~ "[" ~> varRetTyp <~ "]" ^^ ( "List["+_+"]" ) | "Option" ~ "[" ~> varRetTyp <~ "]" | litValue  

	def litValue:Parser[String] = "Boolean" | "Int" ^^ (x => "scala.Int") | "Double" ^^ (x => "scala.Double") | "String" | "Long" | "[A-Z]".r


	/** Parst optionale Argumente heraus*/
	def optS(in:Option[String]):String = in match{
		case Some(x) => x
		case None => ""
	}
	
	/** Parses a generic value out of an option*/
	def optG(in:Option[String]):String = in match {
		case Some(x) => "[" + x + "]"
		case None => ""
	}

	/** Baut den Quelltext aus verschiedenen Funktion
		@param fl Funktionenliste
		@return Funktionen zusammengehangen als String*/
	def funcConc(fl:List[String]):String = {
		var res = ""
		for(i <- fl) res += i + "\n"
		res
	}

	/** Verkettet Stringtupel Listen zu Scala Argumentenlisten
		@param args Argumentenliste
		@return Argumente in der Form Scalas*/
	private def argConc(args:List[(String,String)]):String = args match{
		case last :: Nil => last._1 + ":" + last._2
		case Nil => ""
		case h :: t => h._1 + ":" + h._2 + ", " + argConc(t)
	}

	/** Verkettet Argumente (Trenner: ,) mithilfe einer Umgebungsfunktion (Einfach Verkettung f = _ )
		@param args Argumente, die verkettet werden sollen
		@param f Umgebungsfunktion: Argumente werden mithilfe dieser umschlossen
		@return Zusammengehangene Agumente*/
	private def argConc[T](args:List[T], f:T => String):String = args match{
		case last :: Nil => f(last)
		case Nil => ""
		case h :: t => f(h) + ", " + argConc(t, f)
	}

	/** Konkateniert die Prolog Parameter danach ob sie ein Scala Parameter oder ein Rückgabewert sind 
		@param pArgs Paramterliste der Prolog Funktion
		@param sArgs Parameterliste der Scala Funktion
		@return Konkatenation der Parameter*/
	private def concPrologArgs[T](pArgs:List[T], sArgs:List[(T,T)], sNum:Int):String = pArgs match {
		case x :: Nil => if(sArgs.exists(y => y._1 == x)){
					if(sNum != 0) throw new Exception("Number of scala and prolog parameters do not match.")
					"\" + " + x + " + \""
				}else x.toString
		case x :: xs => if(sArgs.exists(y => y._1 == x))
					"\" + " + x + " + \", " + concPrologArgs(xs,sArgs,sNum - 1) 
				else
					x.toString + ", " + concPrologArgs(xs,sArgs,sNum - 1)
		case Nil => ""
	}

	/** Erstellt aus der Scalog Funktionsdefinition eine Scala/TuProlog Definition
		@param in Eingangsparser
		@return Scala Funktion*/
	def funcTrafo(in: String~String~List[(String,String)]~List[String]~String~List[String]):String = in match{
		case name ~ gen ~ sArgs ~ sRetArgs ~ pName ~ pArgs =>
			println("Parsed " + name + " => " + pName)
			val pRetArgs = pArgs.filter(x => !sArgs.exists(y => y._1 == x))
			val pArg = pArgs.filter(x => sArgs.exists(y => y._1 == x))
			
			var res = "def " + name + gen + "(" + argConc(sArgs) + "):("+ argConc(sRetArgs, "Option["+ (_:String) +"]") + ") = { \n"
			res += "\t" + """val result = engine.solve("""" + pName + """(""" + concPrologArgs(pArgs,sArgs,sArgs.length) + """).")""" + "\n\n" 
			
			if(sRetArgs.length != pRetArgs.length) throw new Exception("Number of scala and prolog parameters do not match.")
			
			var rest = sRetArgs
			var j = 0
			for(i <- pRetArgs){
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

/** Dieses Objekt ist für die externe Bearbeitung der Dateien zuständig.*/
object Scalog extends ScalogPreParser{
	/** Die main Methode nimmt über die Konsole zwei Argumente auf, die die Input- und Outputdatei repräsentieren.
		Gibt es mehr oder weniger als zwei Argumente wird ein Fehler ausgegeben*/
	def main(args:Array[String]){
		if(args.length == 2){
			val file = new FileReader(args(0))
			
			def printEnv[T](in:Reader[T], cnt:Int):String = {
				if(cnt == 0) ""
				else in.first + printEnv(in.rest, cnt - 1)
			}
			
			parseAll(scalaFile,file) match {
				case Success(parsedOutput, s) => if(s.atEnd){
									val newFile = new FileWriter(args(1), false)
									newFile.write(parsedOutput)
									newFile.close
								}else throw new Exception("Parsing failure: expected end of input")
				case NoSuccess(msg,x) => throw new Exception("Parsing failure: " + msg + "\n" + x.first + "  ^  " + printEnv(x.rest, 50))
			}
			
			file.close
		}else println("Nicht genügend Argumente. Versuchen Sie: scalog <SourceFile> <DestFile>")
	}
}
