package scalog

import scala.util.parsing.combinator._

/** Stellt eine Liste mit [1,2,3,...] als toString Methode bereit*/
case class PrologList[T](data:List[T]){
	override def toString:String = {
		def help(list:List[T]):String = list match{
			case Nil => ""
			case x :: Nil =>        x.toString
			case x :: xs =>         x.toString + ", " + help(xs)
		}
		
		"[" + help(data) + "]"
	}
}

/** Stellt Standardfunktionen für Scalog-Dateien bereit
	@author Florian Dobener
	@version 0.1*/
object Predef{
	/** A prolog list parser*/
	class PListParser extends JavaTokenParsers{
		def list:Parser[List[Any]] = "[" ~> listArgs <~ "]"
		def listArgs:Parser[List[Any]] = repsep(args, ",")
		def args:Parser[Any] = list | "[^]]*".r
	}

	/** Zerschneidet einen String in eine PrologListe
		@param s Eingangstring, der eine PrologListe enthält
		@return Extrahierte Liste*/
	def cutPList[T](s:String):List[T] = {
		val parser = new PListParser	
		parser.parseAll(parser.list,s) match {
			case parser.Success(pO, s) => 	if(s.atEnd) pO.asInstanceOf[List[T]]
							else throw new Exception("List parsing exception: expected end of list")
			case parser.NoSuccess(msg,_) => throw new Exception("List parsing failure: " + msg)
		}
	}

	implicit def list2PrologList[T](in:List[T]):PrologList[T] = new PrologList[T](in)
	implicit def pList2List[T](in:PrologList[T]):List[T] = in match {
		case PrologList(x) => x
	}
	implicit def str2List[T](in:String):List[T] = cutPList[T](in)
	implicit def str2PrologList[T](in:String):PrologList[T] = new PrologList[T](cutPList[T](in))
	implicit def str2Int(in:String):scala.Int = augmentString(in).toInt
	implicit def str2Double(in:String):scala.Double = augmentString(in).toDouble
	implicit def str2Bool(in:String):Boolean = {
		if(in == "TRUE") true
		else if (in == "FALSE") false
		else throw new Exception("Failure at trying to convert String to Boolean")
	}
}
