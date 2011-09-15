package scalog

import scala.util.parsing.combinator._
import alice.tuprolog._

/** Stellt eine Liste mit [1,2,3,...] als toString Methode bereit*/
case class PrologList[T](data:List[T]){
	override def toString:String = "[" + data.mkString(", ") + "]"
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
			case parser.Success(pO, s) => 	if(s.atEnd) pO.asInstanceOf[List[T]] //Bad because List[...] isn't checked for conversion errors..
							else throw new ParseException("expected end of list", s)
			case parser.NoSuccess(msg,x) => throw new ParseException(msg,x)
		}
	}

	implicit def list2PrologList[T](in:List[T]):PrologList[T] = PrologList[T](in)
	implicit def pList2List[T](in:PrologList[T]):List[T] = in match {
		case PrologList(x) => x
	}
	implicit def str2List[T](in:String):List[T] = cutPList[T](in)
	implicit def str2PrologList[T](in:String):PrologList[T] = PrologList[T](cutPList[T](in))
	implicit def str2Int(in:String):scala.Int = augmentString(in).toInt
	implicit def str2Double(in:String):scala.Double = augmentString(in).toDouble
	implicit def str2Long(in:String):scala.Long = augmentString(in).toLong
	implicit def str2Float(in:String):scala.Float = augmentString(in).toFloat
	implicit def str2Bool(in:String):Boolean = {
		if(in == "TRUE") true
		else if (in == "FALSE") false
		else throw new Exception("Failure at trying to convert String to Boolean")
	}
}	
