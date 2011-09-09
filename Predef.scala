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
	
	//Term implicits... they are in work and not used at the moment
	implicit def term2Bool(x:Term):Boolean = {
		if(x == Term.TRUE) true
		else if(x == Term.FALSE) false
		else throw new ConvException("tuprolog.Term","Boolean")
	}
	
	implicit def term2Int(x:Term):scala.Int = x match {
		case n:Number => 	if(n.isInstanceOf[Int]) n.intValue
					else throw new ConvException("tuprolog.Term","Int")
		case _ => throw new ConvException("tuprolog.Term","Int")
	}
	
	implicit def term2Long(x:Term):scala.Long = x match {
		case n:Number =>	if(n.isInstanceOf[Long]) n.longValue
					else if(n.isInstanceOf[Int]) n.intValue.toLong
					else throw new ConvException("tuprolog.Term","Long")
		case _ => throw new ConvException("tuprolog.Term","Long")
	}
	
	implicit def term2Double(x:Term):scala.Double = x match {
		case n:Number => 	if(n.isInstanceOf[Double]) n.doubleValue
					else if(n.isInstanceOf[Float]) n.floatValue.toDouble
					else throw new ConvException("tuprolog.Term","Double")
		case _ => throw new ConvException("tuprolog.Term","Double")
	}
	
	implicit def term2Float(x:Term):scala.Float = x match {
		case n:Number => 	if(n.isInstanceOf[Float]) n.floatValue
					else throw new ConvException("tuprolog.Term","Float")
		case _ => throw new ConvException("tuprolog.Term","Float")
	}

	implicit def term2Num(x:Term):Number = x match {
		case n:Number => n
	}
	
	implicit def term2Struct(x:Term):Struct = x match {
		case n:Struct => n
	}
	
	implicit def term2Str(x:Term):String = x.toString
	
	implicit def term2Sth[T](x:Term):Any = x match {
		case n:Number => 	if(n.isInstanceOf[Int]) n.intValue
					else if(n.isInstanceOf[Double]) n.doubleValue
					else if(n.isInstanceOf[Long]) n.longValue
					else if(n.isInstanceOf[Float]) n.floatValue
					else throw new ConvException("tuprolog.Term","Number")

		case n:Struct =>	if(n.getArity == 0) n.toString
					else if(n.getArity == 2) term2List(n)
					else throw new ConvException("tuprolog.Term","String")
		case _ => throw new ConvException("tuprolog.Term","Any")
	}
	
	implicit def term2List[T](x:Term):List[T] = x match {
		case n:Struct => 	if(n.isList){
						if(n.isEmptyList) List[T]()
						else parseHead[T](n.listHead) :: term2List(n.listTail)
					}else throw new ConvException("tuprolog.Term","List")
		case _ => throw new ConvException("tuprolog.Term","List")
	}
	
	def parseHead[T](x:Term):T = x match {
		case x:Number => 	if(n.isInstanceOf[T] && n.isInstanceOf[Int]) n.intValue
					else if(n.isInstanceOf[T] && n.isInstanceOf[Double]) n.doubleValue
					else if(n.isInstanceOf[T] && n.isInstanceOf[Float]) n.floatValue
					else if(n.isInstanceOf[T] && n.isInstanceOf[Long]) n.longValue
		case x:Struct =>	if(n.isAtomic) 
		case _ => throw new ConvException("tuprolog.Term","T")
	}
	
	//implicit def term2Array[T:ClassManifest](x:Term):Array[T] = term2List[T](x).toArray
}
