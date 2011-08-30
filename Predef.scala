package scalog

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
	/** Zerschneidet einen String in eine PrologListe
		@param s Eingangstring, der eine PrologListe enthält
		@return Extrahierte Liste*/
	def cutPList[T](s:String):List[T] = {
		def cut(s:String):List[T] = {
			if(augmentString(s)(0) == ',' || augmentString(s)(0) == ' ' || augmentString(s)(0) == '[') cut(s.substring(1))
			else if(augmentString(s)(0) == ']') Nil
			else augmentString(s)(0).asInstanceOf[T] :: cut(s.substring(1))
		}
		
		if(augmentString(s)(0) == '[' && augmentString(s)(augmentString(s).length - 1) == ']'){
			cut(s)  
		}else throw new Exception("Failure at converting string to list: wrong format.")
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
