package scalog

import scala.util.parsing.input.Reader

class ParseException[T](msg:String, pos:Reader[T]) extends Exception(msg){
	private def printEnv[T](in:Reader[T], cnt:Int):String = {
		if(cnt == 0) ""
		else in.first + printEnv(in.rest, cnt - 1)
	}
	
	override def toString = {
		"Parsing failure: " + msg + "\nat:  " + pos.first + printEnv(pos.rest, 50) + "\n     ^"
	}
}

class ParamException extends Exception("Number of scala and prolog parameters do not match.")