package trafacct;
import scala.collection.mutable.ArrayBuffer

class PrettyPrinter {
	import PrettyPrinter._
	private val widthArray = new ArrayBuffer[Int]
	def format(input:String*):String = {
		while (widthArray.length < input.length)
			widthArray += 0
		var rv=""
		for (i <- 0 to input.length-1) {
			val in = if (input(i) == null) "null" else input(i)
			val width = Math.max(widthArray(i), in.length+1)
			widthArray(i) = width
			rv = rv + spaces(width - in.length) + in
		}
		rv
	}
}

object PrettyPrinter {
	def spaces(len:Int):String = {
		var rv = ""
		while (rv.length < len)
			rv+=" "
		rv
	}
}
