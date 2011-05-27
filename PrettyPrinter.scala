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
			val width = math.max(widthArray(i), in.length+1)
			widthArray(i) = width
			rv = rv + spaces(width - in.length) + in
		}
		rv
	}
}

object PrettyPrinter {
	implicit def bytesToHumanReadable(l:Long) = longToHumanReadable(l, 1024, Seq("", "k", "M", "G"))
	implicit def longToHumanReadable(l:Long, base:Int, names:Seq[String]):String  = {
		var d = l.toDouble
		for (name <- names) {
			if (math.abs(d/base) < 1.) {
				return "%.1f".format(d)+name
			}
			d /= base
		}
		return l.toString
	}
	def spaces(len:Int):String = {
		" " * len
	}
}
