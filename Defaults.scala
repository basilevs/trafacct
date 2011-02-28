package trafacct;
import jargs.gnu.CmdLineParser
import CmdLineParser.Option
import java.util.Date
import java.text.SimpleDateFormat
import java.lang.IllegalArgumentException

//import scala.collection.mutable.HashSet
import java.io.File

import Summator.compareBySecond

object NoSum {
	val runner = new Configured {
		def run = {
			val srcs = new AccSources(sources)
			configure(srcs)
			val ab = new scala.collection.mutable.ArrayBuffer[(Host, Long)]
			var count = 0
			for (i <- srcs) {
				ab+=(i.src.host, i.size)
				ab.trimStart(ab.length-limit)
				count += 1
			}
			ab.foreach(println)
			println("Total units: "+count)
			0
		}
	}
	def main(args:Array[String]) {
		runner.main(args)
	}
}



object Destination {
	case class Rule(dst:Host, protocol:String) {
		def this(i:AccUnit) {this(i.dst.host, i.protocol)}
	}
	type AccResult = (Rule, Long)
	case class Comparator(a:AccResult) extends Ordered[AccResult] {
		def compare(that:AccResult) = compareBySecond(a, that)
	}
	val runner = new Configured {
		def run = {
			val s = new Summator[Rule]((x:AccUnit) => new Rule(x))
			val srcs = new AccSources(sources)
			configure(srcs)
			s.sum(srcs)
			val data = s.toArray
			scala.util.Sorting.quickSort(data)(new Comparator(_))
			val pp = new PrettyPrinter
			def printAcc(i:AccResult) {
				implicit def hostToStr(i:Host) = i.toString
				println(pp.format(i._1.dst, i._1.protocol, i._2.toString))
			}
			data.slice(data.length-limit).foreach(printAcc)
			0
		}
	}
	def main(args:Array[String]) {
		runner.main(args)
	}
}
