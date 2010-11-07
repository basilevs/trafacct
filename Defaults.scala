package trafacct;
import jargs.gnu.CmdLineParser
import CmdLineParser.Option
import java.util.Date

//import scala.collection.mutable.HashSet
import java.io.File

trait Args {
	var start:Date = null
	var end:Date = null
	var limit = 50
	def parse(args:Array[String]) = {
		import DateTools._
		val parser = new CmdLineParser
		val startOpt = parser.addStringOption('s', "start")
		val endOpt = parser.addStringOption('e', "end")
		parser.parse(args)
		start = parseDate(parser.getOptionValue(startOpt).toString)
		end = parseDate(parser.getOptionValue(endOpt).toString)
		var rem = Set[String]()
		rem ++= parser.getRemainingArgs
		if (rem contains "today") {
			end = null
			start = dayStart(now)
		}
		if (rem contains "yesterday") {
			end = dayStart(now)
			start = dayBefore(end)
		}
	}
	def parseDate(s:String): Date = (if (s == null) null else new Date(s))
}

trait Configured extends Args {
	def main(args:Array[String]) {
		parse(args)
		run
	}
	def run
}

class Full extends Configured {
	case class Rule(src:Host, dst:Host, protocol:String) {
		def this(i:AccUnit) {this(i.src.host, i.dst.host, i.protocol)}
	}
	def run {
		val s = new Summator[Rule]((x:AccUnit) => new Rule(x))
		s.sum(new AccSources(Defaults.getSrcs))
		val data = s.toArray
		scala.util.Sorting.quickSort(data)
		data.slice(data.length-50).foreach(println("%s\t:%d".format(_._1.toString, _._2)))
	}
}

object Defaults {
def getSrcs: scala.collection.Set[AccSource] = {
	val rv = new scala.collection.mutable.HashSet[AccSource]
	
	var dir = new File("/var/log/net-acct/")
	if (dir.isDirectory)
		rv+=new NetAcct.Dir(dir)
	dir = new File("/var/log/squid3/")
	if (dir.isDirectory)
		rv+=new SquidDir(dir)
	rv
}

}

