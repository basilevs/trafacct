package trafacct;
import jargs.gnu.CmdLineParser
import CmdLineParser.Option
import java.util.Date
import java.text.SimpleDateFormat
import java.lang.IllegalArgumentException

//import scala.collection.mutable.HashSet
import java.io.File

import Summator._

trait Args {
	var start:Date = null
	var end:Date = null
	var limit = 50
	var skipHosts = Set[Host]("10.3.0.1", "10.0.0.1")
	var selectHosts:Set[Host] = null
	def parse(args:Array[String]) = {
		import DateTools._
		val parser = new CmdLineParser
		val startOpt = parser.addStringOption('s', "start")
		val endOpt = parser.addStringOption('e', "end")
		val selectHostOpt = parser.addStringOption('h', "host")
		parser.parse(args)
		start = parseDate(parser.getOptionValue(startOpt))
		end = parseDate(parser.getOptionValue(endOpt))
		
		while (parseHost(parser.getOptionValue(selectHostOpt))!=null) {}
		var rem = Set[String]()
		rem ++= parser.getRemainingArgs
		if (rem contains "today") {
			end = null
			start = dayStart(now)
		} else if (rem contains "yesterday") {
			end = dayStart(now)
			start = dayBefore(end)
		}
	}
	def parseHost(optVal:AnyRef) : Host = {
		if (optVal == null)
			return null
		val h = Host.strToHost(optVal.toString)
		if (h == null)
			return null
		if (selectHosts == null) {
			selectHosts = Set(h)
		} else {
			selectHosts += h
		}
		h
	}
	def parseDate(optVal:AnyRef): Date = {
		if (optVal==null)
			return null
		parseDate(optVal.toString)
	}
	def parseDate(s:String): Date = {
		val format = new SimpleDateFormat("yyyy-MM-dd")
		if (s == null) null else  {
			try {
				format.parse(s)
			}catch {
				case e:IllegalArgumentException => throw new ParseError("Can't parse "+s, e)
			}
		}
	}
	def configure(i:AccSource) {
		i.start = start
		i.end = end
		i.skipHosts = skipHosts
		i.selectHosts = selectHosts
	}
}

trait Configured extends Args {
	def main(args:Array[String]) = {
		parse(args)
		run
	}
	def run:Int
}

object Full {
	case class Rule(src:Host, dst:Host, protocol:String) {
		def this(i:AccUnit) {this(i.src.host, i.dst.host, i.protocol)}
	}
	type AccResult = (Rule, Long)
	case class Comparator(a:AccResult) extends Ordered[AccResult] {
		def compare(that:AccResult) = compareBySecond(a, that)
	}
	val runner = new Configured {
		def run = {
			val s = new Summator[Rule]((x:AccUnit) => new Rule(x))
			val srcs = new AccSources(Defaults.getSrcs)
			configure(srcs)
			s.sum(srcs)
			val data = s.toArray
			scala.util.Sorting.quickSort(data)(new Comparator(_))
			val pp = new PrettyPrinter
			def printAcc(i:AccResult) {
				implicit def hostToStr(i:Host) = i.toString
				println(pp.format(i._1.src, i._1.dst, i._1.protocol, i._2.toString))
			}
			data.slice(data.length-limit).foreach(printAcc)
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
			val srcs = new AccSources(Defaults.getSrcs)
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

