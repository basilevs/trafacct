package trafacct;
import jargs.gnu.CmdLineParser
import CmdLineParser.Option
import java.util.Date
import java.text.SimpleDateFormat
import java.lang.IllegalArgumentException

//import scala.collection.mutable.HashSet
import java.io.File

import Summator.compareBySecond

trait Configuration {
	var start:Date = null
	var end:Date = null
	var limit = 50
	var skipHosts = Set[Host]("10.3.0.1", "10.0.0.1")
	var selectHosts:Set[Host] = null
	var sources = Configuration.getSrcs
	def parse(args:Array[String]): Seq[String] = {
		import DateTools._
		val parser = new CmdLineParser
		val startOpt = parser.addStringOption('s', "start")
		val endOpt = parser.addStringOption('e', "end")
		val selectHostOpt = parser.addStringOption('h', "host")
		parser.parse(args)
		start = parseDate(parser.getOptionValue(startOpt))
		end = parseDate(parser.getOptionValue(endOpt))
		
		while (parseHost(parser.getOptionValue(selectHostOpt))!=null) {}
		var rem = Seq[String]()
		for (arg <- parser.getRemainingArgs) {
			arg match {
				case "today" => {
					end = null
					start = dayStart(now)
				}
				case "yesterday" => {
					end = dayStart(now)
					start = dayBefore(end)
				}
				case "week" => {
					end = now
					start = weekBefore(end)
				}
				case _ => rem = rem ++ Seq(arg)
			}
		}
		rem
	}
	def parseHost(optVal:AnyRef) : Host = {
		if (optVal == null)
			return null
		val h = Host.strToHost(optVal.toString).resolve
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

trait Configured extends Configuration {
	def main(args:Array[String]) = {
		val rem = parse(args)
		if (rem.length > 0) {
			val strings = rem.map(_.toString)
			val string = strings.reduceLeft(_ + "," + _)
			throw new IllegalArgumentException("There were illegal arguments: "+string)
		}
		run
	}
	def run:Int
}


object Configuration {
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

