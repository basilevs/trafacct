package trafacct;
import jargs.gnu.CmdLineParser
import CmdLineParser.Option
import java.util.Date
import java.text.SimpleDateFormat
import java.lang.IllegalArgumentException
import scala.xml.NodeSeq
import scala.collection.jcl.Conversions._


//import scala.collection.mutable.HashSet
import java.io.File

import Summator.compareBySecond

trait Configuration {
	import Configuration._
	var start:Date = null
	var end:Date = null
	var limit = 50
	def rh(host:Host) = host.resolve
	var skipHosts = Set[Host](Seq[Host]("10.3.0.1", "10.0.0.1").map(rh): _*)
	var selectHosts:Set[Host] = null
	var sources = Configuration.getSrcs
	var humanReadable = false

	def formatBytes(bytes:Long): String =
		if (humanReadable) 
			PrettyPrinter.bytesToHumanReadable(bytes) 
		else bytes.toString
	def format(h:Host):String =
		if (humanReadable) 
			h.humanReadable
		else h.toString
	def parse(args:Array[String]): Seq[String] = {
		import DateTools._
		val parser = new CmdLineParser
		val startOpt = parser.addOption(new DateOption('s', "start"))
		val endOpt = parser.addOption(new DateOption('e', "end"))
		val dateOpt = parser.addOption(new DateOption('d', "date"))
		val humanReadableOption = parser.addBooleanOption('a', "human-readable")
		val selectHostOpt = parser.addOption(new HostOption('h', "host"))
		parser.parse(args)
		humanReadable = parser.getOptionValue(humanReadableOption).asInstanceOf[Boolean]
		var d = parser.getOptionValue(dateOpt).asInstanceOf[Date]
		if (d != null) {
			start = d
			end = addDays(d, 1)
		}
		d = parser.getOptionValue(startOpt).asInstanceOf[Date]
		if (d != null)
			start = d
		d = parser.getOptionValue(endOpt).asInstanceOf[Date]
		if (d != null)
			end = d
		val hosts = parser.getOptionValues(selectHostOpt).map(_.asInstanceOf[Host])
		if (hosts.length > 0) {
			val was = if (selectHosts != null) selectHosts else Set[Host]()
			selectHosts = was ++ Set[Host](hosts: _*)
		}
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
	def configure(i:AccSource) {
		i.start = start
		i.end = end
		i.skipHosts = skipHosts
		i.selectHosts = selectHosts
	}
	def toXml =
		<traffact:Configuration>
			<start>{start}</start>
			<end>{end}</end>
			<skipHosts>
				{hostsToXml(skipHosts)}
			</skipHosts>
			<selectHosts>
				{hostsToXml(selectHosts)}
			</selectHosts>
		</traffact:Configuration>
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
			rv+=new Squid.Dir(dir)
		rv
	}
	def parseHost(optVal:AnyRef) : Host = {
		if (optVal == null)
			return null
		val h = Host.strToHost(optVal.toString).resolve
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
	def hostToXml(host: Host) =
		if (host.ip != null && host.name != null)
			<host ip={host.ip.toString} name={host.name} />
		else if (host.ip == null)
			<host name={host.name} />
		else if (host.name == null)
			<host ip={host.ip.toString} />
		else 
			throw new RuntimeException("Both name and ip are null for host object")				
			
	def hostsToXml(hosts: Seq[Host]): NodeSeq  =
		for (host <- hosts) yield
			hostToXml(host)

	def hostsToXml(hosts: Set[Host]): NodeSeq  = {
		if (hosts!=null)
			hostsToXml(hosts.toSeq)
		else
			hostsToXml(Seq[Host]())
	}
	implicit def vectorToSeq(v: java.util.Vector[AnyRef]): Seq[AnyRef] = v
}

