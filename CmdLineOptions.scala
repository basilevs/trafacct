package trafacct;
import jargs.gnu.CmdLineParser
import CmdLineParser.Option
import java.util.{Date, Locale}
import java.text.SimpleDateFormat
import java.lang.IllegalArgumentException
import scala.collection.jcl.Conversions._

class DateOption(short:Char, long:String) extends Option(short, long, true) {
	override def parseValue(arg:String, locale:Locale) = Configuration.parseDate(arg)
}

class HostOption(short:Char, long:String) extends Option(short, long, true) {
	override def parseValue(arg:String, locale:Locale):AnyRef  = {
		val host = Host.strToHost(arg)
		try {
			return host.resolve
		} catch {
			case e:java.net.UnknownHostException => return host
		}
	}
}

object CmdLine {
	def parse(c:Configuration, args:Array[String]): Seq[String] = {
		import DateTools._
		val parser = new CmdLineParser
		val configOpt = parser.addStringOption('c', "config")
		val startOpt = parser.addOption(new DateOption('s', "start"))
		val endOpt = parser.addOption(new DateOption('e', "end"))
		val dateOpt = parser.addOption(new DateOption('d', "date"))
		val humanReadableOption = parser.addBooleanOption('a', "human-readable")
		val selectHostOpt = parser.addOption(new HostOption('h', "host"))
		val dumpConfigOpt = parser.addBooleanOption("dump-configuration")
		parser.parse(args)
		var configFileName = parser.getOptionValue(configOpt).asInstanceOf[String]
		if (configFileName != null) {
			Configuration.applyXML(c, scala.xml.XML.loadFile(configFileName))
		}
		
		c.humanReadable = parser.getOptionValue(humanReadableOption).asInstanceOf[Boolean]
		var d = parser.getOptionValue(dateOpt).asInstanceOf[Date]
		if (d != null) {
			c.start = d
			c.end = addDays(d, 1)
		}
		d = parser.getOptionValue(startOpt).asInstanceOf[Date]
		if (d != null)
			c.start = d
		d = parser.getOptionValue(endOpt).asInstanceOf[Date]
		if (d != null)
			c.end = d
		val hosts = parser.getOptionValues(selectHostOpt).map(_.asInstanceOf[Host])
		if (hosts.length > 0) {
			val was = if (c.selectHosts != null) c.selectHosts else Set[Host]()
			c.selectHosts = was ++ Set[Host](hosts: _*)
		}
		var rem = Seq[String]()
		for (arg <- parser.getRemainingArgs) {
			arg match {
				case "today" => {
					c.end = null
					c.start = dayStart(now)
				}
				case "yesterday" => {
					c.end = dayStart(now)
					c.start = dayBefore(c.end)
				}
				case "week" => {
					c.end = now
					c.start = weekBefore(c.end)
				}
				case _ => rem = rem ++ Seq(arg)
			}
		}
		if (parser.getOptionValue(dumpConfigOpt).asInstanceOf[Boolean]) {
			val pp = new scala.xml.PrettyPrinter(200, 2)
			print(pp.format(c.toXml))
			c.sources=Set()
		}
		rem
	}
}