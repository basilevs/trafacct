package trafacct;
import jargs.gnu.CmdLineParser
import CmdLineParser.Option
import java.util.{Date, Locale}
import java.text.SimpleDateFormat
import java.io.File
import java.lang.IllegalArgumentException
import scala.collection.JavaConversions._


import scala.util.control.Exception.ignoring


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

class HostCategoryOption(short:Char, long:String) extends Option(short, long, true) {
	def parseHost(arg:String):HostCategory = {
		try {
			return new SingleHost(Host.strToHost(arg).resolve)
		} catch {
			case e:ParseError => return null
			case e:java.net.UnknownHostException => return null
		}
	}
}

object CmdLine {
	def parseHost(arg:String):HostCategory = {
		try {
			return new SingleHost(Host.strToHost(arg).resolve)
		} catch {
			case e:ParseError => return null
			case e:java.net.UnknownHostException => return null
		}
	}
	def searchInNamed(arg:String):HostCategory = {
		for (i <- AllCategories) {
			if (arg == i.toString)
				return i
		}
		return null
	}
	def parseHostCategory(arg:String):HostCategory = {
		for (i <- Seq(searchInNamed(_), parseHost(_))) {
			val category = i(arg)
			if (category != null)
				return category
		}
		val allString = AllCategories.map(_.toString).reduceLeft(_+", "+_)
		throw new ParseError("Can't parse host category "+arg+". Available named categories are: "+allString)
	}

	def parse(c:Configuration, args:Array[String]): Seq[String] = {
		import DateTools._
		ignoring(classOf[NumberFormatException]) {
			c.limit = math.abs(System.getenv("LINES").toInt)
		} 
		val parser = new CmdLineParser
		val configOpt = parser.addStringOption('c', "config")
		val startOpt = parser.addOption(new DateOption('s', "start"))
		val endOpt = parser.addOption(new DateOption('e', "end"))
		val dateOpt = parser.addOption(new DateOption('d', "date"))
		val humanReadableOption = parser.addBooleanOption('a', "human-readable")
		val selectHostOpt = parser.addStringOption('h', "host")
		parser.parse(args)
		var configFileNames = parser.getOptionValues(configOpt).map(_.toString)
		def loadFile(fileName:String) {Configuration.applyXML(c, scala.xml.XML.loadFile(fileName))}
		if (configFileNames.length > 0 ) {
			configFileNames.foreach(loadFile)
		} else {
			for (file <- Seq(new File (new File (System.getProperty("user.home"), ".trafacct"), "config.xml"), new File("config.xml"))) {
				try {
					val f = scala.xml.XML.loadFile(file)
					Configuration.applyXML(c, f)
				} catch {
					case e:java.io.FileNotFoundException => 
				}
			}
		}
		if (c.sources.size == 0)
			c.sources = Configuration.getSrcs
		
		
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
		val hosts = parser.getOptionValues(selectHostOpt).map(_.asInstanceOf[String])
		if (hosts.length > 0) {
			c.select = HostCategory.Collection(hosts.map(parseHostCategory))
		}
		var rem = Seq[String]()
		for (arg <- parser.getRemainingArgs) {
			arg match {
				case "hour" => {
					c.end = now
					c.start = addHours(c.end, -1)
				}
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
				case "month" => {
					c.end = now
					c.start = addDays(c.end, -30)
				}
				case _ => rem = rem ++ Seq(arg)
			}
		}
		rem
	}
}
