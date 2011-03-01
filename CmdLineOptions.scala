package trafacct;
import jargs.gnu.CmdLineParser
import CmdLineParser.Option
import java.util.{Date, Locale}
import java.text.SimpleDateFormat
import java.lang.IllegalArgumentException

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

