package trafacct
import java.net.{URI, URL}
import FileOperations.{stringToURL, open}
import scala.collection.mutable.Queue
import java.io.{BufferedReader, File}
import java.util.{Date}
import java.net.InetAddress
import java.util.regex.Pattern

/*
unix_time protocol_code src src_port dst dst_port size interface
*/
class Squid(reader: BufferedReader) extends AccSource {
	def elements = new Iterator[AccUnit] {
		def hasNext = reader.ready
		def next:AccUnit = {
			import NetAcct._
			import DateTools._
			import Endpoint._
			import Squid._
			val line = reader.readLine()
			if (line == null) 
				return null
			try {
				val fields = line.split("[ \t]+")
				var src:Endpoint = null
				if (fields(6).length <= 2)
					return null
				if (fields(2) == "127.0.0.1")
					return null
				var url = FastURL.strToFastUrl(fields(6))
				if (url.getHost==null)
					throw new ParseError("Malformed URI: "+url, null)
				src = new Endpoint(url.getHost, url.getPort)
				var prot = url.getProtocol
				if (prot == null)
					prot = fields(5)

				val size = fields(4).toInt
				val dst = new Endpoint(fields(2), 0)
				val status=fields(3)
			
				val cached = status.indexOf("HIT/") >= 0
				val protCached = prot + {if (cached) "C" else ""}
				new AccUnit(size, fields(0).toDouble, src, dst, protCached)
			} catch {
				case e:Exception => throw new ParseError("Unable to parse "+line, e)
			}
		}
	}.filter(accept)
}

case class FastURL(protocol:String, host:String, port:Int) {
	def getHost = host
	def getPort = port
	def getProtocol = protocol
	def setProtocol(p:String) = new FastURL(p, host, port)
}
object FastURL {
	val urlRe = Pattern.compile("^(?:(\\w+)://)?([\\w\\-\\d\\.]+)(?:(\\d+))?")
	implicit def strToFastUrl(s:String) = {
		val m = urlRe.matcher(s)
		if (m.find()) {
			var proto = m.group(1)		
			var port:Int = 0
			if (m.group(3) != null ) {
				port = m.group(3).toInt 
			} else {
				port = proto match {
					case "http" => 80
					case "ftp" => 21
					case "https" => 443
					case null => 0
				}
			}
			new FastURL(proto, m.group(2), port) 
		} else {
			throw new ParseError("Can't parse url: "+s, null)
		}
	}
}
object Squid {
/*	def parseUrl(s:String): URL = {
		for (method <- Set(parseUrl1(_), parseUrl2(_))) {
			try {
				return method(s)
			} catch {
				case e:java.net.MalformedURLException =>
			}
		}
		throw new ParseError("Unable to parse url: " + s, null)
	}
	
	def parseUrl1(s:String) = new URL(s)
	def parseUrl2(s:String) = new URL("http://"+s)
*/
	def parseUrl(s:String): FastURL = FastURL.strToFastUrl(s)
	class Dir(dir:File) extends DirScanner(dir) {
		fileFilter = x => x.getName.matches(".*access.*log(\\.\\d\\d?)?(\\.gz)?$")
		def open(u:URL): AccSource = new Squid(FileOperations.open(u))
	}
}

