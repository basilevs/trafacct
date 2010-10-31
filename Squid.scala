package trafacct
import java.net.URL
import FileOperations.{stringToURL, open}
import scala.collection.mutable.Queue
import java.io.{BufferedReader, File}
import java.util.{Date}
import java.net.InetAddress

/*
unix_time protocol_code src src_port dst dst_port size interface
*/
class Squid(reader: BufferedReader) extends AccSource {
	def elements = new Iterator[AccUnit] {
		def hasNext = reader.ready
		def next = {
			import NetAcct._
			import DateTools._
			import Endpoint._
			import Squid._
			val line = reader.readLine()
			val fields = line.split("[ \t]+")
			var url:URL = parseUrl(fields(6))
			val src = new Endpoint(url.getHost, url.getPort)
			val size = fields(4).toInt
			val dst = new Endpoint(fields(2), 0)
			val status=fields(3)
			var cached=false
			if (status.indexOf("HIT/") >= 0)
				cached=true
			new AccUnit(size, fields(0).toDouble, src, dst, url.getProtocol)
		}
	}
}

object Squid {
	def parseUrl(s:String): URL = {
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
	
	class Dir(dir:File) extends DirScanner(dir) {
		fileFilter = x => x.getName.matches(".*access.*log(\\.\\d\\d?)?(\\.gz)?$")
		def open(u:URL): AccSource = new Squid(FileOperations.open(u))
	}
}

