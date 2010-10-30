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
			val line = reader.readLine()
			val fields = line.split("[ \t]+")
			val url = new URL(fields(6))
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

class SquidDir(dir:File) extends DirScanner(dir) {
	fileFilter = x => x.getName.matches(".*access.*log(\\.\\d\\d?)?(\\.gz)?$")
	def open(u:URL): AccSource = new Squid(FileOperations.open(u))
}

