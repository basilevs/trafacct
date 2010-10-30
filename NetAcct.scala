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
class NetAcct(reader: BufferedReader) extends AccSource {
	def elements = new Iterator[AccUnit] {
		def hasNext = reader.ready
		def next = {
			val line = reader.readLine()
			val fields = line.split("[ \t]+");
			import NetAcct._
			val date = UnixTimeStampToDateTime(fields(0).toLong)
			val from = parseEndPoint(fields(2), fields(3))
			val to = parseEndPoint(fields(4), fields(5))
			val size:Long = fields(6).toLong
			new AccUnit(size, date, from, to, fields(1))
		}
	}
}

object NetAcct {
	def UnixTimeStampToDateTime( unixTimeStamp: Long ) : Date = {
		// Unix timestamp is seconds past epoch
		new Date(unixTimeStamp * 1000);
	}
	import Endpoint.hostToInetAddress
	def parseEndPoint(host:String , port:String) = new Endpoint(host, port.toInt)
	class Dir(dir:File) extends DirScanner(dir) {
		fileFilter = x => x.getName.matches(".*net.*log(\\.\\d\\d?)?(\\.gz)?$")
		def open(u:URL): AccSource = new NetAcct(FileOperations.open(u))
	}
}
