package trafacct
import java.net.URL
import FileOperations.{stringToURL, open}
import scala.collection.mutable.Queue
import java.io.BufferedReader
import java.util.Date
/*
unix_time protocol_code src src_port dst dst_port size interface
*/
class NetAcct(file: URL, interfaces: Collection[String]) extends AccSource {
	var reader = open(file)
	var elements = new Iterator[AccUnit] {
		def hasNext = reader.ready
		def next = {
			val line = reader.readLine()
			val fields = line.split("[ \t]+");
			val date = NetAcct.UnixTimeStampToDateTime(fields(0).toLong)
			val from = NetAcct.parseEndPoint(fields(2), fields(3))
			val to = NetAcct.parseEndPoint(fields(4), fields(5))
			val dir = new Direction(from, to)
			val size:Long = fields(6).toLong
			new AccUnit(size, date, dir)
		}
	}
}

object NetAcct {
	def UnixTimeStampToDateTime( unixTimeStamp: Long ) : Date = {
		// Unix timestamp is seconds past epoch
		new Date(unixTimeStamp * 1000);
	}
	def parseEndPoint(host:String , port:String) = new Endpoint(Host parse host, port.toInt)
}