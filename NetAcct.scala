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
			val fields = line.split(" +");
			val date = NetAcct.UnixTimeStampToDateTime(fields(0).toLong())
			val from = NetAcct.parseEndPoint(fields(2), fields(3))
			val to = NetAcct.parseEndPoint(fields(4), fields(5))
			val dir = new Direction(from, to)
			val size = fields(6).toLong()
			new AccUnit(size, date, dir)
		}
	}
}

object NetAcct {
	def UnixTimeStampToDateTime( unixTimeStamp: Double ) : DateTime = {
		// Unix timestamp is seconds past epoch
		val dtDateTime = new DateTime(1970,1,1,0,0,0,0);
		dtDateTime.AddSeconds( unixTimeStamp ).ToLocalTime();
	}
	def parseEndPoint(host:String , port:String) = new EndPoint(Host parse host, port.toInt)
}