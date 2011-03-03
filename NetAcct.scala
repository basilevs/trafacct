package trafacct
import java.net.URL
import FileOperations.{stringToURL, open}
import scala.collection.mutable.Queue
import java.io.{BufferedReader, File}
import java.util.{Date}

/*
unix_time protocol_code src src_port dst dst_port size interface
*/
class NetAcct(reader: BufferedReader) extends AccSource {
	def elements = new Iterator[AccUnit] {
		var reader = NetAcct.this.reader
		def hasNext = if (reader == null) false else reader.ready
		def next:AccUnit = {
			assert(hasNext)
			val line = reader.readLine()
			if (line==null) {
				reader = null
				return null
			}
			val fields = line.split("[ \t]+");
			import NetAcct._
			val date = DateTools.timeStampToDate(fields(0).toDouble)
			val from = parseEndPoint(fields(2), fields(3))
			val to = parseEndPoint(fields(4), fields(5))
			val size:Long = fields(6).toLong
			if (end != null && date.compareTo(end) > 0) {
				// We are stopping reading too new files as there is no way to read older data futher in this new file
				reader = null
				null
			} else {
				new AccUnit(size, date, from, to, fields(1))
			}
		}
	}.filter(accept)
}

object NetAcct {
	def parseEndPoint(host:String , port:String) = new Endpoint(host, port.toInt)
	case class Dir(dir:File) extends DirScanner(dir) {
		fileFilter = x => x.getName.matches(".*net.*log(\\.\\d\\d?)?(\\.gz)?$")
		def open(u:URL): AccSource = new NetAcct(FileOperations.open(u))
	}
}
