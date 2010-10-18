package trafacct;
import java.util.Date
import java.lang.{Exception, Throwable, NumberFormatException}
import scala.collection.mutable.Queue

class ParseError(message:String, reason:Throwable) extends Exception(message, reason)

class Ip(bytes: Array[Int]) {
	assert(bytes.length==4)
}

object Ip {
	def parseBytes(s:String): Array[Int] = {
		try {
			val fields = s.split("\\.")
			if (fields != 4)
				throw new ParseError("Wrong fields count: " + s, null)
			return fields.map( _.toInt )
		} catch {
			case e:NumberFormatException => throw new ParseError("Bad ip: "+s, e)
		}
	}
	def parse(s: String): Ip = new Ip(parseBytes(s))
}

class Host(hostname:String, ip:Ip)

object Host {
	def ip(hostname: String) = null
	def hostname(ip: Ip) = ip.toString()
	def parse(s:String): Host  = {
		try {
			val ip = Ip.parse(s)
			return new Host(hostname(ip), ip);
		} catch {
			case e:ParseError =>
		}
		return new Host(s, ip(s))
	}
}

class Endpoint(host:Host, port:Int)

object Endpoint {
	def parse(s:String) = {
		val hostPort = s.split(":")
		new Endpoint(Host.parse(hostPort(0)), hostPort(1).toInt)
	}
}

class Direction(from:Endpoint, to:Endpoint)

class AccUnit(size: Long, start: Date, direction:Direction)

trait AccSource extends Iterable[AccUnit]

class AccSourceCached extends private Queue[AccUnit] with AccSource {
	def ++=(from: AccSource) = super ++= from
}

