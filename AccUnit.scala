package trafacct;
import java.util.Date
import java.lang.{Exception, Throwable, NumberFormatException}

class ParseError(message:String, reason:Throwable) extends Exception(message, reason)

class Ip(bytes: Array[Int]) {
	assert(bytes.length==4)
	def this(s: String) = this(Ip.parseBytes(s))
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
	def parse(s: String): Ip = new Ip(s)
}

class Host(hostname:String, ip:Ip)

object Host {
	def parse(s:String): Host  = {
		try {
			return new Host(null, Ip.parse(s));
		} catch {
			case e:ParseError => {}
		}
		return new Host(s, null)
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

class AccUnit(size: Int, start: Date, direction:Direction)
