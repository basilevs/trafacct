package trafacct;
import java.lang.{Exception, Throwable, NumberFormatException}
import java.net.{InetAddress, UnknownHostException}
import java.util.regex.Pattern

case class Host(name:String, val ip:InetAddress) {
	import Host.{addressToBytes, compareSeqs}
	def compare(that: Host):Int = {
		if (ip != null && that.ip != null) {
			return compareSeqs(ip, that.ip)
		}
		name.compare(that.name)
	}
	override def toString:String = if (ip!=null) {ip.getHostName} else {name}
}

object Host {
	implicit def addressToBytes(a:InetAddress):Array[Byte] = a.getAddress
	def compareSeqs(a:Seq[Byte], b:Seq[Byte]):Int = {
		val len = scala.Math.min(a.length, b.length)
		for (i <- 0 until len)
			if (a(i)!=b(i))
				return a(i).compare(b(i))
		a.length.compare(b.length)
	}
	implicit def stringToPattern(s:String): Pattern = Pattern.compile(s)
	val separators = Seq[Pattern]("\\.", ":")
	def separatedStrToBytes(s:String, separator:Pattern): Array[Byte] = {
		val fields = separator.split(s)
		var rv = new Array[Byte](fields.length);
		fields.map(_.toInt.toByte).copyToArray(rv, 0)
		rv
	}
	implicit def strToBytes(s:String): Array[Byte] = {
		try {
			for (sep <- separators)
				if (sep.matcher(s).find())
					return separatedStrToBytes(s, sep)
		} catch {
			case e:NumberFormatException => return null
		}
		null
	}
	implicit def strToHost(s:String):Host = {
		var name = s
		var ip:InetAddress = null
		try {
			val bytes = strToBytes(s)
			if (bytes != null) { 
				ip = InetAddress.getByAddress(bytes)
//				println( "parsed ip: "+s)
			}
//			if (ip == null)
//				ip = InetAddress.getByName(s)
		} catch {
			case e:UnknownHostException =>
		}
		new Host(name, ip)
	}
}


