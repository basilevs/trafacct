package trafacct;
import java.lang.{Exception, Throwable, NumberFormatException}
import java.net.{InetAddress, UnknownHostException}
import java.util.regex.Pattern

case class Host(name:String, val ip:InetAddress) {
	import Host.{addressToBytes, compareSeqs}
	if (name == null && ip == null)
		throw new IllegalArgumentException("Both arguments are null")
	override def equals(that: Any) = if (that==null) false else  that.asInstanceOf[Host].compare(this) == 0
	override def hashCode = {
		if (ip != null)
			ip.hashCode
		else if (name != null)
			name.hashCode
		else 
			assert(false)
	}
	def compare(that: Host) = {
		val rv = compare1(that)
//		val res = if (rv == 0) "==" else if (rv < 0) "<" else ">"
//		print(this + " " + res + " " + that + "\n")
		rv
	}
	def compare1(that: Host):Int = {
		if (ip != null && that.ip != null) {
			compareSeqs(ip, that.ip)
		} else if (name != null && that.name != null) {
			name.compare(that.name)
		} else {
			print ("Warning: slow comparison for " + this + " and " + that)
			val a = if (this.ip == null) this.resolve else this
			val b = if (that.ip == null) that.resolve else that
			compareSeqs(ip, that.ip)
		}
	}
	def resolve: Host = {
		assert(name !=null || ip != null)
		if (ip == null) {
			new Host(name, InetAddress.getByName(name))
		} else {
			new Host(ip.getHostName, ip)
		}
	}
	override def toString:String = if (name == null) {ip.getHostName} else {name}
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
		assert(s!=null)
		var name:String = null
		var ip:InetAddress = null
		try {
			val bytes = strToBytes(s)
			if (bytes != null) { 
				ip = InetAddress.getByAddress(bytes)
			} else {
				name = s
			}
		} catch {
			case e:UnknownHostException => name = s
		}
		new Host(name, ip)
	}
}


