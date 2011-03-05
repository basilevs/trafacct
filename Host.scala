package trafacct;
import java.lang.{Exception, Throwable, NumberFormatException}
import java.net.{InetAddress, UnknownHostException}
import java.util.regex.Pattern

case class Host(name:String, val ip:InetAddress) {
	import Host.{addressToBytes, compareSeqs, bytesToString}
	val performSlowComparison = false
	if (name == null && ip == null)
		throw new IllegalArgumentException("Both arguments are null")
	override def equals(that: Any) = if (that==null) false else  that.asInstanceOf[Host].compare(this) == 0
	override def hashCode = {
		if (ip != null)
			ip.hashCode
		else if (name != null)
			name.hashCode
		else 
			throw new IllegalArgumentException("Both name and ip are null")
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
			if (this.ip == null)
				-1
			else 
				1
		}
	}
	def resolve: Host = {
		assert(name !=null || ip != null)
		if (ip == null) {
			new Host(name, InetAddress.getByName(name))
		} else if ( name == null) {
			new Host(ip.getHostName, ip)
		} else {
			this
		}
	}
	def ipString: String = bytesToString(addressToBytes(ip))
	
	def humanReadable: String = 
		if (name == null)
			ip.getHostName
		else name
		
	override def toString:String = 
		if (name == null)
			ipString
		else name
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
	def byteToLong(i :Byte) = if (i >= 0) {i.toLong} else {256L + i}
	implicit def bytesToString(bytes: Seq[Byte]) = {
		val separator = if (bytes.length == 6) "::" else "."
		bytes.map(byteToLong(_).toString).reduceLeft(_+separator+_)
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
	def strToInetAddress(input:String) = {
			val bytes = strToBytes(input)
			if (bytes == null)
				throw new ParseError("Is not an ip address: "+input)
			bytes.length match {
				case 4 =>
				case 6 =>
				case _ => throw new ParseError("Can't parse ip "+input)
			}
			InetAddress.getByAddress(bytes)
	}
	implicit def strToHost(s:String):Host = {
		assert(s!=null)
		var name:String = null
		var ip:InetAddress = null
		try {
			val bytes = strToBytes(s)
			if (bytes != null) { 
				ip = strToInetAddress(s)
			} else {
				name = s
			}
		} catch {
			case e:UnknownHostException => name = s
		}
		new Host(name, ip)
	}
}


