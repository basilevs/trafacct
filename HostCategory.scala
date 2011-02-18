package trafacct;
import java.net.InetAddress
import scala.collection.mutable.{BufferProxy, ListBuffer}

trait HostCategory {
	def contains(host: Host): Boolean
}

object HostCategory {
	case class CategorizedAccUnit (src:HostCategory, dst:HostCategory, protocol:String) 
	trait Collection extends Iterable[HostCategory] with HostCategory {
		def contains(host: Host) = !forall(! _.contains(host))
		def getCategory(host: Host) = {
			val r =	find(_.contains(host))
			if (r.isEmpty) {
				null
			} else {
				r.get
			}
		}
		def process(unit: AccUnit) = {
			val src = getCategory(unit.src.host)
			val dst = getCategory(unit.dst.host)
			new CategorizedAccUnit(src, dst, unit.protocol)
		}
	}
	case class List extends BufferProxy[HostCategory] with Collection {
		val b = new ListBuffer[HostCategory]
		def self = b
	}
}

object SubNet {
	def byteToLong(i :Byte) = if (i >= 0) {i.toLong} else {256L + i}
	def bytesToLong(bytes: Seq[Byte]) = bytes.foldLeft(0L)((mask, byte) => (mask<<8)+byteToLong(byte))
	implicit def addressToLong(address: InetAddress) = bytesToLong(address.getAddress)
	def setBit(mask:Long, shift:Int) = mask | 1 << shift
	def maskLengthToMask(length: Int, byteCount:Int) = {
		val bits = byteCount*8
		(bits-length).until(bits).foldLeft(0L)(setBit)
	}
	def longToBytes(input: Long, count: Int):Seq[Byte] = {
		for (  i <- (count-1).until(-1, -1) )
			yield ((input >> i*8) & 255).toByte
	}
	implicit def bytesToString(bytes: Seq[Byte]) = bytes.map(byteToLong(_).toString).reduceLeft(_+"."+_)
	implicit def longToAddress(input:Long, count: Int) = InetAddress.getByAddress(longToBytes(input, count).toArray)
	implicit def stringToAddress(input: String) = InetAddress.getByName(input)
}

case class SubNet(ip:Long,  maskLength:Int, byteCount:Int) extends HostCategory {
	val mask = SubNet.maskLengthToMask(maskLength, byteCount)
	if ((ip & ~mask) != 0) throw new IllegalArgumentException("Subnet ip should be covered by its mask");
	def this(bytes: Seq[Byte], maskLength: Int) = this(SubNet.bytesToLong(bytes), maskLength, bytes.length)
	def this(address: InetAddress, maskLength: Int) = this(address.getAddress, maskLength)
	def this(address: String, maskLength: Int) = this(InetAddress.getByName(address), maskLength)
	def this(address: InetAddress) = this(address, address.getAddress.length*8)
	def contains(host: Host) = {
		if (host.ip != null) {
			val address = host.ip
			(mask & ip) == (mask & SubNet.addressToLong(address))
		} else {
			false
		}
	}
	override def toString = {
		import SubNet.bytesToString
		SubNet.longToBytes(ip, byteCount) +"/"+ maskLength
	}
}

case class SingleHost(host: Host) extends HostCategory {
	def contains(that: Host) = host == that
	override def toString = host.toString
}

class Msecn extends IterableProxy[HostCategory] with HostCategory.Collection {
	val subnets = Seq(
		new SubNet("213.199.148.0", 24),
		new SubNet("213.199.149.0", 24),
		new SubNet("65.54.93.0", 24),
		new SubNet("65.54.89.0", 24)
	)
	def self = subnets
	val hash = subnets.hashCode
	override def hashCode = hash
	override def equals(that: Any) = that.isInstanceOf[Msecn]
	override def toString = "*.msecn.net"
}

class Homenet extends IterableProxy[HostCategory] with HostCategory.Collection {
	val subnets = Seq(
		new SubNet("10.46.0.0", 16),
		new SubNet("10.49.0.0", 16),
		new SubNet("10.168.0.0", 16),
		new SubNet("109.174.0.0", 16)
	)
	def self = subnets
	override def toString = "HomeNet"
	val hash = subnets.hashCode
	override def hashCode = hash
	override def equals(that: Any) = that.isInstanceOf[Homenet]
}

class Akamai extends IterableProxy[HostCategory] with HostCategory.Collection {
	val subnets = Seq(
		new SubNet("92.123.65.0", 24),
		new SubNet("92.123.155.0", 24)
	)
	def self = subnets
	override def toString = "*.akamaitechnologies.com"
	val hash = subnets.hashCode
	override def hashCode = hash
	override def equals(that: Any) = that.isInstanceOf[Akamai]
}

class Nsu extends IterableProxy[HostCategory] with HostCategory.Collection {
	val subnets = Seq(
		new SubNet("10.3.0.0", 16)
	)
	def self = subnets
	override def toString = "NSU"
	val hash = subnets.hashCode
	override def hashCode = hash
	override def equals(that: Any) = that.isInstanceOf[Nsu]
}

class Categorization extends HostCategory.List {
	override def getCategory(host: Host) = {
		val c = super.getCategory(host)
		if (c != null) {
			c
		} else {
			val newHost = new SingleHost(host)
			newHost
		}
	}
}
