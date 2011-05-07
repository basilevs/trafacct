package trafacct;
import java.net.InetAddress
import scala.collection.mutable.{BufferProxy, HashSet}
import scala.collection.IterableProxy

trait HostCategory {
	def contains(host: Host): Boolean
	def humanReadable = toString
	def getCategory(host: Host):HostCategory = null // Returns a subcategory
	def isBuiltin = true
}

object HostCategory  {
	trait Collection extends Iterable[HostCategory] with HostCategory {
		def contains(host: Host) = !forall(! _.contains(host))
		override def getCategory(host: Host) = {
			val r =	find(_.contains(host))
			if (r.isEmpty) {
				null
			} else {
				r.get
			}
		}
	}
	object Collection {
		def apply(iterable:Iterable[HostCategory]) = {
			new Collection with IterableProxy[HostCategory] {
				val self = iterable
			}
		}
		def empty = new Collection with IterableProxy[HostCategory] {
			val self = Seq()
		}
	}
	class Set(name:String) extends HashSet[HostCategory] with Collection	{
		def this() = this(null)
		override def isBuiltin = false
		override def toString = if (name!= null) name else super.toString
	}
}

object AllCategories extends IterableProxy[HostCategory] with HostCategory.Collection {
	private val allCategories = new HostCategory.Set
	val self = allCategories
	def register(c:HostCategory) {
		if (allCategories.find(_.toString == c.toString).isDefined)
			return
		allCategories + c
	}
	override def getCategory(host: Host) = allCategories.getCategory(host)
	allCategories ++= Set(Homenet, Akamai, NSU, Google, ChoopaCom, Msecn, UpdateMicrosoftCom)
	assert(allCategories.size>0)
}

object SubNet {
	def bytesToLong(bytes: Seq[Byte]) = bytes.foldLeft(0L)((mask, byte) => (mask<<8)+Host.byteToLong(byte))
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
	implicit def longToAddress(input:Long, count: Int) = InetAddress.getByAddress(longToBytes(input, count).toArray)
	implicit def stringToAddress(input: String) = InetAddress.getByName(input)
	def stringToAddressAndMaskLength(input:String) = {
		try {
			val pos = input.indexOf("/")
			if (pos < 0)
				throw new ParseError("No separator", null)
			val length = input.substring(pos+1).toInt
			(input.substring(0, pos), length)
		} catch {
			case e:Exception => throw new ParseError("Can't parse SubNet "+input, e)
		}
	}
}

case class SubNet(ip:Long,  maskLength:Int, byteCount:Int) extends HostCategory {
	import SubNet.longToBytes
	import Host.bytesToString
	val mask = SubNet.maskLengthToMask(maskLength, byteCount)
	if ((ip & ~mask) != 0) throw new IllegalArgumentException("Subnet ip " + ip + "is not covered by its mask " + longToBytes(mask, byteCount));
	def this(bytes: Seq[Byte], maskLength: Int) = this(SubNet.bytesToLong(bytes), maskLength, bytes.length)
	def this(address: InetAddress, maskLength: Int) = this(address.getAddress, maskLength)
	def this(address: String, maskLength: Int) = this(InetAddress.getByName(address), maskLength)
	def this(address: InetAddress) = this(address, address.getAddress.length*8)
	def this(input:(String, Int)) = this(input._1, input._2)
	def this(subnet:String) = this(SubNet.stringToAddressAndMaskLength(subnet))
	def contains(host: Host) = {
		if (host.ip != null) {
			val address = host.ip
			(mask & ip) == (mask & SubNet.addressToLong(address))
		} else {
			false
		}
	}
	def ipString = Host.bytesToString(longToBytes(ip, byteCount))
	override def isBuiltin = false
	override def toString = {
		longToBytes(ip, byteCount) +"/"+ maskLength
	}
}

case class Domain(suffix:String) extends HostCategory {
	def contains(host: Host) = {
		if (host.name == null) {
			false
		} else {
			host.name.endsWith(suffix)
		}
	}	
	override def toString = {
		"*"+suffix
	}
	override def isBuiltin = false
}

case class SingleHost(host:Host) extends HostCategory {
	def contains(that: Host) = host == that
	override def toString = host.toString
	override def humanReadable = host.humanReadable
	override def isBuiltin = false
}

object Msecn extends IterableProxy[HostCategory] with HostCategory.Collection {
	val subnets = Seq(
		new SubNet("213.199.148.0", 24),
		new SubNet("213.199.149.0", 24),
		new SubNet("65.54.93.0", 24),
		new SubNet("65.54.89.0", 24),
		new SubNet("94.245.64.0", 19)
	)
	def self = subnets
	override def toString = "mscen.net"
}

object UpdateMicrosoftCom extends IterableProxy[HostCategory] with HostCategory.Collection {
	val subnets = Seq(
		new SubNet("65.52.0.0", 14)
	);
	def self = subnets
	override def toString = "update.microsoft.com"
}

object Homenet extends IterableProxy[HostCategory] with HostCategory.Collection {
	val subnets = Seq(
		new SubNet("10.46.0.0", 16),
		new SubNet("10.49.0.0", 16),
		new SubNet("10.168.0.0", 16),
		new SubNet("109.174.0.0", 16)
	)
	def self = subnets
	override def toString = "Homenet"
}

object Akamai extends IterableProxy[HostCategory] with HostCategory.Collection {
	val subnets = Seq(
		new SubNet("92.123.65.0", 24),
		new SubNet("92.123.155.0", 24),
		new SubNet("95.100.0.0", 16),
		new SubNet("95.101.0.0", 16)
	)
	def self = subnets
	override def toString = "*.akamaitechnologies.com"
}

object NSU extends IterableProxy[HostCategory] with HostCategory.Collection {
	val subnets = Seq(
		new SubNet("10.0.0.0", 12)
	)
	def self = subnets
	override def toString = "NSU"
}

object Google extends IterableProxy[HostCategory] with HostCategory.Collection {
	val subnets = Seq(
		new SubNet("74.125.0.0", 16)
	)
	def self = subnets
	override def toString = "Google"
}

object ChoopaCom extends IterableProxy[HostCategory] with HostCategory.Collection {
	val subnets = Seq(
		new SubNet("216.155.128.0", 19),
		new SubNet("209.222.0.0", 19),
		new SubNet("64.237.32.0", 19),
		new SubNet("66.55.128.0", 19)
	)
	def self = subnets
	override def toString = "Choopa.com"
}

object Gym3 extends IterableProxy[HostCategory] with HostCategory.Collection {
	val subnets = Seq(
		new SubNet("10.3.0.0", 16),
		new SubNet("10.10.0.0", 16)
	)
	def self = subnets
	override def toString = "Gym3"
}

class Categorization extends HostCategory.Set {
	val preventCategorization = new HostCategory.Set
	override def getCategory(host: Host) = {
		val c = super.getCategory(host)
		val antiC = preventCategorization.getCategory(host)
		if (c != null && antiC == null) {
			c
		} else {
			new SingleHost(host)
		}
	}
}
