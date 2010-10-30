package trafacct;
import java.util.Date
import java.lang.{Exception, Throwable, NumberFormatException}
import java.net.InetAddress
import scala.collection.mutable.{Queue, ArrayBuffer, HashSet}
import scala.collection.Set

class ParseError(message:String, reason:Throwable) extends Exception(message, reason)

case class Endpoint(host:InetAddress, port:Int) {
	override def toString:String = "%s:%d".format(host.getHostName(), port)
}

object Endpoint {
	implicit def hostToInetAddress(host:String) = InetAddress.getByName(host)
	def parse(s:String) = {
		val hostPort = s.split(":")
		new Endpoint(hostPort(0), hostPort(1).toInt)
	}
}

case class AccUnit(size: Long, start: Date, src:Endpoint, dst:Endpoint, protocol:String);

trait AccSource extends Iterable[AccUnit] {
	var start:Date =null
	var end:Date = null
}

class AccSourceCached extends Queue[AccUnit] with AccSource {
	def ++=(from: AccSource) = super.++=(from)
}

trait AccUnitProcessor[T] {
	def process(i: AccUnit):T
	def format(i:T):String
}

object DateTools {
	import java.util.Calendar
	def now = new Date
	def dayStart(d:Date) = {
		val cal = Calendar.getInstance()
		cal.setTime(d)
		for (i <- Manipulator.genSeq(Calendar.MILLISECOND, Calendar.HOUR_OF_DAY, Calendar.SECOND, Calendar.MINUTE))
			cal.set(i, 0)
		cal.getTime
	}
	def dayBefore(d:Date) = {
		val cal = Calendar.getInstance()
		cal.add(Calendar.DAY_OF_MONTH, -1)
		cal.getTime
	}
}

object Manipulator {
	trait Field {
		def extract(i:AccUnit):String
		def reset(i:AccUnit):AccUnit
	}
	val allFields = new ArrayBuffer[Field]

	trait EndpointField {
		def extract(i:Endpoint):String
		def reset(i:Endpoint):Endpoint
	}
	trait FromField extends Field with EndpointField {
		def extract(i:AccUnit) = extract(i.src)
		def reset(i:AccUnit) = new AccUnit(i.size, i.start, reset(i.src), i.dst, i.protocol)
	}
	trait ToField extends Field with EndpointField {
		def extract(i:AccUnit) = extract(i.dst)
		def reset(i:AccUnit) = new AccUnit(i.size, i.start, i.src, reset(i.dst), i.protocol)
	}
	trait HostField extends EndpointField {
		def extract(i:Endpoint) = ""+i.host.getHostName()
		def reset(i:Endpoint) = new Endpoint(null, i.port)
	}
	trait PortField extends EndpointField {
		def extract(i:Endpoint) = i.port.toString()
		def reset(i:Endpoint) = new Endpoint(i.host, 0)
	}
	class Src extends FromField with HostField
	class SrcPort extends FromField with PortField
	class Dst extends ToField with HostField
	class DstPort extends ToField with PortField
	class Protocol extends Field {
		def extract(i:AccUnit) = i.protocol
		def reset(i:AccUnit) =  new AccUnit(i.size, i.start, i.src, i.dst, null)
	}
	def dateToString(d:Date) = {
		if (d!=null)
			String.format("%1$tY-%1$tm-%1$td %1$tT ", d)
		else 
			"null"
	}
	class DateField extends Field {
		def extract(i:AccUnit) = dateToString(i.start)
		def reset(i:AccUnit) =  new AccUnit(i.size, null,  i.src, i.dst, i.protocol)
	}
	class SizeField extends Field {
		def extract(i:AccUnit):String = ""+i.size
		def reset(i:AccUnit) =  new AccUnit(0, i.start,  i.src, i.dst, i.protocol)
	}

	allFields += new DateField
	allFields += new Protocol
	allFields += new Src
	allFields += new SrcPort
	allFields += new Dst
	allFields += new DstPort
	allFields += new SizeField
		
	def genSeq[T](i:T*) = i

	//2.7.7 misses toSet members
	implicit def toSet[T](i:Iterable[T]): Set[T] = {
		val rv = new HashSet[T]
		rv ++= i
		rv
	}
	
	def filterOut[T](all:Iterable[T], toRemove:Set[T]) = {
		all.filter(x => ! toRemove.contains(x))
	}
	
	def filterOutByType[T <: AnyRef](all:Iterable[T], toFilter:Set[T]):Iterable[T] = {
		def toClass(x:AnyRef) = x.getClass.asInstanceOf[Class[T]]
		val allTypes = all map toClass
		val extraTypes = toSet(filterOut(allTypes, toFilter map toClass))
		all.filter(extraTypes contains toClass(_))
	}
}


class AccDropper(val shownFields:Iterable[Manipulator.Field]) extends AccUnitProcessor[AccUnit] {
	import Manipulator._
	val droppedFields = toSet(filterOutByType(allFields, shownFields))
	
	def process(input: AccUnit) = {
		var i = input
		for (val field <- droppedFields)
			i = field.reset(i)
//		println("Applied %d resetters".format(droppedFields.length))
//		println(input + " => " + i)
		i
	}
	def format(i:AccUnit):String = {
		val rv = new StringBuilder
		for (val field <- shownFields)
			rv.append(field.extract(i) + " ")
		rv.toString
	}
}
