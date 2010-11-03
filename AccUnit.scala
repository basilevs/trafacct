package trafacct;
import java.util.Date
import java.lang.{Exception, Throwable, NumberFormatException}
import java.net.{InetAddress, UnknownHostException}
import scala.collection.mutable.{Queue, HashSet}
import scala.collection.Set
import scala.runtime.RichByte
import java.util.regex.Pattern

class ParseError(message:String, reason:Throwable) extends Exception(message, reason)

case class Host(name:String, var ip:InetAddress) {
	import Host.{addressToBytes, compareSeqs}
	def comparejhgjhg(that: Host):Int = {
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
			case e:NumberFormatException =>
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
			if (ip == null)
				ip = InetAddress.getByName(s)
		} catch {
			case e:UnknownHostException =>
		}
		new Host(name, ip)
	}
}

case class Endpoint(host:Host, port:Int) {
	override def toString:String = "%s:%d".format(host.toString, port)
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
	implicit def timeStampToDate( unixTimeStamp: Double ) : Date = {
		// Unix timestamp is seconds past epoch
		new Date((unixTimeStamp * 1000).toLong);
	}
	implicit def dateToString(d:Date) = {
		if (d!=null)
			String.format("%1$tY-%1$tm-%1$td %1$tT ", d)
		else 
			"null"
	}
}

object Manipulator {

		
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


class AccDropper(val shownFields:Iterable[Field]) extends AccUnitProcessor[AccUnit] {
	import Manipulator._
	import Field._
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
