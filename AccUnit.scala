package trafacct;
import java.util.Date
import java.lang.{Exception, Throwable, NumberFormatException}
import java.net.{InetAddress, UnknownHostException}
import scala.collection.mutable.{Queue, HashSet}
import scala.collection.Set
import scala.runtime.RichByte
import java.util.regex.Pattern

class ParseError(message:String, reason:Throwable) extends Exception(message, reason) {
	def this(message:String) = this(message, null)
}

case class Endpoint(host:Host, port:Int) {
	assert(host!=null)
	override def toString:String = "%s:%d".format(host.toString, port)
}

object Endpoint {
	implicit def hostToInetAddress(host:String) = InetAddress.getByName(host)
	implicit def parse(s:String) = {
		val hostPort = s.split(":")
		new Endpoint(hostPort(0), hostPort(1).toInt)
	}
}

case class AccUnit(size: Long, start: Date, src:Endpoint, dst:Endpoint, protocol:String);

trait AccSource extends Iterable[AccUnit] {
	var start:Date = null
	var end:Date = null
	var skip:HostCategory = HostCategory.Collection.empty
	var select:HostCategory = null
	def copySettings(i:AccSource) {
		start = i.start
		end = i.end
		skip = i.skip
		select = i.select
	}
	def accept(i:AccUnit): Boolean = {
		if (i == null) 
			return false
		if (start != null && i.start.getTime < start.getTime)
			return false
		if (end != null && i.start.getTime >= end.getTime)
			return false
		if (skip contains i.src.host)
			return false
		if (skip contains i.dst.host)
			return false
		if (select != null)
			if (!select.contains(i.src.host) && !select.contains(i.dst.host))
				return false
		true
	}
}

class AccSources(srcs:Iterable[AccSource]) extends AccSource {
	def elements = new Iterator[AccUnit] {

		var colIter = srcs.elements
		var curIter:Iterator[AccUnit] = null
//		println("AccSources: "+start)	
		def next:AccUnit = {
			var n:AccUnit = null
			do {
				if (curIter != null && curIter.hasNext)
					return curIter.next
				if (colIter.hasNext) {
					var col = colIter.next
					col.copySettings(AccSources.this)
					curIter = col.elements
				} else
					return null
			} while (true)
			null
		}
		def hasNext = curIter!=null && curIter.hasNext || colIter.hasNext
	}.filter(accept)
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
	implicit def dateToOrdered(d:Date):Ordered[Date] = {
		class OrderedDate(val d:Date) extends Ordered[Date] {
			override def equals(that:Any) = compare(that.asInstanceOf[OrderedDate].d) == 0
			def compare(that:Date) = d.compareTo(that)
		}
		new OrderedDate(d)
	}
	def dayStart(d:Date) = {
		val cal = Calendar.getInstance()
		cal.setTime(d)
		for (i <- Manipulator.genSeq(Calendar.MILLISECOND, Calendar.HOUR_OF_DAY, Calendar.SECOND, Calendar.MINUTE))
			cal.set(i, 0)
		cal.getTime
	}
	def addHours(d:Date, n:Int) = {
		val cal = Calendar.getInstance()
		cal.setTime(d)
		cal.add(Calendar.HOUR, n)
		cal.getTime
	}
	def addDays(d:Date, n:Int) = {
		val cal = Calendar.getInstance()
		cal.setTime(d)
		cal.add(Calendar.DAY_OF_MONTH, n)
		cal.getTime
	}
	def dayBefore(d:Date) = addDays(d, -1)
	def weekBefore(d:Date) = addDays(d, -7)
	implicit def timeStampToDate( unixTimeStamp: Double ) : Date = {
		// Unix timestamp is seconds past epoch
		new Date((unixTimeStamp * 1000).toLong);
	}
	implicit def dateToString(d:Date):String = {
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
