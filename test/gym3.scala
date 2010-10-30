import java.net.{URL, InetAddress}
import java.io.File

import trafacct._
import FileOperations.{stringToURL, open}

import Manipulator._
import DateTools._

val parser = new NetAcct.Dir(new File("/var/log/net-acct/"))

parser.end = dayStart(now)
//parser.start = dayBefore(parser.end)

println("Start date: "+parser.start)
val d  = new AccDropper(genSeq(new Src, new Dst))
def hasHost(u:AccUnit, h:InetAddress) = u.src.host == h || u.dst.host == h
def hasHosts(u:AccUnit, h:Set[InetAddress]) = h.contains(u.src.host) || h.contains(u.dst.host)
val badHosts = Set(InetAddress.getByName("10.3.0.1"))
var stream = parser.filter(!hasHosts(_, badHosts))
val s = new Summator(d.process)

s.sum(stream)
val data = s.toArray

type AccResult = (AccUnit, Long)
class Comparator(a:AccResult) extends Ordered[AccResult] {
	def compare(that:AccResult) = lessCompare(a, that)
	def lessCompare(a:AccResult, that:AccResult) = if (a._2 == that._2) 0 else if (a._2 < that._2) -1 else 1
}
scala.util.Sorting.quickSort(data)(d => new Comparator(d))

data.slice(data.length-50).foreach(x => println(d.format(x._1), x._2))
