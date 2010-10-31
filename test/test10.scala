import java.net.{URL, InetAddress}
import java.io.File

import trafacct._
import FileOperations.{stringToURL, open}

import Manipulator._
import DateTools._


def hasHost(u:AccUnit, h:InetAddress) = u.src.host == h || u.dst.host == h
def hasHosts(u:AccUnit, h:Set[InetAddress]) = h.contains(u.src.host) || h.contains(u.dst.host)
val badHosts = Set(InetAddress.getByName("10.3.0.1"))
def noBadHosts(u:AccUnit) = !hasHosts(u, badHosts)

val end = dayStart(now)
//val start = dayBefore(end)
val start = null

val d  = new AccDropper(genSeq(new Src, new Dst))
val s = new Summator(d.process)

println("Start date: "+start)

for (parser <- Set(
	new NetAcct(open("/var/log/net-acct/net-acct.log.10.gz"))
//	new NetAcct.Dir(new File("/var/log/net-acct/")),
//	new SquidDir(new File("/var/log/squid3/"))
		))
{
	parser.end = end
	parser.start = start
	s.sum(parser.filter(noBadHosts))
}

val data = s.toArray

type AccResult = (AccUnit, Long)
class Comparator(a:AccResult) extends Ordered[AccResult] {
	def compare(that:AccResult) = lessCompare(a, that)
	def lessCompare(a:AccResult, that:AccResult) = if (a._2 == that._2) 0 else if (a._2 < that._2) -1 else 1
}
scala.util.Sorting.quickSort(data)(d => new Comparator(d))

data.slice(data.length-50).foreach(x => println(d.format(x._1), x._2))
