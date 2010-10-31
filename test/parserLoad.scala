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
	new NetAcct.Dir(new File("/var/log/net-acct/")),
	new SquidDir(new File("/var/log/squid3/"))
		))
{
	parser.end = end
	parser.start = start
	parser.filter(noBadHosts)
}

