import java.net.URL
import java.io.File

import trafacct._
import FileOperations.{stringToURL, open}


def testUrl(s: String) = {
	val u: URL = s
	println(s + " -> " + u)
	try {
		val br = open(u);
		var i = 0;
		while (br.ready && i < 3) {
			println(br.readLine)
			i+=1
		}
		if (i>0) println("End of "+u)
	} catch {
		case e:java.io.FileNotFoundException => println("No file: " + u)
		case e:java.net.ConnectException => println("Can't connect: " +u)
	}
}

testUrl("file:///local.txt")
testUrl("http://localhost/")
testUrl("c:\\windows")
testUrl("test.scala")
testUrl("/home/gulevich/development/scala/test.scala")

val interfaces = new Array[String](1)
interfaces(0)="eth4"

import Manipulator._
import DateTools._

val parser = new NetAcct.Dir(new File("/var/log/net-acct/"))

parser.end = dayStart(now)
//parser.start = dayBefore(parser.end)

println("Start date: "+parser.start)
val d = new AccDropper(genSeq(new Src, new Dst))
val s = new Summator(d.process)
s sum parser
val data = s.toArray

type AccResult = (AccUnit, Long)
class Comparator(a:AccResult) extends Ordered[AccResult] {
	def compare(that:AccResult) = lessCompare(a, that)
	def lessCompare(a:AccResult, that:AccResult) = if (a._2 == that._2) 0 else if (a._2 < that._2) -1 else 1
}
scala.util.Sorting.quickSort(data)(d => new Comparator(d))


println("Parsed")
data.slice(data.length-50).foreach(x => println(d.format(x._1), x._2))
