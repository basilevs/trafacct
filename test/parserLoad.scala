import java.net.{URL, InetAddress}
import java.io.File
import java.util.Date

import trafacct._
import FileOperations._
import Manipulator._
import Defaults._
import DateTools._

var count = 0
val start = new Date
for (parser <- getSrcs)
{
//	for (i <- parser.elements.filter(x => true))
//		count += 1
	for (i <- parser)
		count += 1
}
val end = new Date

implicit def dateToDouble(d:Date) = d.getTime / 1000.

val time = end - start
val speed = count / time
println("%d units. %f unit per second".format(count, speed))