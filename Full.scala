package trafacct;

import Summator.compareBySecond

case class Direction(src:Host, dst:Host, protocol:String) {
	def this(i:AccUnit) {this(i.src.host, i.dst.host, i.protocol)}
}

object Full extends SumTool[Direction]  {
	def categorize(input:AccUnit) = new Direction(input)
	implicit def hostToStr(i:Host) = format(i)
	def toFields(c:Direction):Seq[String] = Seq(c.src, c.dst, c.protocol)
}

