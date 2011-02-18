package trafacct;

import Summator.compareBySecond

object Full extends Configured {
	case class Rule(src:Host, dst:Host, protocol:String) {
		def this(i:AccUnit) {this(i.src.host, i.dst.host, i.protocol)}
	}
	type AccResult = (Rule, Long)
	case class Comparator(a:AccResult) extends Ordered[Comparator] {
		override def equals(that:Any) = compare(that.asInstanceOf[Comparator]) == 0
		def compare(that:Comparator) = compareBySecond(a, that.a)
	}
	def run = {
		val s = new Summator[Rule]((x:AccUnit) => new Rule(x))
		val srcs = new AccSources(Defaults.getSrcs)
		configure(srcs)
		s.sum(srcs)
		implicit def toOrdered(a:AccResult) = new Comparator(a)
		val data = s.toArray map toOrdered
		scala.util.Sorting.quickSort(data)
		val pp = new PrettyPrinter
		def printAcc(i:AccResult) {
			implicit def hostToStr(i:Host) = i.toString
			println(pp.format(i._1.src, i._1.dst, i._1.protocol, i._2.toString))
		}
		data.slice(data.length-limit).foreach((c: Comparator) => printAcc(c.a))
		0
	}
}

