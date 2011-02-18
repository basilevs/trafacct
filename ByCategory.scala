package trafacct;
import Summator.compareBySecond

object ByCategory extends Configured {
	case class Comparator(a: (HostCategory.CategorizedAccUnit, Long)) extends Ordered[Comparator] {
		override def equals(that:Any) = compare(that.asInstanceOf[Comparator]) == 0
		def compare(that:Comparator) = compareBySecond(a, that.a)
	}
	def run = {
		val categorization = new Categorization
		categorization + new Homenet + new Nsu 
		categorization + new SubNet("10.51.0.0", 16) + new SubNet("10.33.0.0", 16) 
		categorization + new SubNet("216.155.0.0", 16) +  new SubNet("209.222.23.0", 24)
		categorization + new SubNet("192.168.0.0", 16)
		val s = new Summator[HostCategory.CategorizedAccUnit]((x:AccUnit) => categorization.process(x))
		val srcs = new AccSources(Defaults.getSrcs)
		configure(srcs)
		s.sum(srcs)
		// implicit def toOrdered(a:(HostCategory.CategorizedAccUnit, Long)) = new Comparator(a)
		// val data = s.toArray map toOrdered
		// scala.util.Sorting.quickSort(data)
		val pp = new PrettyPrinter
		def printAcc(i:(HostCategory.CategorizedAccUnit, Long)) {
			implicit def hostToStr(i:Host) = i.toString
			implicit def catToStr(i:HostCategory) = i.toString
			println(pp.format(i._1.src, i._1.dst, i._1.protocol, i._2.toString))
		}
		// data.slice(data.length-limit).foreach((c: Comparator) => printAcc(c.a))
		val data = s.sorted
		data.slice(data.length-limit).foreach(printAcc)
		0
	}
}

