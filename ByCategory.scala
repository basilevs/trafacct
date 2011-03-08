package trafacct;
import Summator.compareBySecond

object CategoryTypes {
	case class CategorizedAccUnit(src:HostCategory, dst:HostCategory, protocol:String)
}
trait ByHostCategory extends Configured {
	def categorize(a:AccUnit): HostCategory
	implicit def hostToStr(i:Host) = format(i)
	implicit def catToStr(i:HostCategory) = 
		if (humanReadable) i.humanReadable
		else i.toString
	def run = {
		val s = new Summator[HostCategory]((x:AccUnit) => categorize(x))
		val srcs =  new AccSources(sources)
		configure(srcs)
		s.sum(srcs)
		val pp = new PrettyPrinter
		def printAcc(i:(HostCategory, Long)) {
			println(pp.format(i._1, formatBytes(i._2)))
		}
		val data = s.sorted
		data.drop(data.length-limit).foreach(printAcc)
		0
	}
}

object ByCategory extends SumTool[CategoryTypes.CategorizedAccUnit] {
	implicit def hostToCategory = getCategory(_)
//	import PrettyPrinter.bytesToHumanReadable
	implicit def formatBytes1(l:Long) = formatBytes(l)
	implicit def catToStr(i:HostCategory) = 
		if (humanReadable) i.humanReadable
		else i.toString
	def categorize(u:AccUnit) =
		new CategoryTypes.CategorizedAccUnit(u.src.host, u.dst.host, u.protocol)
	def toFields(c:CategoryTypes.CategorizedAccUnit) = {
		if (humanReadable)
			Seq(c.src.humanReadable, c.dst.humanReadable, c.protocol)
		else
			Seq(c.src.toString, c.dst.toString, c.protocol)
	}
}

object BySourceCategory extends ByHostCategory {
	def categorize(x:AccUnit) = getCategory(x.src.host)
}

object ByDestinationCategory extends ByHostCategory {
	def categorize(x:AccUnit) = getCategory(x.dst.host)
}
