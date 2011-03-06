package trafacct;
import Summator.compareBySecond

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

object ByCategory extends Configured {
	case class CategorizedAccUnit(src:HostCategory, dst:HostCategory, protocol:String) 
	implicit def hostsToCategory(h:Host) = active.getCategory(h)
//	import PrettyPrinter.bytesToHumanReadable
	implicit def formatBytes1(l:Long) = formatBytes(l)
	implicit def catToStr(i:HostCategory) = 
		if (humanReadable) i.humanReadable
		else i.toString
	def process(u:AccUnit) = 
		new CategorizedAccUnit(u.src.host, u.dst.host, u.protocol)
	def run = {
		val s = new Summator[CategorizedAccUnit](process)
		val srcs = new AccSources(sources)
		configure(srcs)
		s.sum(srcs)
		val pp = new PrettyPrinter
		def printAcc(i:(CategorizedAccUnit, Long)) {
			println(pp.format(i._1.src, i._1.dst, i._1.protocol, i._2))
		}
		val data = s.sorted
		data.drop(data.length-limit).foreach(printAcc)
		0
	}
}

object BySourceCategory extends ByHostCategory {
	def categorize(x:AccUnit) = active.getCategory(x.src.host)
}

object ByDestinationCategory extends ByHostCategory {
	def categorize(x:AccUnit) = active.getCategory(x.dst.host)
}
