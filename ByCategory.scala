package trafacct;
import Summator.compareBySecond


class Categories extends Categorization {
	preventCategorization + new SubNet("10.0.0.0", 24) + Gym3
	this + Homenet + Msecn + Akamai + UpdateMicrosoftCom +ChoopaCom + Nsu + Google
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
		data.slice(data.length-limit).foreach(printAcc)
		0
	}
}

object ByCategory extends Configured {
	val categories = new Categories
	case class CategorizedAccUnit(src:HostCategory, dst:HostCategory, protocol:String) 
	implicit def hostsToCategory(h:Host) = categories.getCategory(h)
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
		data.slice(data.length-limit).foreach(printAcc)
		0
	}
}

object BySourceCategory extends ByHostCategory {
	val categories = new Categories
	def categorize(x:AccUnit) = categories.getCategory(x.src.host)
}

object ByDestinationCategory extends ByHostCategory {
	val categories = new Categories
	def categorize(x:AccUnit) = categories.getCategory(x.dst.host)
}
