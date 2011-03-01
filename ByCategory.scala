package trafacct;
import Summator.compareBySecond

object Categories {
	val categorization = new Categorization
	categorization + Homenet + Msecn + Akamai + UpdateMicrosoftCom
	categorization + new SubNet("10.51.0.0", 16) + new SubNet("10.33.0.0", 16) 
	categorization + new SubNet("216.155.0.0", 16) +  new SubNet("209.222.23.0", 24)
	categorization + new SubNet("192.168.0.0", 16)
}

trait ByHostCategory extends Configured {
	def categorize(a:AccUnit): HostCategory
	def run = {
		val s = new Summator[HostCategory]((x:AccUnit) => Categories.categorization.getCategory(x.src.host))
		val srcs =  new AccSources(sources)
		configure(srcs)
		s.sum(srcs)
		val pp = new PrettyPrinter
		def printAcc(i:(HostCategory, Long)) {
			implicit def hostToStr(i:Host) = i.toString
			implicit def catToStr(i:HostCategory) = i.toString
			println(pp.format(i._1, i._2.toString))
		}
		val data = s.sorted
		data.slice(data.length-limit).foreach(printAcc)
		0
	}
}

object ByCategory extends Configured {
	def run = {
		val s = new Summator[HostCategory.CategorizedAccUnit]((x:AccUnit) => Categories.categorization.process(x))
		val srcs = new AccSources(sources)
		configure(srcs)
		s.sum(srcs)
		val pp = new PrettyPrinter
		def printAcc(i:(HostCategory.CategorizedAccUnit, Long)) {
			implicit def hostToStr(i:Host) = i.toString
			implicit def catToStr(i:HostCategory) = i.toString
			println(pp.format(i._1.src, i._1.dst, i._1.protocol, i._2.toString))
		}
		val data = s.sorted
		data.slice(data.length-limit).foreach(printAcc)
		0
	}
}

object BySourceCategory extends ByHostCategory {
	def categorize(x:AccUnit) = Categories.categorization.getCategory(x.src.host)
}

object ByDestinationCategory extends ByHostCategory {
	def categorize(x:AccUnit) = Categories.categorization.getCategory(x.dst.host)
}
