package trafacct;
import Summator.compareBySecond

object Categories {
	val categorization = new Categorization
	categorization.preventCategorization + new SubNet("10.0.0.0", 24)
	categorization + Homenet + Msecn + Akamai + UpdateMicrosoftCom +ChoopaCom + Nsu + Google
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
