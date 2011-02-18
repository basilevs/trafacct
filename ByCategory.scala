package trafacct;
import Summator.compareBySecond

object Categories {
	val categorization = new Categorization
	categorization + new Homenet + new Msecn + new Akamai
	categorization + new SubNet("10.51.0.0", 16) + new SubNet("10.33.0.0", 16) 
	categorization + new SubNet("216.155.0.0", 16) +  new SubNet("209.222.23.0", 24)
	categorization + new SubNet("192.168.0.0", 16)
}

object ByCategory extends Configured {
	def run = {
		val s = new Summator[HostCategory.CategorizedAccUnit]((x:AccUnit) => Categories.categorization.process(x))
		val srcs = new AccSources(Defaults.getSrcs)
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

object BySourceCategory extends Configured {
	def run = {
		val s = new Summator[HostCategory]((x:AccUnit) => Categories.categorization.getCategory(x.src.host))
		val srcs = new AccSources(Defaults.getSrcs)
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
