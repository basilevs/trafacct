package trafacct;
import Summator.compareBySecond

object CategoryTypes {
	case class CategorizedAccUnit(src:HostCategory, dst:HostCategory, protocol:String)
}

trait ByHostCategory extends SumTool[HostCategory] {
	implicit def hostCategoryToStr(i:HostCategory) = {
		if (humanReadable) i.humanReadable
		else i.toString
	}
	implicit def hostToStr(i:Host) = format(i)
	def toFields(c:HostCategory):Seq[String] = Seq(c)
}

object ByCategory extends SumTool[CategoryTypes.CategorizedAccUnit] {
	implicit def hostToCategory = getCategory(_)
	implicit def hostCategoryToStr(i:HostCategory) = {
		if (humanReadable) i.humanReadable
		else i.toString
	}
	def categorize(u:AccUnit) =
		new CategoryTypes.CategorizedAccUnit(u.src.host, u.dst.host, u.protocol)
	def toFields(c:CategoryTypes.CategorizedAccUnit): Seq[String] = {
		Seq(c.src, c.dst, c.protocol)
	}
}

object BySourceCategory extends ByHostCategory {
	def categorize(x:AccUnit) = getCategory(x.src.host)
}

object ByDestinationCategory extends ByHostCategory {
	def categorize(x:AccUnit) = getCategory(x.dst.host)
}
