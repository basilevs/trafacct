package trafacct;

/** Sums all traffic sizes by their categories */
trait SumTool[T] extends Configured {
	def doSort = true
	def categorize(input:AccUnit): T
	def toFields(category:T): Seq[String]
	def debug = true
	def compare(category1:T, category2:T): Int = {
		val h1 = category1.hashCode
		val h2 = category2.hashCode
		if (h1==h2) 0
		else if (h1<h2) -1
		else 1
	}
	def run = {
		val s = new Summator[T](categorize)
		val srcs =  new AccSources(sources)
		configure(srcs)
		s.sum(srcs)
		val pp = new PrettyPrinter
		def printAcc(i:(T, Long)) {
			val s:Seq[String] = toFields(i._1) ++ Seq(formatBytes(i._2))
//			println("Sending to output:"+s.toString)
			output.println(pp.format( s: _* ))
		}
		val result: Seq[(T, Long)] =
		if (doSort) {
			val data = s.sizeSorted
			data.drop(data.size - limit)
		} else {
			def compare1(x:T, y:T) = compare(x,y)
			case class Comparator extends Ordering[T] {
				def compare(x:T, y:T) = compare1(x,y)
			}
			s.categorySorted(new Comparator)
		}
//		println("There are %d entries in result".format(result.size))
		result.foreach(printAcc)
		0
	}
}

case class Direction(src:Host, dst:Host, protocol:String) {
	def this(i:AccUnit) {this(i.src.host, i.dst.host, i.protocol)}
}

object Full extends SumTool[Direction]  {
	implicit def hostToStr(i:Host) = format(i)
	def categorize(input:AccUnit) = {
		var rv = new Direction(input)
		rv
	}
	def toFields(c:Direction) = Seq(c.src, c.dst, c.protocol)
}

object Total extends Configured {
	def run = {
		val srcs =  new AccSources(sources)
		configure(srcs)
		val sum = srcs.foldLeft(0L)(_+_.size)
		output.println(formatBytes(sum))
		0
	}
}
