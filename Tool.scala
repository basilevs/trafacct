package trafacct;
import Summator.compareBySecond

/** Sums all traffic sizes by their categories */
trait SumTool[T] extends Configured {
	def categorize(input:AccUnit): T
	def toFields(category:T): Seq[String]
	def run = {
		val s = new Summator[T](categorize)
		val srcs =  new AccSources(sources)
		configure(srcs)
		s.sum(srcs)
		val pp = new PrettyPrinter
		def printAcc(i:(T, Long)) {
			output.println(pp.format((toFields(i._1) ++ Seq(formatBytes(i._2))): _*))
		}
		val data = s.sorted
		data.drop(data.length-limit).foreach(printAcc)
		0
	}

}