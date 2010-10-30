package trafacct;
import scala.collection.mutable.HashMap

class Summator[T](process: AccUnit => T) extends HashMap[T, Long] {
	def sum(from: Iterable[AccUnit]) = for (val u <- from) {
		val key = process(u)
//		println("Key: ", key)
		put(key, getOrElseUpdate(key, 0) + u.size)
	}
}
