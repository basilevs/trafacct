package trafacct;
import scala.collection.mutable.HashMap

class Summator[T](process: AccUnit => T) extends HashMap[T, Long] {
	def sum(from: Iterable[AccUnit]) {sum(from.elements)}
	def sum(from: Iterator[AccUnit]) {
		for (val u <- from) {
			val key = process(u)
	//		println("Key: ", key)
			put(key, getOrElseUpdate(key, 0) + u.size)
		}
	}
	
}

class SecondFieldOrdered[T](a:(T, Long)) extends Ordered[(T, Long)] {
	type Pair = (T, Long)
	def compare(that:Pair) = if (a._2 == that._2) 0 else if (a._2 < that._2) -1 else 1
}

object Summator {
       def compareBySecond[T](a:(T, Long), b:(T, Long)):Int = {
               if (a._2 == b._2) 0 else if (a._2 < b._2) -1 else 1
       }
}
