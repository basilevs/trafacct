package trafacct;
import scala.collection.mutable.HashMap
import scala.util.Sorting
import scala.math.Ordering

class Summator[T](process: AccUnit => T) extends HashMap[T, Long] {
	def sum(from: Iterable[AccUnit]) {sum(from.iterator)}
	def sum(from: Iterator[AccUnit]) {
		for (u <- from) {
			val key = process(u)
	//		println("Key: ", key)
			put(key, getOrElseUpdate(key, 0) + u.size)
		}
	}
	def sizeSorted:Seq[(T, Long)] = {
		class C extends Ordering[(T, Long)] {
			def compare(x: (T, Long), y:(T, Long)) = {
				(x._2 - y._2).toInt
			}
		}
//		println("There are %d entries in category map".format(size))
		implicit val c = new C
		val a = toArray
		scala.util.Sorting.quickSort(a)
		a
	}
	def categorySorted(implicit o:Ordering[T]):Seq[(T, Long)] = {		
		class C extends Ordering[(T, Long)] {
			def compare(x: (T, Long), y:(T, Long)) = {
				o.compare(x._1, y._1)
			}
		}
		implicit val c = new C
		val a = toArray
		scala.util.Sorting.quickSort(a)
		a
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
