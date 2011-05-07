package trafacct;
import scala.collection.mutable.HashMap

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
		case class Comparator(a: (T, Long)) extends Ordered[Comparator] {
			override def equals(that:Any) = compare(that.asInstanceOf[Comparator]) == 0
			def compare(that:Comparator) = Summator.compareBySecond(a, that.a)
		}
		implicit def toOrdered(a:(T, Long)) = new Comparator(a)
//		println("There are %d entries in category map".format(size))
		val data = toArray.map(toOrdered)
		scala.util.Sorting.quickSort(data)
		data.map(_.a)
	}
	def categorySorted(implicit conv:(T) => Ordered[T]):Seq[(T, Long)] = {
		case class Comparator(a: (T, Long)) extends Ordered[Comparator] {
			override def equals(that:Any) = compare(that.asInstanceOf[Comparator]) == 0
			def compare(that:Comparator) = {
				if (a._1 == that.a._1) 0 else if (a._1 < that.a._1) -1 else 1
			}
		}
		implicit def toOrdered(a:(T, Long)) = new Comparator(a)
		val data = toArray.map(toOrdered)
		scala.util.Sorting.quickSort(data)
		data.map(_.a)
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
