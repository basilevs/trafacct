package trafacct;

//import scala.collection.mutable.HashSet
import java.io.File

object Defaults {
def getSrcs: scala.collection.Set[AccSource] = {
	val rv = new scala.collection.mutable.HashSet[AccSource]
	
	var dir = new File("/var/log/net-acct/")
	if (dir.isDirectory)
		rv+=new NetAcct.Dir(dir)
	dir = new File("/var/log/squid3/")
	if (dir.isDirectory)
		rv+=new SquidDir(dir)
	rv
}

}

