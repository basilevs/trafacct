import java.net.URL
import java.io.File
import trafacct._
import Manipulator._
import DateTools._

val parser = new NetAcct.Dir(new File("unzip/"))
val d = new AccDropper(genSeq(new Src, new Dst))

for (i <- parser) {d.process(i)}

