import java.net.URL
import trafacct.FileOperations.{stringToURL, open}


def testUrl(s: String) = {
	val u: URL = s
	println(s + " -> " + u)
	try {
		val br = open(u);
		var i = 0;
		while (br.ready && i < 3) {
			println(br.readLine)
			i+=1
		}
		if (i>0) println("End of "+u)
	} catch {
		case e:java.io.FileNotFoundException => println("No file: " + u)
	}
}

testUrl("file:///local.txt")
testUrl("http://localhost/")
testUrl("c:\\windows")
testUrl("test.scala")
testUrl("/home/gulevich/development/scala/test.scala")

