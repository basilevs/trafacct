import java.net.URL
import java.io.File

import trafacct._
import FileOperations.{stringToURL, open}


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
		case e:java.net.ConnectException => println("Can't connect: " +u)
	}
}

testUrl("file:///local.txt")
testUrl("http://localhost/")
testUrl("c:\\windows")
testUrl("test.scala")
testUrl("/home/gulevich/development/scala/test.scala")

val interfaces = new Array[String](1)
interfaces(0)="eth4"

import Manipulator._
import DateTools._
import Squid._

def tUrl(s:String) {
	val url = parseUrl(s)
	println ("%s => %s://%s:%d".format(s, url.protocol, url.host, url.port))
}
 
tUrl("www.update.microsoft.com:443")
tUrl("http://www.update.microsoft.com/v9/windowsupdate/selfupdate/wuident.cab?")
tUrl("https://www.update.microsoft.com")
tUrl("ftp://buaka.com/fhdksdh")
