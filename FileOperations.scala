package trafacct
import java.net.URL
import java.lang.{Exception, Throwable}
import java.io.{BufferedReader, InputStreamReader, File}

object FileOperations {
	class FileError(message: String, reason:Throwable) extends Exception(message, reason) {
	}
	class URLError(url:String , reason:Throwable) extends FileError(url, reason) {
		override def getMessage(): String = "Can't parse " + url
	}
	def processWindowsPath(p: String): String = {
		"file:///" + p.replaceAll("\\\\", "/")
	}
	
	implicit def stringToURL(s: String): URL = {
		try {
			if (s.startsWith("file://"))
				return new URL(s)
			if (s.matches("\\w:\\\\.*")) 
				return new URL(processWindowsPath(s))
			if (!s.matches("\\w+://.*")) {
				return (new File(s)).toURL()
			}
			return new URL(s);
		} catch {
			case e:Exception => throw new URLError(s, e)
		}
	}
	def open(u:URL) = new BufferedReader(new InputStreamReader(u.openStream()))
}

