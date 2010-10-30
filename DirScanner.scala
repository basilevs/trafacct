package trafacct;
import java.io.{File, FileFilter}
import java.net.URL

trait FileOpener {
	def open(u:URL): AccSource
}
	
abstract class DirScanner(dir:File)  extends AccSource  with FileOpener {
	var fileFilter = (_:File) => true
	private def fileFilterImp = new FileFilter {
		def accept(f:File): Boolean = return fileFilter(f)
	}
	def elements = new Iterator[AccUnit] {
		var fileIter = dir.listFiles(fileFilterImp).filter(start == null || _.lastModified > start.getTime).elements
		var accIter:Iterator[AccUnit] = null
		var url:URL = null
		def next:AccUnit = {
			do {
				try {
					if (accIter!=null && accIter.hasNext)
						return accIter.next
					if (!fileIter.hasNext)
						return null
				} catch {
					case e:Exception => throw new ParseError("Can't parse "+url, e)
				}
				val file = fileIter.next
				url = file.toURL
				val s = open( url )
				s.start = start
				accIter = s.elements
				println(url)
			} while (true)
			null
		}
		def hasNext() = fileIter.hasNext || (accIter != null && accIter.hasNext)
	}
}
