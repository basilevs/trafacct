package trafacct;
import java.io.{File, FileFilter}
import java.net.URL
import java.lang.System

trait FileOpener {
	def open(u:File): AccSource
}
	
abstract class DirScanner(dir:File)  extends AccSource  with FileOpener {
	var fileFilter = (_:File) => true
	private def fileFilterImp = new FileFilter {
		def accept(f:File): Boolean = return fileFilter(f)
	}
	def listFiles = {
		val rawList = dir.listFiles(fileFilterImp)
		if (rawList == null)
			throw new ParseError("Not a directory: "+dir, null)
		rawList.filter(start == null || _.lastModified > start.getTime)
	}
	def iterator = new Iterator[AccUnit] {
		var fileIter = listFiles.iterator
		var accIter:Iterator[AccUnit] = null
//		println("DirScanner: %s - %s".format(start, end))
		def next:AccUnit = {
			do {
				if (accIter!=null && accIter.hasNext)
					return accIter.next
				if (!fileIter.hasNext)
					return null
				val file = fileIter.next
				val s = open(file)
				s.copySettings(DirScanner.this)
				accIter = s.iterator
				System.err.println(file.getPath)
			} while (true)
			null
		}
		def hasNext() = fileIter.hasNext || (accIter != null && accIter.hasNext)
	}.filter(_ != null)
}
