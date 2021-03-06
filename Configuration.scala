package trafacct;
import java.util.Date
import java.text.SimpleDateFormat
import java.lang.IllegalArgumentException
import java.io.PrintStream
import java.lang.System
import scala.xml.{Node, NodeSeq, SpecialNode, Text}


//import scala.collection.mutable.HashSet
import java.io.File

import Summator.compareBySecond

trait Configuration {
	import Configuration._
	var start:Date = null
	var end:Date = null
	var limit = 50
	def rh(host:Host) = host.resolve
	var skip:HostCategory.Collection = HostCategory.Collection.empty
	var select:HostCategory.Collection = null
	var inactiveCategories:HostCategory.Collection = HostCategory.Collection.empty
	var sources = Set[AccSource]()
	var humanReadable = false
	var output:PrintStream = System.out
	def getCategory(h:Host) = {
		if (inactiveCategories contains h) {
			new SingleHost(h)
		} else {
			val c = AllCategories.getCategory(h)
			if (c !=null) {
				c
			} else {
				new SingleHost(h)
			}
		}
	}
	def formatBytes(bytes:Long): String =
		if (humanReadable) 
			PrettyPrinter.bytesToHumanReadable(bytes) 
		else bytes.toString
		
	def format(h:Host):String =
		if (humanReadable) 
			h.humanReadable
		else h.toString

	def configure(i:AccSource) {
		i.start = start
		i.end = end
		i.skip = skip
		i.select = select
	}
	
	def toXml = {
<configuration>
	<limit>{limit}</limit>
	{if (start!=null) <start>{dateToString(start)}</start> else null}
	{if (end!=null) <end>{dateToString(end)}</end> else null}
	<categories>
		{AllCategories.map(categoryDefinitionToXml)}
	</categories>
	<inactiveCategories>
		{inactiveCategories.map(categoryToXml)}
	</inactiveCategories>
	<skip>
		{skip map categoryToXml}
	</skip>
	<select>
		{if (select != null) select map categoryToXml else null}
	</select>
	<sources>
		{for (s <- sources.toSeq) yield sourceToXml(s)}
	</sources>
</configuration>
	}
}

trait Configured extends Configuration {
	def main(args:Array[String]) = {
		val rem = CmdLine.parse(this, args)
		if (rem.length > 0) {
			val strings = rem.map(_.toString)
			val string = strings.reduceLeft(_ + "," + _)
			throw new IllegalArgumentException("There were illegal arguments: "+string)
		}
		run
	}
	def run:Int
}

object DumpConfig  extends Configured {
	def run = {
		val pp = new scala.xml.PrettyPrinter(200, 2)
		print(pp.format(toXml)+"\n")
		0
	}
}

object Configuration {
	implicit def intToString(i:Int) = i.toString
	implicit def booleanToString(i:Boolean) = i.toString
	def getSrcs: Set[AccSource] = {
		val rv = new scala.collection.mutable.HashSet[AccSource]
	
		var dir = new File("/var/log/net-acct/")
		if (dir.isDirectory)
			rv+=new NetAcct.Dir(dir)
		dir = new File("/var/log/squid3/")
		if (dir.isDirectory)
			rv+=new Squid.Dir(dir)
		Set() ++ rv
	}
	val dateFormat = new SimpleDateFormat("yyyy-MM-dd")
	val dateFormat2 = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm")
	def parseDate(s:String): Date = {
		if (s == null) {
			null
		} else  {
			try {
				if (s.indexOf("T") >=0)
					dateFormat2.parse(s)
				else
					dateFormat.parse(s)
			} catch {
				case e:IllegalArgumentException => throw new ParseError("Can't parse date "+s, e)
			}
		}
	}
	def dateToString(date:Date):String = {
		if (date == null) {
			""
		} else {
			dateFormat2.format(date)
		}
	}
	def hostToXml(host: Host) = {
		if (host.ip != null && host.name != null)
			<Host ip={host.ipString} name={host.name} />
		else if (host.name != null)
			<Host name={host.name} />
		else if (host.ip != null)
			<Host ip={host.ipString} />
		else 
			throw new RuntimeException("Both name and ip are null for host object")				
	}
	
	def xmlToHost(xml: NodeSeq): Host = {
		try {
			val ipNode = (xml \ "@ip")
			val nameNode = (xml \ "@name")
			val ipString = if (ipNode!=null) ipNode.text else null
			val nameString = if (nameNode!=null) nameNode.text else null
			new Host(nameString, Host.strToInetAddress(ipString))
		} catch {
			case e:ParseError => throw new ParseError("Can't parse host: "+xml, e)
		}
	}
			
	def hostsToXml(hosts: Seq[Host]): NodeSeq  = hosts.map(hostToXml)

	def hostsToXml(hosts: Set[Host]): NodeSeq  = {
		if (hosts!=null)
			hostsToXml(hosts.toSeq)
		else
			hostsToXml(Seq[Host]())
	}

	def xmlToHosts(xml: NodeSeq): Seq[Host] = {
		try {
			for (host @ <host>{_*}</host> <- xml) yield
				xmlToHost(host)
		} catch {
			case e:ParseError => throw new ParseError("Can't parse hosts: "+xml, e)
		}
	}

	def categoryDefinitionToXml(cd:HostCategory):Node = cd match {
		case cd:HostCategory.Collection => <Category name={cd.toString} builtin={cd.isBuiltin}>{cd.map(categoryDefinitionToXml)}</Category>
		case cd:SubNet => categoryToXml(cd)
		case cd:Domain => categoryToXml(cd)
		case SingleHost(host) => categoryToXml(cd)
		case _ => <Category name={cd.toString} builtin={cd.isBuiltin}/>
	}

	def xmlToCategoryDefinition(xml:Node):HostCategory = {
//		print("xmlToCategoryDefinition(%s)\n".format(xml.toString))
		xml match {
			case <SubNet/> => xmlToCategory(xml)
			case <Host/> => xmlToCategory(xml)
			case <Domain/> => xmlToCategory(xml)
			case <Category>{_*}</Category> => {
				val name = (xml \ "@name").text
				AllCategories.find(_.toString == name).getOrElse {
					val rv = new HostCategory.Set(name)
					(xml \ "_").filter(!_.isInstanceOf[SpecialNode]).map(xmlToCategoryDefinition).foreach(rv+=_)
					rv
				}
			}
		}
	}

	def categoryToXml(c:HostCategory) = c match {
		case s:SubNet => <SubNet ip={Host.bytesToString(SubNet.longToBytes(s.ip, s.byteCount))} maskLength={s.maskLength.toString}/>
		case s:SingleHost => hostToXml(s.host)
		case s:Domain => {println("Domain:"+s.suffix); <Domain suffix={s.suffix}/>}
		case _ => <Category name={c.toString}/>
	}

	def xmlToCategory(xml:Node):HostCategory = try {
		xml match {
			case <SubNet/> => new SubNet((xml \ "@ip").text, (xml \ "@maskLength").text.toInt)
			case <Host/> => new SingleHost(xmlToHost(xml))
			case <Domain/> => new Domain((xml \ "@suffix").text)
			case <Category/> => {
				val name = (xml \ "@name").text
				try {
					AllCategories.find(_.toString == name).get
				} catch {
					case e:java.util.NoSuchElementException => {
						val allString = AllCategories.toString
						throw new ParseError("No collection named "+name+" was found. Known collections are: "+allString, e)
					}
				}
			}
		}
	} catch {
		case e => throw new ParseError("Error while parsing XML entry "+xml, e)
	}

	def sourceToXml(s:AccSource): Node = {
		s match {
			case NetAcct.Dir(f) => <NetAcctDir dir={f.toString}/>
			case Squid.Dir(f) => <SquidDir dir={f.toString}/>
		}
	}
	def xmlToSource(xml:NodeSeq) = {
		xml match {
			case s @ <NetAcctDir/> => {
				new NetAcct.Dir(new File((s \ "@dir").text))
			}
			case s @ <SquidDir/> => {
				new Squid.Dir(new File((s \ "@dir").text))
			}
		}
	}
	def applyXML(c:Configuration, input:NodeSeq) {
		if (input.length <= 0)
			throw new ParseError("Config XML is empty")
		/*
		val sections = input \ "configuration"
		if (sections.length <= 0)
			throw new ParseError("No configuration section in XML: " + input)
		*/
		def nodeSeqToCategories(xml:NodeSeq):Seq[HostCategory] = xml match {
			case <categories>{nodes @ _*}</categories> => nodes.filter(!_.isInstanceOf[SpecialNode]).map(xmlToCategoryDefinition)
		}
		{input \ "categories"}.flatMap(nodeSeqToCategories).foreach(AllCategories.register)
		input match {
			case <configuration>{nodes @ _*}</configuration> => {
				for (node <- nodes) {
					node match {
						case <inactiveCategories>{categories @ _*}</inactiveCategories> => {
							val data = categories.filter(!_.isInstanceOf[SpecialNode]).map(xmlToCategory)
							c.inactiveCategories = HostCategory.Collection(c.inactiveCategories ++ data)
						}
						case <categories>{categories @ _*}</categories> => {
							categories
							.filter(!_.isInstanceOf[SpecialNode])
							.map(xmlToCategoryDefinition)
							.foreach(AllCategories.register)
						}
						case <limit>{l}</limit> => c.limit = l.text.toInt
						case <start>{s}</start> => c.start = parseDate(s.text)
						case <end>{s}</end> => c.end = parseDate(s.text)
						case <skip>{hosts @ _*}</skip> => c.skip = HostCategory.Collection(c.skip ++ hosts.filter(!_.isInstanceOf[SpecialNode]).map(xmlToCategory))
						case <select>{hosts @ _*}</select> => {
							val res = hosts.filter(!_.isInstanceOf[SpecialNode]).map(xmlToCategory)
							if (c.select == null)
								if (res.length > 0)
									c.select =  HostCategory.Collection(res)
								else 
									null
							else 
								c.select =  HostCategory.Collection(c.select ++ res)
						}
								
						case <sources>{sources @ _*}</sources> => for (source <- sources if (!source.isInstanceOf[SpecialNode])) {
							c.sources = c.sources + xmlToSource(source)
						}
						case x:SpecialNode =>
					}
				}
			}
		}
	}
}

