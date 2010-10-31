package trafacct;
import scala.collection.mutable.ArrayBuffer
trait Field {
	def extract(i:AccUnit):String
	def reset(i:AccUnit):AccUnit
}

trait EndpointField {
	def extract(i:Endpoint):String
	def reset(i:Endpoint):Endpoint
}
trait FromField extends Field with EndpointField {
	def extract(i:AccUnit) = extract(i.src)
	def reset(i:AccUnit) = new AccUnit(i.size, i.start, reset(i.src), i.dst, i.protocol)
}
trait ToField extends Field with EndpointField {
	def extract(i:AccUnit) = extract(i.dst)
	def reset(i:AccUnit) = new AccUnit(i.size, i.start, i.src, reset(i.dst), i.protocol)
}
trait HostField extends EndpointField {
	def extract(i:Endpoint) = ""+i.host.toString
	def reset(i:Endpoint) = new Endpoint(null, i.port)
}
trait PortField extends EndpointField {
	def extract(i:Endpoint) = i.port.toString()
	def reset(i:Endpoint) = new Endpoint(i.host, 0)
}
class Src extends FromField with HostField
class SrcPort extends FromField with PortField
class Dst extends ToField with HostField
class DstPort extends ToField with PortField
class Protocol extends Field {
	def extract(i:AccUnit) = i.protocol
	def reset(i:AccUnit) =  new AccUnit(i.size, i.start, i.src, i.dst, null)
}
class DateField extends Field {
	def extract(i:AccUnit) = DateTools.dateToString(i.start)
	def reset(i:AccUnit) =  new AccUnit(i.size, null,  i.src, i.dst, i.protocol)
}
class SizeField extends Field {
	def extract(i:AccUnit):String = ""+i.size
	def reset(i:AccUnit) =  new AccUnit(0, i.start,  i.src, i.dst, i.protocol)
}

object Field {
	val allFields = new ArrayBuffer[Field]
	allFields += new DateField
	allFields += new Protocol
	allFields += new Src
	allFields += new SrcPort
	allFields += new Dst
	allFields += new DstPort
	allFields += new SizeField
}
