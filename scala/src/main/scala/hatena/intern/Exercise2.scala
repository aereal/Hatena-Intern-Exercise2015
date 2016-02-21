package hatena.intern

import scalax.file.Path

object SafeConvertableImplicits {
  trait SafeConversion[I, O] {
    def run(in: I): Option[O]
  }

  implicit def StringToIntConversion = new SafeConversion[String, Int] {
    def run(in: String) = {
      import scala.util.control.Exception._
      catching(classOf[NumberFormatException]).opt(in.toInt)
    }
  }

  def safeConvert[I, O](in: I)(implicit instance: SafeConversion[I, O]) = instance.run(in)
}

object LTSVLine {
  import SafeConvertableImplicits._

  def unapply(line: String): Option[Log] = {
    val pairs = for (records <- line.split("\t")) yield records.split(":", 2)
    val records = pairs.map { case Array(k, v) => (k, v) }.toMap
    for (
      host <- records.get("host");
      user <- records.get("user");
      req <- records.get("req");
      referer <- records.get("referer");
      rawStatus <- records.get("status");
      status: Int <- safeConvert(rawStatus);
      rawSize <- records.get("size");
      size <- safeStringToInt(rawSize);
      rawEpoch <- records.get("epoch");
      epoch <- safeStringToInt(rawEpoch)
    ) yield Log(host, user, epoch, req, status, size, referer)
  }

  def safeStringToInt(s: String): Option[Int] = {
    import scala.util.control.Exception._
    catching(classOf[NumberFormatException]).opt(s.toInt)
  }
}

object LtsvParser {
  def safeParse(filePath: String): Iterable[Option[Log]] = {
    Path.fromString(filePath).lines().map {
      case LTSVLine(log) => Some(log)
    }.toIterable
  }

  def parse(filePath: String): Iterable[Log] = {
    safeParse(filePath).map(_.get)
  }
}
