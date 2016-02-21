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
    val records = pairsToMap(parseLine(line))
    for (
      host <- records.get("host");
      user <- records.get("user");
      req <- records.get("req");
      referer <- records.get("referer");
      rawStatus <- records.get("status");
      status: Int <- safeConvert(rawStatus);
      rawSize <- records.get("size");
      size: Int <- safeConvert(rawSize);
      rawEpoch <- records.get("epoch");
      epoch: Int <- safeConvert(rawEpoch)
    ) yield Log(host, user, epoch, req, status, size, referer)
  }

  def parseLine(line: String): Array[Array[String]] = line.split("\t").map(_.split(":", 2))

  def pairsToMap(pairs: Array[Array[String]]): Map[String, String] = pairs.map {
    case Array(k, v) => (k, v)
  }.toMap
}

object LtsvParser {
  import scalax.io.LongTraversable

  def safeParseLines(path: scalax.file.Path): Option[LongTraversable[String]] = {
    Some(path.lines())
  }

  def parseRecords(lines: Iterable[String]): Iterable[Option[Log]] =
    lines.map {
      case LTSVLine(log) => Some(log)
    }

  def safeParse(filePath: String): Option[Iterable[Option[Log]]] = {
    for (
      lines <- safeParseLines(Path.fromString(filePath));
      logs <- Some(parseRecords(lines.toIterable))
    ) yield logs
  }

  def parse(filePath: String): Iterable[Log] = {
    safeParse(filePath).get.flatten
  }
}
