package hatena.intern

import scalax.file.Path

object LTSVLine {
  def unapply(line: String): Option[Log] = {
    val pairs = for (records <- line.split("\t")) yield records.split(":", 2)
    val records = pairs.map { case Array(k, v) => (k, v) }.toMap
    for (
      host <- records.get("host");
      user <- records.get("user");
      req <- records.get("req");
      referer <- records.get("referer");
      status <- records.get("status");
      statusInt <- safeStringToInt(status);
      size <- records.get("size");
      sizeInt <- safeStringToInt(size);
      epoch <- records.get("epoch");
      epochInt <- safeStringToInt(epoch)
    ) yield Log(host, user, epochInt, req, statusInt, sizeInt, referer)
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
