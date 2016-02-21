package hatena.intern

import org.joda.time.{ DateTime, DateTimeZone }
import org.joda.time.format.{ DateTimeFormat }

case class Log(host: String, user: String, epoch: Int, req: String, status: Int, size: Int, referer: String) {
  private val Array(reqMethod, reqPath, reqProtocol) = req.split(' ')
  def method: String = reqMethod
  def path: String = reqPath
  def protocol: String = reqProtocol
  def uri: String = "http://" ++ host ++ path
  def time: String = {
    val d = new DateTime(epoch.toLong * 1000).withZone(DateTimeZone.UTC)
    val fmt = DateTimeFormat.forPattern("YYYY-MM-dd'T'HH:mm:ss")
    fmt.print(d)
  }
}
