package sandbox

import cats._
import cats.implicits._

import scala.concurrent.Future

object TestingAsyncCode {

  trait UptimeClient[F[_]] {
    def getUptime(hostname: String): F[Int]
  }

  trait RealUptimeClient extends UptimeClient[Future] {
    override def getUptime(hostname: String): Future[Int]
  }

  class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
    override def getUptime(hostname: String): Int =
      hosts.getOrElse(hostname, 0)
  }

  class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
    def getTotalUptime(hostNames: List[String]): F[Int] =
      hostNames.traverse(client.getUptime).map(_.sum)
  }

  def testTotalUptime() = {
    val hosts = Map("host1" -> 10, "jost2" -> 6)
    val client = new TestUptimeClient(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }

}
