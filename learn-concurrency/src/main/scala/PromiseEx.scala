import java.util.{Timer, TimerTask}
import scala.concurrent.{Await, Future, Promise}
import scala.language.postfixOps
import scala.util.Try

object PromiseEx extends App {

  import scala.concurrent.ExecutionContext.Implicits.global

  implicit val timer: Timer = new Timer()

  def schedule[T](delay: Int, block: => T)(implicit timer: Timer): Future[T] = {
    val promise = Promise[T]()

    timer.schedule(new TimerTask {
      override def run(): Unit = promise.complete(Try(block))
    }, delay)

    promise.future
  }


  val helloWorld = for {
    hello <- schedule(1000, "Hello")
    world <- schedule(1000, "world!")
  } yield s"$hello $world"

  import scala.concurrent.duration._

  println(Await.result(helloWorld, 5 seconds))


  timer.cancel()
}
