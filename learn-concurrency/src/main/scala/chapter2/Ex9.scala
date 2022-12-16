package chapter2

import scala.annotation.tailrec
import scala.collection.mutable

class PriorityTaskPool2(important: Int) extends Thread {

  require(important > -1)

  import PriorityTaskPool2._

  private var interrupted: Boolean = false

  private val tasks = mutable.PriorityQueue[Task]()

  private def poll(): Option[Task] = tasks.synchronized {
    while(tasks.isEmpty && !interrupted) tasks.wait()
    if (tasks.isEmpty) None
    else Some(tasks.dequeue())
  }

  def asynchronous(priority: Int)(work: Work): Unit = tasks.synchronized {
    require(priority > -1)
    tasks.enqueue(new Task(priority, work))
    tasks.notify()
  }

  override def run(): Unit = {
    @tailrec
    def loop(): Unit = {
      val task = poll()
      if (!interrupted || (interrupted && task.map(_.priority).getOrElse(-1) >= important)) {
        task.foreach(_.work())
        loop()
      }
      else ()
    }
    loop()
  }

  def shutdown(): Unit = tasks.synchronized {
    interrupted = true
    tasks.notify()
  }
}

object PriorityTaskPool2 {

  type Work = () => Unit
  private class Task(val priority: Int, val work: Work)

  private implicit val taskOrdering: Ordering[Task] = Ordering.by(_.priority)
}

object Ex9 extends App {

  private val pool = new PriorityTaskPool2(2)

  pool.asynchronous(1)(() => println("Hello!"))
  pool.asynchronous(2)(() => println("World"))

  pool.start()

  Thread.sleep(5000)

  pool.shutdown()

}
