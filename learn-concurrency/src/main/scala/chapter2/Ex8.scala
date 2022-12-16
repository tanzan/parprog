package chapter2

import collection.mutable

class PriorityTaskPool extends Thread {

  import PriorityTaskPool._

  setDaemon(true)

  private val tasks = mutable.PriorityQueue[Task]()

  private def poll(): Work = tasks.synchronized {
    while(tasks.isEmpty) tasks.wait()
    tasks.dequeue().work
  }

  def asynchronous(priority: Int)(work: Work): Unit = tasks.synchronized {
    tasks.enqueue(new Task(priority, work))
    tasks.notify()
  }

  override def run(): Unit = {
    while(true) {
      val work = poll()
      work()
    }
  }
}

object PriorityTaskPool {

  type Work = () => Unit
  private class Task(val priority: Int, val work: Work)

  private implicit val taskOrdering: Ordering[Task] = Ordering.by(_.priority)
}

object Ex8 extends App {

  private val pool = new PriorityTaskPool

  pool.asynchronous(1)(() => println("Hello!"))
  pool.asynchronous(2)(() => println("World"))

  pool.start()

  Thread.sleep(5000)

}
