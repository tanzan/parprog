package chapter2

import scala.collection.mutable

object Ex6 extends App {

  class SyncQueue[T](size: Int) {

    private val queue = mutable.Queue[T]()

    def isEmpty: Boolean = this.synchronized {
      queue.isEmpty
    }

    def nonEmpty: Boolean = !isEmpty

    def getWait: T = this.synchronized {
      while(queue.isEmpty) {
        try {
          wait()
        } catch {
          case _: InterruptedException =>
        }
      }
      val x = queue.dequeue()
      notify()
      x
    }

    def putWait(x: T): Unit = this.synchronized {
      while (queue.size == size) {
        try {
          wait()
        } catch {
          case _: InterruptedException =>
        }
      }
      queue.enqueue(x)
      notify()
    }

  }

  val q = new SyncQueue[Int](2)

  val t1 = new Thread(() => {
    var x = 0
    while (x < 10) {
      q.putWait(x)
      x += 1
    }
  })

  val t2 = new Thread(() => {
    var x = 0
    while (x < 9) {
      x = q.getWait
      println(x)
    }
  })

  t1.start()
  t2.start()

  t1.join()
  t2.join()

}
