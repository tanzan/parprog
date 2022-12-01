package chapter2

object Ex5 extends App {

  class SyncVar[T] {
    private var v: T = null.asInstanceOf[T]
    @volatile private var initialized = false

    def isEmpty: Boolean = !initialized


    def nonEmpty: Boolean = !isEmpty

    def get(): T = {
      if (initialized) {
        val x = v
        v = null.asInstanceOf[T]
        initialized = false
        x
      }
      else throw sys.error("Uninitialized")
    }

    def getWait: T = this.synchronized {
      while (!initialized) {
        try {
          wait()
        } catch {
          case _: InterruptedException =>
        }
      }
      initialized = false
      notify()
      v
    }

    def put(x: T): Unit = {
      if (!initialized) {
        v = x
        initialized = true
      }
      else throw sys.error("Initialized")
    }

    def putWait(x: T): Unit = this.synchronized {
      while (initialized) {
        try {
          wait()
        } catch {
          case _: InterruptedException =>
        }
      }
      v = x
      initialized = true
      notify()
    }
  }

  val sVar = new SyncVar[Int]

  val t1 = new Thread(() => {
    var x = 0
    while (x < 10) {
      sVar.putWait(x)
      x += 1
    }
  })

  val t2 = new Thread(() => {
    var x = 0
    while (x < 9) {
      x = sVar.getWait
      println(x)
    }
  })

  t1.start()
  t2.start()

  t1.join()
  t2.join()
}
