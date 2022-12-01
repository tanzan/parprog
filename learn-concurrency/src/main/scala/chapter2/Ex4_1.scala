package chapter2

object Ex4_1 extends App {

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

    def put(x: T): Unit = {
      if (!initialized) {
        v = x
        initialized = true
      }
      else throw sys.error("Initialized")
    }
  }

  val sVar = new SyncVar[Int]

  val t1 = new Thread (() => {
    var x = 0
    while(x < 10) {
      if (sVar.isEmpty) {
          sVar.put(x)
        x += 1
      }
    }
    for(i <- 0 until 10){}
  })

  val t2 = new Thread (() => {
    var x = 0
    while (x < 9 ) {
      if (sVar.nonEmpty)  {
        x = sVar.get()
        println(x)
      }
    }
  })

  t1.start()
  t2.start()

  t1.join()
  t2.join()
}
