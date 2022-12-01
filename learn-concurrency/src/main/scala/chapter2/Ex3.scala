package chapter2

object Ex3 extends App {

  class SyncVar[T] {
    private var v: T = null.asInstanceOf[T]
    private var initialized = false

    def get(): T = this.synchronized {
      if (initialized) {
        val x = v
        v = null.asInstanceOf[T]
        initialized = false
        x
      }
      else throw sys.error("Uninitialized")
    }

    def put(x: T): Unit = this.synchronized {
      if (!initialized) {
        v = x
        initialized = true
      }
      else throw sys.error("Initialized")
    }
  }

}
