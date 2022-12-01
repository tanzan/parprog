package chapter2

object Ex1 extends App {
  def parallel[A, B](a: =>A, b: =>B): (A, B) = {

    class Res[T](val r: T)

    var res1: Res[A] = null
    var res2: Res[B] = null

    val t1 = new Thread(() => res1 = new Res(a))
    t1.start()

    val t2 = new Thread(() => res2 = new Res(b))
    t2.start()

    t1.join()
    t2.join()

    (res1.r, res2.r)
  }

  println(parallel(10, 20))
}
