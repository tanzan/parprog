package chapter2

object Ex2 extends App {

  def periodically(duration: Long)(b: =>Unit): Unit = {

    val t = new Thread (() => {
      while(true){
        b
        Thread.sleep(duration)
      }
    })
    t.setName("Worker")
    t.setDaemon(true)
    t.start()
  }


  periodically(5000){println("Hello")}
  Thread.sleep(1000 * 60)

}
