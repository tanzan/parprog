package chapter2

import scala.util.Random

object Ex7 extends App {

  class Account(val id: Int, val name: String, var money: Int) {
    override def hashCode(): Int = id

    override def equals(obj: Any): Boolean = obj match {
      case account: Account => id == account.id
      case _ => false
    }

    override def toString: String = s"Account($id, $name, $money)"
  }

  private def send(a: Account, b: Account, n: Int): Unit = a.synchronized {
    b.synchronized {
      a.money -= n
      b.money += n
    }
  }

  private def sendSafe(a: Account, b: Account, n: Int): Unit = {
    if (a.id < b.id) {
      send(a, b, n)
    } else {
      send(b, a, -n)
    }
  }

  def sameTransaction() = {
    val a = new Account(1, "Jack", 1000)
    val b = new Account(2, "Jill", 2000)

    val t1 = new Thread(() => {
      for (i <- 0 until 100) sendSafe(a, b, 1)
    })

    val t2 = new Thread(() => {
      for (i <- 0 until 100) sendSafe(b, a, 1)
    })

    t1.start()
    t2.start()

    t1.join()
    t2.join()

    println(s"a = ${a.money}, b = ${b.money}")
  }

  def sendAll(accounts: Set[Account], target: Account): Unit = {

    def adjust(): Unit ={
      var sum = 0
      for {
        account <- accounts
      } {
        sum += account.money
        account.money = 0
      }
      target.money = sum
    }

    def adjustAll(accounts: List[Account]): Unit = accounts match {
      case Nil => adjust()
      case head :: tail => head synchronized {
        adjustAll(tail)
      }
    }

    adjustAll(target :: accounts.toList.sortBy(_.id))
  }


  private val accounts = (1 to 10).map(i => new Account(i , i.toString, i * 10)).toSet
  private val target = new Account(0, "0", 0)

  sendAll(accounts, target)

  accounts.foreach(println)
  println(target)
}
