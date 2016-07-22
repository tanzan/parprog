package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 1000000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def count(idx: Int, until: Int, n:Int):Int = {
      if (idx >= until || n < 0) n
      else if (chars(idx) == '(') count(idx + 1, until, n + 1)
      else if (chars(idx) == ')') count(idx + 1, until, n - 1)
      else count(idx + 1, until, n)
    }

    count(0, chars.size, 0) == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, total: Int, minValue: Int):(Int, Int) = {
      if (idx >= until) (total, minValue)
      else if (chars(idx) == '(') traverse(idx + 1, until, total + 1, Math.min(total, total + 1))
      else if (chars(idx) == ')') traverse(idx + 1, until, total - 1, Math.min(total, total - 1))
      else traverse(idx + 1, until, total, minValue)
    }

    def reduce(from: Int, until: Int):(Int, Int) = {
      if ((until - from) <= threshold) {
        traverse(from, until, 0, 0)
      } else {
        val mid = (until - from)/2
        val result = parallel(reduce(from, from + mid), reduce(from + mid, until))
        val left = result._1
        val right = result._2
        (left._1 + right._1, Math.min(Math.min(0, left._2), left._1 + right._2))
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
