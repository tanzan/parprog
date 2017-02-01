package reductions

/**
  * Created by serg on 2/1/17.
  */
object PrefixSum {

  import common._

  sealed trait Tree {
    def sum:Int

  }

  case class Node(left:Tree, right: Tree) extends Tree {
    def sum: Int = left.sum + right.sum
    override def toString: String = s"Leaf($left, $right, ${sum})"
  }

  case class Leaf(i:Int, v:Int) extends Tree {
    def sum:Int = v
    override def toString: String = s"Leaf($i, $v, ${sum})"
  }


  def scanUp(from:Int, until:Int, array:Array[Int]):Tree = {
    if (until - from <= 1) Leaf(from, array(from))
    else {
      val mid = (until - from)/2
      val (left, right) = parallel(scanUp(from, from + mid, array), scanUp(from + mid, until, array))
      Node(left, right)
    }
  }

  def scanDown(array:Array[Int], fromLeft:Int, tree: Tree):Unit = {
    tree match {
      case leaf:Leaf => array(leaf.i) = leaf.sum + fromLeft
      case Node(left, right) => parallel(scanDown(array, fromLeft, left), scanDown(array, fromLeft + left.sum, right))
    }
  }

  def main(args: Array[String]): Unit = {
    val list = List(6, 4, 16, 10, 16, 14, 2, 8)
    val array = list.toArray
    println(scanUp(0, array.size, array))
    scanDown(array, 0, scanUp(0, array.size, array))
    println(array.toSeq)
    println(list.scanLeft(0)((a,b) => a + b))
  }

}
