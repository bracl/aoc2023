package day17

import utils.ReadFiles
import zio.Scope
import zio.test.{Assertion, Spec, TestAspect, TestEnvironment, ZIOSpecDefault, assert}

import scala.annotation.tailrec
import scala.collection.mutable

object day17 extends ZIOSpecDefault {
  type VVS = Vector[Vector[String]]

  case class Node(
    x: Int,
    y: Int,
    dot: (Int, Int) = (-1, -1),
    n: Int = 0,
    heatLost: Int = Int.MaxValue,
    fullPath: Vector[(Int, Int)] = Vector.empty
  )

  object Node {
    implicit val ord: Ordering[Node] = NodeOrdering
  }

  object NodeOrdering extends Ordering[Node] {
    override def compare(x: Node, y: Node): Int = y.heatLost.compare(x.heatLost)
  }

  val inputText = ReadFiles.readFile(17)
//  val inputText = """111111111111
//                    |999999999991
//                    |999999999991
//                    |999999999991
//                    |999999999991""".stripMargin.split("\n").toVector
  val input     = inputText.map(_.split("").toVector)

  val nodes: Vector[Vector[Node]] = (for {
    y <- Range(0, input.length)
    x <- Range(0, input.head.length)
  } yield Node(x, y)).toVector.grouped(input.head.length).toVector

  def lastNode(n: Node): Boolean = n.x == input.head.length - 1 && n.y == input.length - 1

  def showPath(visited: Vector[(Int, Int)]) = {
    val b = for {
      y <- Range(0, input.length)
      x <- Range(0, input.head.length)
      c = if (visited.contains((x, y))) input(y)(x) else "."
    } yield c
    b.grouped(input.head.length).foreach(r => println(r.mkString("")))
  }

  def validMoves(node: Node, part1: Boolean) =
    List((1, 0), (-1, 0), (0, 1), (0, -1)).filter { case (x, y) =>
      val (xx, yy)        = (node.x + x, node.y + y)
      val onGrid          = xx >= 0 && xx < input.head.length && yy >= 0 && yy < input.length
      val isMoreThanThree = node.dot == (x, y) && node.n >= 3
      val isMoreThanTen   = node.dot == (x, y) && node.n >= 10
      val isReverse       = node.dot == (-1 * x, -1 * y)
      val change          = if (node.dot == (x, y)) true else node.n >= 4
      if (node.dot == (-1, -1)) onGrid
      else if (part1) onGrid && !isReverse && !isMoreThanThree
      else onGrid && !isReverse && !isMoreThanTen && change
    }

  def search(start: Node, part1: Boolean): Node = {
    val queue = mutable.PriorityQueue(start)
    @tailrec
    def loop(pos: Map[(Int, Int, (Int, Int), Int), Node]): Map[(Int, Int, (Int, Int), Int), Node] =
      if (queue.isEmpty) pos
      else {
        val min = queue.dequeue()
        if (pos.contains((min.x, min.y, min.dot, min.n))) loop(pos)
        else {
          val vm = validMoves(min, part1)
          val newNodes = vm.map { case (x, y) =>
            val (xx, yy) = (min.x + x, min.y + y)
            val n        = if (min.dot == (x, y)) min.n + 1 else 1
            Node(xx, yy, (x, y), n, min.heatLost + input(yy)(xx).toInt, min.fullPath :+ (xx, yy))
          }
          newNodes.foreach(n => queue.enqueue(n))
          loop(pos.updated((min.x, min.y, min.dot, min.n), min))
        }
      }
    val positions = loop(Map.empty)
    positions.filter((_, node) => lastNode(node)).map { case (_, node) => node }.minBy(_.heatLost)
  }

  val p1 = test("p1") {
    val task = search(Node(0, 0, heatLost = 0, fullPath = Vector((0, 0))), true)
    println("p1")
    showPath(task.fullPath)
    assert(task.heatLost)(Assertion.equalTo(684))
  }

  val p2 = test("p2") {
    val task = search(Node(0, 0, heatLost = 0, fullPath = Vector((0, 0))), false)
    println("p2")
    showPath(task.fullPath)
    assert(task.heatLost)(Assertion.equalTo(822))
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("17")(p1, p2) @@ TestAspect.sequential
}
