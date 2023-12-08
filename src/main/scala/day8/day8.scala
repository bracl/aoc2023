package day8

import utils.ReadFiles
import zio.Scope
import zio.test.{Assertion, Spec, TestAspect, TestEnvironment, ZIOSpecDefault, assert}

object day8 extends ZIOSpecDefault {

  case class Node(name: String, left: String, right: String)

  val input        = ReadFiles.readFile(8)
  val instructions = input.head

  val nodes = input
    .filterNot(l => l.isEmpty || l == instructions)
    .map { case s"$name = ($left, $right)" =>
      Node(name, left, right)
    }
    .map(n => n.name -> n)
    .toMap
  nodes.foreach(println)

  def walk(
    instructions: String,
    index: Int,
    positions: Map[String, Node],
    position: Node,
    steps: Int,
    endCondition: String
  ): (String, Int, Map[String, Node], Node, Int, String) =
    if (position.name.endsWith(endCondition)) (instructions, index, positions, position, steps, endCondition)
    else {
      val loopedIndex = if (index == instructions.length) 0 else index
      val instruction = instructions(loopedIndex)
      val nextPositionName = instruction match {
        case 'L' => position.left
        case 'R' => position.right
      }
      val nextPosition = positions(nextPositionName)
      walk(instructions, loopedIndex + 1, positions, nextPosition, steps + 1, endCondition)
    }

  val p1 = test("p1") {
    val task = walk(instructions, 0, nodes, nodes("AAA"), 0, "ZZZ")._5
    assert(task)(Assertion.equalTo(19783))
  }

  // https://stackoverflow.com/questions/40875537/fp-lcm-in-scala-in-1-line
  def lcm(list: Seq[BigInt]): BigInt = list.foldLeft(1: BigInt) { (a, b) =>
    b * a / LazyList.iterate((a, b)) { case (x, y) => (y, x % y) }.dropWhile(_._2 != 0).head._1.abs
  }

  val p2 = test("p2") {
    val task = nodes.values
      .filter(_.name.endsWith("A"))
      .toList
      .map(node => walk(instructions, 0, nodes, node, 0, "Z")._5)
      .map(BigInt.apply)
    val steps: BigInt = lcm(task)
    assert(steps)(Assertion.equalTo(9177460370549L))
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("8")(p1, p2) @@ TestAspect.sequential
}
