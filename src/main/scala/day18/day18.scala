package day18

import utils.ReadFiles
import zio.Scope
import zio.test.{Assertion, Spec, TestAspect, TestEnvironment, ZIOSpecDefault, assert}

object day18 extends ZIOSpecDefault {

  case class Instruction(dot: (Int, Int), n: Int, colour: String)

  object Instruction {

    def apply(s: String): Instruction = s match {
      case s"$dir $n $colour" =>
        dir match
          case "R" => Instruction((1, 0), n.toInt, colour)
          case "L" => Instruction((-1, 0), n.toInt, colour)
          case "U" => Instruction((0, -1), n.toInt, colour)
          case "D" => Instruction((0, 1), n.toInt, colour)
    }

    def applyP2(s: String): Instruction = s match {
      case s"$_(#$hex)" =>
        hex match
          case h =>
            val (dist, dir) = h.splitAt(5)
            val dot = dir match {
              case "0" => (1, 0)
              case "1" => (0, 1)
              case "2" => (-1, 0)
              case "3" => (0, -1)
            }
            val n = Integer.parseInt(dist, 16)
            Instruction(dot, n, "")
    }
  }

  val inputText      = ReadFiles.readFile(18)
  val p1instructions = inputText.map(Instruction.apply)
  val p2instructions = inputText.map(Instruction.applyP2)

  def corners(instructions: Vector[Instruction]): Vector[(Long, Long)] = instructions
    .foldLeft(Vector((0L, 0L))) { case (coords, i) =>
      val (x, y) = coords.last
      val next   = (x + i.n * i.dot._1, y + i.n * i.dot._2)
      coords :+ next
    }

  def shoelace(coordinates: Vector[(Long, Long)]): Long =
    coordinates.sliding(2).map(v => v.head._1 * v.last._2 - v.head._2 * v.last._1).sum / 2

  def solve(i: Vector[Instruction]): Long = {
    val cs  = corners(i)
    val ans = shoelace(cs) + i.foldLeft(0) { case (p, i) => p + i.n } / 2 + 1
    ans
  }

  val p1 = test("p1") {
    val task = solve(p1instructions)
    assert(task)(Assertion.equalTo(70253))
  }

  val p2 = test("p2") {
    val task = solve(p2instructions)
    assert(task)(Assertion.equalTo(131265059885080L))
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("18")(p1, p2) @@ TestAspect.sequential
}
