package day21

import utils.ReadFiles
import zio.Scope
import zio.test.{Assertion, Spec, TestAspect, TestEnvironment, ZIOSpecDefault, assert}

import scala.annotation.tailrec

object day21 extends ZIOSpecDefault {

  case class Coord(x: Int, y: Int) {

    def allSteps: Set[Coord] = Set(Coord(x, y - 1), Coord(x, y + 1), Coord(x + 1, y), Coord(x - 1, y))
  }

  def isRock(c: Coord, rocks: Set[Coord], width: Int, height: Int): Boolean = {
    val x = ((c.x % width) + width)   % width
    val y = ((c.y % height) + height) % height
    rocks.contains(Coord(x, y))
  }

  case class Garden(start: Coord, step: Int, rocks: Set[Coord], reachablePlots: Set[Coord], dimension: Int) {

    def availableSteps: Set[Coord] =
      reachablePlots.flatMap(rp => rp.allSteps).filterNot(c => isRock(c, rocks, dimension, dimension))

    def takeStep: Garden =
      val nextSteps = availableSteps
      this.copy(step = step + 1, reachablePlots = nextSteps)
  }

  object Garden {

    def apply(in: Seq[Seq[String]]): Garden =
      val (start, rocks) = in.zipWithIndex.foldLeft((Coord(-1, -1), Set.empty[Coord])) {
        case ((start, rocks), (row, y)) =>
          row.zipWithIndex.foldLeft((start, rocks)) { case ((start2, rocks2), (char, x)) =>
            char match
              case "S" => (Coord(x, y), rocks2)
              case "#" => (start2, rocks2 + Coord(x, y))
              case _   => (start2, rocks2)
          }
      }
      Garden(start, 0, rocks, Set(start), in.length)
  }

  def takeStep(garden: Garden): Garden =
    val nextSteps = garden.availableSteps
    garden.copy(step = garden.step + 1, reachablePlots = nextSteps)

  @tailrec
  def walk(in: Garden, steps: Int): Garden =
    if (steps == 0)
      in
    else
      walk(in.takeStep, steps - 1)

  val input = ReadFiles.readFile(21)
//  val input = """...........
//                |.....###.#.
//                |.###.##..#.
//                |..#.#...#..
//                |....#.#....
//                |.##..S####.
//                |.##..#...#.
//                |.......##..
//                |.##.#.####.
//                |.##..##.##.
//                |...........""".stripMargin.split("\n")
  input.foreach(println)
  val garden = Garden(input.map(_.toList.map(_.toString)))

  val p1 = test("p1") {
    val task = walk2(garden, 64)
    assert(task.reachablePlots.size)(Assertion.equalTo(3642))
  }

  @tailrec
  def walk2(in: Garden, steps: Int, count: Int = 0): Garden =
    println(s"$count/$steps")
    if (count == steps)
      in
    else
      walk2(in.takeStep, steps, count+1)

  val p2 = test("p2") {
    val steps     = 26501365
    val width     = input.head.length
    val cycles    = steps / width
    val remainder = steps % width
    val one       = walk2(garden, width + remainder)
    val two       = walk2(garden, 2 * width + remainder)
    val three     = walk2(garden, 3 * width + remainder)

    def solve(cycles: Long, one: (Int, Int), two: (Int, Int), three: (Int, Int)): Long = {
      val x  = cycles
      val x1 = one._1
      val y1 = one._2
      val x2 = two._1
      val y2 = two._2
      val x3 = three._1
      val y3 = three._2
      ((x - x2) * (x - x3)) / ((x1 - x2) * (x1 - x3)) * y1 +
        ((x - x1) * (x - x3)) / ((x2 - x1) * (x2 - x3)) * y2 +
        ((x - x1) * (x - x2)) / ((x3 - x1) * (x3 - x2)) * y3
    }

    // too low 468225490794
    // too high 608609039941752
    //          608603023105276
    //          1363439550630552
    assert(solve(cycles, (1, one.reachablePlots.size), (2, two.reachablePlots.size), (3, three.reachablePlots.size)))(
      Assertion.equalTo(0)
    )
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("21")(p1, p2) @@ TestAspect.sequential
}
