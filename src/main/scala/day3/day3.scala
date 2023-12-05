package day3

import utils.ReadFiles
import zio.Scope
import zio.test.{Assertion, Spec, TestAspect, TestEnvironment, ZIOSpecDefault, assert}

import scala.annotation.tailrec

object day3 extends ZIOSpecDefault {

  val input = ReadFiles.readFile(3).map(_.replace(".", " "))
  input.foreach(println)

  def isNumber(x: Int, y: Int, input: Vector[String] = input) =
    y >= 0 && y < input.length && x >= 0 && x < input.head.length && input(y)(x).isDigit

  def isCharacter(x: Int, y: Int, input: Vector[String] = input) =
    y >= 0 && y < input.length && x >= 0 && x < input.head.length && (!input(y)(x).isSpaceChar && !input(y)(x).isDigit)

  def isGear(x: Int, y: Int, input: Vector[String] = input) =
    y >= 0 && y < input.length && x >= 0 && x < input.head.length && input(y)(x) == '*'

  def surroundingCoords(x: Int, y: Int, input: Vector[String] = input): (String, Set[(Int, Int)]) = {
    val numbers = input(y).substring(x).takeWhile(_.isDigit)
    val coords = Range
      .inclusive(x, x + numbers.length - 1)
      .toSet
      .flatMap(x =>
        for {
          x <- Set(-1, 0, 1).map(_ + x)
          y <- Set(-1, 0, 1).map(_ + y)
        } yield (x, y)
      )
    (numbers, coords)
  }

  @tailrec
  def loop(
    wholeInput: Vector[String],
    parts: List[Int],
    gears: List[(Int, Int, Int)],
    x: Int,
    y: Int
  ): (List[Int], List[(Int, Int, Int)]) = {
    val onNumber = isNumber(x, y, wholeInput)
    if (x >= wholeInput.head.length) loop(wholeInput, parts, gears, 0, y + 1)
    else if (y >= wholeInput.length) (parts, gears)
    else if (!onNumber) loop(wholeInput, parts, gears, x + 1, y)
    else {
      val (number, coordsToSearch) = surroundingCoords(x, y, input)
      val enginePart               = coordsToSearch.exists { case (x, y) => isCharacter(x, y, wholeInput) }
      val foundGears = coordsToSearch.collect { case (gx, gy) if isGear(gx, gy, wholeInput) => (gx, gy, number.toInt) }
      val updatedGears = if (foundGears.nonEmpty) gears ++ foundGears else gears
      if (enginePart) loop(wholeInput, parts.appended(number.toInt), updatedGears, x + number.length, y)
      else loop(wholeInput, parts, updatedGears, x + number.length, y)
    }
  }

  val p1 = test("p1") {
    val task = loop(input, List.empty, List.empty, 0, 0)
    assert(task._1.sum)(Assertion.equalTo(509115))
  }

  val p2 = test("p2") {
    val potentialGears = loop(input, List.empty, List.empty, 0, 0)._2
    val actualGears    = potentialGears.groupBy { case (x, y, _) => (x, y) }.filter { case (_, l) => l.length == 2 }
    val ratios         = actualGears.map { case (_, l) => l.head._3 * l.last._3 }
    assert(ratios.sum)(Assertion.equalTo(75220503))
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("3")(p1, p2) @@ TestAspect.sequential
}
