package day13

import utils.ReadFiles
import zio.Scope
import zio.test.{Assertion, Spec, TestAspect, TestEnvironment, ZIOSpecDefault, assert}

object day13 extends ZIOSpecDefault {

  def search(input: Vector[String], banned: Option[Int] = None): Option[Int] =
    Range(1, input.length).filterNot(banned.contains).find { i =>
      val (before, after) = input.splitAt(i)
      val toCompare       = math.min(before.length, after.length)
      before.takeRight(toCompare) == after.take(toCompare).reverse
    }

  def fold(input: Vector[String], bannedX: Option[Int] = None, bannedY: Option[Int] = None): (Int, Int) = {
    val rows    = input
    val columns = input.transpose.map(_.mkString(""))
    val x       = search(columns, bannedX).getOrElse(0)
    val y       = search(rows, bannedY).getOrElse(0)
    (x, y)
  }

  def flip(input: Vector[String], x: Int, y: Int): Vector[String] =
    input(y)(x) match
      case '.' => input.updated(y, input(y).updated(x, '#'))
      case '#' => input.updated(y, input(y).updated(x, '.'))

  def foldSmudged(input: Vector[String]): (Int, Int) = {
    val existing: (Int, Int) = fold(input)
    val banX                 = Option.when(existing._1 > 0)(existing._1)
    val banY                 = Option.when(existing._2 > 0)(existing._2)
    val coords = for {
      y <- Range(0, input.length)
      x <- Range(0, input.head.length)
    } yield (x, y)
    val withSmudge = coords.find { case (x, y) =>
      val smudged             = flip(input, x, y)
      val newFold: (Int, Int) = fold(smudged, banX, banY)
      newFold != (0, 0)
    }
    withSmudge match
      case Some((x, y)) =>
        val flipped = flip(input, x, y)
        val res     = fold(flipped, banX, banY)
        res
      case None => ???
  }

  val inputText: Vector[Vector[String]] = ReadFiles.readGrouped(13)

  val p1 = test("p1") {
    val task = inputText.map(s => fold(s)).map { case (x, y) => y * 100 + x }
    assert(task.sum)(Assertion.equalTo(26957))
  }

  val p2 = test("p2") {
    val task = inputText.map(foldSmudged).map { case (x, y) => y * 100 + x }
    assert(task.sum)(Assertion.equalTo(42695))
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("13")(p1, p2) @@ TestAspect.sequential
}
