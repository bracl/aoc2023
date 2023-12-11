package day11

import utils.ReadFiles
import zio.Scope
import zio.test.{Assertion, Spec, TestAspect, TestEnvironment, ZIOSpecDefault, assert}

object day11 extends ZIOSpecDefault {

  type VVS = Vector[Vector[String]]
  val star = "#"

  def toExpand(vecs: VVS): Set[Int] = vecs.zipWithIndex.filterNot(_._1.contains(star)).map(_._2).toSet

  val inputText  = ReadFiles.readFile(11)
  val input      = inputText.map(_.split("").toVector)
  val ysToExpand = toExpand(input)
  val xsToExpand = toExpand(input.transpose)

  def allStars(in: VVS): Seq[(Int, Int)] = for {
    x <- Range.inclusive(0, in.head.length - 1)
    y <- Range.inclusive(0, in.length - 1)
    if in(y)(x) == star
  } yield (x, y)

  val stars = allStars(input)

  def dist(star1: (Int, Int), star2: (Int, Int), xs: Set[Int], ys: Set[Int], expansion: Int): Long = {
    val (y0, y1) = if (star1._2 < star2._2) (star1._2, star2._2) else (star2._2, star1._2)
    val (x0, x1) = if (star1._1 < star2._1) (star1._1, star2._1) else (star2._1, star1._1)
    val xExpands = xs.count(x => x0 < x && x < x1)
    val yExpands = ys.count(y => y0 < y && y < y1)
    x1 - x0 + y1 - y0 + (xExpands + yExpands) * (expansion - 1)
  }

  def distances(coords: Seq[(Int, Int)], expansion: Int): Long =
    coords.combinations(2).map(stars => dist(stars.head, stars.last, xsToExpand, ysToExpand, expansion)).sum

  val p1 = test("p1") {
    val task = distances(stars, 2)
    assert(task)(Assertion.equalTo(9545480))
  }

  val p2 = test("p2") {
    val task = distances(stars, 1000000)
    assert(task)(Assertion.equalTo(406725732046L))
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("11")(p1, p2) @@ TestAspect.sequential
}
