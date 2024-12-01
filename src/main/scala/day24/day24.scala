package day24

import utils.ReadFiles
import zio.Scope
import zio.test.{Assertion, Spec, TestAspect, TestEnvironment, ZIOSpecDefault, assert}

object day24 extends ZIOSpecDefault {

  val input = ReadFiles.readFile(24)
  val hails = input.map(Hail.apply)

  extension (c: (Double, Double, Double)) {
    def -(c1: (Double, Double, Double)) = (c._1 - c1._1, c._2 - c1._2, c._3 - c1._3)
    def +(c1: (Double, Double, Double)) = (c._1 + c1._1, c._2 + c1._2, c._3 + c1._3)

    def x(c1: (Double, Double, Double)) = {
      val cx: Double = (c._2 * c1._3) - (c._3 * c1._2)
      val cy: Double = (c._3 * c1._1) - (c._1 * c1._3)
      val cz: Double = (c._1 * c1._2) - (c._2 * c1._1)
      (cx, cy, cz)
    }

    def *(c1: (Double, Double, Double)): Double = c._1 * c1._1 + c._2 * c1._2 + c._3 * c1._3
    def *(i: Double)                            = (c._1 * i, c._2 * i, c._3 * i)
    def /(i: Double)                            = (c._1 / i, c._2 / i, c._3 / i)
  }

  case class Hail(px: Double, py: Double, pz: Double, dx: Double, dy: Double, dz: Double) {

    val p: (Double, Double, Double) = (px, py, pz)
    val v: (Double, Double, Double) = (dx, dy, dz)

    val m: Double = dy / dx

    def c: Double = -(m * px - py)

    def intersect(h: Hail) = {
      val iX = (-h.c + c) / (-m + h.m)
      val iY = (c * h.m - h.c * m) / (-m + h.m)
      (iX, iY)
    }
  }

  object Hail {

    def apply(s: String): Hail =
      s match {
        case s"$px, $py, $pz @ $dx, $dy, $dz" =>
          Hail(px.toDouble, py.toDouble, pz.toDouble, dx.toDouble, dy.toDouble, dz.toDouble)
      }
  }

  def permute(hailStones: Seq[Hail], testLower: Double, testUpper: Double): Int =
    hailStones.combinations(2).count { hailPair =>
      val hailA    = hailPair.head
      val hailB    = hailPair.last
      val (iX, iY) = hailA.intersect(hailB)
      val inArea   = testLower <= iX && iX <= testUpper && testLower <= iY && iY <= testUpper
      val futureA  = (iX - hailA.px) / hailA.dx >= 0
      val futureB  = (iX - hailB.px) / hailB.dx >= 0
      inArea && futureA && futureB
    }

  def findRock(h0: Hail, h1: Hail, h2: Hail): (Double, Double, Double) = {
    val p1 = h1.p - h0.p
    val v1 = h1.v - h0.v
    val p2 = h2.p - h0.p
    val v2 = h2.v - h0.v
    val t1 = ((p1 x p2) * v2) * -1 / ((v1 x p2) * v2)
    val t2 = ((p1 x p2) * v1) * -1 / ((p1 x v2) * v1)

    val c1 = h1.p + (h1.v * t1)
    val c2 = h2.p + (h2.v * t2)
    val v  = (c2 - c1) / (t2 - t1)
    val p  = c1 - (v * t1)
    p
  }

  val p1 = test("p1") {
    val task = permute(hails, 200000000000000d, 400000000000000d)
    assert(task)(Assertion.equalTo(13754))
  }

  val p2 = test("p2") {
    val task = findRock(hails.last, hails(4), hails(17))
    assert((task._1 + task._2 + task._3).toLong)(Assertion.equalTo(711031616315001L))
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("24")(p1, p2) @@ TestAspect.sequential
}
