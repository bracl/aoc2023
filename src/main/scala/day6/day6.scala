package day6

import utils.ReadFiles
import zio.Scope
import zio.test.{Assertion, Spec, TestAspect, TestEnvironment, ZIOSpecDefault, assert}

import scala.annotation.tailrec

object day6 extends ZIOSpecDefault {

  val input = ReadFiles.readFile(6)

  val races: Array[(Int, Int)] =
    input.head
      .split(":")
      .last
      .trim
      .split("\\s+")
      .map(_.toInt)
      .zip(input.last.split(":").last.trim.split("\\s+").map(_.toInt))

  val oneRace = (input.head.filter(_.isDigit).mkString("").toLong, input.last.filter(_.isDigit).mkString("").toLong)

  input.foreach(println)

  def distance(hold: Long, totalSeconds: Long): Long = (totalSeconds - hold) * hold

  val p1 = test("p1") {
    val task = races.map { case (time, dist) => Range.Long.inclusive(0, time, 1L).count(t => distance(t, time) > dist) }
    assert(task.product)(Assertion.equalTo(512295))
  }

  val p2 = test("p2") {

    def binarySearch(totalTime: Long, distToBeat: Long): Long = {

      @tailrec
      def loop(lower: Long, upper: Long, comparisonFn: (Long, Long) => Boolean): (Long, Long) = {
        val mid = lower + (upper - lower) / 2
        if (lower + 1 == upper) {
          (lower, upper)
        } else {
          if (comparisonFn(distance(mid, totalTime), distToBeat))
            loop(lower, mid, comparisonFn)
          else
            loop(mid, upper, comparisonFn)
        }
      }

      val first = loop(0, (totalTime / 2) - 1, (x, y) => x > y)._2
      val last  = loop((totalTime / 2) + 1, totalTime, (x, y) => x < y)._2
      last - first
    }

    val res = binarySearch(oneRace._1, oneRace._2)
    assert(res)(Assertion.equalTo(36530883))
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("6")(p1, p2) @@ TestAspect.sequential
}
