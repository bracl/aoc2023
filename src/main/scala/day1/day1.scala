package day1

import utils.ReadFiles
import zio.Scope
import zio.test.{Assertion, Spec, TestAspect, TestEnvironment, ZIOSpecDefault, assert}

import scala.annotation.tailrec

object day1 extends ZIOSpecDefault {

  val input = ReadFiles.readFile(1)

  val p1 = test("p1") {
    val task = input.map(l => l.filter(_.isDigit)).map(digits => s"${digits.head}${digits.last}".toInt).sum
    assert(task)(Assertion.equalTo(54667))
  }

  val p2 = test("p2") {

    val numbers = Map(
      "one"   -> "1",
      "two"   -> "2",
      "three" -> "3",
      "four"  -> "4",
      "five"  -> "5",
      "six"   -> "6",
      "seven" -> "7",
      "eight" -> "8",
      "nine"  -> "9",
    )

    @tailrec
    def findDigit(str: String, f: (String, String) => Boolean, f2: String => String, f3: String => Char): String =
      str match {
        case _ if f3(str).isDigit                => f3(str).toString
        case _ if numbers.keys.exists(f(str, _)) => numbers.keys.find(f(str, _)).map(numbers(_)).get
        case _                                   => findDigit(f2(str), f, f2, f3)
      }

    val task = input.map { l =>
      val first = findDigit(l, (a, b) => a.startsWith(b), s => s.drop(1), s => s.head)
      val last  = findDigit(l, (a, b) => a.endsWith(b), s => s.dropRight(1), s => s.last)
      s"$first$last".toInt
    }.sum
    assert(task)(Assertion.equalTo(54203))
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("1")(p1, p2) @@ TestAspect.sequential
}
