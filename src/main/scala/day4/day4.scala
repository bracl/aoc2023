package day4

import utils.ReadFiles
import zio.Scope
import zio.test.{Assertion, Spec, TestAspect, TestEnvironment, ZIOSpecDefault, assert}

object day4 extends ZIOSpecDefault {

  /// Card   1: 33 13 28 76 16 91 52 41 38 64 | 52 10  7 61 12 70 84 38 16 40  5 49 33 11 31 43 71 28 72 23 98 47 14 44 90

  case class Card(num: Int, have: Vector[Int], winning: Vector[Int]) {
    val numWins: Int = have.count(winning.contains)

    val score: Int = numWins match {
      case 0 => 0
      case x => scala.math.pow(2, x - 1).toInt
    }
  }

  object Card {

    def apply(s: String): Card = {
      val id   = s.split(":").head.split(" ").last
      val nums = s.split(":").last.split("\\|").map(_.trim).map(_.split("\\s+")).map(_.toVector)
      Card(id.toInt, nums.head.map(_.toInt), nums.last.map(_.toInt))
    }
  }

  val input = ReadFiles.readFile(4).map(Card.apply)
  input.foreach(println)

  def collectingCards(cards: Vector[Card]): Map[Int, Int] =
    cards.foldLeft(cards.map(c => c.num -> 1).toMap) { case (cardMap, c) =>
      val range = Range.inclusive(c.num + 1, c.num + c.numWins)
      range.foldLeft(cardMap) { case (m, n) =>
        m.updated(n, m(n) + m(c.num))
      }
    }

  val p1 = test("p1") {
    val task = input.map(_.score).sum
    assert(task)(Assertion.equalTo(23673))
  }

  val p2 = test("p2") {
    val allCards = collectingCards(input)
    val task     = allCards.values.sum
    assert(task)(Assertion.equalTo(12263631))
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("4")(p1, p2) @@ TestAspect.sequential
}
