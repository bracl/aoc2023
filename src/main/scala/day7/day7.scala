package day7

import utils.ReadFiles
import zio.Scope
import zio.test.{Assertion, Spec, TestAspect, TestEnvironment, ZIOSpecDefault, assert}

object day7 extends ZIOSpecDefault {

  val joker = "J"

  def allCards(withJack: Boolean, withJoker: Boolean): Vector[String] =
    if (withJack) Vector("2", "3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K", "A")
    else if (withJoker) Vector(joker, "2", "3", "4", "5", "6", "7", "8", "9", "T", "Q", "K", "A")
    else Vector("2", "3", "4", "5", "6", "7", "8", "9", "T", "Q", "K", "A")

  def cardValue(card: String, jokers: Boolean): Int = allCards(!jokers, jokers).indexOf(card)

  def containsX(cards: Vector[String], x: Int, jokers: Boolean) = if (jokers) {
    allCards(false, false).exists { c =>
      val replaced = cards.map(cc => if (cc == joker) c else cc)
      replaced.map(c => replaced.count(_ == c)).contains(x)
    }
  } else cards.map(c => cards.count(_ == c)).contains(x)

  def fiveOfaKind(cards: Vector[String], jokers: Boolean): Boolean = containsX(cards, 5, jokers)

  def fourOfaKind(cards: Vector[String], jokers: Boolean): Boolean = containsX(cards, 4, jokers)

  def threeOfaKind(cards: Vector[String], jokers: Boolean): Boolean = containsX(cards, 3, jokers)

  def twoOfaKind(cards: Vector[String], jokers: Boolean): Boolean = containsX(cards, 2, jokers)

  def isPattern(cards: Vector[String], pattern: Vector[Int]): Boolean =
    cards.map(c => cards.count(_ == c)).sorted == pattern

  def fullHouse(cards: Vector[String], jokers: Boolean): Boolean = {
    val fh = Vector(2, 2, 3, 3, 3)
    if (jokers) {
      allCards(false, false).exists { c =>
        val replaced = cards.map(cc => if (cc == joker) c else cc)
        isPattern(replaced, fh)
      }
    } else isPattern(cards, fh)
  }

  def twoPair(cards: Vector[String], jokers: Boolean): Boolean = {
    val tp   = Vector(1, 2, 2, 2, 2)
    val four = fourOfaKind(cards, jokers)
    if (four) false
    else if (jokers) {
      allCards(false, false).exists { c =>
        val replaced = cards.map(cc => if (cc == joker) c else cc)
        isPattern(replaced, tp)
      }
    } else isPattern(cards, tp)
  }

  def highCard(cards: Vector[String], jokers: Boolean): Boolean = {
    val high  = twoOfaKind(cards, jokers)
    val pair  = twoOfaKind(cards, jokers)
    val tPair = twoPair(cards, jokers)
    val trip  = threeOfaKind(cards, jokers)
    val full  = fullHouse(cards, jokers)
    val four  = fourOfaKind(cards, jokers)
    val five  = fiveOfaKind(cards, jokers)
    !high && !pair && !tPair && !trip && !full && !four && !five
  }

  case class Hand(cards: Vector[String], bid: Int, jokers: Boolean) {

    val score = cards match {
      case cs if fiveOfaKind(cs, jokers)  => 7
      case cs if fourOfaKind(cs, jokers)  => 6
      case cs if fullHouse(cs, jokers)    => 5
      case cs if threeOfaKind(cs, jokers) => 4
      case cs if twoPair(cs, jokers)      => 3
      case cs if twoOfaKind(cs, jokers)   => 2
      case cs if highCard(cs, jokers)     => 1
    }

    def isWeakerThan(h2: Hand): Boolean =
      if (h2.score > score) true
      else if (score > h2.score) false
      else {
        val cardValues = cards.zip(h2.cards).map { case (x, y) => (cardValue(x, jokers), cardValue(y, jokers)) }
        val diff       = cardValues.find { case (x, y) => x != y }
        val res = diff match
          case None                  => false
          case Some((x, y)) if x < y => true
          case _                     => false
        res
      }

  }

  object Hand {

    def apply(s: String): Hand = {
      val splt  = s.split(" ")
      val bid   = splt.last.toInt
      val cards = splt.head.toList.map(_.toString).toVector
      Hand(cards, bid, false)
    }

    def apply(s: String, j: Boolean): Hand = {
      val splt  = s.split(" ")
      val bid   = splt.last.toInt
      val cards = splt.head.toList.map(_.toString).toVector
      Hand(cards, bid, j)
    }
  }

  val input = ReadFiles.readFile(7)

  val p1 = test("p1") {
    val sortedHands = input.map(Hand.apply).sortWith(_.isWeakerThan(_))
    sortedHands.foreach(h => println(h.cards.mkString("")))
    val task = sortedHands.zipWithIndex.map { case (h, i) => h.bid * (i + 1) }.sum
    assert(task)(Assertion.equalTo(248422077))
  }

  val p2 = test("p2") {
    val sortedHands = input.map(s => Hand.apply(s, true)).sortWith(_.isWeakerThan(_))
    val task        = sortedHands.zipWithIndex.map { case (h, i) => h.bid * (i + 1) }.sum
    assert(task)(Assertion.equalTo(249817836))
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("7")(p1, p2) @@ TestAspect.sequential
}
