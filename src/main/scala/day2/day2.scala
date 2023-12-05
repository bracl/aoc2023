package day2

import utils.ReadFiles
import zio.Scope
import zio.test.{Assertion, Spec, TestAspect, TestEnvironment, ZIOSpecDefault, assert}

object day2 extends ZIOSpecDefault {

  /// Game 1: 7 red, 8 blue; 6 blue, 6 red, 2 green; 2 red, 6 green, 8 blue; 9 green, 2 red, 4 blue; 6 blue, 4 green
  def toGame(l: (String, Int)): Game = {
    val turns: Seq[(Int, Int, Int)] = l._1.split(":").last.split(";").toSeq.map { t =>
      t.split(",").foldLeft((0, 0, 0)) { case (rgb, tokens) =>
        tokens match {
          case r if r.contains("red")   => (rgb._1 + r.filter(_.isDigit).toInt, rgb._2, rgb._3)
          case g if g.contains("green") => (rgb._1, rgb._2 + g.filter(_.isDigit).toInt, rgb._3)
          case b if b.contains("blue")  => (rgb._1, rgb._2, rgb._3 + b.filter(_.isDigit).toInt)
        }
      }
    }
    Game(l._2 + 1, turns)
  }

  val input = ReadFiles.readFile(2).zipWithIndex.map(toGame)
  input.foreach(println)

  // R G B
  case class Game(id: Int, turns: Seq[(Int, Int, Int)]) {
    val power: Int = turns.maxBy(_._1)._1 * turns.maxBy(_._2)._2 * turns.maxBy(_._3)._3

    def possible(red: Int, green: Int, blue: Int): Boolean =
      turns.forall { case (r, g, b) => r <= red && g <= green && b <= blue }
  }

  val p1 = test("p1 - only 12 red cubes, 13 green cubes, and 14 blue cubes") {
    val task = input.filter(_.possible(12, 13, 14))

    assert(task.map(_.id).sum)(Assertion.equalTo(2679))
  }

  val p2 = test("p2") {
    val task = input.map(_.power).sum
    assert(task)(Assertion.equalTo(77607))
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("2")(p1, p2) @@ TestAspect.sequential
}
