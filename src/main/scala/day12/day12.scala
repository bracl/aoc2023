package day12

import utils.ReadFiles
import zio.Scope
import zio.test.{Assertion, Spec, TestAspect, TestEnvironment, ZIOSpecDefault, assert}

import scala.annotation.tailrec

object day12 extends ZIOSpecDefault {

  case class Springs(str: String, groups: Vector[Int]) {

    def unfold(n: Int = 5): Springs =
      Springs(List.fill(n)(str).mkString("?"), Vector.fill(n)(groups).flatten)
  }

  object Springs {

    def apply(s: String): Springs = s match
      case s"$springs $groups" => Springs(springs, groups.split(",").map(_.toInt).toVector)
  }

  @tailrec
  // This would work if only #s could be replaced with ?s...
  def parse(str: String, groupsToFind: Vector[Int], combinations: Vector[Int]): (String, Vector[Int], Vector[Int]) =
    if (groupsToFind.isEmpty) (str, groupsToFind, combinations)
    else {
      val withoutLeadingDots = str.dropWhile(_ == '.')
      val (nextGroup, rest) =
        if (withoutLeadingDots.contains('.')) withoutLeadingDots.splitAt(withoutLeadingDots.indexOf('.'))
        else (withoutLeadingDots, "")
      val numWindows = nextGroup.sliding(groupsToFind.head).length
      parse(rest, groupsToFind.tail, combinations.appended(numWindows))
    }

  def walk(
    str: String,
    groups: Vector[Int],
  ): Long = {
    var states: Map[String, Long] = Map()

    def loop(
      str: String,
      gs: Vector[Int],
      i: Int,
      g: Int,
      currentNumHashes: Int,
      acc: Long,
    ): Long = {
      val key = s"$i-$g-$currentNumHashes"
      if (states.contains(key))
        states(key)
      else if (i == str.length) {
        (g, currentNumHashes) match {
          case (group, 0) if group == gs.length                                             => acc + 1
          case (group, lastGroupSize) if group == (gs.length - 1) && gs(g) == lastGroupSize => acc + 1
          case _                                                                            => acc + 0
        }
      } else {
        val res = List('.', '#')
          .map(c =>
            if (str(i) == c || str(i) == '?') {
              if (c == '.' && currentNumHashes == 0)
                loop(str, gs, i + 1, g, 0, 0)
              else if (c == '.' && g < gs.length && gs(g) == currentNumHashes)
                loop(str, gs, i + 1, g + 1, 0, 0)
              else if (c == '#')
                loop(str, gs, i + 1, g, currentNumHashes + 1, 0)
              else acc
            } else 0
          )
        states = states.updated(key, res.sum)
        res.sum
      }
    }
    val res = loop(str, groups, 0, 0, 0, 0)
    println(s"$str => $res")
    res
  }

  val input = ReadFiles.readFile(12).map(Springs.apply)
  input.foreach(println)

  val p1 = test("p1") {
    val task = input.map(springs => walk(springs.str, springs.groups))
    assert(task.sum)(Assertion.equalTo(7379))
  }

  val p2 = test("p2") {
    val task = input.map(springs => walk(springs.unfold().str, springs.unfold().groups))
    assert(task.sum)(Assertion.equalTo(7732028747925L))
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("12")(p1, p2) @@ TestAspect.sequential
}
