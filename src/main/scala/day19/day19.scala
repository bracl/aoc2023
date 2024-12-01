package day19

import utils.ReadFiles
import zio.Scope
import zio.test.{Assertion, Spec, TestAspect, TestEnvironment, ZIOSpecDefault, assert}

object day19 extends ZIOSpecDefault {

  val input       = ReadFiles.readFile(19)
  val (ps, flows) = input.filterNot(_.isEmpty).partition(_.startsWith("{"))

  def deconstructRule(r: String): (String, String, Int) =
    (r(0).toString, r(1).toString, r.filter(_.isDigit).mkString("").toInt)

  case class Workflow(name: String, rules: List[String]) {

    def dest(r: String): String = r.split(":").last

    def rule(r: String): Part => Boolean =
      if (r.contains(":")) {
        val (variable, op, num) = deconstructRule(r)
        (variable, op) match {
          case ("x", ">") => (p: Part) => p.x > num
          case ("x", "<") => (p: Part) => p.x < num
          case ("m", ">") => (p: Part) => p.m > num
          case ("m", "<") => (p: Part) => p.m < num
          case ("a", ">") => (p: Part) => p.a > num
          case ("a", "<") => (p: Part) => p.a < num
          case ("s", ">") => (p: Part) => p.s > num
          case ("s", "<") => (p: Part) => p.s < num
        }
      } else _ => true

    def process(p: Part): String = {
      def loop(remainingRules: List[String]): String =
        remainingRules match {
          case h :: _ if rule(h)(p) => dest(h)
          case h :: Nil             => dest(h)
          case _ :: t               => loop(t)
        }
      loop(rules)
    }

    def ranges(r: Range): List[(String, Range)] = {

      def loop(remainingRules: List[String], range: Range, acc: List[(String, Range)]): List[(String, Range)] =
        remainingRules match {
          case Nil      => acc
          case h :: Nil => acc ++ List((dest(h), range))
          case h :: t =>
            if (!h.exists(_.isDigit)) {
              loop(Nil, range, acc ++ List((dest(h), range)))
            } else {
              val (variable, op, num) = deconstructRule(h)

              val (goSomewhereElse, stayInWorkflow): (Option[Range], Option[Range]) = if (op == "<") {
                variable match {
                  case "x" if num < range.x0 => (None, Some(range))
                  case "x" if num < range.x1 => (Some(range.copy(x1 = num - 1)), Some(range.copy(x0 = num)))
                  case "x"                   => (Some(range), None)
                  case "m" if num < range.m0 => (None, Some(range))
                  case "m" if num < range.m1 => (Some(range.copy(m1 = num - 1)), Some(range.copy(m0 = num)))
                  case "m"                   => (Some(range), None)
                  case "a" if num < range.a0 => (None, Some(range))
                  case "a" if num < range.a1 => (Some(range.copy(a1 = num - 1)), Some(range.copy(a0 = num)))
                  case "a"                   => (Some(range), None)
                  case "s" if num < range.s0 => (None, Some(range))
                  case "s" if num < range.s1 => (Some(range.copy(s1 = num - 1)), Some(range.copy(s0 = num)))
                  case "s"                   => (Some(range), None)
                }
              } else {
                variable match {
                  case "x" if num > range.x1 => (None, Some(range))
                  case "x" if num > range.x0 => (Some(range.copy(x0 = num + 1)), Some(range.copy(x1 = num)))
                  case "x"                   => (Some(range), None)
                  case "m" if num > range.m1 => (None, Some(range))
                  case "m" if num > range.m0 => (Some(range.copy(m0 = num + 1)), Some(range.copy(m1 = num)))
                  case "m"                   => (Some(range), None)
                  case "a" if num > range.a1 => (None, Some(range))
                  case "a" if num > range.a0 => (Some(range.copy(a0 = num + 1)), Some(range.copy(a1 = num)))
                  case "a"                   => (Some(range), None)
                  case "s" if num > range.s1 => (None, Some(range))
                  case "s" if num > range.s0 => (Some(range.copy(s0 = num + 1)), Some(range.copy(s1 = num)))
                  case "s"                   => (Some(range), None)
                }
              }

              stayInWorkflow match
                case Some(stay) => loop(t, stay, acc ++ goSomewhereElse.map(r => (dest(h), r)).toList)
                case None       => loop(Nil, range, acc ++ goSomewhereElse.map(r => (dest(h), r)).toList)

            }
        }

      loop(rules, r, List.empty)
    }

  }

  object Workflow {

    def apply(s: String): Workflow = {
      val name  = s.takeWhile(_.isLetter).mkString("")
      val rules = s.dropWhile(_.isLetter).drop(1).dropRight(1).mkString("").split(",").toList
      Workflow(name, rules)
    }
  }

  case class Range(x0: Int, x1: Int, m0: Int, m1: Int, a0: Int, a1: Int, s0: Int, s1: Int) {

    def score: Long = {
      val x: Long = x1 + 1 - x0
      val m: Long = m1 + 1 - m0
      val a: Long = a1 + 1 - a0
      val s: Long = s1 + 1 - s0
      x * m * a * s
    }

  }

  case class Part(x: Int, m: Int, a: Int, s: Int) {
    def score = x + m + a + s

    def process(wfs: Map[String, Workflow]): Boolean = {

      def loop(wfName: String): Boolean = wfs.get(wfName) match
        case None => ???
        case Some(wf) =>
          wf.process(this) match
            case "R" => false
            case "A" => true
            case x   => loop(x)

      loop("in")
    }
  }

  object Part {

    def apply(s: String): Part = {

      val parts = s.split(",")
      Part(
        parts(0).filter(_.isDigit).mkString("").toInt,
        parts(1).filter(_.isDigit).mkString("").toInt,
        parts(2).filter(_.isDigit).mkString("").toInt,
        parts(3).filter(_.isDigit).mkString("").toInt
      )
    }
  }

  val workflows = flows.map(Workflow.apply).map(wf => wf.name -> wf).toMap
  val parts     = ps.map(Part.apply)

  def walk(ranges: Set[(String, Range)]): Set[(String, Range)] = {
    val (acc, rest) = ranges.partition(_._1 == "A")
    val (_, rest2)  = rest.partition(_._1 == "R")
    val newState    = rest2.flatMap { case (wfName, range) => workflows(wfName).ranges(range) }
    newState match
      case s if s.isEmpty => acc
      case s              => walk(acc ++ s)
  }

  val p1 = test("p1") {
    val accepted = parts.filter(_.process(workflows))
    val task     = accepted.map(_.score).sum
    assert(task)(Assertion.equalTo(350678))
  }

  val p2 = test("p2") {
    val task      = walk(Set(("in", Range(1, 4000, 1, 4000, 1, 4000, 1, 4000))))
    val res: Long = task.map(_._2.score).sum
    assert(res)(Assertion.equalTo(124831893423809L))
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("19")(p1, p2) @@ TestAspect.sequential
}
