package day15

import utils.ReadFiles
import zio.Scope
import zio.test.{Assertion, Spec, TestAspect, TestEnvironment, ZIOSpecDefault, assert}

object day15 extends ZIOSpecDefault {

  def hash(str: String): Int = str.foldLeft(0) { case (i, c) => (i + c.toInt) * 17 % 256 }

  def curateBox(current: Option[Vector[(String, Int)]], label: String, digit: Option[Int]): Vector[(String, Int)] =
    (current, digit) match {
      case (Some(existing), Some(x)) if existing.exists(_._1 == label) =>
        existing.updated(existing.indexWhere(_._1 == label), (label, x))
      case (Some(existing), Some(x)) => existing.appended((label, x))
      case (None, Some(x))           => Vector((label, x))
      case (Some(value), None)       => value.filterNot(_._1 == label)
      case _                         => Vector()
    }

  def hashMap(ops: Vector[String]) = {
    val m: Map[Int, Vector[(String, Int)]] = Map.empty
    ops.foldLeft(m) { case (hashMap, op) =>
      val label = op.filter(_.isLetter).mkString("")
      val h     = hash(label)
      val digit = op.filter(_.isDigit).mkString("").toIntOption
      val vec   = curateBox(hashMap.get(h), label, digit)
      hashMap.updated(h, vec)
    }
  }

  def focussingPower(m: Map[Int, Vector[(String, Int)]]): Int =
    m.flatMap { case (i, v) => v.zipWithIndex.map { case ((_, lens), j) => (j + 1) * lens }.map(_ * (i + 1)) }.sum

  val input = ReadFiles.readFile(15).map(_.trim).head.split(",").toVector

  val p1 = test("p1") {
    val task = input.map(hash).sum
    assert(task)(Assertion.equalTo(517551))
  }

  val p2 = test("p2") {
    val task = hashMap(input)
    val fp   = focussingPower(task)
    assert(fp)(Assertion.equalTo(286097))
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("15")(p1, p2) @@ TestAspect.sequential
}
