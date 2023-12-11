package day9

import utils.ReadFiles
import zio.Scope
import zio.test.{Assertion, Spec, TestAspect, TestEnvironment, ZIOSpecDefault, assert}

import scala.annotation.tailrec

object day9 extends ZIOSpecDefault {

  type VVI = Vector[Vector[Int]]

  @tailrec
  def goDown(sequences: VVI): VVI = {
    val diffs = sequences.last.sliding(2).map(s => s.last - s.head).toVector
    if (diffs.forall(_ == 0)) sequences.appended(diffs)
    else goDown(sequences.appended(diffs))
  }

  @tailrec
  def goUp(sequences: VVI, building: VVI, createFn: (VVI, VVI) => Vector[Int]): (VVI, VVI) =
    if (sequences.isEmpty) (sequences, building)
    else {
      if (sequences.last.forall(_ == 0)) goUp(sequences.dropRight(1), building.appended(sequences.last :+ 0), createFn)
      else goUp(sequences.dropRight(1), building.appended(createFn(sequences, building)), createFn)
    }

  def walk(
    in: Vector[Int],
    upFn: (VVI, VVI, (VVI, VVI) => Vector[Int]) => (VVI, VVI),
    predict: (VVI, VVI) => Vector[Int]
  ): Vector[Int] = {
    val down = goDown(Vector(in))
    val up   = upFn(down, Vector.empty[Vector[Int]], predict)
    up._2.last
  }

  val input: List[Vector[Int]] = ReadFiles.readFile(9).map(s => s.trim.split("\\s+").map(_.toInt).toVector).toList
  input.foreach(println)

  val p1 = test("p1") {

    def predictFuture(sequences: VVI, building: VVI) = {
      val lastV  = sequences.last.last
      val toAdd  = building.last.last
      val newSeq = sequences.last :+ (lastV + toAdd)
      newSeq
    }

    val withExtras = input.map(i => walk(i, goUp, predictFuture))
    val task       = withExtras.map(_.last).sum
    assert(task)(Assertion.equalTo(1479011877))
  }

  val p2 = test("p2") {

    def predictPast(sequences: VVI, building: VVI) = {
      val lastV  = sequences.last.head
      val toAdd  = building.last.head
      val newSeq = Vector(lastV - toAdd) ++ sequences.last
      newSeq
    }

    val withExtras = input.map(i => walk(i, goUp, predictPast))
    val task       = withExtras.map(_.head).sum
    assert(task)(Assertion.equalTo(973))
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("9")(p1, p2) @@ TestAspect.sequential
}
