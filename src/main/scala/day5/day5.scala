package day5

import utils.ReadFiles
import zio.Scope
import zio.test.{Assertion, Spec, TestAspect, TestEnvironment, ZIOSpecDefault, assert}

import scala.annotation.tailrec

object day5 extends ZIOSpecDefault {

  case class Mapping(
    source: String,
    destination: String,
    rules: Vector[(Long, Long, Long, Long)]
  ) {

    def output(input: Long): Long =
      val ruleOpt = rules.find(r => r._1 <= input && r._2 >= input)
      ruleOpt match
        case None       => input
        case Some(rule) => (input - rule._1) + rule._3

    def input(output: Long): Long =
      val ruleOpt = rules.find(r => r._3 <= output && r._4 >= output)
      ruleOpt match
        case None       => output
        case Some(rule) => (output - rule._3) + rule._1
  }

  object Mapping {

    def apply(v: Vector[String]): Mapping = {
      val source: String = v.head.split("-").head
      val dest: String   = v.head.split("-")(2).split(" ").head
      val ranges: Vector[(Long, Long, Long, Long)] = v.tail.map { t =>
        val nums              = t.split(" ")
        val destStart: Long   = nums.head.toLong
        val sourceStart: Long = nums(1).toLong
        val length: Long      = nums(2).toLong
        (
          sourceStart,
          sourceStart + length,
          destStart,
          destStart + length
        )
      }
      Mapping(source, dest, ranges)
    }
  }

  val input = ReadFiles.readGrouped(ReadFiles.inText(5), "")

  val seeds =
    input.filter(_.head.startsWith("seeds")).head.head.split(" ").filter(_.forall(_.isDigit)).map(_.toLong).toVector
  println(seeds)
  val mappings                                 = input.filterNot(_.head.startsWith("seeds")).map(Mapping.apply)
  val mappingsMap: Map[String, Mapping]        = mappings.map(m => m.source -> m).toMap
  val reverseMappingsMap: Map[String, Mapping] = mappings.map(m => m.destination -> m).toMap

  def surfTheMaps(
    start: Long,
    source: String,
    mappings: Map[String, Mapping],
    mappingFn: (Mapping, Long) => Long,
    nextMappingFn: Mapping => String
  ): Long = {
    val goal = Range
      .inclusive(1, mappings.size)
      .foldLeft((start, source)) { case ((number, destination), _) =>
        val mapping = mappings(destination)
        val out     = mappingFn(mapping, number)
        (out, nextMappingFn(mapping))
      }
      ._1
    goal
  }

  val p1 = test("p1") {
    val task = seeds.map(s => surfTheMaps(s, "seed", mappingsMap, (m, n) => m.output(n), m => m.destination))
    assert(task.min)(Assertion.equalTo(240320250))
  }

  val p2 = test("p2") {
    val allSeeds: Vector[(Long, Long)] = seeds.grouped(2).map(t => (t.head, t.head + t.last)).toVector

    def findFirstSeed(): Long = {

      val minBucket     = reverseMappingsMap.values.map(_.rules.minBy(r => r._2 - r._1)).minBy(r => r._2 - r._1)
      val minBucketSize = (minBucket._2 - minBucket._1) / 2

      @tailrec
      def loop(l: Long, inc: Long = 1): Long = {
        println(l)
        val seed = surfTheMaps(l, "location", reverseMappingsMap, (m, n) => m.input(n), m => m.source)
        if (allSeeds.exists(r => r._1 <= seed && r._2 >= seed))
          l
        else
          loop(l + inc, inc)
      }
      val minBucketMarker = loop(0, minBucketSize)
      val seed            = loop(minBucketMarker - minBucketSize)
      seed
    }

    val task = findFirstSeed()
    assert(task)(Assertion.equalTo(28580589))
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("5")(p1, p2) @@ TestAspect.sequential
}
