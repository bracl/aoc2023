package day22

import utils.ReadFiles
import zio.Scope
import zio.test.{Assertion, Spec, TestAspect, TestEnvironment, ZIOSpecDefault, assert}

import scala.annotation.tailrec
import scala.collection.immutable.{AbstractSeq, LinearSeq}

object day22 extends ZIOSpecDefault {

  case class Coord(x: Int, y: Int, z: Int) {

    def to(c: Coord): Set[Coord] =
      (for {
        x1 <- Range.inclusive(x, c.x)
        y1 <- Range.inclusive(y, c.y)
        z1 <- Range.inclusive(z, c.z)
      } yield Coord(x1, y1, z1)).toSet

    def downOne: Coord = this.copy(z = z - 1)
  }

  object Coord {
    def apply(is: Array[Int]): Coord = Coord(is.head, is(1), is(2))
  }

  case class Brick(label: String, coords: Set[Coord], set: Boolean) {
    def downOne: Set[Coord] = coords.map(_.downOne)
  }

  object Brick {

    def apply(in: (String, Int)): Brick = {
      val (s, i)  = in
      val coords  = s.split("~")
      val start   = Coord(coords.head.split(",").map(_.toInt))
      val end     = Coord(coords.last.split(",").map(_.toInt))
      val coords1 = start.to(end)
      Brick(i.toString, coords1, coords1.exists(c => c.z <= 1))
    }

  }

  val input = ReadFiles.readFile(22)
//  val input = """1,0,1~1,2,1
//                |0,0,2~2,0,2
//                |0,2,3~2,2,3
//                |0,0,4~0,2,4
//                |2,0,5~2,2,5
//                |0,1,6~2,1,6
//                |1,1,8~1,1,9""".stripMargin.split("\n")
  val bricks: Seq[Brick] = input.zipWithIndex.map(Brick.apply).sortBy(_.coords.map(_.z).min)

  def fallOne(bricks: Seq[Brick]): Seq[Brick] = {
    val (alreadySet, falling) = bricks.partition(_.set)
    val (_, areNowSet, areStillFalling) =
      falling.foldLeft((alreadySet.flatMap(_.coords).toSet, Seq.empty[Brick], Seq.empty[Brick])) {
        case ((setCoords, areNowSet, areStillFalling), brick) =>
          val fellCandidate            = brick.downOne
          val cantMoveDown             = fellCandidate.intersect(setCoords).nonEmpty
          val candidateIsNowOnTheFloor = fellCandidate.map(_.z).min == 1
          if (cantMoveDown)
            (setCoords.union(brick.coords), areNowSet.appended(brick), areStillFalling)
          else if (candidateIsNowOnTheFloor)
            (setCoords.union(fellCandidate), areNowSet.appended(brick.copy(coords = fellCandidate)), areStillFalling)
          else
            (setCoords, areNowSet, areStillFalling.appended(brick.copy(coords = fellCandidate)))
      }
    alreadySet ++ areNowSet.map(_.copy(set = true)) ++ areStillFalling
  }

  @tailrec
  def fall(bricks: Seq[Brick]): Seq[Brick] = {
    val fell = fallOne(bricks)
    if (fell == bricks)
      bricks
    else
      fall(fell)
  }

  def findSupportingBricks(bricks: Seq[Brick]): Map[String, Seq[String]] = {
    val whoDoYouRestOn = Map.empty[String, Seq[String]]
    bricks.foldLeft(whoDoYouRestOn) { case (youRestOn, b) =>
      if (b.coords.map(_.z).min == 1) youRestOn.updated(b.label, Seq())
      else
        val downOne = b.downOne
        val resting = bricks.filter(brick => brick.coords.intersect(downOne).nonEmpty && brick.label != b.label)
        if (resting.isEmpty)
          throw new RuntimeException("this should never happen")
        youRestOn.updated(b.label, resting.map(_.label))
    }
  }

  def whichWouldFall(
    keys: Seq[String],
    resting: Map[String, Seq[String]],
    cache: Map[String, Set[String]] = Map.empty
  ): (Set[String], Map[String, Set[String]]) = {
    val cacheKey = keys.sorted.mkString(",")
    println(cacheKey)
    if (cache.contains(cacheKey))
      println(s"cacheHit $cacheKey")
      (cache(cacheKey), cache)
    else
      val wouldFall = resting.collect {
        case (k, v) if v.nonEmpty && v.diff(keys).isEmpty && !keys.contains(k) => k
      }.toSet
      if (wouldFall.isEmpty) (keys.toSet, cache.updated(cacheKey, Set.empty))
      else {
        val (wouldFallFrom, cache2) = whichWouldFall(keys ++ wouldFall, resting, cache)
        val wouldFallTotal          = keys.toSet.union(wouldFallFrom)
        val cache3                  = cache2.updated(cacheKey, wouldFallTotal)
        (wouldFallTotal, cache3)
      }
  }

  val keys           = bricks.map(_.label)
  val settled        = fall(bricks)
  val whoDoYouRestOn = findSupportingBricks(settled)

  val p1 = test("p1") {
    val unsafe = keys.count(key => !whoDoYouRestOn.exists { case (k, v) => v == Seq(key) })
    println(unsafe)
    println(whoDoYouRestOn)
    println('n')
    // 607 too high
    assert(unsafe)(Assertion.equalTo(398))
  }

  val p2 = test("p2") {
    val task = whichWouldFall(Seq("5"), whoDoYouRestOn)

    val (c, count) = keys.foldLeft((Map.empty[String, Set[String]], 0)) { case ((cache, acc), brick) =>
      val (wouldFall, cache2) = whichWouldFall(Seq(brick), whoDoYouRestOn, cache)
      val bothCache = cache ++ cache2
      (bothCache, acc + wouldFall.size)
    }
    
    val toFallAsAResult = count - bricks.length

    assert(toFallAsAResult)(Assertion.equalTo(70727))
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("22")(p1, p2) @@ TestAspect.sequential
}
