package day16

import utils.ReadFiles
import zio.stream.ZStream
import zio.test.{Assertion, Spec, TestAspect, TestEnvironment, ZIOSpecDefault, assert, assertZIO}
import zio.{Scope, ZIO}

import scala.annotation.tailrec

object day16 extends ZIOSpecDefault {

  type VVS = Vector[Vector[String]]
  val input: VVS = ReadFiles.readFile(16).map(_.split("").toVector)

  def show(in: VVS, beams: Set[Beam]) = {
    val cs = for {
      y <- Range.inclusive(0, in.length - 1)
      x <- Range.inclusive(0, in.head.length - 1)
    } yield (x, y)
    val withBeams = cs.foldLeft(in) { case (acc, (x, y)) =>
      val nBeams = beams.filter(b => b.x == x && b.y == y).toList
      nBeams match {
        case Nil => acc
        case b :: Nil =>
          b.dot match {
            case (1, 0) if acc(y)(x) == "."  => acc.updated(y, acc(y).updated(x, ">"))
            case (-1, 0) if acc(y)(x) == "." => acc.updated(y, acc(y).updated(x, "<"))
            case (0, 1) if acc(y)(x) == "."  => acc.updated(y, acc(y).updated(x, "v"))
            case (0, -1) if acc(y)(x) == "." => acc.updated(y, acc(y).updated(x, "^"))
            case _                           => acc
          }
        case bs if acc(y)(x) == "." => acc.updated(y, acc(y).updated(x, bs.size.toString))
        case _                      => acc
      }
    }
    withBeams.foreach(l => println(l.mkString("")))
  }

  case class Beam(x: Int, y: Int, dot: (Int, Int)) {

    private def move(d: (Int, Int)): Beam = Beam(x + d._1, y + d._2, d)

    def next: Set[Beam] = input(y)(x) match {
      case "-" if dot._1 == 0 => Set(move((-1, 0)), move((1, 0)))
      case "|" if dot._2 == 0 => Set(move((0, -1)), move((0, 1)))
      case "/"                => Set(move((-1 * dot._2, -1 * dot._1)))
      case """\"""            => Set(move(dot.swap))
      case "." | "-" | "|"    => Set(move(dot))
    }

    def nextOnGrid: Set[Beam] = next.filter(b => b.x >= 0 && b.x < input.head.length && b.y >= 0 && b.y < input.length)
  }

  @tailrec
  def play(allBeams: Set[Beam], nBeams: Set[Beam]): (Set[Beam], Set[Beam]) = {
    val next = nBeams.flatMap(b => b.nextOnGrid) -- allBeams
    if (next.isEmpty) (allBeams ++ nBeams, Set.empty)
    else play(allBeams ++ nBeams, next)
  }

  val p1 = test("p1") {
    val tiles: Set[Beam] = play(Set.empty, Set(Beam(0, 0, (1, 0))))._1
//    show(input, tiles)
    assert(tiles.groupBy(b => (b.x, b.y)).size)(Assertion.equalTo(7242))
  }

  val p2 = test("p2") {
    val startingBeams =
      Range(0, input.length).map(y => Beam(0, y, (1, 0))) ++
        Range(0, input.length).map(y => Beam(input.head.length - 1, y, (-1, 0))) ++
        Range(0, input.head.length).map(x => Beam(x, 0, (0, 1))) ++
        Range(0, input.head.length).map(x => Beam(x, input.length - 1, (0, -1)))

    val task = ZStream
      .from(startingBeams)
      .mapZIOPar(startingBeams.size)(b => ZIO.succeed(play(Set.empty, Set(b))._1.map(b => (b.x, b.y)).size))
      .runCollect
      .map(_.max)

    assertZIO(task)(Assertion.equalTo(7572))
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("16")(p1, p2) @@ TestAspect.sequential
}
