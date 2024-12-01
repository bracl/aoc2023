package day23

import utils.ReadFiles
import zio.Scope
import zio.test.{Assertion, Spec, TestAspect, TestEnvironment, ZIOSpecDefault, assert}

import scala.annotation.tailrec

object day23 extends ZIOSpecDefault {

  val input = ReadFiles.readFile(23)
//  val input = """#.#####################
//                |#.......#########...###
//                |#######.#########.#.###
//                |###.....#.>.>.###.#.###
//                |###v#####.#v#.###.#.###
//                |###.>...#.#.#.....#...#
//                |###v###.#.#.#########.#
//                |###...#.#.#.......#...#
//                |#####.#.#.#######.#.###
//                |#.....#.#.#.......#...#
//                |#.#####.#.#.#########v#
//                |#.#...#...#...###...>.#
//                |#.#.#v#######v###.###v#
//                |#...#.>.#...>.>.#.###.#
//                |#####v#.#.###v#.#.###.#
//                |#.....#...#...#.#.#...#
//                |#.#########.###.#.#.###
//                |#...###...#...#...#.###
//                |###.###.#.###v#####v###
//                |#...#...#.#.>.>.#.>.###
//                |#.###.###.#.###.#.#v###
//                |#.....###...###...#...#
//                |#####################.#""".stripMargin.split("\n")
//  input.foreach(println)

  case class Coord(x: Int, y: Int) {
    def +(i: (Int, Int)): Coord = Coord(x + i._1, y + i._2)
  }

  case class Trail(input: Seq[String], part2: Boolean = false) {
    val startingPosition: Coord = Coord(input.head.indexOf("."), 0)
    val targetPosition: Coord   = Coord(input.last.indexOf("."), input.length - 1)

    def tile(c: Coord): String       = s"${input(c.y)(c.x)}"
    def tile(x: Int, y: Int): String = s"${input(y)(x)}"

    def availableMovesFrom(c: Coord): Set[Coord] = availableMovesFrom(c.x, c.y)

    def availableMovesFrom(x: Int, y: Int): Set[Coord] = {
      val position = tile(x, y)
      val allD: Seq[Coord] = if (part2) {
        Seq(Coord(x + 1, y), Coord(x - 1, y), Coord(x, y + 1), Coord(x, y - 1))
      } else {
        position match
          case "." => Seq(Coord(x + 1, y), Coord(x - 1, y), Coord(x, y + 1), Coord(x, y - 1))
          case ">" => Seq(Coord(x + 1, y))
          case "<" => Seq(Coord(x - 1, y))
          case "v" => Seq(Coord(x, y + 1))
          case "^" => Seq(Coord(x, y - 1))
      }

      val onTheMap = allD.filter(d => d.x >= 0 && d.x < input.head.length && d.y >= 0 && d.y < input.length)
      val noTrees  = onTheMap.filterNot(d => tile(d.x, d.y) == "#")
      noTrees.toSet
    }
  }

  case class Path(path: Vector[Coord]) {
    def step(c: Coord): Path = Path(path :+ c)
  }

  @tailrec
  def walkCorridor(t: Trail, path: Vector[Coord], direction: (Int, Int)): Vector[Coord] = {
    val last             = path.last
    val forwardCandidate = last + direction
    val left             = forwardCandidate + (direction._2, -direction._1)
    val right            = forwardCandidate + (-direction._2, direction._1)
    if (forwardCandidate == t.targetPosition)
      path :+ forwardCandidate
    else if (t.tile(forwardCandidate) == "#")
      path
    else if (t.tile(left) != "#" || t.tile(right) != "#")
      path :+ forwardCandidate
    else
      walkCorridor(t, path :+ forwardCandidate, direction)
  }

  def takeOneStep(t: Trail, p: Path): Set[Path] = {
    val moves     = t.availableMovesFrom(p.path.last)
    val nextSteps = moves.filterNot(m => p.path.contains(m))
    nextSteps.map(m => p.step(m))
  }

  def takeMultipleSteps(t: Trail, p: Path): Set[Path] = {
    val current          = p.path.last
    val allMoves         = t.availableMovesFrom(current)
    val directionsToMove = allMoves.filterNot(m => p.path.contains(m))
//    if (directionsToMove.size > 1)
//      t.input.zipWithIndex.foldLeft(p) { case (path, (row, y)) =>
//        val row2 = row.zipWithIndex.collect {
//          case (c, x) if path.path.contains(Coord(x, y)) => '0'
//          case (c, x)                                    => c
//        }
//        println(row2.mkString(""))
//        path
//      }
//      println("junction")
    val paths = directionsToMove.map { d =>
      val unitDirection = (d.x - current.x, d.y - current.y)
      Path(walkCorridor(t, p.path, unitDirection))
    }
    paths
  }

  def walk(trail: Trail, walkFn: (Trail, Path) => Set[Path]): Seq[Path] = {

    @tailrec
    def loop(paths: Seq[Path], completePaths: Seq[Path] = Nil, c: Int = 0): Seq[Path] =
      println(c + 1)
      paths match
        case s if s.isEmpty => completePaths
        case s =>
          val p = s.head

          val pp = s.tail
          if (p.path.last == trail.targetPosition) loop(pp, completePaths :+ p, c + 1)
          else
            val nextSteps = walkFn(trail, p)
            loop(nextSteps.toSeq ++ pp, completePaths, c + 1)

    val allPaths: Seq[Path] = loop(Seq(Path(Vector(trail.startingPosition))))
    allPaths
  }

  val p1 = test("p1") {
//    val task = walk(Trail(input), takeOneStep)
    val task    = walk(Trail(input), takeMultipleSteps)
    val longest = task.maxBy(_.path.length)
    assert(longest.path.length - 1)(Assertion.equalTo(2110))
  }

  val p2 = test("p2") {
    // 5592
    val task    = walk(Trail(input, true), takeMultipleSteps)
    val longest = task.maxBy(_.path.length)
    assert(longest.path.length - 1)(Assertion.equalTo(0))
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("23")(p1, p2) @@ TestAspect.sequential
}
