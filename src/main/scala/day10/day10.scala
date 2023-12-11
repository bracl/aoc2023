package day10

import utils.ReadFiles
import zio.Scope
import zio.test.{Assertion, Spec, TestAspect, TestEnvironment, ZIOSpecDefault, assert}

import scala.annotation.tailrec

object day10 extends ZIOSpecDefault {

  trait Tile(val rep: String) {
    def coord: (Int, Int)
    def connecting: Vector[(Int, Int)]
    def leftSide(prev: (Int, Int)): Coords
    def rightSide(prev: (Int, Int)): Coords
  }
  type VVT = Vector[Vector[Tile]]
  type Coords = Set[(Int, Int)]

  case class S(x: Int, y: Int) extends Tile("S") {
    override def coord: (Int, Int) = (x, y)

    override def connecting: Vector[(Int, Int)] = {
      val up    = Option.when(pipeAt(x, y - 1).containsAny(List(Pipe.rep, Seven.rep, F.rep)))((x, y - 1))
      val down  = Option.when(pipeAt(x, y + 1).containsAny(List(Pipe.rep, L.rep, J.rep)))((x, y + 1))
      val left  = Option.when(pipeAt(x - 1, y).containsAny(List(Dash.rep, L.rep, F.rep)))((x - 1, y))
      val right = Option.when(pipeAt(x + 1, y).containsAny(List(Dash.rep, J.rep, Seven.rep)))((x + 1, y))
      Vector(up, down, left, right).flatten
    }

    override def leftSide(prev: (Int, Int)): Coords  = Set.empty
    override def rightSide(prev: (Int, Int)): Coords = Set.empty
  }

  object S {
    val rep: String = S(0, 0).rep
  }

  case class Pipe(x: Int, y: Int) extends Tile("|") {
    override def coord: (Int, Int) = (x, y)

    override def connecting: Vector[(Int, Int)] = {
      val up   = Option.when(pipeAt(x, y - 1).containsAny(List(S.rep, Pipe.rep, Seven.rep, F.rep)))((x, y - 1))
      val down = Option.when(pipeAt(x, y + 1).containsAny(List(S.rep, Pipe.rep, L.rep, J.rep)))((x, y + 1))
      Vector(up, down).flatten
    }

    override def leftSide(prev: (Int, Int)): Coords =
      if (prev._2 > y) {
        Set((x - 1, y))
      } else {
        Set((x + 1, y))
      }

    override def rightSide(prev: (Int, Int)): Coords =
      if (prev._2 > y) {
        Set((x + 1, y))
      } else {
        Set((x - 1, y))
      }
  }

  object Pipe {
    val rep: String = Pipe(0, 0).rep
  }

  case class Dash(x: Int, y: Int) extends Tile("-") {
    override def coord: (Int, Int) = (x, y)

    override def connecting: Vector[(Int, Int)] = {
      val left  = Option.when(pipeAt(x - 1, y).containsAny(List(S.rep, Dash.rep, L.rep, F.rep)))((x - 1, y))
      val right = Option.when(pipeAt(x + 1, y).containsAny(List(S.rep, Dash.rep, J.rep, Seven.rep)))((x + 1, y))
      Vector(left, right).flatten
    }

    override def leftSide(prev: (Int, Int)): Coords = if (prev._1 < x) {
      Set((x, y - 1))
    } else {
      Set((x, y + 1))
    }

    override def rightSide(prev: (Int, Int)): Coords = if (prev._1 < x) {
      Set((x, y + 1))
    } else {
      Set((x, y - 1))
    }
  }

  object Dash {
    val rep: String = Dash(0, 0).rep
  }

  case class L(x: Int, y: Int) extends Tile("L") {
    override def coord: (Int, Int) = (x, y)

    override def connecting: Vector[(Int, Int)] = {
      val up    = Option.when(pipeAt(x, y - 1).containsAny(List(S.rep, Pipe.rep, Seven.rep, F.rep)))((x, y - 1))
      val right = Option.when(pipeAt(x + 1, y).containsAny(List(S.rep, Dash.rep, J.rep, Seven.rep)))((x + 1, y))
      Vector(up, right).flatten
    }

    override def leftSide(prev: (Int, Int)): Coords = if (prev._2 < y) {
      Set((x + 1, y - 1))
    } else Set((x, y + 1), (x - 1, y + 1), (x - 1, y))

    override def rightSide(prev: (Int, Int)): Coords = if (prev._2 < y) {
      Set((x, y + 1), (x - 1, y + 1), (x - 1, y))
    } else Set((x + 1, y - 1))
  }

  object L {
    val rep: String = L(0, 0).rep
  }

  case class J(x: Int, y: Int) extends Tile("J") {
    override def coord: (Int, Int) = (x, y)

    override def connecting: Vector[(Int, Int)] = {
      val up   = Option.when(pipeAt(x, y - 1).containsAny(List(S.rep, Pipe.rep, Seven.rep, F.rep)))((x, y - 1))
      val left = Option.when(pipeAt(x - 1, y).containsAny(List(S.rep, Dash.rep, L.rep, F.rep)))((x - 1, y))
      Vector(up, left).flatten
    }

    override def leftSide(prev: (Int, Int)): Coords = if (prev._2 < y) {
      Set((x + 1, y), (x + 1, y + 1), (x, y + 1))
    } else Set((x - 1, y - 1))

    override def rightSide(prev: (Int, Int)): Coords = if (prev._2 < y) {
      Set((x - 1, y - 1))
    } else Set((x + 1, y), (x + 1, y + 1), (x, y + 1))
  }

  object J {
    val rep: String = J(0, 0).rep
  }

  case class Seven(x: Int, y: Int) extends Tile("7") {
    override def coord: (Int, Int) = (x, y)

    override def connecting: Vector[(Int, Int)] = {
      val down = Option.when(pipeAt(x, y + 1).containsAny(List(S.rep, Pipe.rep, L.rep, J.rep)))((x, y + 1))
      val left = Option.when(pipeAt(x - 1, y).containsAny(List(S.rep, Dash.rep, L.rep, F.rep)))((x - 1, y))
      Vector(down, left).flatten
    }

    override def leftSide(prev: (Int, Int)): Coords = if (prev._1 < x) {
      Set((x, y - 1), (x + 1, y - 1), (x + 1, y))
    } else Set((x - 1, y + 1))

    override def rightSide(prev: (Int, Int)): Coords = if (prev._1 < x) {
      Set((x - 1, y + 1))
    } else Set((x, y - 1), (x + 1, y - 1), (x + 1, y))
  }

  object Seven {
    val rep: String = Seven(0, 0).rep
  }

  case class F(x: Int, y: Int) extends Tile("F") {
    override def coord: (Int, Int) = (x, y)

    override def connecting: Vector[(Int, Int)] = {
      val down  = Option.when(pipeAt(x, y + 1).containsAny(List(S.rep, Pipe.rep, L.rep, J.rep)))((x, y + 1))
      val right = Option.when(pipeAt(x + 1, y).containsAny(List(S.rep, Dash.rep, J.rep, Seven.rep)))((x + 1, y))
      Vector(down, right).flatten
    }

    override def leftSide(prev: (Int, Int)): Coords = if (prev._1 > x) {
      Set((x + 1, y + 1))
    } else Set((x - 1, y), (x - 1, y - 1), (x, y - 1))

    override def rightSide(prev: (Int, Int)): Coords = if (prev._1 > x) {
      Set((x - 1, y), (x - 1, y - 1), (x, y - 1))
    } else Set((x + 1, y + 1))
  }

  object F {
    val rep: String = F(0, 0).rep
  }

  case class Dot(x: Int, y: Int) extends Tile(".") {
    override def coord: (Int, Int)              = (x, y)
    override def connecting: Vector[(Int, Int)] = Vector.empty

    override def leftSide(prev: (Int, Int)): Coords  = Set.empty
    override def rightSide(prev: (Int, Int)): Coords = Set.empty
  }

  object Dot {
    val rep: String = Dot(0, 0).rep
  }

  extension (opt: Option[Tile]) {
    def containsAny(l: List[String]): Boolean = l.exists(s => opt.map(_.rep).contains(s))
  }

  extension (vec: Vector[Tile]) {
    def penultimate: Tile = vec(vec.length - 2)
  }

  def pipeAt(x: Int, y: Int): Option[Tile] =
    if (y < 0 || y >= input.length || x < 0 || x >= input.head.length) None
    else Some(input(y)(x))

  val input: VVT = ReadFiles.readFile(10).zipWithIndex.map { case (row, y) =>
    row
      .map(_.toString)
      .zipWithIndex
      .collect {
        case (S.rep, x)     => S(x, y)
        case (Pipe.rep, x)  => Pipe(x, y)
        case (Dash.rep, x)  => Dash(x, y)
        case (L.rep, x)     => L(x, y)
        case (J.rep, x)     => J(x, y)
        case (Seven.rep, x) => Seven(x, y)
        case (F.rep, x)     => F(x, y)
        case (Dot.rep, x)   => Dot(x, y)
      }
      .toVector
  }

  val starting: Tile = input.flatMap(s => s.filter(t => t.isInstanceOf[S])).head

  @tailrec
  def walk(
    path: Vector[Tile],
    pathCoords: Coords,
    leftStarting: Coords,
    rightStarting: Coords
  ): (Vector[Tile], Coords, Coords, Coords) =
    if (path.last.isInstanceOf[S]) (path, pathCoords, leftStarting, rightStarting)
    else {
      val connections   = path.last.connecting
      val notBeenBefore = connections.find(_ != path.penultimate.coord)
      notBeenBefore match
        case Some((x, y)) =>
          val tile  = input(y)(x)
          val lhs   = tile.leftSide(path.last.coord)
          val rhs   = tile.rightSide(path.last.coord)
          val left  = leftStarting -- Set(tile.coord) -- rhs ++ (lhs -- pathCoords)
          val right = rightStarting -- Set(tile.coord) -- lhs ++ (rhs -- pathCoords)
          walk(path.appended(tile), pathCoords ++ Set(tile.coord), left, right)
        case None => ???
    }

  val path                   = starting.connecting.map((x, y) => Vector(starting, input(y)(x))).head
  val (loop, _, left, right) = walk(path, path.map(_.coord).toSet, Set(), Set())

  val p1 = test("p1") {
    assert(loop.length / 2)(Assertion.equalTo(6714))
  }

  val p2 = test("p2") {

    val ys = Range(0, input.length)
    val xs = Range(0, input.head.length)

    val accountedForCoords = loop.map(_.coord) ++ right ++ left
    val toInspect = (for {
      y <- ys
      x <- xs
      if !accountedForCoords.contains((x, y))
    } yield (x, y)).toVector

    def areYouInside(tuple: (Int, Int), inside: Coords, outside: Coords): Boolean = {
      val distance = Range.inclusive(0, input.length)
      val coords   = List((1, 0), (-1, 0), (0, 1), (0, -1))

      def dist(toWhere: Coords): Int =
        distance.takeWhile { i =>
          val toCheck = coords.map(c => (tuple._1 + c._1 * i, tuple._2 + c._2 * i))
          !toCheck.exists(x => toWhere.contains(x))
        }.size

      val distToInside  = dist(inside)
      val distToOutside = dist(outside)
      distToInside < distToOutside
    }

    val (inside, outside) = if (left.size < right.size) (left, right) else (right, left)
    val areAlsoInside     = toInspect.filter(c => areYouInside(tuple = c, inside = inside, outside = outside)).toSet

    val toPrint = ys
      .map(y =>
        xs.map { x =>
          loop
            .find(_.coord == (x, y))
            .map(_.rep)
            .getOrElse(
              if (inside.contains((x, y))) "I"
              else if (areAlsoInside.contains((x, y))) "I"
              else if (outside.contains((x, y))) "O"
              else "."
            )
        }.mkString("")
      )
      .mkString("\n")
    println(toPrint)

    assert(inside.size + areAlsoInside.size)(Assertion.equalTo(429))
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("10")(p1, p2) @@ TestAspect.sequential
}
