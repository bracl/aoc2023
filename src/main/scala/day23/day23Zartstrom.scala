package day23

enum Dir:

  def toVec: Vec = this match {
    case North => Vec(0, -1)
    case East  => Vec(1, 0)
    case South => Vec(0, 1)
    case West  => Vec(-1, 0)
  }
  case North, East, South, West

import Dir.*
import utils.ReadFiles

sealed trait Field
case object Forest                   extends Field
case class Slope(from: Dir, to: Dir) extends Field
case object Plain                    extends Field

object Field {

  def apply(c: Char): Field =
    c match
      case '#' => Forest
      case '.' => Plain
      case '<' => Slope(from = East, to = West)
      case 'v' => Slope(from = North, to = South)
      case '>' => Slope(from = West, to = East)
      case '^' => Slope(from = South, to = North)
}

case class Vec(x: Int, y: Int) {
  def +(other: Vec)             = Vec(x + other.x, y + other.y)
  lazy val neighbours: Seq[Vec] = Dir.values.map(d => this + d.toVec)

  def accessible(using hikingMap: Map[Vec, Field]): Seq[Vec] =
    neighbours.filter(n =>
      hikingMap.getOrElse(n, Forest) match
        case Forest          => false
        case Plain           => true
        case Slope(from, to) => true // part2
        // case Slope(from, to) => n + from.toVec == vec  // part1
    )
}

case class Edge(from: Vec, to: Vec, length: Int) {
  def arrow: String = s"-- $length --> $to"
}

@main def main = {
  val lines = ReadFiles.readFile(23)
//  val lines = fromFile("input.txt").getLines.toSeq

  given hikingMap: Map[Vec, Field] = (for
    (line, y) <- lines.zipWithIndex
    (char, x) <- line.zipWithIndex
  yield (Vec(x, y), Field(char))).toMap

  val crossroads: Seq[Vec] = (for
    (v, f) <- hikingMap
    if f != Forest
    if v.neighbours.filter(n => hikingMap.getOrElse(n, Forest) != Forest).size >= 3
  yield v).toSeq

  val start: Vec  = Vec(lines.head.indexOf('.'), 0)
  val target: Vec = Vec(lines.last.indexOf('.'), hikingMap.keys.map(_.y).max)

  val startNodes: Seq[Vec] = start +: crossroads
  val allNodes: Seq[Vec]   = startNodes :+ target

  def goPath(current: Vec, path: Seq[Vec], crossroads: Seq[Vec]): Seq[Vec] = {
    val newPath = path :+ current
    if (crossroads.contains(current)) newPath
    else {
      val nextVecs = current.accessible.filter(_ != path.last)
      assert(nextVecs.size <= 1, "There should be at most one path, otherwise we're at a crossroad.")
      if (nextVecs.size == 0) newPath // Defensive, there are no dead-ends in the input.
      else goPath(nextVecs.head, newPath, crossroads)
    }
  }
  def exploreFrom(position: Vec, crossroads: Seq[Vec]): Seq[Seq[Vec]] =
    position.accessible.map(n => goPath(n, Seq(position), crossroads))
  val fieldPaths: Seq[Seq[Vec]] = startNodes.flatMap(node => exploreFrom(node, crossroads = allNodes))

  val edges: Seq[Edge] = fieldPaths.map { vs =>
    assert(allNodes.contains(vs.head))
    assert(allNodes.contains(vs.last))
    Edge(vs.head, vs.last, vs.size - 1)
  }

  val dag: Map[Vec, Seq[Edge]] = edges.groupBy(_.from)

  def getPaths(current: Vec, target: Vec, path: Seq[Edge]): Seq[Seq[Edge]] =
    if (current == target) Seq(path)
    else {
      val es: Seq[Edge] = dag(current).filter(e => !path.map(_.to).contains(e.to))
      es.flatMap(e => getPaths(e.to, target, path :+ e))
    }
  val graphPaths: Seq[Seq[Edge]] = getPaths(start, target, Seq())

  val result: Int = graphPaths.map(es => es.map(_.length).sum).max
  println(result)
}
