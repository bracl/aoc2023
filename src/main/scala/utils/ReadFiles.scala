package utils

object ReadFiles {
  def inText(day: Int) = s"/Users/bradley.king/dev/bracl/aoc2023/src/main/scala/day$day/in.txt"
  def exText(day: Int) = s"/Users/bradley.king/dev/bracl/aoc2023/src/main/scala/day$day/ex.txt"

  def readFile(day: Int, trim: Boolean = true): Vector[String] =
    readFile(inText(day), trim)

  def readFile(filename: String, trim: Boolean): Vector[String] = {
    val bufferedSource = io.Source.fromFile(filename)
    val lines          = bufferedSource.getLines().map(l => if (trim) l.trim else l).toVector
    bufferedSource.close
    lines
  }

  def readGrouped(day: Int, delim: String = ""): Vector[Vector[String]] = readGrouped(
    inText(day),
    delim
  )

  def readGrouped(filename: String, delim: String): Vector[Vector[String]] = {
    val bufferedSource = io.Source.fromFile(filename)
    val lines          = bufferedSource.getLines().map(_.trim).toVector.zipWithIndex
    val lastIndex      = lines.last._2
    val grouped: Vector[Vector[String]] = lines
      .foldLeft((Vector[Vector[String]](), Vector[String]())) { case ((acc, currentGroup), (line, index)) =>
        line match {
          case s if s == delim         => (acc :+ currentGroup, Vector.empty)
          case _ if index == lastIndex => (acc :+ currentGroup.appended(line), Vector.empty)
          case _                       => (acc, currentGroup.appended(line))
        }
      }
      ._1
    bufferedSource.close
    grouped
  }

  def readInts(day: Int): Vector[Int] = readInts(inText(day))

  def readInts(filename: String): Vector[Int] = {
    val strings = readFile(filename, true)
    strings.map(s => s.toInt)
  }
}
