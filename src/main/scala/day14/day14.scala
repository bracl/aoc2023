package day14

import utils.ReadFiles
import zio.Scope
import zio.test.{Assertion, Spec, TestAspect, TestEnvironment, ZIOSpecDefault, assert}

import scala.annotation.tailrec

object day14 extends ZIOSpecDefault {
  type VVS = Vector[Vector[String]]

  def canMoveNorth(board: VVS, x: Int, y: Int): Boolean =
    y > 0 && board(y - 1)(x) == "."

  def canMoveEast(board: VVS, x: Int, y: Int): Boolean =
    x < board.head.length - 1 && board(y)(x + 1) == "."

  def canMoveWest(board: VVS, x: Int, y: Int): Boolean =
    x > 0 && board(y)(x - 1) == "."

  def canMoveSouth(board: VVS, x: Int, y: Int): Boolean =
    y < board.length - 1 && board(y + 1)(x) == "."

  def updateBoard(board: VVS, x: Int, y: Int, v: String): VVS = board.updated(y, board(y).updated(x, v))

  def isRollingStone(board: VVS, x: Int, y: Int): Boolean =
    board(y)(x) == "O"

  def coords(board: VVS): Seq[(Int, Int)] = for {
    y <- Range(0, board.length)
    x <- Range(0, board.head.length)
  } yield (x, y)

  def turn(canMoveDir: (VVS, Int, Int) => Boolean, updateX: Int => Int, updateY: Int => Int)(board: VVS): VVS = {
    val cs = coords(board)
    cs.foldLeft(board) { case (b, (x, y)) =>
      val stone   = isRollingStone(b, x, y)
      val canMove = canMoveDir(b, x, y)
      if (stone && canMove) {
        val b1 = updateBoard(b, updateX(x), updateY(y), "O")
        val j  = updateBoard(b1, x, y, ".")
        j
      } else b
    }
  }

  @tailrec
  def play(in: VVS, t: VVS => VVS): VVS = {
    val played = t(in)
    if (played == in)
      played
    else play(played, t)
  }

  def spin(board: VVS): VVS = {
    val north = play(board, turn(canMoveNorth, x => x, y => y - 1))
    val west  = play(north, turn(canMoveWest, x => x - 1, y => y))
    val south = play(west, turn(canMoveSouth, x => x, y => y + 1))
    val east  = play(south, turn(canMoveEast, x => x + 1, y => y))
    east
  }

  def spinX(board: VVS, x: Int) = (1 to x).foldLeft(board) { case (b, i) => spin(b) }

  def spinUntilLoop(in: VVS): (VVS, Int, Int) = {
    def key(i: VVS): String      = i.map(_.mkString("")).mkString("")
    val states: Map[String, Int] = Map(key(in) -> 0)

    @tailrec
    def loop(b: VVS, m: Map[String, Int], i: Int, loopSize: Int): (VVS, Map[String, Int], Int, Int) = {
      val spun        = spin(b)
      val k           = key(spun)
      val alreadySeen = m.get(k)
      alreadySeen match
        case None    => loop(spun, m.updated(k, m.size), i + 1, -1)
        case Some(j) => (spun, m.updated(k, m.size), i, m.size - j)
    }
    val res = loop(in, states, 0, 0)
    (res._1, res._3, res._4)
  }

  def score(board: VVS): Int =
    coords(board).map { case (x, y) =>
      if (isRollingStone(board, x, y)) {
        board.length - y
      } else 0
    }.sum

  val input: Vector[Vector[String]] = ReadFiles.readFile(14).map(_.split("").toVector)

  val p1 = test("p1") {
    val task = play(input, turn(canMoveNorth, x => x, y => y - 1))
    assert(score(task))(Assertion.equalTo(105623))
  }

  val p2 = test("p2") {
    val (state, turns, loopSize) = spinUntilLoop(input)
    val turnsLeft                = 1000000000L - turns
    val remaining                = (turnsLeft - (turnsLeft / loopSize) * loopSize).toInt - 1
    val res                      = spinX(state, remaining)
    assert(score(res))(Assertion.equalTo(98029))
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("14")(p1, p2) @@ TestAspect.sequential
}
