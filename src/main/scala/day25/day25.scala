package day25

import utils.ReadFiles
import zio.Scope
import zio.stream.ZStream
import zio.test.{Assertion, Spec, TestAspect, TestEnvironment, ZIOSpecDefault, assert, assertZIO}

import scala.annotation.tailrec
import scala.util.Random

object day25 extends ZIOSpecDefault {

  val input = ReadFiles.readFile(25)

  type ConnectionMap = Map[String, Set[String]]
  type MultiGraph    = Map[Set[String], Seq[Set[String]]]

  val connectionMap: ConnectionMap = input.foldLeft(Map.empty[String, Set[String]]) { case (m1, comp) =>
    comp match {
      case s"$c: $rest" =>
        val connections: Seq[(String, String)] = rest.split(" ").flatMap(r => Seq(c -> r, r -> c))
        connections.foldLeft(m1) { case (m2, comb) =>
          val existingA = m2.getOrElse(comb._1, Set())
          val m3        = m2.updated(comb._1, existingA + comb._2)
          val existingB = m3.getOrElse(comb._2, Set())
          val m4        = m3.updated(comb._2, existingB + comb._1)
          m4
        }
    }
  }

  extension (mg: MultiGraph) {

    def contractEdge(from: Set[String], to: Set[String]): MultiGraph = {
      val fromEdges      = mg(from).filterNot(_ == to)
      val toEdges        = mg(to).filterNot(_ == from)
      val connectedEdges = fromEdges ++ toEdges
      val newEdge        = to ++ from
      val edgeContracted = mg
        .removed(from)
        .removed(to)
        .updated(newEdge, connectedEdges)

      connectedEdges.foldLeft(edgeContracted) { (mg_, next) =>
        mg_.updatedWith(next) {
          _.map { xs =>
            xs.map {
              case x if x == to || x == from => newEdge
              case x                         => x
            }
          }
        }
      }
    }
  }

  @tailrec
  def probabilisticMinCut(graph: MultiGraph): (Set[String], Int) =
    if (graph.size == 2) {
      val value = graph.head
      (value._1, value._2.size)
    } else {
      val from = graph.keys.toSeq(Random.nextInt(graph.size))
      val to   = graph(from)(Random.nextInt(graph(from).size))
      probabilisticMinCut(graph.contractEdge(from, to))
    }

  @tailrec
  def kargers(graph: MultiGraph): Set[String] = {
    val (partition, cutSize) = probabilisticMinCut(graph)
    if (cutSize == 3) partition
    else kargers(graph)
  }

  def findCut(graph: ConnectionMap) = {
    val multiGraph: MultiGraph = graph.map((k, v) => (Set(k), v.map(Set(_)).toSeq))
    val component              = kargers(multiGraph)
    component.size * (multiGraph.size - component.size)
  }

  val p1 = test("p1") {
    assert(findCut(connectionMap))(Assertion.equalTo(603368))
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("25")(p1) @@ TestAspect.sequential
}
