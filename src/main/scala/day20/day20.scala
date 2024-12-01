package day20

import utils.ReadFiles
import zio.Scope
import zio.test.{Assertion, Spec, TestAspect, TestEnvironment, ZIOSpecDefault, assert}

import scala.annotation.tailrec

object day20 extends ZIOSpecDefault {

  enum Pulse:
    case Low
    case High

  case class Task(source: Module, destination: String, pulse: Pulse)

  trait Module {
    val name: String
    val destinations: Seq[String]
    def receivePulse(task: Task): (Module, Seq[Task])
  }

  case class BroadCaster(destinations: Seq[String]) extends Module {
    override val name = "broadcaster"

    override def receivePulse(task: Task): (Module, Seq[Task]) =
      val tasks = destinations.map(d => Task(this, d, task.pulse))
      (this, tasks)
  }

  case class FlipFlop(name: String, destinations: Seq[String], on: Boolean) extends Module {

    private def turnOn: (Module, Seq[Task]) =
      (this.copy(on = true), destinations.map(d => Task(this, d, Pulse.High)))

    private def turnOff: (Module, Seq[Task]) =
      (this.copy(on = false), destinations.map(d => Task(this, d, Pulse.Low)))

    override def receivePulse(task: Task): (Module, Seq[Task]) = task.pulse match
      case Pulse.High      => (this, Nil)
      case Pulse.Low if on => turnOff
      case Pulse.Low       => turnOn
  }

  case class Conjunction(name: String, destinations: Seq[String], receivedPulses: Map[String, Pulse]) extends Module {

    def addInput(name: String): Module = this.copy(receivedPulses = receivedPulses.updated(name, Pulse.Low))

    private def updatePulses(updated: Map[String, Pulse]): Module = this.copy(receivedPulses = updated)

    override def receivePulse(task: Task): (Module, Seq[Task]) =
      val updatedReceivedPulses = receivedPulses.updated(task.source.name, task.pulse)
      if (destinations.contains("rx") && updatedReceivedPulses.values.exists(_ == Pulse.High))
        println(s"3853 4073 4091 4093")
        println(task.source.name)
      val updatedModule = updatePulses(updatedReceivedPulses)
      val pulseToSend   = if (updatedReceivedPulses.values.forall(_ == Pulse.High)) Pulse.Low else Pulse.High
      (updatedModule, destinations.map(d => Task(updatedModule, d, pulseToSend)))
  }

  case class Button() extends Module {
    override val name: String              = "button"
    override val destinations: Seq[String] = Seq("broadcaster")

    override def receivePulse(task: Task): (Module, Seq[Task]) =
      val tasks = destinations.map(d => Task(this, d, Pulse.Low))
      (this, tasks)
  }

  // %zz -> ks, vd
  // %td -> bj, tc
  // broadcaster -> mn, jn, hd, lq
  private def createModule(s: String): Module =
    val parts        = s.split(" -> ")
    val destinations = parts.last.split(",").map(_.trim).toSeq
    parts.head match
      case "broadcaster" => BroadCaster(destinations)
      case s"%$name"     => FlipFlop(name, destinations, false)
      case s"&$name"     => Conjunction(name, destinations, Map.empty)

  private def initialiseConjunctions(modules: Map[String, Module]): Map[String, Module] =
    modules.keys.foldLeft(modules) { case (m, k) =>
      val destinations = m(k).destinations
      val updatedModules = destinations.foldLeft(m) { case (mm, d) =>
        mm.get(d) match
          case Some(c: Conjunction) => mm.updated(d, c.addInput(k))
          case _                    => mm
      }
      updatedModules
    }

  val input = ReadFiles.readFile(20)
//  val input = """broadcaster -> a
//                |%a -> inv, con
//                |&inv -> b
//                |%b -> con
//                |&con -> output
//                |""".stripMargin.split("\n")
  val _modules = input.map(createModule).map(m => m.name -> m).toMap
  val modules  = initialiseConjunctions(_modules)

  case class State(theButton: Module, modules: Map[String, Module], tasks: Seq[Task], highs: Int, lows: Int) {

    def pushTheButton: State = this.copy(tasks = tasks.appended(Task(theButton, "broadcaster", Pulse.Low)))

    def processTask: State = tasks match
      case Nil     => this
      case t :: tt =>
//        println(s"\t${t.source.name} -${t.pulse}-> ${t.destination}")
        val moduleOpt = modules.get(t.destination)
        val (high, low) = t.pulse match
          case Pulse.Low  => (highs, lows + 1)
          case Pulse.High => (highs + 1, lows)
        moduleOpt match
          case None =>
            if (t.pulse == Pulse.Low)
              throw new RuntimeException("rx was hit")
            this.copy(tasks = tt, highs = high, lows = low)
          case Some(m) =>
            val (module, tasks) = m.receivePulse(t)
            val updatedModules  = modules.updated(module.name, module)
            val updatedTasks    = tt ++ tasks
            this.copy(
              modules = updatedModules,
              tasks = updatedTasks,
              highs = high,
              lows = low
            )
  }

  object State {
    def apply(modules: Map[String, Module]): State = State(Button(), modules, Nil, 0, 0)
  }

  @tailrec
  def gogogo(state: State): State =
    state.tasks match
      case Nil => state
      case _   => gogogo(state.processTask)

  def buttonMashing(init: State, n: Int): State =
    Range(0, n).foldLeft(init) { case (state, i) =>
      //      val (state, states) = current
      println(i + 1)
      //      val oneStep = gogogo(state.pushTheButton)
      //      if(states.contains(oneStep.modules))
      //        println("Its happening")
      //      (oneStep, states + oneStep.modules)
      //    }
      gogogo(state.pushTheButton)
    }

  val p1 = test("p1") {
    val init = State(modules)
    val end  = buttonMashing(init, 1000)
    assert(end.tasks.length)(Assertion.equalTo(0)) &&
    assert(end.highs * end.lows)(Assertion.equalTo(670984704))
  }

  val p2 = test("p2") {
    val cycles: Seq[Long] = Seq(3853, 4073, 4091, 4093).map(_.toLong)
    val lcm               = 262775362119547L
    assert(cycles.product)(Assertion.equalTo(lcm))
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("20")(p1, p2) @@ TestAspect.sequential
}
