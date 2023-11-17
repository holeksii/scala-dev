import scala.compiletime.ops.boolean

trait Simulation:
  type Action = () => Unit
  private case class Event(time: Int, action: Action)
  private type Agenda = List[Event]
  private var agenda: Agenda = List()
  private var curtime = 0

  def currentTime: Int = curtime

  def afterDelay(delay: Int)(block: => Unit): Unit =
    val item = Event(currentTime + delay, () => block)
    agenda = insert(agenda, item)

  private def insert(agenda: Agenda, item: Event): Agenda =
    agenda match
      case first :: rest if first.time <= item.time =>
        first :: insert(rest, item)
      case _ =>
        item :: agenda

  private def loop(): Unit = agenda match
    case first :: rest =>
      agenda = rest
      curtime = first.time
      first.action()
      loop()
    case Nil =>
  end loop

  def run(): Unit =
    afterDelay(0) {
      println("*** simulation started, time = " + currentTime + " ***")
    }
    loop()
end Simulation

trait Gates extends Simulation:
  def InverterDelay: Int
  def AndGateDelay: Int
  def OrGateDelay: Int

  class Wire:
    private var signalVal = false
    private var actions = List[Action]()

    def getSignal = signalVal
    def setSignal(s: Boolean): Unit =
      if s != signalVal then signalVal = s
      actions.foreach(_())

    def addAction(a: Action) =
      actions = a :: actions
      a()
  end Wire

  def inverter(input: Wire, output: Wire): Unit =
    def invertAction(): Unit =
      afterDelay(InverterDelay) { output.setSignal(!input.getSignal) }
    input.addAction(invertAction)

  def andGate(in1: Wire, in2: Wire, output: Wire): Unit =
    def andAction(): Unit =
      afterDelay(AndGateDelay) {
        output.setSignal(in1.getSignal & in2.getSignal)
      }
    in1.addAction(andAction)
    in2.addAction(andAction)

  def orGate(in1: Wire, in2: Wire, output: Wire): Unit =
    def andAction(): Unit =
      afterDelay(OrGateDelay) {
        output.setSignal(in1.getSignal | in2.getSignal)
      }
    in1.addAction(andAction)
    in2.addAction(andAction)

  def probe(name: String, wire: Wire): Unit =
    def probeAction(): Unit =
      println(name + " " + currentTime + " new-value = " + wire.getSignal)
    wire.addAction(probeAction)
end Gates

trait Circuits extends Gates:
  def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire): Unit =
    val d, e = new Wire
    orGate(a, b, d)
    andGate(a, b, c)
    inverter(c, e)
    andGate(d, e, s)

  def fullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, cout: Wire): Unit =
    val s, c1, c2 = new Wire
    halfAdder(b, cin, s, c1)
    halfAdder(a, s, sum, c2)
    orGate(c1, c2, cout)

end Circuits

trait Parameters:
  def InverterDelay = 2
  def AndGateDelay = 3
  def OrGateDelay = 5

object MySimulation extends Circuits with Parameters:
  def main(args: Array[String]): Unit =
    val input1, input2, sum, carry = new Wire
    probe("sum", sum)
    probe("carry", carry)
    halfAdder(input1, input2, sum, carry)
    input1 setSignal true
    run()
    input2 setSignal true
    run()

end MySimulation

MySimulation.main
