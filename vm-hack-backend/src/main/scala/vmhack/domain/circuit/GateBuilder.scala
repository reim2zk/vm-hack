package vmhack.domain.circuit

case class CompositeGateBuilder(
  id: Gate.GateId,
  ins: Seq[Pin],
  outs: Seq[Pin],
  connections: Seq[Composite.Connection],
  gates: Seq[Gate]) {
  def idMax: Pin.Id = gates.map(_.maxId).max
  def setGate(gate: Gate): CompositeGateBuilder = {
    val adjustedGate = gate.resetPinId(idMax)
    this.copy(gates = gates ++ Seq(adjustedGate ))
  }
  def ins(names: Seq[String]): CompositeGateBuilder = {
    val inPins = names.zipWithIndex.map{case (name, i) => Pin(i + idMax + 1, name)}
    this.copy(ins = inPins)
  }
  def outs(names: Seq[String]): CompositeGateBuilder = {
    val outPins = names.zipWithIndex.map{case (name, i) => Pin(i + idMax + 1, name)}
    this.copy(outs = outPins)
  }


  def build(): Gate = ???
}
