package vmhack.domain.circuit

case class Composite(id: GateId, ins: Seq[Pin], outs: Seq[Pin], connections: Seq[Composite.Connection], gates: Seq[Gate]) extends Gate {
  override def update: Either[GateException, Gate] = ???
}
object Composite {
  private case class Connection(inPin: Pin, outPin: Pin)
}
