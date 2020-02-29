package vmhack.domain.circuit

case class GateId(value: Int)
case class PinName(value: String)

trait Bits
case class Bit1(value: Boolean) extends Bits
case class Bit16(value: Seq[Bit1]) extends Bits

trait Pin {
  type B <: Bits
  def setBits(bits: B)
  def bits(): Option[B]
}
case class PinImpl[BB <: Bits]() extends Pin {
  type B = BB
  override def setBits(bits: BB) = ???
  override def bits(): Option[BB] = ???
}

trait Connector {
  def update(): Unit
}
case class ConnectorImpl[B <: Bits](inPin: PinImpl[B], outPin: PinImpl[B])
    extends Connector {
  override def update(): Unit = {
    inPin.bits().foreach(outPin.setBits)
  }
}

trait Gate {
  val id: GateId
  def ins: Seq[Pin]
  def outs: Seq[Pin]
}

object Gate {
  type GateId = Int
}
