package vmhack.domain.circuit

case class GateId(value: Int)

trait Gate {
  val id: GateId
  def ins: Seq[Pin]
  def outs: Seq[Pin]
  def update: Either[GateException, Gate]
}

trait GateException

object Gate {
  type GateId = Int
  case object NullInExists extends GateException
  case object OutHasValue extends GateException
}

case class Connector(inPin: Pin, outPin: Pin)