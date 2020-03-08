package vmhack.domain.circuit

trait Gate {
  val id: Gate.GateId
  def ins: Seq[Pin]
  def outs: Seq[Pin]
  def run(bits: Pin.BitMap): Either[GateException, Pin.BitMap]
}

trait GateException

object Gate {
  type GateId = Int
  case object NullInExists extends GateException
  case object OutHasValue extends GateException
  case class BitsOnPinsNotFound(gate: Gate) extends GateException
  case class BitsNotFound(gate: Gate, pin: Pin) extends GateException
  case class ErrorOnPin(e: PinException) extends GateException
}

case class Connector(inPin: Pin, outPin: Pin)