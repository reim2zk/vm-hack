package vmhack.domain.circuit

trait Pin {
  type B <: Bits
  def getName: String
  def setBits(bits: B): Pin
  def getBits(): Option[B]
  def copyBits(other: Pin): Either[PinException, Pin]
  def clear(): Pin
}
trait PinException
object Pin {
  case class PinImpl[BB <: Bits](name: String, bits: Option[BB]) extends Pin {

    type B = BB
    override def getName = name
    override def setBits(bits: BB): PinImpl[BB] = PinImpl[BB](name, Some(bits))
    override def getBits(): Option[BB] = bits
    override def copyBits(other: Pin): Either[PinException, Pin] =
      other match {
        case PinImpl(_, Some(value: BB)) => Right(setBits(value))
        case PinImpl(_, None) => Left(OtherPinIsNull)
        case _ => Left(PinTypeMismatch)
      }
    override def clear(): Pin = PinImpl[BB](name, None)
  }
  def apply[B <: Bits](name: String): Pin = PinImpl[B](name, None)

  case object OtherPinIsNull extends PinException
  case object PinTypeMismatch extends PinException

  type Pin1 = PinImpl[Bit1]
}