package vmhack.domain.mutable.circuit

trait Pin {
  type B <: Bits
  val name: Pin.Name
  var bits: Option[B]
  def setBits(b: B): Unit
  def copyBitsFrom(other: Pin): Either[PinException, Unit]
}
trait PinException
object Pin {
  class PinImpl[BB <: Bits](argName: Name) extends Pin {
    type B = BB
    var bits: Option[BB] = None
    val name: Pin.Name = argName
    override def setBits(b: BB): Unit = {
      this.bits = Some(b)
    }
    override def copyBitsFrom(other: Pin): Either[PinException, Unit] = {
      other match {
        case otherBB: PinImpl[BB] => for {
          a: BB <- other.bits.toRight(BitsNotFound(this))
          res <- this.setBits(a)
        } yield res
        case _ => Left(Pin.PinTypeMismatch(this, other))
      }
    }
  }

  type Id = Int
  type Name = String
  type BitMap = Map[Id, Bits]
  def emptyBitMap: BitMap  = Map.empty
  def apply[B <: Bits](name: String): PinImpl[B] = new PinImpl[B](name)

  case object OtherPinIsNull extends PinException
  case class PinTypeMismatch(value: Pin, other: Pin) extends PinException
  case class BitsNotFound(pin: Pin) extends PinException
  case class BitsAlreadyAssigned(pin: Pin) extends PinException

  type Pin1 = PinImpl[Bit1]
}