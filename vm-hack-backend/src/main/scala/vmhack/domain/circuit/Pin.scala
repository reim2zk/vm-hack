package vmhack.domain.circuit

trait Pin {
  type B <: Bits
  val id: Pin.Id
  val name: Pin.Name
  def bits(bitsOnPins: Pin.BitMap): Option[B]
  def setBits(bitsOnPins: Pin.BitMap, b: B): Option[Pin.BitMap]
  def copyBits(other: Pin, bitsOnPins: Pin.BitMap): Either[PinException, Pin.BitMap]
}
trait PinException
object Pin {
  case class PinImpl[BB <: Bits](id: Id, name: Name) extends Pin {
    type B = BB
    override def bits(bitsOnPins: Pin.BitMap): Option[BB] = {
      bitsOnPins.get(id) match {
        case Some(value: BB) => Some(value)
        case None            => None
      }
    }
    override def setBits(bitsOnPins: Pin.BitMap, b: B): Option[Pin.BitMap] = {
      if (bitsOnPins.contains(id)) {
        None
      } else {
        Some(bitsOnPins.+((id, b)))
      }
    }
    override def copyBits(other: Pin, bitsOnPins: Pin.BitMap): Either[PinException, Pin.BitMap] = {
      other match {
        case otherBB: PinImpl[BB] => for {
          a <- this.bits(bitsOnPins).toRight(BitsNotFound(this))
          res <- otherBB.setBits(bitsOnPins, a).toRight(BitsAlreadyAssigned(this))
        } yield res
        case _ => Left(Pin.PinTypeMismatch(this, other))
      }
    }
  }
  type Id = Int
  type Name = String
  type BitMap = Map[Id, Bits]
  def emptyBitMap: BitMap  = Map.empty
  def apply[B <: Bits](id: Id, name: String): PinImpl[B] = PinImpl[B](id, name)

  case object OtherPinIsNull extends PinException
  case class PinTypeMismatch(value: Pin, other: Pin) extends PinException
  case class BitsNotFound(pin: Pin) extends PinException
  case class BitsAlreadyAssigned(pin: Pin) extends PinException


  type Pin1 = PinImpl[Bit1]
}