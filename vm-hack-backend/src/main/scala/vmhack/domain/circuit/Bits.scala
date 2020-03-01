package vmhack.domain.circuit

trait Bits
case class Bit1(value: Boolean) extends Bits {
  def nand(other: Bit1): Bit1 = Bit1(!(value && other.value))
}
case class Bit16(value: Seq[Bit1]) extends Bits
