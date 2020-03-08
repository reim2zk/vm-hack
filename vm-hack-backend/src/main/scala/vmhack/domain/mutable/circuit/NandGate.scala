package vmhack.domain.mutable.circuit

import vmhack.domain.mutable.circuit

case class NandGate(id: Gate.GateId, a: Pin.Pin1, b: Pin.Pin1, out: Pin.Pin1) extends Gate {
  override def ins: Seq[Pin] = Seq(a, b)
  override def outs: Seq[Pin] = Seq(out)
  override def run(bits: Pin.BitMap): Either[GateException, Pin.BitMap] = {
    for {
      aBits <- a.bits(bits).toRight(Gate.BitsNotFound(this, a))
      bBits <- b.bits(bits).toRight(Gate.BitsNotFound(this, b))
      outBits = aBits.nand(bBits)
      newBits <- out.setBits(bits, outBits).toRight(Gate.BitsNotFound(this, out))
    } yield newBits
  }
}
object NandGate {
  def apply(id: Gate.GateId, id0: Pin.Id): NandGate = {
    val a: Pin.Pin1 = Pin[Bit1](id0, "a")
    val b: Pin.Pin1 = Pin[Bit1](id0 + 1, "b")
    val out: Pin.Pin1 = Pin[Bit1](id0 + 2, "out")
    NandGate(id, a, b, out)
  }
}