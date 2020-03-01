package vmhack.domain.circuit

case class NandGate(id: GateId, a: Pin.Pin1, b: Pin.Pin1, out: Pin.Pin1) extends Gate {
  override def ins: Seq[Pin] = Seq(a, b)
  override def outs: Seq[Pin] = Seq(out)
  override def update: Either[GateException, Gate] = {
    (a.getBits(), b.getBits(), out.getBits()) match {
      case (Some(aa), Some(bb), None) => Right(NandGate(id, a, b, out.setBits(aa.nand(bb))))
      case (_, _, Some(_))            => Left(Gate.OutHasValue)
      case _                          => Left(Gate.NullInExists)
    }
  }
}
