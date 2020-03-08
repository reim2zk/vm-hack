package vmhack.domain.circuit

case class Composite(id: Gate.GateId, ins: Seq[Pin], outs: Seq[Pin], connections: Seq[Composite.Connection], gates: Seq[Gate]) extends Gate {
  override def run(bits: Pin.BitMap): Either[GateException, Pin.BitMap] = {
    if (this.endRun(bits)) {
      Right(bits)
    } else {
      this.run(bits)
    }
  }
  def endRun(bits: Pin.BitMap): Boolean = {
    outs.forall(b => bits.contains(b.id))
  }
  def runOne(bits: Pin.BitMap): Either[GateException, Pin.BitMap] = {
    val bitsE1 = connections.foldLeft[Either[GateException, Pin.BitMap]](Right(bits)) { case (bitsE, connection) =>
      bitsE.map(connection.run(_).get)
    }
    val bitsE2 = gates.foldLeft(bitsE1) { case (bitsE, gate) =>
      bitsE.flatMap(bits => gate.run(bits))
    }
    bitsE2
  }
}
object Composite {
  trait Connection {
    def run(bitsOnGates: Pin.BitMap): Option[Pin.BitMap]
  }
  case class ConnectionImpl[B <: Bits](inPin: Pin.PinImpl[B], outPin: Pin.PinImpl[B]) extends Connection {
    override def run(bitMap: Pin.BitMap): Option[Pin.BitMap] = {
      for {
        inBits <- bitMap.get(inPin.id)
      } yield bitMap.+((outPin.id, inBits))
    }
  }

}
