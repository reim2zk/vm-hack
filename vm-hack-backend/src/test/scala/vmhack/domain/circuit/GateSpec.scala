package vmhack.domain.circuit

import org.scalatest.FunSuite
import vmhack.domain.mutable.circuit.Pin

class CubeCalculatorTest extends FunSuite {
  test("NandGate") {
    val nand = NandGate(0, 0)
    val bits: Pin.BitMap = Seq(
      nand.a.id->Bit1(true),
      nand.b.id->Bit1(false)).toMap
    val outBits = nand.run(bits).toOption.get
    val out = outBits(nand.out.id)
    assert(out === Bit1(true))
  }
}