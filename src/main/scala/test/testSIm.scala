package test

import spinal.core.sim._
import spinal.core._

object testSim {
  def main(args: Array[String]) {
    SimConfig.withWave.doSim(new TopLevel) { dut =>
      //Fork a process to generate the reset and the clock on the dut

      dut.clockDomain.forkStimulus(period = 10)
      dut.io.streamin.valid #= false
      dut.io.streamout.ready #= true
      dut.clockDomain.waitFallingEdge()
      dut.clockDomain.waitFallingEdge()

      dut.io.streamin.valid #= true
      dut.io.streamin.payload #= 0xaa
      waitUntil(dut.io.streamin.valid.toBoolean && dut.io.streamin.ready.toBoolean)
      dut.clockDomain.waitFallingEdge()
      dut.io.streamin.payload #= 0xbb
      dut.io.streamin.valid #= false
      dut.io.streamout.ready #= false
      sleep(20)
      dut.clockDomain.waitFallingEdge()
      dut.io.streamin.valid #= true
      sleep(10)
      dut.io.streamout.ready #= true
      waitUntil(dut.io.streamout.valid.toBoolean && dut.io.streamout.ready.toBoolean)
      dut.clockDomain.waitFallingEdge()
      dut.io.streamin.valid #= false
      sleep(100)

    }
  }

}
