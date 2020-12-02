package textDecode
import spinal.core.sim._
import spinal.core._
object oProcessSim {
  def main(args:Array[String]): Unit = {
    SimConfig
      .withWave
      .allOptimisation
//      .workspacePath("./out")
      .compile(new oProcess())
      .doSimUntilVoid { dut =>
        // Simulation Code Here
        dut.clockDomain.forkStimulus(period = 400000000)
        dut.clockDomain.waitFallingEdge()
        val
      }
  }
}
