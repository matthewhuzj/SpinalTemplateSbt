package textDecode
import spinal.core.sim._
import spinal.core._

import scala.util.Random
object oProcessSim {
  def main(args:Array[String]): Unit = {
    SimConfig
      .withWave
      .allOptimisation
//      .workspacePath("./out")
      .compile(new oProcess())
//      .doSimUntilVoid { dut =>
      .doSimUntilVoid(seed = 525679804) { dut =>
        // Simulation Code Here
        dut.clockDomain.forkStimulus(period = 10)
        SimTimeout(100000 * 10)
//        dut.clockDomain.waitFallingEdge()
        var oCnt = 0
        val timeCtrl = fork {
          dut.io.oEn #= false
          sleep(2)
          dut.io.oEn #= true
          waitUntil(dut.io.oFinish.toBoolean)
          dut.io.oEn #= false
          sleep(10)
          simSuccess()
        }
        val streamThread = fork {
          dut.io.oStreamIn.valid #= false
          dut.io.oStreamIn.payload #= 0
          sleep(10)
          var flag = true
          var cnt = 0
          for (i <- 0 to 1000 if flag) {
            dut.clockDomain.waitFallingEdge()
            dut.io.oStreamIn.valid #= true
            dut.io.oStreamIn.payload #= Random.nextInt(125)
            dut.clockDomain.waitFallingEdge()
            waitUntil(dut.io.oStreamIn.valid.toBoolean && dut.io.oStreamIn.ready.toBoolean)
            dut.io.oStreamIn.valid #= false
            cnt += 1
            print(simTime())
            print(" cnt:")
            print(cnt)

            print(" oCnt:")
            print(dut.oCnt.toInt)
            println()
            if(cnt == oCnt) {
              flag = false
            }
          }
        }
        val luoThread = fork {
          dut.io.oUPyuv.map(_ #= (Random.nextInt(5)<<6) |(Random.nextInt(5)<<3) | Random.nextInt(3))
          dut.io.luoTable.map(_.map(_ #= Random.nextInt(3)))
          sleep(1)
          println("upYUV")
          for (i <- dut.io.oUPyuv) {
            var temp = i.toInt
            print("{")
            print(math.floor(temp/64).toInt)
            print(",")
            print(math.floor((temp%64)/8).toInt)
            print(",")
            print(temp%8)
            print("} ")
          }
          println()
          println("luotable")
          for(row <- dut.io.luoTable) {
            for(col <- row) {
              if(col.toInt == 0)
                print("L ")
              else if(col.toInt == 1)
                print("U ")
              else if(col.toInt == 2) {
                oCnt += 1
                print("O ")
              } else {
                print("x ")
              }
            }
            println()
          }
          println(oCnt)
        }
      }
  }
}
