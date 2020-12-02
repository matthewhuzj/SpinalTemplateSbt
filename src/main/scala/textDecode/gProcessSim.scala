package textDecod

import spinal.core.sim._
import spinal.core._
import textDecode.gProcess

import scala.util.Random

object gProcessSim {
  def main(args:Array[String]): Unit = {
    SimConfig
      .withWave
      .allOptimisation
//      .workspacePath("./out")
      .compile(new gProcess())
      .doSimUntilVoid { dut =>
        // Simulation Code Here
        val newThread = fork {
          dut.io.gEn #= false
          dut.io.streamIn.valid #= false
          dut.clockDomain.forkStimulus(period = 400000000)
          for (i <- 0 to 15)
            dut.clockDomain.waitFallingEdge()

          var cnt = 0
          var ocnt = 0
          var randData = 0
          dut.io.gEn #= true
          var flag = true
          for (i <- 0 to 64 if flag) {
            dut.clockDomain.waitFallingEdge()
            dut.io.streamIn.valid #= true
            if (cnt % 4 == 0) {
              randData = math.abs(Random.nextInt) % 4
              if (randData == 0) {
                randData = math.abs(Random.nextInt()) % 80
              } else {
                randData += 80
              }
            } else {
              randData = math.abs(Random.nextInt()) % 80
            }
            dut.io.streamIn.payload #= randData
            if (randData > 80) {
              cnt += 4
            } else {
              cnt += 1
            }
            if (randData > 80) {
              if(randData== 83) ocnt +=16
            } else {
              if (randData % 3 == 2) ocnt += 1
              if (math.floor((randData % 9) / 3).toInt == 2) ocnt += 1
              if (math.floor((randData % 27) / 9).toInt == 2) ocnt += 1
              if (math.floor(randData / 27).toInt == 2) ocnt += 1
            }
            print(ocnt)
            print(" ")
            dut.clockDomain.waitFallingEdge()
            waitUntil(dut.io.streamIn.valid.toBoolean)
            dut.io.streamIn.valid #= false

            if (cnt == 64) {
              flag = false
            }

          }
          dut.clockDomain.waitFallingEdge()
          println(dut.io.gCnt.toInt)
          println(ocnt)
          println(dut.io.oCntmax.toInt)
          for(i <- dut.io.luoTable) {
            for(j <- i)
              print(j.toInt)
            println()
          }
          assert(dut.io.oCntmax.toInt == ocnt,message = "The number of o symbol is not correct!")
          dut.io.gEn #= false
          //          assert(dut.io.oCntmax.toInt == ocnt,message = "The number of o symbol is not correct!",severity = WARNING)
        }
        newThread.join()


      }
  }
}
