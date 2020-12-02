package huffman

import spinal.core.sim._
import spinal.core._

object HuffmanDecodeSim {

  def main(args: Array[String]) {
    val bitWidth = 10
    val codeLen  = 20
    SimConfig.withWave.doSim(new HuffmanDecode(bitWidth,codeLen)) { dut =>
      //Fork a process to generate the reset and the clock on the dut
      var streamIn = "1110101010110110001111001101110101"
      val zeroStr ="00000000000000000000000000000000"
      val alpha = "abcdefghijklmnopqrstuvwxyz"

      dut.clockDomain.forkStimulus(period = 10)
      for (i <- 0 to codeLen -1){
        dut.io.code_index(i) #= 0
      }
      dut.io.code_index(2) #= 0
      dut.io.code_index(3) #= 1
      dut.io.code_index(4) #= 9
      dut.io.code_index(6) #= 17
      dut.io.code_enc(0) #= 0x0
      dut.io.code_enc(1) #= 0x0
      dut.io.code_enc(2) #= 0x0
      dut.io.code_enc(3) #= 0x20<<(codeLen-8)
      dut.io.code_enc(4) #= 0xa0<<(codeLen-8)
      dut.io.code_enc(5) #= 0xe0<<(codeLen-8)
      dut.io.code_enc(6) #= 0xe0<<(codeLen-8)
      for (i <- 7 to codeLen -1) {
        dut.io.code_enc(i) #= math.pow(2,codeLen).toInt -1
      }
      dut.io.code_valid #= false
      dut.io.code #= 0
      dut.clockDomain.waitFallingEdge()
      dut.clockDomain.waitFallingEdge()
      dut.io.code_valid #= true
      for (idx <- 0 to 6){
        dut.io.code #= BigInt( streamIn.substring(0,codeLen),2)
        dut.clockDomain.waitFallingEdge()
//        println(dut.io.length_o.toInt)
//        println(dut.io.index_o.toInt)
//        println(streamIn)
        print(alpha(dut.io.index_o.toInt))
        streamIn = streamIn.substring(dut.io.length_o.toInt+1,streamIn.length)
          .concat(zeroStr.substring(0,dut.io.length_o.toInt+1))
      }
      println()
      dut.io.code_valid #= false

      sleep(20)
      assert(BigInt(streamIn,2) == 0)


    }
  }

}
