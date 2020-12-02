package textDecode

import spinal.core._
import spinal.lib._

class gProcess extends Component{
  val io = new Bundle {
    val streamIn = slave(Stream(Bits(7 bits)))
    val gEn      = in Bool()
    val gCnt     = out UInt(7 bits)
    val luoTable = out Vec(Vec(Bits(2 bits),16),16)
    val oCntmax  = out UInt(9 bits)
  }

  val rLuoTable = Reg(Vec(Vec(Bits(2 bits),16),16),init = Vec.fill(16)(Vec.fill(16)(B(0))))
  val rOCntmax  = Reg(UInt(9 bits)) init(0)
  val rGCnt     = Reg(UInt(7 bits)) init(0)

  val luoGroupTable = Vec(Bits(8 bits),81)
  val luoONumberTable = Vec(UInt(3 bits),81)

  for (i <- 0 to 2)
    for(j <- 0 to 2)
      for(k <-0 to 2)
        for(l <- 0 to 2) {
          luoGroupTable(i*3*3*3+j*3*3+k*3+l) := (i << 6) | (j << 4) | (k << 2) | l
          luoONumberTable(i*3*3*3+j*3*3+k*3+l) := (U(i) === U(2)).asUInt.resize(3) + (U(j) === U(2)).asUInt.resize(3) + (U(k) === U(2)).asUInt.resize(3) + (U(l) === U(2)).asUInt.resize(3)
        }

  io.streamIn.ready := True
  when(io.gEn) {
    when(io.streamIn.fire) {
      switch(io.streamIn.payload.asUInt) {
        is(U(81)) {
          rGCnt := rGCnt + U(4).resized
          rLuoTable(rGCnt(5 downto 2)).map(_ := B(0,2 bits)) //:= Vec.fill(16)(B(0))
        }
        is(U(82)) {
          rGCnt := rGCnt + U(4).resized
          rLuoTable(rGCnt(5 downto 2)).map(_ := B(1,2 bits)) //:= Vec.fill(16)(B(1))
        }
        is(U(83)) {
          rGCnt := rGCnt + U(4).resized
          rOCntmax := rOCntmax + U(16).resized
          rLuoTable(rGCnt(5 downto 2)).map(_ := B(2,2 bits)) //:= Vec.fill(16)(B(2))
        }
        default {
          assert(io.streamIn.payload.asUInt < U(81),message = "Input must be less than 81.")
          rOCntmax := rOCntmax + luoONumberTable(io.streamIn.payload.asUInt).resized
          rGCnt := rGCnt + U(1).resized
          for (i <- 0 to 3) {
            rLuoTable(rGCnt(5 downto 2))(((rGCnt(1 downto 0) <<2) +i).resize(4)) := luoGroupTable(io.streamIn.payload.asUInt)(i*2 +1 downto i*2)
          }
        }
      }
    }
  }.otherwise {
    rLuoTable := Vec.fill(16)(Vec.fill(16)(B(0)))
    rOCntmax  := U(0)
    rGCnt     := U(0)
  }


  io.gCnt     := rGCnt
  io.luoTable := rLuoTable
  io.oCntmax  := rOCntmax
}

object genVeriloggProcess {
  def main(args:Array[String]): Unit = {
    SpinalVerilog(new gProcess())
  }
}