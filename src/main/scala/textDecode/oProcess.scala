package textDecode

import spinal.core._
import spinal.lib._



class oProcess extends Component{
  val io = new Bundle {
    val oStreamIn = slave(Stream(Bits(7 bits)))
    val oEn       = in Bool()
    val oUPyuv    = in Vec(Bits(9 bits),16)
    val luoTable  = in Vec(Vec(Bits(2 bits),16),16)
    val oFinish   = out Bool()
    val oYUVIndex = out Vec(Vec(Bits(9 bits),16),16)
    val eYUVCntMax= out Vec(UInt(9 bits),3)
    val bYUVMax   = out Vec(UInt(3 bits),3)
    val oUPyuvCur = out Vec(Bits(9 bits),16)
  }

  val inFifo = StreamFifo(Bits(7 bits),16)
  val rCnt   = Reg(UInt(9 bits)) init(0)
//  val vrLeftYUV= Vec(Reg(Bits(9 bits) ,init = B(0)),16)
  val vrLeftYUV= Reg(Vec(Bits(9 bits),16) ,init = Vec.fill(16)(B(0)))
  val vrYUVIndex = Reg(Vec(Vec(Bits(9 bits),16),16),init = Vec.fill(16)(Vec.fill(16)(B(0))))
//  val vrYUVIndex = Vec(Vec(Vec(Reg(Bits(3 bits),init = B(0)),16),16),3)
  val quantizeTable = Vec(Bits(9 bits),125)
  val reYUVCntMax= Vec(Reg(UInt(9 bits),init = U(0)),3)
  val rbYUVMax   = Vec(Reg(UInt(3 bits),init = U(0)),3)
  val roUPyuvCur = Reg(Vec(Bits(9 bits),16),init = Vec.fill(16)(B(0)))
//  val roUPyuvCur = Vec(Reg(Bits(9 bits),init = B(0)),16)
  val rFinish    = Reg(Bool(), init = False)
  val oReady     = Bool()
  val quantizeYUV= UInt(9 bits)

  io.oStreamIn >> inFifo.io.push
  // only O need to read from fifo
  oReady := io.oEn & !rCnt(8) & (io.luoTable(rCnt(7 downto 4))(rCnt(3 downto 0)) === B(2))
  inFifo.io.pop.ready := oReady
  quantizeYUV := quantizeTable(inFifo.io.pop.payload.asUInt).asUInt

  // quantize table
  for (i <- 0 to 4)
    for (j <- 0 to 4)
      for (k <- 0 to 4) {
        quantizeTable(i*25 + j * 5 + k) := (i<<6) | ( j<<3) | k
      }

  when(io.oEn & rCnt(8)) {
    rFinish := True
  }.elsewhen(!io.oEn) {
    rFinish := False
  }

  when(io.oEn & !rCnt(8)) {
    // next up
      when(rCnt(7 downto 4) === U(15)) {
        when(io.luoTable(rCnt(7 downto 4))(rCnt(3 downto 0)) === B(0) ) {
          when(rCnt(3 downto 0) === U(0)) {
            roUPyuvCur(0) := vrLeftYUV(15)
          }.otherwise {
            roUPyuvCur(rCnt(3 downto 0)) := vrYUVIndex(15)(rCnt(3 downto 0) - U(1))
          }
        }.elsewhen(io.luoTable(rCnt(7 downto 4))(rCnt(3 downto 0)) === B(1) ) {
          roUPyuvCur(rCnt(3 downto 0)) := vrYUVIndex(14)(rCnt(3 downto 0))
        }.otherwise {
          when(inFifo.io.pop.fire) {
            roUPyuvCur(rCnt(3 downto 0)):= quantizeYUV.asBits
          }
        }
      }
    // next left
      when(rCnt(3 downto 0) === U(15)) {
        when(io.luoTable(rCnt(7 downto 4))(rCnt(3 downto 0)) === B(0)) {
          vrLeftYUV(rCnt(7 downto 4)) := vrYUVIndex(rCnt(7 downto 4))(14)
        }.elsewhen(io.luoTable(rCnt(7 downto 4))(rCnt(3 downto 0)) === B(1)) {
          when(rCnt(7 downto 4) === U(0)) {
            vrLeftYUV(rCnt(7 downto 4)) := io.oUPyuv(15)
          }.otherwise {
            vrLeftYUV(rCnt(7 downto 4)) := vrYUVIndex(rCnt(7 downto 4) - U(1))(15)
          }
        }.otherwise {
          when(inFifo.io.pop.fire) {
              vrLeftYUV(rCnt(7 downto 4)) := quantizeYUV.asBits
          }
        }
      }
  }
  // main
  when(io.oEn  & !rCnt(8)) {
    // if L
    when(io.luoTable(rCnt(7 downto 4))(rCnt(3 downto 0)) === B(0)) {
      rCnt := rCnt + U(1).resized
      when(rCnt(3 downto 0) === U(0)) {
        for (i <- 0 to 2) {
          when(rbYUVMax(i) < vrLeftYUV(rCnt(7 downto 4))(i * 3 + 2 downto i * 3).asUInt) {
            rbYUVMax(i) := vrLeftYUV(rCnt(7 downto 4))(i * 3 + 2 downto i * 3).asUInt
          }
          when(vrLeftYUV(rCnt(7 downto 4))(i * 3 + 2 downto i * 3) === B(4)){
            reYUVCntMax(i) := reYUVCntMax(i) + U(1).resized
          }
        }
        vrYUVIndex(rCnt(7 downto 4))(0) := vrLeftYUV(rCnt(7 downto 4)).asBits
      }.otherwise {
        for (i <- 0 to 2) {
          when(vrYUVIndex(rCnt(7 downto 4))(rCnt(3 downto 0) - U(1))(3*i + 2 downto 3*i) === B(4)){
            reYUVCntMax(i) := reYUVCntMax(i) + U(1).resized
          }
        }
        vrYUVIndex(rCnt(7 downto 4))(rCnt(3 downto 0)) := vrYUVIndex(rCnt(7 downto 4))(rCnt(3 downto 0) - U(1))
      }
      // if U
    }.elsewhen(io.luoTable(rCnt(7 downto 4))(rCnt(3 downto 0)) === B(1)) {
      rCnt := rCnt + U(1).resized
      when(rCnt(7 downto 4) === U(0) ) {
        for (i <- 0 to 2) {
          when(rbYUVMax(i) < io.oUPyuv(rCnt(3 downto 0))(i * 3 + 2 downto i * 3).asUInt) {
            rbYUVMax(i) := io.oUPyuv(rCnt(3 downto 0))(i * 3 + 2 downto i * 3).asUInt
          }
          when(io.oUPyuv(rCnt(3 downto 0))(i * 3 + 2 downto i * 3).asUInt === U(4)){
            reYUVCntMax(i) := reYUVCntMax(i) + U(1).resized
          }
        }
        vrYUVIndex(0)(rCnt(3 downto 0)) := io.oUPyuv(rCnt(3 downto 0)).asBits
      }.otherwise {
        for (i <- 0 to 2) {
          when(vrYUVIndex(rCnt(7 downto 4))(rCnt(3 downto 0) - U(1))(3*i +2 downto 3*i) === B(4)){
            reYUVCntMax(i) := reYUVCntMax(i) + U(1).resized
          }
        }
        vrYUVIndex(rCnt(7 downto 4))(rCnt(3 downto 0)) := vrYUVIndex(rCnt(7 downto 4) - U(1).resized)(rCnt(3 downto 0))
      }
      // else O
    }.otherwise {
      // get data from fifo
      when(inFifo.io.pop.fire) {
        rCnt := rCnt + U(1).resized
        for (i <- 0 to 2) {
          when(rbYUVMax(i) < quantizeYUV(i*3 +2 downto i*3)) {
            rbYUVMax(i) := quantizeYUV(i*3 +2 downto i*3)
          }
          when(quantizeYUV(i*3 +2 downto i*3) === U(4)){
            reYUVCntMax(i) := reYUVCntMax(i) + U(1).resized
          }
        }
        vrYUVIndex(rCnt(7 downto 4))(rCnt(3 downto 0)) := quantizeYUV.asBits
      }
    }
  }.elsewhen(!io.oEn) {
    rCnt := U(0)
    for (i <- 0 to 2) {
      rbYUVMax(i) := U(0)
      reYUVCntMax(i) := U(0)
    }
  }


  io.oFinish    := rFinish
  io.oYUVIndex  := vrYUVIndex
  io.eYUVCntMax := reYUVCntMax
  io.bYUVMax    := rbYUVMax
  io.oUPyuvCur  := roUPyuvCur
}

object genVerilogoProcess {
  def main(args:Array[String]): Unit = {
    SpinalVerilog(new oProcess())
  }
}
