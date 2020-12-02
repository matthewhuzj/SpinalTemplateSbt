package textDecode
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.regif._
import spinal.lib.bus.regif.AccessType._

import scala.math._
import spinal.lib.memory._
import sun.jvm.hotspot.ui.classbrowser.HTMLGenerator

class decReg(apbconfig:Apb3Config,bitWidth:Int,codeLen:Int) extends Component{
  val apb = slave(Apb3(apbconfig))
  val busif = Apb3BusInterface(apb,(0x000,1 KiB))
  val CONFIG = busif.newReg("mblock config")
  val start  = CONFIG.field(1 bits,RW,doc = "Mblock decode start").asOutput()
  val mb_x   = CONFIG.fieldAt(pos = 4,8 bits,RW,doc = "Mblock x index").asOutput()
  val mb_y   = CONFIG.field(8 bits,RW,doc = "Mblock y index").asOutput()
  val CONFIG1= busif.newReg("mblock config1")
  val mb_w   = CONFIG1.field(5 bits,RW,16,doc = "Mblock width").asOutput()
  val mb_h   = CONFIG1.fieldAt(8,5 bits,RW,16,doc = "Mblock height").asOutput()
  val gCodeEnc   = out Vec(UInt(13 bits),7)
  val gCodeIndex = out Vec(UInt(7 bits),7)
  val oCodeEnc   = out Vec(UInt(13 bits),7)
  val oCodeIndex = out Vec(UInt(7 bits),7)
  val bCodeEnc   = out Vec(UInt(codeLen bits),bitWidth)
  val bCodeIndex = out Vec(UInt(bitWidth bits),bitWidth)
  val eCodeEnc   = out Vec(UInt(codeLen bits),bitWidth)
  val eCodeIndex = out Vec(UInt(bitWidth bits),bitWidth)
//  val mm = Mem(UInt(8 bits),256)



  for (i <- 0 to 6) {
    val GCODEENC_i = busif.newReg("group encode and index "+i)
    val gCodeEnc_i = GCODEENC_i.field(BitCount(13),RW,doc = "group encode data "+i)
    val gCodeIndex_i = GCODEENC_i.fieldAt(16,BitCount(7),RW,0x7f,doc = "group index data "+i)
    gCodeEnc(i) := gCodeEnc_i.asUInt
    gCodeIndex(i) := gCodeIndex_i.asUInt
  }

  for (i <- 0 to 6) {
    val OCODEENC_i = busif.newReg("osymbol encode and index "+i)
    val oCodeEnc_i = OCODEENC_i.field(BitCount(13), RW, doc = "osymbol encode data "+i)
    val oCodeIndex_i = OCODEENC_i.fieldAt(16, BitCount(7), RW, 0x7f, doc = "osymbol index data "+i)
    oCodeEnc(i) := oCodeEnc_i.asUInt
    oCodeIndex(i) := oCodeIndex_i.asUInt
  }

  for (i <- 0 to bitWidth -1) {
    val bCODEENC_i = busif.newReg("base color encode and index " +i)
    val bCodeEnc_i = bCODEENC_i.field(BitCount(codeLen), RW, doc = "base color encode data " +i)
    val bCodeIndex_i = bCODEENC_i.field(BitCount(bitWidth), RW, pow(2, bitWidth).toInt - 1, doc = "base color index data "+i)
    bCodeEnc(i) := bCodeEnc_i.asUInt
    bCodeIndex(i) := bCodeIndex_i.asUInt

  }

  for (i <- 0 to bitWidth -1) {
    val ESCAPECODEENC_i = busif.newReg("escape color encode and index " +i)
    val eCodeEnc_i = ESCAPECODEENC_i.field(BitCount(codeLen), RW, doc = "escape color encode data "+i)
    val eCodeIndex_i = ESCAPECODEENC_i.field(BitCount(bitWidth), RW, pow(2, bitWidth).toInt - 1, doc = "escape color index data " +i)
    eCodeEnc(i) := eCodeEnc_i.asUInt
    eCodeIndex(i) := eCodeIndex_i.asUInt
  }
//  busif.accept(HTMLGenerator("TextDecodeReg.html",name = "Gnss"))

}

object decRegVerilog {
  def main(args:Array[String]): Unit = {
    SpinalConfig(targetDirectory = "./out")
      .generateVerilog(new decReg(Apb3Config(12,32,1,false),8,16))
  }
}