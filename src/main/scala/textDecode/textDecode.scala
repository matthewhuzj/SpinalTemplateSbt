package textDecode
import spinal.core._
import spinal.lib.fsm._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import huffman.HuffmanDecode
import textDecode.apb3ram

import spinal.lib.bus.regif._
import spinal.lib.bus.regif.AccessType._

class textDecode(bitWidth:Int,codeLen:Int) extends Component {
  val apbConfig = Apb3Config(
    addressWidth = 10,
    dataWidth    = 32,
    useSlaveError = false
  )
  val io = new Bundle {
    val apbreg     = slave(Apb3(apbConfig))
    val mbDecStart = in Bool()
    val streamIn   = slave Stream(UInt(16 bits))
    val sgreamY    = master Stream(UInt(bitWidth bits))
    val sgreamU    = master Stream(UInt(bitWidth bits))
    val sgreamV    = master Stream(UInt(bitWidth bits))
    val valid_o    = out Bool()
    val length_o   = out UInt(codeLen bits)
  }
  val m0 = slave(Apb3(Apb3Config(12,32)))
  val s1 = Apb3(Apb3Config(10,32))
  val s2 = Apb3(Apb3Config(9,32))
  val s3 = Apb3(Apb3Config(9,32))
  val s4 = Apb3(Apb3Config(10,32))
  val s5 = Apb3(Apb3Config(10,32))
  val apbMux = Apb3Decoder(master = m0,
                           slaves = List(s1 -> (0x0,1 KiB),
                                         s2 -> (0x400,512),
                                         s3 -> (0x600,512),
                                         s4 -> (0x800,1 KiB),
                                         s5 -> (0xC00,1 KiB))
                           )
  val ramTDGHuff = new apb3ram(apbConfig,7,84)
  val ramTDOHuff = new apb3ram(apbConfig,7,125)
  val ramTDBHuff = new apb3ram(apbConfig,10,1024)
  val ramTDEHuff = new apb3ram(apbConfig,10,1024)
  val regif = new decReg(apbConfig,bitWidth,codeLen)
  s1>>regif.apb
  s2>>ramTDGHuff.apb3
  s3>>ramTDOHuff.apb3
  s4>>ramTDBHuff.apb3
  s5>>ramTDEHuff.apb3
  val gHuffDec = new HuffmanDecode(bitWidth,codeLen)
  val isGOState= Bool()
  val isBEState= Bool()
  val gFireAdv = Reg(Bool())
  val gGroupCnt = Reg(UInt(7 bits)) init(0)
  val groupValid = Reg(Bool()) init(false)
  val groupData  = UInt(7 bits)
  val groupIncValue = UInt(3 bits)
  val gQuantizedValid = Reg(Bool()) init(false)
  val gStateLockEn    = Reg(Bool()) init(false)
  val oCntMax         = Reg(UInt(7 bits))
  val streamInReadyOut = Bool()


  io.mbDecStart := regif.start.asBool


  val decState=new StateMachine{
    val StateIdle = new State with EntryPoint
    val StateG    = new State   // state g index huffman
    val StateO    = new State   // state o symbol huffman
    val StateB    = new State   // state base color huffman
    val StateE    = new State   // state escape color huffman

    StateIdle
      .whenIsActive(
        when(regif.start.asBool) {
          goto(StateG)
        }
      )
    StateG
      .whenIsActive(
        when(gGroupCnt === U(64)) {
          goto(StateO)
        }
      )
    StateO
      .whenIsActive(
        goto(StateB)
      )

    StateB
      .whenIsActive(
        goto(StateE)
      )

    StateE
      .whenIsActive(
        goto(StateIdle)
      )
  }
  groupData := ramTDGHuff.apb3.PRDATA.asUInt.resized
  groupIncValue := U(1)
  when(groupData > U(80)) {
    groupIncValue := U(4)
  }

  io.streamIn.ready := streamInReadyOut
  streamInReadyOut := True
  when(decState.isEntering(decState.StateG)) {
    streamInReadyOut := !gStateLockEn
  }

  when(decState.isEntering(decState.StateG)) {
    when(gFireAdv && (gGroupCnt === U(60) || gGroupCnt === U(63))) {
      gStateLockEn := True
    } .otherwise {
      gStateLockEn := False
    }
  }
  when(decState.isEntering(decState.StateG)) {
    gFireAdv := False
  }.elsewhen(decState.isActive(decState.StateG)) {
    when(io.streamIn.fire && !groupValid) {
      gFireAdv := True
    }.elsewhen(!io.streamIn.fire && groupValid){
      gFireAdv := False
    }
  }
  when(decState.isEntering(decState.StateG)) {
    gGroupCnt := U(0)
  }.elsewhen(decState.isActive(decState.StateG )&& groupValid) {
    gGroupCnt := gGroupCnt + groupIncValue.resized
  }



  val gCnt        = UInt(7 bits)
  val oCnt        = UInt(9 bits)
  val bcCnt       = UInt(4 bits)
  val escapeCnt   = UInt(9 bits)
//  val oCntMax     = UInt(9 bits)
  val bcCntMax    = UInt(4 bits)
  val escapeCntMax= UInt(4 bits)
  val decodeFire  = Bool()
  val isGroupLine = Bool()
  val lockDecode  = Bool()

  io.apbreg >> regif.apb





}


object gentextDecodeVerilog {
  def main(args:Array[String]): Unit = {
    SpinalVerilog(new textDecode(8,16))
  }
}
