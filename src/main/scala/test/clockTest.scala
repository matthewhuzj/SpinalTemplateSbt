package test

import spinal.core._
import spinal.lib._


//class Ram_dual_port(wordWidth:Int,wordCount:Int) extends Component {
//  val generic = new Generic {
//    val wordCount = Ram_dual_port.this.wordCount
//    val wordWidth = Ram_dual_port.this.wordWidth
//  }
//  val io = new Bundle {
//    val wr = new Bundle {
//      val clkb = in Bool()
//      val en   = in Bool()
//      val addr = in UInt(log2Up(wordCount) bits)
//      val data = in Bits(wordWidth bits)
//    }
//    val rd = new Bundle {
//      val clka = in Bool()
//      val en   = in Bool()
//      val addr = in UInt(log2Up(wordCount) bits)
//      val data = out Bits(wordWidth bits)
//    }
//  }
//
//  val wrdomain = ClockDomain(io.wr.clkb)
//  val rddomain = ClockDomain(io.rd.clka)
//  val wrarea = new ClockingArea(wrdomain) {
//    val mems = Reg(Vec(Bits(wordWidth bits),wordCount))
//    when(io.wr.en){
//      mems(io.wr.addr) := io.wr.data
//    }
//  }
//  val rdarea = new ClockingArea(rddomain){
//    val rddata = Reg(Bits(wordWidth bits))
//    when(io.rd.en){
//      rddata := BufferCC(wrarea.mems(io.rd.addr),false)
//    }
//    io.rd.data := rddata
//  }
//}
class Ram_1w_1r(wordWidth: Int, wordCount: Int) extends BlackBox {

  // SpinalHDL will look at Generic classes to get attributes which
  // should be used as VHDL generics / Verilog parameters
  // You can use String, Int, Double, Boolean, and all SpinalHDL base
  // types as generic values
  val generic = new Generic {
    val wordCount = Ram_1w_1r.this.wordCount
    val wordWidth = Ram_1w_1r.this.wordWidth
  }

  // Define IO of the VHDL entity / Verilog module
  val io = new Bundle {
    val wr = new Bundle {
      val clk = in Bool
      val en   = in Bool
      val addr = in UInt (log2Up(wordCount) bit)
      val data = in Bits (wordWidth bit)
    }
    val rd = new Bundle {
      val clk = in Bool
      val en   = in Bool
      val addr = in UInt (log2Up(wordCount) bit)
      val data = out Bits (wordWidth bit)
    }
  }

  // Map the current clock domain to the io.clk pin
//  mapClockDomain(clockDomain ,io.wr.clk)
  // Remove io_ prefix
  noIoPrefix()
  addRTLPath("./ram_dp.v")
  // Function used to rename all signals of the blackbox
//  private def renameIO(): Unit = {
//    io.flatten.foreach(bt => {
//      if(bt.getName().contains("wr_")) bt.setName(bt.getName().repalce("wr_", "") + "_b")
//      if(bt.getName().contains("rd_")) bt.setName(bt.getName().repalce("rd_", "") + "_a")
//    })
//  }
}
// Create the top level and instantiate the Ram
class TopLevel extends Component {
  val io = new Bundle {
    val streamin = slave Stream(UInt(8 bits))
    val streamout = master Stream(UInt(8 bits))
//    val wr = new Bundle {
//      val clk = in Bool()
//      val en   = in Bool
//      val addr = in UInt (log2Up(16) bit)
//      val data = in Bits (8 bit)
//    }
//    val rd = new Bundle {
//      val clk = in Bool()
//      val en   = in Bool
//      val addr = in UInt (log2Up(16) bit)
//      val data = out Bits (8 bit)
//    }
  }
  val streamTest0  = Stream(UInt(8 bits))
  val streamTest1  = Stream(UInt(8 bits))
  val streamTest2  = Stream(UInt(8 bits))
  val streamTest3  = Stream(UInt(8 bits))
  val streamTest4  = Stream(UInt(8 bits))
  val streamTest5  = Stream(UInt(8 bits))
  val streamTest6  = Stream(UInt(8 bits))
  val streamTest7  = Stream(UInt(8 bits))

  streamTest0 <<  io.streamin
  streamTest1 <-< streamTest0
  streamTest2 </< streamTest1
  streamTest3 <-/< streamTest2
  streamTest4 << streamTest3.halfPipe()
  streamTest5 << streamTest4.m2sPipe()
  streamTest6 << streamTest5.s2mPipe()
  streamTest7 << streamTest6.queue(2)
  io.streamout << streamTest7


//  when(sel0 === U(1)){
//    streamTest << io.streamin
//  }.elsewhen(sel0 === U(1)){
//    streamTest << io.streamin.m2sPipe()
//  }.elsewhen(sel0 === U(2)){
//    streamTest << io.streamin.queue(2)
//  }.elsewhen(sel0 === U(3)){
//    streamTest << io.streamin.halfPipe()
//  }.otherwise {
//    streamTest << io.streamin.s2mPipe()
//  }
//io.streamout << io.streamin.halfPipe()
//  // Instantiate the blackbox
//  val ram = new Ram_1w_1r(8,16)
//
//  // Connect all the signals
//  io.wr.clk  <> ram.io.wr.clk
//  io.wr.en   <> ram.io.wr.en
//  io.wr.addr <> ram.io.wr.addr
//  io.wr.data <> ram.io.wr.data
//  io.rd.clk  <> ram.io.rd.clk
//  io.rd.en   <> ram.io.rd.en
//  io.rd.addr <> ram.io.rd.addr
//  io.rd.data <> ram.io.rd.data
}

object Main {
  def main(args: Array[String]): Unit = {
    val report =SpinalVerilog(new TopLevel)
    report.mergeRTLSource("mergeRTL")
  }
}
