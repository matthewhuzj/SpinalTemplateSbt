package textDecode

import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config, Apb3SlaveFactory}
import spinal.lib.bus.regif.Apb3BusInterface
import spinal.core._
import spinal.lib.slave

class apb3ram(apb3config:Apb3Config,memWidth:Int,memDepth:Int) extends Component{
  val io = new Bundle {
    val readByTD = in Bool()
    val rden     = in Bool()
    val readAddr = in UInt(log2Up(memDepth) bits)
    val readData = out Bits(memWidth bits)
  }
  val apb3 = slave(Apb3(apb3config))
  val apb3bus = Apb3SlaveFactory(apb3)
  val mem = Mem(Bits(memWidth bits),memDepth)
  val addr = Mux(io.readByTD,io.readAddr,apb3.PADDR.resized)
  val rden = Mux(io.readByTD,io.rden,apb3bus.doRead)
  val data = Bits(memWidth bits)


  mem.write(apb3.PADDR.resized,apb3.PWDATA.asBits.resize(memWidth),apb3bus.doWrite)
  apb3.PRDATA.allowOverride
  data := mem.readSync(addr,rden)
  io.readData := data
  apb3.PRDATA := data.resized

}

object genVerilogApb3ram{
  def main(args:Array[String]) {
    SpinalVerilog(new apb3ram(Apb3Config(10,32),16,256))
  }
}
