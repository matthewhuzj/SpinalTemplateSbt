package test

import spinal.core._
import spinal.lib._
import spinal.lib.bus.regif._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.regif.AccessType._

class myRegIf extends Component{
  val slot_end, fram_end, dec_end = in Bool()
  val interrupt = out Bool()
  val ahb = slave(Apb3(Apb3Config(16,32)))

  val busif = BusInterface(ahb,(0x0200,100 Byte),0)

  val M_CRM = busif.newReg("clock reset")
  val M_TRIGGER = busif.newReg("start triggers")
  val M_TURBO_EARLY_QUIT    = busif.newReg(doc = "Turbo Hardware-mode register1")
  val early_quit  = M_TURBO_EARLY_QUIT.field(1 bit, RW, 0, doc = "CRC validate early quit enable").asOutput()
  val early_count = M_TURBO_EARLY_QUIT.field(2 bit, RW, 2, doc = "CRC validate early quit enable").asOutput()

  interrupt := InterruptFactory(busif,"M_NB",slot_end,fram_end,dec_end)
}

object genVerilog extends App{
  SpinalConfig(mode= Verilog,
    targetDirectory = "./"
  ).generate(new myRegIf)
}