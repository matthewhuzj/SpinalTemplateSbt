package huffman

import spinal.core.Component
import spinal.core._
import spinal.lib._

class HuffmanDecode(bitWidth:Int,codeLen:Int) extends Component{

//  val generic = new Generic {
//    val bitWidth  = HuffmanDecode.this.bitWidth
//    val codeLen   = HuffmanDecode.this.codeLen
//  }
  val bitOfCodeLen:Int = log2Up(codeLen)
  val io = new Bundle {
    val code        = in UInt(codeLen bits)
    val code_valid  = in Bool
    val code_enc    = in Vec(UInt(codeLen bits),codeLen)
    val code_index  = in Vec(UInt(bitWidth bits),codeLen)
    val length_o    = out UInt(bitOfCodeLen bits)
    val index_o     = out UInt(bitWidth bits)
    val valid_o     = out Bool
  }
  val cmp = UInt(codeLen bits)
  val get_index = UInt(codeLen bits)
//  val get_length_r = Reg(UInt(16 bits)) init(0)
  val index_r    = Reg(UInt(bitWidth bits)) init(0)
  val valid_r    = Reg(Bool) init(false)
  val length_r   = Reg(UInt(bitOfCodeLen bits)) init(0)
  val oneHotEnc  = UInt(bitOfCodeLen bits)
  val sub_index  = UInt(codeLen bits)
  val shift_len  = UInt(bitOfCodeLen bits)
  when(io.code_valid){
    for(i <- 0 to codeLen-1) {
      if(i == codeLen -1) {
        cmp(i) := io.code >= io.code_enc(i)
      } else {
        cmp(i) := io.code >= io.code_enc(i) && io.code < io.code_enc(i+1)
      }
    }
  } otherwise {
      cmp := U(0,codeLen bits)
  }

  get_index := io.code_enc(oneHotEnc)
  oneHotEnc := OHToUInt(cmp)
  shift_len := U(codeLen-1,bitOfCodeLen bits) - oneHotEnc
  sub_index := (io.code>>shift_len) - (get_index>>shift_len)

  when(io.code_valid) {
    length_r := oneHotEnc
    index_r := sub_index.resize(bitWidth)+ io.code_index(oneHotEnc)
    valid_r := io.code_valid
  }.otherwise {
    valid_r := Bool(false)
  }

  io.valid_o  := valid_r
  io.length_o := length_r
  io.index_o  := index_r

}
object HuffmanDecodeVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new HuffmanDecode(8,16))
  }
}