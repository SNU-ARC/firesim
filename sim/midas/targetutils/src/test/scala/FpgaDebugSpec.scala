// See LICENSE for license details.

package midas.targetutils

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

import chisel3._
import chisel3._

class FpgaDebugSpec extends AnyFlatSpec with ElaborationUtils {
  def annotator(t: Bool) = FpgaDebug(t)
  def ioGen = Input(Bool())

  behavior of "FPGADebug"
  checkBehaviorOnUnboundTargets(ioGen, annotator)
}

class RAMStyleHintSpec extends AnyFlatSpec with ElaborationUtils {
  import midas.targetutils.xdc._
  class MemoryModuleIO(addrBits: Int, dataBits: Int) extends Bundle {
    val readAddress  = Input(UInt(addrBits.W))
    val readEnable   = Input(Bool())
    val readData     = Output(UInt(dataBits.W))

    val writeAddress = Input(UInt(addrBits.W))
    val writeData    = Input(UInt(dataBits.W))
    val writeEnable  = Input(Bool())
  }

  abstract class BaseMemoryModule(annotator: MemBase[_] => Unit) extends Module {
    // Arbitrarily selected.
    val dataBits = 64
    val addrBits = 16
    val io = IO(new MemoryModuleIO(addrBits, dataBits))

    def mem: MemBase[_]
    annotator(mem)
  }


  class SyncReadMemModule(annotator: MemBase[_] => Unit) extends BaseMemoryModule(annotator) {
    // Lazy so this is elaborated  before annotator executes
    lazy val mem = SyncReadMem(1 << addrBits, UInt(dataBits.W))
    when(io.readEnable)  { io.readData          := mem(io.readAddress) }
    when(io.writeEnable) { mem(io.writeAddress) := io.writeData }
  }

  // This uses a combinational-read chisel memory, but could be infered as a BRAM
  // due to the pipelining of the data read out. So the check annotator still accepts these.
  class CombReadMemModule(annotator: MemBase[_] => Unit) extends BaseMemoryModule(annotator) {
    lazy val mem = Mem(1 << addrBits, UInt(dataBits.W))
    when(io.readEnable)  { io.readData          := RegNext(mem(io.readAddress)) }
    when(io.writeEnable) { mem(io.writeAddress) := io.writeData }
  }

  class WrapperModule(annotator: MemBase[_] => Unit) extends BaseMemoryModule(annotator) {
    lazy val memModule = Module(new SyncReadMemModule(_ => Nil))
    lazy val mem = memModule.mem
    memModule.io <> io
  }

  def annotateMem(style: RAMStyle)(mem: MemBase[_]): Unit = RAMStyleHint(mem, style)

  def elaborateAndCollectXDCAnnos(mod: =>Module) =
    elaborate(mod)._2.collect { case a: XDCAnnotation => a }

  behavior of "RAMStyleHint"

  it should "correctly annotate a chisel3.SyncReadMem as BRAM" in {
    val annos = elaborateAndCollectXDCAnnos(new SyncReadMemModule(annotateMem(RAMStyles.BRAM)))
    assert(annos.size == 1)
  }

  it should "correctly annotate a chisel3.SyncReadMem as URAM" in {
    val annos = elaborateAndCollectXDCAnnos(new SyncReadMemModule(annotateMem(RAMStyles.ULTRA)))
    assert(annos.size == 1)
  }

  it should "correctly annotate a chisel3.Mem as URAM" in {
    val annos = elaborateAndCollectXDCAnnos(new CombReadMemModule(annotateMem(RAMStyles.ULTRA)))
    assert(annos.size == 1)
  }

  it should "reject instance paths" in {
    val annos = elaborateAndCollectXDCAnnos(new WrapperModule(annotateMem(RAMStyles.ULTRA)))
    annos foreach println
  }

}


