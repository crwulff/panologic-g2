
package cc

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.simple._
import spinal.lib.com.jtag.Jtag
import spinal.lib.system.debugger.{JtagAxi4SharedDebugger, JtagBridge, SystemDebugger, SystemDebuggerConfig}

import scala.collection.mutable.ArrayBuffer
import vexriscv.plugin.{NONE, _}
import vexriscv.{VexRiscv, VexRiscvConfig, plugin}
import vexriscv.demo._

case class CpuComplexConfig(
                       onChipRamSize      : BigInt,
                       onChipRamHexFile   : String,
                       pipelineDBus       : Boolean,
                       pipelineMainBus    : Boolean,
                       pipelineApbBridge  : Boolean,
                       apb3Config         : Apb3Config,
                       axi4Config         : Axi4Config,
                       cpuPlugins         : ArrayBuffer[Plugin[VexRiscv]]){

  require(pipelineApbBridge || pipelineMainBus, "At least pipelineMainBus or pipelineApbBridge should be enable to avoid wipe transactions")
}

object CpuComplexConfig{

    def default =  CpuComplexConfig(

        // Files to update when changing memory size:
        // - ./spinal/sw/Makefile (MEM_WORDS)
        // - ./spinal/sw/start.S (stack pointer)
        // - ./spinal/ise/progmem.bmm (it's complicated...)
        // - ./spinal/src/main/scala/cc/CpuComplex.scala (onChipRamSize)

        onChipRamSize         = 128 kB,
        onChipRamHexFile      = null,
        pipelineDBus          = true,
        pipelineMainBus       = true,
        pipelineApbBridge     = true,
        cpuPlugins = ArrayBuffer(
            new IBusSimplePlugin(
                resetVector = 0x00000000l,
                cmdForkOnSecondStage = true,
                cmdForkPersistence = false,
                prediction = NONE,
                catchAccessFault = false,
                compressedGen = false
            ),
            new DBusSimplePlugin(
                catchAddressMisaligned = false,
                catchAccessFault = false,
                earlyInjection = false
            ),
            new CsrPlugin(CsrPluginConfig.smallest(mtvecInit = 0x00000020l)),
            new DecoderSimplePlugin(
                catchIllegalInstruction = false
            ),
            new RegFilePlugin(
                regFileReadyKind = plugin.SYNC,
                zeroBoot = false
            ),
            new IntAluPlugin,
            new SrcPlugin(
                separatedAddSub = false,
                executeInsertion = false
            ),
            new LightShifterPlugin,
            new HazardSimplePlugin(
                bypassExecute = false,
                bypassMemory = false,
                bypassWriteBack = false,
                bypassWriteBackBuffer = false,
                pessimisticUseSrc = false,
                pessimisticWriteRegFile = false,
                pessimisticAddressMatch = false
            ),
            new BranchPlugin(
                earlyBranch = false,
                catchAddressMisaligned = false
            ),
            new DebugPlugin(ClockDomain.current.clone(reset = Bool(false))), //Bool().setName("debugReset"))),
            new YamlPlugin("cpu0.yaml")
        ),
        apb3Config = Apb3Config(
            addressWidth = 20,
            dataWidth = 32
        ),
        axi4Config = Axi4Config(
            addressWidth = 32,
            dataWidth = 32,
            idWidth = 4
        )
  )

  def fast = {
    val config = default

    // Replace HazardSimplePlugin to get datapath bypass
    config.cpuPlugins(config.cpuPlugins.indexWhere(_.isInstanceOf[HazardSimplePlugin])) = new HazardSimplePlugin(
      bypassExecute = true,
      bypassMemory = true,
      bypassWriteBack = true,
      bypassWriteBackBuffer = true
    )
//    config.cpuPlugins(config.cpuPlugins.indexWhere(_.isInstanceOf[LightShifterPlugin])) = new FullBarrelShifterPlugin()

    config
  }
}


case class CpuComplex(config : CpuComplexConfig) extends Component
{
    import config._

    val io = new Bundle {
        val apb                     = master(Apb3(config.apb3Config))
        val axiMem1                 = master(Axi4Shared(config.axi4Config))
        val axiMem2                 = master(Axi4Shared(config.axi4Config))
        val externalInterrupt       = in(Bool)
        val timerInterrupt          = in(Bool)
        val jtag                    = slave(Jtag())
    }

    val core = new Area {
        // Instantiate the CPU
        val cpu = new VexRiscv(
            config = VexRiscvConfig(
                plugins = cpuPlugins
            )
        )

        var iBus : Axi4ReadOnly = null
        var dBus : Axi4Shared = null
        for(plugin <- config.cpuPlugins) plugin match{
            case plugin : IBusSimplePlugin => iBus = plugin.iBus.toAxi4ReadOnly()
            case plugin : IBusCachedPlugin => iBus = plugin.iBus.toAxi4ReadOnly()
            case plugin : DBusSimplePlugin => dBus = plugin.dBus.toAxi4Shared()
            case plugin : DBusCachedPlugin => dBus = plugin.dBus.toAxi4Shared(true)
            case plugin : CsrPlugin        => {
                plugin.externalInterrupt    := io.externalInterrupt
                plugin.timerInterrupt       := io.timerInterrupt
            }
            case plugin : DebugPlugin      => {
                //resetCtrl.axiReset setWhen(RegNext(plugin.io.resetOut))
                io.jtag <> plugin.io.bus.fromJtag()
            }
            case _ =>
        }
    }

    //****** MainBus slaves ********
    val mainBusMapping = ArrayBuffer[(PipelinedMemoryBus,SizeMapping)]()

    val ram = Axi4SharedOnChipRamMultiPort(
        portCount = 1,
        dataWidth = 32,
        byteCount = onChipRamSize,
        idWidth = 4
    )
    //val ram = new MuraxPipelinedMemoryBusRam(
    //    onChipRamSize = onChipRamSize,
    //    onChipRamHexFile = onChipRamHexFile,
    //    pipelinedMemoryBusConfig = pipelinedMemoryBusConfig
    //)

    //mainBusMapping += ram.io.bus -> (0x00000000l, onChipRamSize)

    val apbBridge = Axi4SharedToApb3Bridge(
      addressWidth = 20,
      dataWidth    = 32,
      idWidth      = 4
    )

    io.apb <> apbBridge.io.apb

    val axiCrossbar = Axi4CrossbarFactory()

    axiCrossbar.addSlaves(
        ram.io.axis(0)       -> (0x00000000L,   onChipRamSize),
        io.axiMem1       -> (0x40000000L,   64 MB),
        io.axiMem2       -> (0x44000000L,   64 MB),
        apbBridge.io.axi -> (0x80000000L,   1 MB)
    )

    axiCrossbar.addConnections(
        core.iBus       -> List(ram.io.axis(0), io.axiMem1, io.axiMem2),
        core.dBus       -> List(ram.io.axis(0), io.axiMem1, io.axiMem2, apbBridge.io.axi)
    )

    axiCrossbar.addPipelining(apbBridge.io.axi)((crossbar,bridge) => {
      crossbar.sharedCmd.halfPipe() >> bridge.sharedCmd
      crossbar.writeData.halfPipe() >> bridge.writeData
      crossbar.writeRsp             << bridge.writeRsp
      crossbar.readRsp              << bridge.readRsp
    })

    axiCrossbar.addPipelining(io.axiMem1)((crossbar,ctrl) => {
      crossbar.sharedCmd.halfPipe()  >>  ctrl.sharedCmd
      crossbar.writeData            >/-> ctrl.writeData
      crossbar.writeRsp              <<  ctrl.writeRsp
      crossbar.readRsp               <<  ctrl.readRsp
    })

    axiCrossbar.addPipelining(io.axiMem2)((crossbar,ctrl) => {
      crossbar.sharedCmd.halfPipe()  >>  ctrl.sharedCmd
      crossbar.writeData            >/-> ctrl.writeData
      crossbar.writeRsp              <<  ctrl.writeRsp
      crossbar.readRsp               <<  ctrl.readRsp
    })

    axiCrossbar.addPipelining(ram.io.axis(0))((crossbar,ctrl) => {
      crossbar.sharedCmd.halfPipe()  >>  ctrl.sharedCmd
      crossbar.writeData            >/-> ctrl.writeData
      crossbar.writeRsp              <<  ctrl.writeRsp
      crossbar.readRsp               <<  ctrl.readRsp
    })
    axiCrossbar.addPipelining(core.dBus)((cpu,crossbar) => {
      cpu.sharedCmd             >>  crossbar.sharedCmd
      cpu.writeData             >>  crossbar.writeData
      cpu.writeRsp              <<  crossbar.writeRsp
      cpu.readRsp               <-< crossbar.readRsp //Data cache directly use read responses without buffering, so pipeline it for FMax
    })

    axiCrossbar.build()
}

