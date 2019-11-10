
package spartan6

import spinal.core._

import spinal.lib._

import spartan6._


class ICONCore(nSlaves : Integer) extends Component {

    val io = new Bundle {
        val drck    = in(Bool)                         // Scan Clock
        val tdi     = out(Bool)                        // Data from the host
        val tdo     = in(Vec(Bool, nSlaves))           // Data to the host
        val cmd_sel = out(Vec(Bits(32 bits), nSlaves)) // One-hot command select

        val BSCAN_SEL    = in(Bool)  // Scan mode Select instruction
        val BSCAN_SHIFT  = in(Bool)  // Scan Chain Shift instruction
        val BSCAN_TDI    = in(Bool)  // Scan Chain Output. Mirror of the TDI input to the FPGA
        val BSCAN_UPDATE = in(Bool)  // Scan Register Update instruction
        val BSCAN_TDO    = out(Bool) // Scan Chain INput
    }

    val updateDomain = ClockDomain(
        clock       = io.BSCAN_UPDATE,
        reset       = io.BSCAN_SEL,
        frequency   = FixedFrequency(10 MHz), // Actually much less as there are several states in between updates (Platform Cable USB II max is 6 MHz)
        config      = ClockDomainConfig(resetKind = ASYNC, resetActiveLevel = LOW)
    )

    val drckDomain = ClockDomain(
        clock       = io.drck,
        frequency   = FixedFrequency(10 MHz), // Probably less (Platform Cable USB II max is 6 MHz)
        config      = ClockDomainConfig(resetKind = BOOT)
    )

    // Update only goes high when drck is not clocking so we have to use it as a clock
    val update_area = new ClockingArea(updateDomain) {
        // Command/Data state
        val command = Reg(Bool) init(True) addTag(crossClockDomain)

        command := !command
    }

    val drck_area = new ClockingArea(drckDomain) {

        val tdiReg = Reg(Bool)
        tdiReg := io.BSCAN_TDI
        io.tdi := tdiReg

        val tdoReg = Reg(Bool)
        io.BSCAN_TDO := tdoReg

        // Sync search
        val syncShift = Reg(Bits(7 bits))
        val SYNC      = Reg(Bool)
        when (update_area.command) {
          syncShift := 0
          SYNC      := False
        }.otherwise {
            syncShift := tdiReg ## syncShift(6 downto 1)
            when (tdiReg ## syncShift === B"8'xA9'") {
                SYNC := True
            }
        }

        // Command
        val cmdShift = Reg(Bits(16 bits))
        when (!io.BSCAN_SEL) {
            cmdShift := 0
        }.elsewhen (update_area.command && io.BSCAN_SHIFT) {
            cmdShift := tdiReg ## cmdShift(15 downto 1)
        }
        val core = UInt(4 bits)
        val command = UInt(4 bits)
        val group = UInt(2 bits)
        core    := cmdShift(15 downto 12).asUInt
        command := cmdShift(11 downto 8).asUInt
        group   := cmdShift(7 downto 6).asUInt

        // Command Out
        for (slave <- 0 to nSlaves - 1) {
           for (cmd <- 0 to 31) {
               when (SYNC && io.BSCAN_SHIFT && slave === core && cmd === (command + 16 * group)) {
                   io.cmd_sel(slave)(cmd) := True
               }.otherwise{
                   io.cmd_sel(slave)(cmd) := False
               }
           }
        }

        // Status
        val MANUFACTURER = 1 // Xilinx
        val CORE_TYPE = 1    // ICON
        val MAJOR = 14
        val MINOR = 7
        val BUILD = 0
        val CORE_MAJOR = 1
        val CORE_MINOR = 2
        val CORE_ALPHA = 1
        val STATUS_VEC = B(nSlaves, 4 bits) ## B(CORE_ALPHA, 8 bits) ## B(CORE_MINOR, 8 bits) ## B(CORE_MAJOR, 4 bits) ## B(BUILD, 8 bits) ## B(MAJOR, 4 bits) ## B(MINOR, 4 bits) ## B(CORE_TYPE, 8 bits) ## B(MANUFACTURER, 8 bits) ## B("8'x01")

        val statData = STATUS_VEC.asBools
        val statCtr = Counter(0, 63)
        when(SYNC && io.BSCAN_SHIFT && core === 15 && command === 0 && group === 0){
            statCtr.increment()
        }.otherwise{
            statCtr.clear()
        }

        // TDO Mux
        val tdoVec = Bits(16 bits)

        for (slave <- 0 to 14) {
            if (slave < nSlaves) {
                tdoVec(slave) := io.tdo(slave)
            } else {
                tdoVec(slave) := True
            }
        }

        val statReg = RegNext(statData(statCtr))
        tdoVec(15) := statReg

        tdoReg := tdoVec(core)
    }
}

class ICON(nSlaves : Integer, jtagChain : Integer) extends Component {
    val io = new Bundle {
        val drck    = out(Bool)                        // Scan Clock
        val tdi     = out(Bool)                        // Data from the host
        val tdo     = in(Vec(Bool, nSlaves))           // Data to the host
        val cmd_sel = out(Vec(Bits(32 bits), nSlaves)) // One-hot command select
    }

    val bscan = new BSCAN_SPARTAN6(jtagChain = jtagChain)
    val icon = new ICONCore(nSlaves = nSlaves)

    // Clock buffer for the scan chain clock
    val drck_bufg = new BUFG()
    drck_bufg.io.I := bscan.io.DRCK
    io.drck        := drck_bufg.io.O

    icon.io.BSCAN_SEL     := bscan.io.SEL
    icon.io.BSCAN_SHIFT   := bscan.io.SHIFT
    icon.io.BSCAN_TDI     := bscan.io.TDI
    icon.io.BSCAN_UPDATE  := bscan.io.UPDATE
    bscan.io.TDO          := icon.io.BSCAN_TDO

    icon.io.drck := drck_bufg.io.O
    io.tdi  := icon.io.tdi
    icon.io.tdo := io.tdo
    io.cmd_sel := icon.io.cmd_sel
}


object ICONTests {

    import spinal.sim._
    import spinal.core.sim._

    def main(args: Array[String]): Unit = {
        SimConfig.withWave.compile(new ICONCore(nSlaves = 1)).doSim{ dut =>
            SimTimeout(10000)

            dut.io.BSCAN_SEL    #= false
            dut.io.BSCAN_SHIFT  #= false
            dut.io.BSCAN_TDI    #= false
            dut.io.BSCAN_UPDATE #= false

            dut.io.tdo(0) #= false

            val clk = ClockDomain(dut.io.drck)
            clk.forkStimulus(period = 10)

            clk.waitRisingEdge(5)

            val cmd = 0x0F000
            for (bit <- 0 to 16) {
                dut.io.BSCAN_SEL   #= true
                dut.io.BSCAN_SHIFT #= true
                dut.io.BSCAN_TDI   #= ((cmd >> bit) & 1) != 0
                clk.waitRisingEdge(1)

                if (bit == 16) {
                    dut.io.BSCAN_UPDATE #= true
                    sleep(1)
                    dut.io.BSCAN_UPDATE #= false
                }
            }

            dut.io.BSCAN_SHIFT  #= false
            clk.waitRisingEdge(1)

            val data = 0x000000000000000000A9
            for (bit <- 0 to 74) {
                dut.io.BSCAN_SEL   #= true
                dut.io.BSCAN_SHIFT #= true
                dut.io.BSCAN_TDI   #= ((data >> bit) & 1) != 0
                clk.waitRisingEdge(1)
                if (bit == 74) {
                    dut.io.BSCAN_UPDATE #= true
                    sleep(1)
                    dut.io.BSCAN_UPDATE #= false
                }
            }

            dut.io.BSCAN_SHIFT  #= false
            clk.waitRisingEdge(1)
            sleep(1000)
        }
    }
}
