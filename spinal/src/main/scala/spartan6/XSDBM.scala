
package spartan6

import spinal.core._

import spinal.lib._

import spartan6._

class XSDBMStatusReg(mclkDomain : ClockDomain, width : Int) extends Component {
    val io = new Bundle {
        // ICON connections
        val tdi     = in(Bool)  // Data from the host
        val tdo     = out(Bool) // Data to the host
        val cmd_sel = in(Bool)  // Command select

        // BUS connections
        val regData = in(Bits(width bits))
    }

    val command = RegNext(io.cmd_sel)

    val mclkArea = new ClockingArea(mclkDomain) {
        val statMclk = Reg(Bits())

        // Hold data while the command is active
        val commandActive = BufferCC(command)
        when (!commandActive) {
            statMclk := io.regData
        }
    }

    val statDrck = BufferCC(mclkArea.statMclk)

    val shift = RegNext(command)
    val shiftData = Reg(Bits(width bits))

    when(command && !shift) {
        shiftData := io.tdi ## statDrck(width - 1 downto 1)
    }.otherwise{
        shiftData := io.tdi ## shiftData(width - 1 downto 1)
    }

    when(!shift) {
        io.tdo := statDrck(0)
    }.otherwise {
        io.tdo := shiftData(0)
    }
}

class XSDBMControlReg(mclkDomain : ClockDomain, width : Int) extends Component {
    val io = new Bundle {
        // ICON connections
        val tdi         = in(Bool)  // Data from the host
        val tdo         = out(Bool) // Data to the host
        val cmd_sel     = in(Bool)  // Command select
        val regDataIcon = out(Bits(width bits)) // Register data in the current clock domain

        // BUS connections (mclk domain)
        val regData = out(Bits(width bits))
        val regReady = out(Bool)
    }

    val command = RegNext(io.cmd_sel)

    val shiftData = Reg(Bits(width bits))

    when(command) {
        shiftData := io.tdi ## shiftData(width - 1 downto 1)
    }

    io.tdo := shiftData(0)
    io.regDataIcon := shiftData

    val mclkArea = new ClockingArea(mclkDomain) {
        val regDataOut = Reg(Bits(width bits)) addTag(crossClockDomain)

        // Hold data while the command is active
        val commandActive = BufferCC(command)
        when (!commandActive) {
            regDataOut := shiftData
        }

        io.regData := regDataOut
        io.regReady := !commandActive
    }
}

class XSDBMReadFifo(mclkDomain : ClockDomain) extends Component {
    val io = new Bundle {
        // ICON connections
        val tdi         = in(Bool)  // Data from the host
        val tdo         = out(Bool) // Data to the host
        val cmd_sel     = in(Bool)  // Command select
    }

    io.tdo := io.tdi // TODO
}

class XSDBMWriteFifo(mclkDomain : ClockDomain) extends Component {
    val io = new Bundle {
        // ICON connections
        val tdi         = in(Bool)  // Data from the host
        val tdo         = out(Bool) // Data to the host
        val cmd_sel     = in(Bool)  // Command select
    }

    io.tdo := io.tdi // TODO
}

class XSDBMaster(nSlaves : Int) extends Component {

    val io = new Bundle {
        // ICON connections
        val drck    = in(Bool)          // Scan Clock
        val tdi     = in(Bool)          // Data from the host
        val tdo     = out(Bool)         // Data to the host
        val cmd_sel = in(Bits(32 bits)) // One-hot command select

        // XSDB slave connections
    }

    val mclkDomain = ClockDomain.current

    val drckDomain = ClockDomain(
        clock       = io.drck,
        frequency   = FixedFrequency(10 MHz), // Probably less (Platform Cable USB II max is 6 MHz)
        config      = ClockDomainConfig(resetKind = BOOT)
    )

    val drck_area = new ClockingArea(drckDomain) {
        val tdoVec = Bits(8 bits)
        val command = RegNext(io.cmd_sel)

        // Static Status
        new Area {
            val MANUFACTURER = 1 // Xilinx
            val CORE_TYPE = 14   // XSDB Master
            val MAJOR = 14
            val MINOR = 7
            val BUILD = 0
            val CORE_MAJOR = 1
            val CORE_MINOR = 2
            val CORE_ALPHA = 1
            val PERIOD_INT = 10
            val PERIOD_FRAC = 0
            val DCLK_HAS_RESET = 0
            val STATUS_VEC = B(0, 39 bits) ## B(DCLK_HAS_RESET, 1 bit) ## B(PERIOD_FRAC, 10 bits) ##
                             B(PERIOD_INT, 10 bits) ## B(nSlaves, 8 bits) ## B(CORE_ALPHA, 8 bits) ##
                             B(CORE_MINOR, 8 bits) ## B(CORE_MAJOR, 4 bits) ## B(BUILD, 8 bits) ##
                             B(MAJOR, 4 bits) ## B(MINOR, 4 bits) ## B(CORE_TYPE, 8 bits) ##
                             B(MANUFACTURER, 8 bits) ## B("8'x01")

            val statData = STATUS_VEC.asBools
            val statCtr = Counter(0, 127)
            when(command(0)){
                statCtr.increment()
            }.otherwise{
                statCtr.clear()
            }

            tdoVec(0) := statData(statCtr)
        }

        //
        // Bus Reset
        //
        val busResetCR = new XSDBMControlReg(mclkDomain, nSlaves + 1)
        busResetCR.io.tdi := io.tdi
        busResetCR.io.cmd_sel := io.cmd_sel(1)
        tdoVec(1) := busResetCR.io.tdo

        val masterReset = Bool
        val slaveReset = Bits()
        masterReset := busResetCR.io.regData(0)
        slaveReset  := busResetCR.io.regData(nSlaves downto 1)

        //
        // Master Status
        //
        val masterStatusSR = new XSDBMStatusReg(mclkDomain, 4)
        masterStatusSR.io.regData := False ## False ## True ## True // WR, RD, IDLE, NORMAL
        masterStatusSR.io.tdi := io.tdi
        masterStatusSR.io.cmd_sel := io.cmd_sel(2)
        tdoVec(2) := masterStatusSR.io.tdo

        //
        // Bus Error Status
        //
        val errorStatusSR = new XSDBMStatusReg(mclkDomain, nSlaves + 2)
        errorStatusSR.io.regData := (default -> False)
        errorStatusSR.io.tdi := io.tdi
        errorStatusSR.io.cmd_sel := io.cmd_sel(3)
        tdoVec(3) := errorStatusSR.io.tdo

        //
        // Bus Access Setup
        //
        val busAccessSetupCR = new XSDBMControlReg(mclkDomain, 28)
        busAccessSetupCR.io.tdi := io.tdi
        busAccessSetupCR.io.cmd_sel := io.cmd_sel(4)
        tdoVec(4) := busAccessSetupCR.io.tdo

        //
        // Address
        //
        val addressCR = new XSDBMControlReg(mclkDomain, 18)
        addressCR.io.tdi := io.tdi
        addressCR.io.cmd_sel := io.cmd_sel(5)
        tdoVec(5) := addressCR.io.tdo

        //
        // Data
        //
        val readFIFO = new XSDBMReadFifo(mclkDomain)
        val writeFIFO = new XSDBMWriteFifo(mclkDomain)

        readFIFO.io.tdi := io.tdi
        readFIFO.io.cmd_sel := io.cmd_sel(6)
        tdoVec(6) := readFIFO.io.tdo

        writeFIFO.io.tdi := io.tdi
        writeFIFO.io.cmd_sel := io.cmd_sel(6)

        //
        // Status
        //
        val statusSR = new XSDBMStatusReg(mclkDomain, 3)
        val statusCR = new XSDBMControlReg(mclkDomain, 3)
        statusSR.io.regData := False ## True ## False // DCLK_STOPPED, DCLK_LOCKED, DCLK_RESET
        statusSR.io.tdi := io.tdi
        statusSR.io.cmd_sel := io.cmd_sel(7)
        tdoVec(7) := statusSR.io.tdo

        statusCR.io.tdi := io.tdi
        statusCR.io.cmd_sel := io.cmd_sel(7)

        // active command drives its data bits out
        io.tdo := (command(7 downto 0) & tdoVec).orR
    }
}

