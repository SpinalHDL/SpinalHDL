## Features
RISC-V CPU
- Pipelined on 5 stages (Fetch Decode Execute0 Execute1 WriteBack)
- Multiple branch prediction modes : (disable, static or dynamic)
- Data path parameterizable between fully bypassed to fully interlocked

Extension
- One cycle multiplication
- 34 cycle division
- Iterative shifter (N shift -> N cycles)
- Single cycle shifter
- Interruption controller
- Debugging module (with JTAG bridge, openOCD port and GDB)
- Instruction cache with wrapped burst memory interface
- Data cache with instructions to evict/flush the whole cache or a given address

Performance/Area (on cyclone II)
- small core -> 846 LE, 0.6 DMIPS/Mhz
- debug module (without JTAG) -> 240 LE
- JTAG Avalon master -> 238 LE
- big core with MUL/DIV/Full shifter/I$/Interrupt/Debug -> 2200 LE, 1.15 DMIPS/Mhz, at least 100 Mhz (with default synthesis option)


## How to generate the CPU VHDL
There is an example of a top level which generate an Altera QSys component that contain the CPU with Avalon interfaces and some timing buffer :
https://github.com/SpinalHDL/SpinalHDL/blob/master/lib/src/main/scala/spinal/lib/cpu/riscv/impl/CoreQSysAvalon.scala#L97
To get the VHDL from that, the easiest way is to get the https://github.com/SpinalHDL/SpinalBaseProject and call `QSysAvalonCore.main(null)` from your main function.

## How to generate the JTAG to Avalon converter VHDL
Like the way how you can generate the CPU, just call the `JtagAvalonDebuggerMain.main(null)` function for your main. It will generate the QSys component.


## Todo
- Documentation
- Optimise instruction/data caches FMax by moving line hit condition forward into combinatorial paths.

Contact spinalhdl@gmail.com for more information