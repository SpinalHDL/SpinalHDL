RISC-V CPU
- Pipelined on 5 stages (Fetch Decode Execute0 Execute1 WriteBack)
- Multiple branch prediction modes : (disable, static or dynamic)
- Data path parameterizable between fully bypassed to fully interlocked

Extension :
- One cycle multiplication
- 34 cycle division
- Iterative shifter (N shift -> N cycles)
- Single cycle shifter
- Interruption controller
- Debugging module (with JTAG bridge, openOCD port and GDB)

External modules
- instruction cache with wrapped burst memory interface

Performance/Area (on cyclone II)
- small core with debug -> 1050 LE, 0.6 DMIPS/Mhz
- big core with everythings -> 2200 LE, 1.15 DMIPS/Mhz, 100 Mhz

Contact spinalhdl@gmail.com for more information