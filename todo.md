## BUG
- [X] Fix B(default -> True)

## spinal.core
- [ ] Ram with only 1 line are emitted with zero width removed address
- [X] Mem API BigInt size missing
- [X] Add more double comparaison into fixedpoints
- [X] Fixedpoints, check construction arguments
- [X] Bits reg with init to big error message
- [X] Need better error when inside a when, something to big is assigned
- [x] Add regression for all implicit spinal hdl check like latch, comb loop etc
- [X] add xxx := (default -> True) support (no B/U/S prefix)
- [X] Add time/frequency DSL
- [X] Do a improvment run on Mem (writeFirst, black box, vendor/family support, ...)
- [X] MemBlackboxer currently manipulate Mem graph before with inferation (OK), but before with check (KO)
- [X] Add version number into elaboration logs
- [X] Function for compinent without the io_ prefix on IO
- [X] Add option to disable the pkg generation in VHDL
- [X] naming rules transformator in SpinalConfig
- [ ] Check xilinx and altera FF about synchronus reset + clock enable interaction kind
- [X] Add rotate left support in the verilog backend
- [ ] Better mux Nodes (more than two inputs)
- [ ] Remove the generation of when nodes condition when they are emited as a case statments
- [X] Rework Namable trait, to allow it to be named by composition + postfix, and maybe to work with Ownable trait for hearchical naming
- [X] bool/bits/uint/sint should extends dataimper to avoid implicites and allow implicite area to uint
- [ ] Range generator 
- [ ] add SlowArea
- [X] Add git commit number in logs
- [X] Fix name allocation scopes
- [X] Rework Hz and times DSL with strongly typed classes
- [X] Catch null pointer exception and document it
- [X] start in FSM
- [ ] Usage of binary array for Rom initialization instead of BaseType + literals
- [ ] Emit HTML from BusSlaveFactory

## spinal.lib

- [X] Check queue minimal latency of queue for AXI4WriteArbiter (probably 2, which is bad)
- [ ] Finalise and document many Stream utils
- [ ] More spinal.lib basic utils (priority encoder, ..)
- [ ] spinal.lib.com.spi
- [ ] spinal.lib.com.serial
- [ ] spinal.lib.com.8b/10b
- [ ] spinal.lib.misc.pwm
- [ ] spinal.lib.misc.pdm
- [ ] spinal.lib.misc.dds

## spinal.tester

- [X] spinal.lib.StreamFork
- [ ] spinal.lib.StreamDemux
- [ ] spinal.lib.StreamCCByToggle
- [X] spinal.lib.StreamDispatcherInOrder
- [X] spinal.lib.StreamFlowArbiter
- [X] spinal.lib.StreamArbiter
- [ ] spinal.lib.StreamMux
- [ ] spinal.lib.EventEmitter
- [X] spinal.lib.StreamJoin
- [ ] spinal.lib.FlowCCByToggle

## http://spinalhdl.github.io/SpinalDoc/
- [ ] Document @dontName + val vs def for rom init pruned stuff
- [ ] component.rework
- [X] MemBlackboxers
- [ ] Custom phases
- [ ] spinal.lib.StreamFork
- [ ] wait verification spinal.lib.StreamDemux
- [ ] wait verification spinal.lib.StreamCCByToggle
- [ ] spinal.lib.StreamDispatcherInOrder
- [ ] spinal.lib.StreamFlowArbiter
- [ ] spinal.lib.StreamArbiter
- [ ] wait verification spinal.lib.StreamMux
- [ ] wait verification spinal.lib.EventEmitter
- [ ] spinal.lib.StreamJoin
- [ ] spinal.lib.RegFlow
- [ ] wait verification spinal.lib.FlowCCByToggle
