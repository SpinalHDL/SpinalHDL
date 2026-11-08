// defines_ddr3.v
//
// Global preprocessor define for the Micron ddr3.v compile. Placed FIRST in
// VERILOG_SOURCES so the `ifdef at the top of ddr3.v sees it. Kept out of
// EXTRA_ARGS because cocotb 1.8.1 forwards EXTRA_ARGS to vvp as well, and we
// want the define to apply to the iverilog preprocessor only.
`define den2048Mb
