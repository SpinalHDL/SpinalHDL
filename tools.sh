#!/bin/sh

install_verilator(){
  sudo apt install -y git make autoconf build-essential flex libfl-dev bison help2man # First time prerequisites
  git clone http://git.veripool.org/git/verilator   # Only first time
  unset VERILATOR_ROOT  # For bash
  cd verilator
  git pull        # Make sure we're up-to-date
  git checkout v4.218
  autoconf        # Create ./configure script
  ./configure --prefix ~/tools
  make -j$(nproc)
  make install
  cd ..
}

install_ghdl(){
  sudo apt install -y gnat-9 libgnat-9 zlib1g-dev libboost-dev
  git clone https://github.com/ghdl/ghdl ghdl-build && cd ghdl-build
  git reset --hard "v1.0.0"
  mkdir build
  cd build
  ../configure --prefix=~/tools
  make -j$(nproc)
  make install
  cd ..
}

install_iverilog(){
  cd ..
  sudo apt install -y iverilog
}

install_cocotb(){
  pip3 install --user cocotb
  sudo apt install -y git make gcc g++ swig python3-dev
}

purge_cocotb(){
  # Force cocotb to compile VPI to avoid race condition when tests are start in parallel
  cd tester/src/test/python/spinal/Dummy
  make TOPLEVEL_LANG=verilog
  make TOPLEVEL_LANG=vhdl
  cd ../../../../../..
}

install_packages(){
  sudo apt update -y -qq
  sudo apt install -y gnat-9  libgnat-9 zlib1g-dev libboost-dev
}

install_fpga_toolchain(){
  wget https://github.com/YosysHQ/fpga-toolchain/releases/download/nightly-20211006/fpga-toolchain-linux_x86_64-nightly-20211006.tar.xz
  tar -xf fpga-toolchain-linux_x86_64-nightly-20211006.tar.xz
  ls ~/fpga-toolchain
}

install_tools(){
  install_verilator
  install_ghdl
  install_iverilog
  install_cocotb
}
