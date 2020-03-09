#!/bin/sh

set -e

#---

enable_color() {
  ENABLECOLOR='-c '
  ANSI_RED="\033[31m"
  ANSI_GREEN="\033[32m"
  ANSI_YELLOW="\033[33m"
  ANSI_BLUE="\033[34m"
  ANSI_MAGENTA="\033[35m"
  ANSI_CYAN="\033[36;1m"
  ANSI_DARKCYAN="\033[36m"
  ANSI_NOCOLOR="\033[0m"
}

disable_color() { unset ENABLECOLOR ANSI_RED ANSI_GREEN ANSI_YELLOW ANSI_BLUE ANSI_MAGENTA ANSI_CYAN ANSI_DARKCYAN ANSI_NOCOLOR; }

enable_color

#---

travis_start () {
  :
}
travis_finish () {
  :
}

[ -n "$TRAVIS" ] && {
  # This is a trimmed down copy of
  # https://github.com/travis-ci/travis-build/blob/master/lib/travis/build/bash/*
  travis_time_start() {
    # `date +%N` returns the date in nanoseconds. It is used as a replacement for RANDOM, which is only available in bash.
    travis_timer_id=`date +%N`
    travis_start_time=$(travis_nanoseconds)
    echo "travis_time:start:$travis_timer_id"
  }
  travis_time_finish() {
    travis_end_time=$(travis_nanoseconds)
    local duration=$(($travis_end_time-$travis_start_time))
    echo "travis_time:end:$travis_timer_id:start=$travis_start_time,finish=$travis_end_time,duration=$duration"
  }

  if [ "$TRAVIS_OS_NAME" = "osx" ]; then
    travis_nanoseconds() {
      date -u '+%s000000000'
    }
  else
    travis_nanoseconds() {
      date -u '+%s%N'
    }
  fi

  travis_start () {
    echo "travis_fold:start:$1"
    travis_time_start
    printf "$ANSI_BLUE[$2] $3$ANSI_NOCOLOR\n"
  }

  travis_finish () {
    travis_time_finish
    echo "travis_fold:end:$1"
  }

}

#--

getDockerCredentialPass () {
  PASS_URL="$(curl -s https://api.github.com/repos/docker/docker-credential-helpers/releases/latest \
    | grep "browser_download_url.*pass-.*-amd64" \
    | cut -d : -f 2,3 \
    | tr -d \" \
    | cut -c2- )"

  [ "$(echo "$PASS_URL" | cut -c1-5)" != "https" ] && PASS_URL="https://github.com/docker/docker-credential-helpers/releases/download/v0.6.3/docker-credential-pass-v0.6.3-amd64.tar.gz"

  echo "PASS_URL: $PASS_URL"
  curl -fsSL "$PASS_URL" | tar -xvz
  chmod + $(pwd)/docker-credential-pass
}

#---

dockerLogin () {
  [ "$CI" = "true" ] && gpg --batch --gen-key <<-EOF ; pass init $(gpg --no-auto-check-trustdb --list-secret-keys | grep ^sec | cut -d/ -f2 | cut -d" " -f1)
%echo Generating a standard key
Key-Type: DSA
Key-Length: 1024
Subkey-Type: ELG-E
Subkey-Length: 1024
Name-Real: Meshuggah Rocks
Name-Email: meshuggah@example.com
Expire-Date: 0
# Do a commit here, so that we can later print "done" :-)
%commit
%echo done
EOF
  echo "$DOCKER_PASS" | docker login -u "$DOCKER_USER" --password-stdin
}

#---

beforeInstall () {
  travis_start "update" "APT" "update"
  sudo apt update -y -qq
  travis_finish "update"

  travis_start "fix" "JDK" "fix"
  cat /etc/hosts # optionally check the content *before*
  sudo hostname "$(hostname | cut -c1-63)"
  sed -e "s/^\\(127\\.0\\.0\\.1.*\\)/\\1 $(hostname | cut -c1-63)/" /etc/hosts | sudo tee /etc/hosts
  cat /etc/hosts # optionally check the content *after*
  cd ..
  travis_finish "fix"

  travis_start "ghdl" "GHDL" "build and install"
  sudo apt install -y gnat-4.9 zlib1g-dev
  git clone https://github.com/ghdl/ghdl ghdl-build && cd ghdl-build
  git reset --hard "50da90f509aa6de2961f1795af0be2452bc2c6d9"
  ./dist/travis/build.sh -b mcode -p ghdl
  mv install-mcode ../ghdl
  cd ..
  rm -rf ghdl-build
  travis_finish "ghdl"

  # Debian package 9.7 contain bugs
  travis_start "iverilog" "iverilog" "build and install"
  sudo apt install -y gperf readline-common bison flex
  curl -fsSL https://github.com/steveicarus/iverilog/archive/v10_2.tar.gz | tar -xvz
  cd iverilog-10_2
  autoconf
  ./configure
  make -j$(nproc)
  sudo make install
  cd ..
  rm -rf iverilog-10_2
  travis_finish "iverilog"

  travis_start "cocotb" "cocotb" "install and compile VPI"
  sudo apt install -y git make gcc g++ swig python-dev
  git clone https://github.com/potentialventures/cocotb
  cd cocotb
  git reset --hard a463cee498346cb26fc215ced25c088039490665
  cd ..
  # Force cocotb to compile VPI to avoid race condition when tests are start in parallel
  export PATH=$(pwd)/ghdl/bin:$PATH
  export COCOTB=$(pwd)/cocotb
  cd SpinalHDL/tester/src/test/python/spinal/Dummy
  make TOPLEVEL_LANG=verilog
  make TOPLEVEL_LANG=vhdl
  cd ../../../../../../..
  travis_finish "cocotb"

  travis_start "verilator" "verilator" "build and install"
  sudo apt install -y git make autoconf g++ flex bison  # First time prerequisites
  git clone http://git.veripool.org/git/verilator   # Only first time
  unset VERILATOR_ROOT  # For bash
  cd verilator
  git pull        # Make sure we're up-to-date
  git checkout v4.008
  autoconf        # Create ./configure script
  ./configure
  make -j$(nproc)
  sudo make install
  cd ..
  travis_finish "verilator"

  cd SpinalHDL
}

#---

compileTest () {
  travis_start "compile" "SBT" "compile"
  sbt -Dsbt.supershell=false -J-Xss2m compile
  travis_finish "compile"

  travis_start "cocotb" "SBT" "cocotb VPI"
  # Force cocotb to compile VPI to avoid race condition when tests are start in parallel
  cd tester/src/test/python/spinal/Dummy
  make TOPLEVEL_LANG=verilog
  make TOPLEVEL_LANG=vhdl
  cd ../../../../../..
  travis_finish "cocotb"

  travis_start "test" "SBT" "test"
  sbt -Dsbt.supershell=false -J-Xss2m test
  travis_finish "test"
}

#---

deploy () {
  travis_start "build-spinalhdl" "BUILD" "spinalhdl/spinalhdl"
  docker build -t spinalhdl/spinalhdl --target spinalhdl .
  travis_finish "build-spinalhdl"

  travis_start "build-riscv" "BUILD" "spinalhdl/riscv"
  docker build -t spinalhdl/riscv --target riscv .
  travis_finish "build-riscv"

  getDockerCredentialPass
  dockerLogin

  travis_start "push-spinalhdl" "PUSH" "spinalhdl/spinalhdl"
  docker push spinalhdl/spinalhdl
  travis_finish "push-spinalhdl"

  travis_start "push-riscv" "PUSH" "spinalhdl/riscv"
  docker push spinalhdl/riscv
  travis_finish "push-riscv"

  docker logout
}

#---

case "$1" in
  "-i")
    beforeInstall
  ;;
  "-t")
    compileTest
  ;;
  "-c")
    docker run --rm -ite TRAVIS="$TRAVIS" -v $(pwd):/src -w /src spinalhdl/dev ./travis.sh -t
  ;;
  "-d")
    deploy
  ;;
  *)
    echo "Unknown arg <$1>"
  ;;
esac
