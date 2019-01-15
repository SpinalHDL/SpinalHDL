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

# This is a trimmed down copy of
# https://github.com/travis-ci/travis-build/blob/master/lib/travis/build/templates/header.sh
travis_time_start() {
  # `date +%N` returns the date in nanoseconds. It is used as a replacement for $RANDOM, which is only available in bash.
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

#--

getDockerCredentialPass () {
  PASS_URL="$(curl -s https://api.github.com/repos/docker/docker-credential-helpers/releases/latest \
    | grep "browser_download_url.*pass-.*-amd64" \
    | cut -d : -f 2,3 \
    | tr -d \")"
  PASS_URL="$(echo $PASS_URL | cut -c2-)"

  if [ "$(echo "$PASS_URL" | cut -c1-5)" != "https" ]; then
    PASS_URL="https://github.com/docker/docker-credential-helpers/releases/download/v0.6.0/docker-credential-pass-v0.6.0-amd64.tar.gz"
  fi

  echo "PASS_URL: $PASS_URL"

  curl -fsSL "$PASS_URL" | tar xv

  chmod + $(pwd)/docker-credential-pass
}

#---

dockerLogin () {
  if [ "$CI" = "true" ]; then
    gpg --batch --gen-key <<-EOF
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

    key=$(gpg --no-auto-check-trustdb --list-secret-keys | grep ^sec | cut -d/ -f2 | cut -d" " -f1)
    pass init $key

    echo "$DOCKER_PASS" | docker login -u "$DOCKER_USER" --password-stdin
  fi
}

#---

beforeInstall () {
  sudo apt update -y -qq

  # JDK fix
  cat /etc/hosts # optionally check the content *before*
  sudo hostname "$(hostname | cut -c1-63)"
  sed -e "s/^\\(127\\.0\\.0\\.1.*\\)/\\1 $(hostname | cut -c1-63)/" /etc/hosts | sudo tee /etc/hosts
  cat /etc/hosts # optionally check the content *after*
  cd ..

  # GHDL
  sudo apt install -y gnat-4.9 zlib1g-dev
  git clone https://github.com/ghdl/ghdl ghdl-build && cd ghdl-build
  git reset --hard "50da90f509aa6de2961f1795af0be2452bc2c6d9"
  ./dist/travis/build.sh -b mcode -p ghdl
  mv install-mcode ../ghdl
  cd ..
  rm -rf ghdl-build

  # iverilog (debian package 9.7 contain bugs)
  sudo apt install -y gperf readline-common bison flex
  curl -fsSL https://github.com/steveicarus/iverilog/archive/v10_2.tar.gz | tar -xvz
  cd iverilog-10_2
  autoconf
  ./configure
  make -j$(nproc)
  sudo make install
  cd ..
  rm -rf iverilog-10_2

  # cocotb
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

  # Verilator
  sudo apt install -y git make autoconf g++ flex bison  # First time prerequisites
  git clone http://git.veripool.org/git/verilator   # Only first time
  unset VERILATOR_ROOT  # For bash
  cd verilator
  git pull        # Make sure we're up-to-date
  git checkout verilator_4_008
  autoconf        # Create ./configure script
  ./configure
  make -j$(nproc)
  sudo make install
  cd ..

  cd SpinalHDL
}

#---

compileTest () {
  echo "travis_fold:start:compile"
  travis_time_start
  printf "$ANSI_BLUE[SBT] compile $ANSI_NOCOLOR\n"
  sbt -J-Xss2m compile
  travis_time_finish
  echo "travis_fold:end:compile"

  echo "travis_fold:start:cocotb"
  travis_time_start
  printf "$ANSI_BLUE[SBT] cocotb VPI $ANSI_NOCOLOR\n"
# Force cocotb to compile VPI to avoid race condition when tests are start in parallel
  cd tester/src/test/python/spinal/Dummy
  make TOPLEVEL_LANG=verilog
  make TOPLEVEL_LANG=vhdl
  cd ../../../../../..
  travis_time_finish
  echo "travis_fold:end:cocotb"

  echo "travis_fold:start:test"
  travis_time_start
  printf "$ANSI_BLUE[SBT] test $ANSI_NOCOLOR\n"
  sbt -J-Xss2m test
  travis_time_finish
  echo "travis_fold:end:test"
}

#---

deploy () {
  echo "travis_fold:start:build-spinalhdl"
  travis_time_start
  printf "$ANSI_BLUE[BUILD] spinalhdl/spinalhdl $ANSI_NOCOLOR\n"
  docker build -t spinalhdl/spinalhdl --target spinalhdl .
  travis_time_finish
  echo "travis_fold:end:build-spinalhdl"

  echo "travis_fold:start:build-riscv"
  travis_time_start
  printf "$ANSI_BLUE[BUILD] spinalhdl/riscv $ANSI_NOCOLOR\n"
  docker build -t spinalhdl/riscv --target riscv .
  travis_time_finish
  echo "travis_fold:end:build-riscv"

  getDockerCredentialPass
  dockerLogin

  echo "travis_fold:start:push-spinalhdl"
  travis_time_start
  printf "$ANSI_BLUE[PUSH] spinalhdl/spinalhdl $ANSI_NOCOLOR\n"
  docker push spinalhdl/spinalhdl
  travis_time_finish
  echo "travis_fold:end:push-spinalhdl"

  echo "travis_fold:start:push-riscv"
  travis_time_start
  printf "$ANSI_BLUE[PUSH] spinalhdl/riscv $ANSI_NOCOLOR\n"
  docker push spinalhdl/riscv
  travis_time_finish
  echo "travis_fold:end:push-riscv"

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
    docker run --rm -itv $(pwd):/src -w /src spinalhdl/dev ./travis.sh -t
  ;;
  "-d")
    deploy
  ;;
  *)
    echo "Unknown arg <$1>"
  ;;
esac
