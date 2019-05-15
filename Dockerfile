FROM spinalhdl/dev as spinalhdl

RUN git clone -b dev --recurse-submodule https://github.com/SpinalHDL/SpinalHDL.git /opt/SpinalHDL && cd /opt/SpinalHDL \
 && sbt clean compile publishLocal

#---

FROM spinalhdl as riscv

RUN apt update -qq && apt upgrade -y && apt install -y --no-install-recommends \
      curl \
  && apt update -qq && apt autoclean && apt clean && apt -y autoremove

ENV RISCV /opt/riscv
ENV NUMJOBS 1

ENV PATH $RISCV/bin:$PATH
RUN echo 'export PATH=/opt/riscv/bin:$PATH' >> $WORKDIR/.bashrc

ARG RISCV_GCC_VER=riscv64-unknown-elf-gcc-20170612-x86_64-linux-centos6

RUN cd /opt && curl -fsSL https://static.dev.sifive.com/dev-tools/$RISCV_GCC_VER.tar.gz | tar -xzv && \
    mv $RISCV_GCC_VER /opt/riscv

RUN mkdir -p $RISCV/test && cd $RISCV/test \
 && echo '#include <stdio.h>\n int main(void) { printf("Hello world!\\n"); return 0; }' > hello.c \
 && riscv64-unknown-elf-gcc -o hello hello.c \
 && cd / && rm -rf $RISCV/test
