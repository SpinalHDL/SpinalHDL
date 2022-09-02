rm ../java/spinal/sim/vpi/*
swig -c++ -java -package spinal.sim.vpi -outdir ../java/spinal/sim/vpi SharedMemIface.i
rm ../java/spinal/sim/xsi/*
swig -c++ -java -package spinal.sim.xsi -outdir ../java/spinal/sim/xsi XSIIface.i