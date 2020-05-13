rm ../java/spinal/sim/vpi/*
swig -c++ -java -package spinal.sim.vpi -outdir ../java/spinal/sim/vpi SharedMemIface.i
