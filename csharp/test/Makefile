#
# Kiwi Scientific Acceleration - JRK Mersenne Test
#
CSC     ?=mcs
KIWI    ?=$(HPRLS)/kiwipro/kiwic/distro
KIWIDLL ?=$(KIWI)/support/Kiwi.dll
CV_INT_ARITH    ?=$(KIWI)/lib/cvgates.v
CV_FP_ARITH    ?=$(KIWI)/lib/cv_fparith.v
VERILATOR_ROOT ?=/usr/share/verilator


#ANAME=jrk-mersenne
#ANAME=jrkk1
ANAME=test

KIWIC  ?=$(KIWI)/bin/kiwic \
	-kiwic-register-colours=disable \
	-give-backtrace \
	-vnl-resets=synchronous   \
        -vnl-roundtrip=disable \
        -kiwic-cil-dump=combined \
        -kiwic-kcode-dump=enable \
        -bevelab-default-pause-mode=soft  \
        -bevelab-soft-pause-threshold=15 \
        -repack-to-roms=disable \
	-kiwic-autodispose=enable
#	-loglevel=3 \
#	-firstpass-loglevel=0 
# -res2-loadstore-port-count=0 
# -firstpass-loglevel=0 -gtrace-loglevel=0 
#  -restructure2=disable 
#  -vnl-ifshare=none 
#  -verboselevel=10 
#      -diosim-tl=0 

all:ll_run

g.sh: Makefile
	true > $@
	echo '#!/usr/bin/gnuplot' >> $@
	echo 'N(s,x)=exp(-(x/s)**2/2)/(s*sqrt(2*pi))' >> $@
	echo 'set term postscript enhanced color' >> $@
	echo 'set output "g.eps"' >> $@
	echo 'p N(5,x), "'"g.dat"'" u ($$1+0.5):($$2/1000000) w l' >> $@
	echo '#pause mouse' >> $@

g.dat: $(ANAME).exe
	MONO_PATH=$(HPRLS)/kiwipro/kiwic/distro/support mono $(ANAME).exe > $@
	md5sum g.dat > $@.md5
	echo 'df8a1aa7db6c25f8dc3c8910e6039473 ' $@ | diff -  $@.md5

plot: g.dat g.sh
	env GNUPLOT_PS_DIR=/usr/share/gnuplot/4.2/PostScript gnuplot g.sh

clean:
	rm -f test.v

KIWIFLAGS= -sim 100 -vnl-resets=synchronous -kiwic-register-colours=1 -cil-uwind-budget=100000 -diosim-tl=100 -kiwic-cil-dump=combined -kiwic-kcode-dump=enable -bevelab-default-pause-mode=soft

spare_target:test.exe
	$(KIWIC) -vnl-rootmodname DUT test.exe -kiwic-cil-dump=combined -give-backtrace -kiwic-kcode-dump=enable -vnl-resets=synchronous -cil-uwind-budget=10
	echo "Test - KiwiC exited"

$(ANAME).exe:$(ANAME).cs  TTsqrt.dll
	$(CSC) $(ANAME).cs  /r:$(KIWIDLL) /r:TTsqrt.dll

monorun:$(ANAME).exe
	MONO_PATH=$(HPRLS)/kiwipro/kiwic/distro/support:.. time mono  $(ANAME).exe | tee goldspool


hl_run:$(ANAME).exe TTsqrt.dll
	$(KIWIC)  -sim 2000  -repack=disable -conerefine=disable -bevelab=disable  -verilog-gen=disable $(ANAME).exe TTsqrt.dll


$(ANAME).v:$(ANAME).exe TTsqrt.dll
	$(KIWIC) $(ANAME).exe -vnl=$(ANAME).v TTsqrt.dll

TTsqrt.dll:TTsqrt.cs
	$(CSC) TTsqrt.cs  /target:library /r:$(KIWIDLL)

#------------------------------------------------
# RTLSIM Verilator Run
ll_run2:verilated/VVERI_SIMSYS.cpp veriltop-dfsin.cpp
	g++ -g -O2 -Iverilated -I$(VERILATOR_ROOT)/include veriltop-dfsin.cpp verilated/VVERI_SIMSYS.cpp $(VERILATOR_ROOT)/include/verilated.cpp verilated/VVERI_SIMSYS__Syms.cpp
	time ./a.out
	tail -8 KiwiC.rpt

verilated/VVERI_SIMSYS.cpp:$(ANAME).v veril_sys.v
	verilator --Mdir verilated -Wno-fatal -cc --top-module VERI_SIMSYS $(ANAME).v veril_sys.v $(CV_INT_ARITH) $(CV_FP_ARITH)

#------------------------------------------------
# RTLSIM Icarus Run
ll_run:$(ANAME).v
	iverilog $(ANAME).v vsys.v $(CV_INT_ARITH)  $(CV_FP_ARITH)
	./a.out| tee icarus.spool
	cp vcd.vcd ~/Dropbox



# RTLSIM
icarus:$(ANAME).v
	iverilog $(ANAME).v vsys.v $(CV_INT_ARITH)
	time ./a.out| tee icarus.spool


# make these links once please - should be automated.
links:
	ln -s $(KIWIDLL)

#---
dot:
	dot -Tpng < controlflow_combined.dot > ~/Dropbox/a.png

# eof
