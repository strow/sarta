# Makefile for SARTA.  Most of the work is done by make_sarta.

#----------------------------------------------------------------------------------------
# Builds
#----------------------------------------------------------------------------------------

# AIRS
apr08_m140_wcon_nte:
	rm -f *.o incFTC.f
	sed 's/XCSNAMEX/m140/g' incFTC_airs_apr08_template_m130_m140_m150_wcon_nte.f > incFTC.f
	make -f make_sarta
#	mv a.out ../bin/airs

apr08_m140_pge_v6_tunmlt:
	rm -f *.o incFTC.f
	sed 's/XCSNAMEX/m140/g' incFTC_airs_apr08_template_m130_m140_m150_pge_v6_tunmlt.f > incFTC.f
	make -f make_sarta
#	mv a.out ../bin/airs_pge

# CrIS standard resolution
crisg4_nov09_wcon_nte:
	rm -f *.o incFTC.f
	ln -s incFTC_crisg4_nov09_wcon_nte.f incFTC.f
	make -f make_sarta
#	mv a.out ../bin/cris

# CrIS extended resolution (high res)
crisg4_oct16:
	rm -f *.o incFTC.f
	ln -s incFTC_crisg4_oct16.f incFTC.f
	make -f make_sarta
	mv a.out ../bin/crisg4_oct16

# IASI
iasi_may09_wcon_nte:
	rm -f *.o incFTC.f
	ln -s incFTC_iasi_may09_wcon_nte.f incFTC.f
	make -f make_sarta
#	mv a.out ../bin/iasi

#----------------------------------------------------------------------------------------
# File management
#----------------------------------------------------------------------------------------

clean:
	rm -f *.o
