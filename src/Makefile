# Makefile for SARTA.  Most of the work is done by make_sarta.

#----------------------------------------------------------------------
# AIRS
airs_2378_400p_7s_optr:
	rm -f *.o incFTC.f
	ln -s incFTC_airs_2378_400p_7s_optr.f incFTC.f
	make -f make_sarta_airs_2378_400p_7s_optr
	mv a.out ../bin/sarta_airs_2378_400p_7s_optr
#----------------------------------------------------------------------
# CrIS
cris_g4_400p_full:
	rm -f *.o incFTC.f
	ln -s incFTC_g4_cris_hrg4_400p_full.f incFTC.f
	make -f make_sarta_hrg4_400p_full
	mv a.out ../bin/sarta_g4_cris_hrg4_400p_full_th2
#----------------------------------------------------------------------
#IASI
iasi_may09_wcon_nte:
	rm -f *.o incFTC.f
	ln -s incFTC_iasi_may09_wcon_nte.f incFTC.f
	make -f make_sarta
	mv a.out ../bin/sarta_iasi_may09_wcon_nte
#---------------------------------------------------------------------
# File management
#----------------------------------------------------------------------------------------
clean:
	rm -f *.o

