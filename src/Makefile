# Makefile for SARTA.  Most of the work is done by make_sarta.

#----------------------------------------------------------------------
# AIRS
apr08_m140_wcon_nte:
	rm -f *.o incFTC.f
	sed 's/XCSNAMEX/m140/g' incFTC_airs_apr08_template_m130_m140_m150_wcon_nte.f > incFTC.f
	make -f make_sarta
	mv a.out ../sarta_apr08_m140_wcon_nte

apr08_m140_pge_v6_tunmlt:
	rm -f *.o incFTC.f
	sed 's/XCSNAMEX/m140/g' incFTC_airs_apr08_template_m130_m140_m150_pge_v6_tunmlt.f > incFTC.f
	make -f make_sarta
	mv a.out ../sarta_apr08_m140_pge_v6_tunmlt
#----------------------------------------------------------------------
# CrIS
crisg4_nov09_wcon_nte:
	rm -f *.o incFTC.f
	ln -s incFTC_crisg4_nov09_wcon_nte.f incFTC.f
	make -f make_sarta
	mv a.out ../sarta_crisg4_nov09_wcon_nte

cris_hrg2_mar16_wcon:
	rm -f *.o incFTC.f
	ln -s incFTC_cris_hrg2.f incFTC.f
	make -f make_sarta_hrg2
	mv a.out ../sarta_cris_hrg2_mar16_wcon
#----------------------------------------------------------------------
#IASI
iasi_may09_wcon_nte:
	rm -f *.o incFTC.f
	ln -s incFTC_iasi_may09_wcon_nte.f incFTC.f
	make -f make_sarta
	mv a.out ../sarta_iasi_may09_wcon_nte
#---------------------------------------------------------------------
#IASI2CrIS
i2c_mar16_wcon_nte:
	rm -f *.o incFTC.f
	ln -s incFTC_iasi2cris_mar16_wcon.f incFTC.f
	make -f make_sarta
	mv a.out ../sarta_i2c_mar16_wcon_nte
#----------------------------------------------------------------------------------------
# File management
#----------------------------------------------------------------------------------------
clean:
	rm -f *.o

