# Makefile for SARTA.  Most of the work is done by make_sarta.

#----------------------------------------------------------------------
# AIRS
apr08_m140_wcon_nte:
	rm -f *.o incFTC.f
	sed 's/XCSNAMEX/m140/g' incFTC_airs_apr08_template_m130_m140_m150_wcon_nte.f > incFTC.f
	make -f make_sarta
	mv a.out ../bin/sarta_apr08_m140_wcon_nte

apr08_m140_pge_v6_tunmlt:
	rm -f *.o incFTC.f
	sed 's/XCSNAMEX/m140/g' incFTC_airs_apr08_template_m130_m140_m150_pge_v6_tunmlt.f > incFTC.f
	make -f make_sarta
	mv a.out ../bin/sarta_apr08_m140_pge_v6_tunmlt
airs_2378_400p_7set:
	rm -f *.o incFTC.f
	ln -s incFTC_airs_2378_400p_7set.f incFTC.f
	make -f make_sarta_airs_2378_400p_7set
	mv a.out ../bin/sarta_airs_2378_400p_7set
airs_2378_400p_7s_optr:
	rm -f *.o incFTC.f
	ln -s incFTC_airs_2378_400p_7s_optr.f incFTC.f
	make -f make_sarta_airs_2378_400p_7s_optr
	mv a.out ../bin/sarta_airs_2378_400p_7s_optr
#----------------------------------------------------------------------
# CrIS
crisg4_nov09_wcon_nte:
	rm -f *.o incFTC.f
	ln -s incFTC_crisg4_nov09_wcon_nte.f incFTC.f
	make -f make_sarta
	mv a.out ../bin/sarta_crisg4_nov09_wcon_nte

cris_hrg4_400p_7s_opt:
	rm -f *.o incFTC.f
	ln -s incFTC_kc_cris_hrg4_400_7set.f incFTC.f
	make -f make_sarta_hrg4_7s_opt
	mv a.out ../bin/sarta_kc_cris_hrg4_400_7set

cris_hrg4_400p_7s_so2_hno3_n2o:
	rm -f *.o incFTC.f
	ln -s incFTC_kc_cris_hrg4_400p_7s_so2_hno3_n2o.f incFTC.f
	make -f make_sarta_hrg4_400p_7s_so2_hno3_n2o
	mv a.out ../bin/sarta_kc_cris_hrg4_400p_7s_so2_hno3_n2o

cris_hrg4_400p_full:
	rm -f *.o incFTC.f
	ln -s incFTC_kc_cris_hrg4_400p_full.f incFTC.f
	make -f make_sarta_hrg4_400p_full
	mv a.out ../bin/sarta_kc_cris_hrg4_400p_full
#----------------------------------------------------------------------
#IASI
iasi_may09_wcon_nte:
	rm -f *.o incFTC.f
	ln -s incFTC_iasi_may09_wcon_nte.f incFTC.f
	make -f make_sarta
	mv a.out ../bin/sarta_iasi_may09_wcon_nte
#---------------------------------------------------------------------
#IASI2CrIS
i2c_mar16_wcon_nte:
	rm -f *.o incFTC.f
	ln -s incFTC_iasi2cris_mar16_wcon.f incFTC.f
	make -f make_sarta
	mv a.out ../bin/sarta_i2c_mar16_wcon_nte
#----------------------------------------------------------------------------------------
# File management
#----------------------------------------------------------------------------------------
clean:
	rm -f *.o

