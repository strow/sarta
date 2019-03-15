# Makefile for SARTA.  Most of the work is done by make_sarta.

#
# Makefile for SARTA v1.08.  Most of the work is either by
# make_sarta_pclsam (sarta_cloudy)
# or by
# make_sarta (sarta clear only)
#
# Default is sarta_cloudy named m140_icebaumGHM_waterdrop_desertdust

#----------------------------------------------------------------------------------------
# Builds cloudy
#----------------------------------------------------------------------------------------
m140_icebaumGHM_waterdrop_desertdust:
	rm -f *.o incFTC.f fnmie.f
	sed 's/XCSNAMEX/m140/' incFTC_airs_apr08.f > incFTC.f
	ln -s fnmie_iceGHMbaum_waterdrop_desertdust.f fnmie.f
	make -f make_sarta_pclsam
	mv a.out ../bin/sarta_apr08_m140_iceGHMbaum_waterdrop_desertdust_slabcloud_hg3X
#----------------------------------------------------------------------------------------
# Builds clear
#----------------------------------------------------------------------------------------

########## AIRS ################
apr08_m140_wcon_nte_2016tuning:
	rm -f *.o incFTC.f
	sed 's/XCSNAMEX/m140/g' incFTC_airs_apr08_template_m130_m140_m150_wcon_nte.f > incFTC.f
	make -f make_sarta
	mv a.out ../bin/sarta_apr08_m140_wcon_nte_2016tuning

apr08_m140_pge_v6_tunmlt:
	rm -f *.o incFTC.f
	sed 's/XCSNAMEX/m140/g' incFTC_airs_apr08_template_m130_m140_m150_pge_v6_tunmlt.f > incFTC.f
	make -f make_sarta
	mv a.out ../bin/apr08_m140_pge_v6_tunmlt

#  ------------  CrIS   ------------ #######
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

# CrIS  (high res) basic coef set[1-7] and wcon.
crisg4_mar18:
	rm -f *.o incFTC.f
	ln -s incFTC_crisHRg4_mar18_basic_optr_co2_so2_nte.f incFTC.f
	make -f make_sarta
	mv a.out ../bin/crisg4_mar18_basic_so2

# CrIS (high Res) with var NH3
crisg4_may18_nh3:
	rm -f *.o incFTC.f
	ln -s incFTC_crisHRg4_may18_optr_co2_so2_nh3_nte.f incFTC.f
	make -f make_sarta_nh3
	mv a.out ../bin/crisg4_may18_nh3

######  -------  IASI  -----------
# IASI basic 7-set w/continuum
iasi_mar2019:
	rm -f *.o incFTC.f
	ln -s incFTC_iasi_dec2018_basic_optr_5tra_hdo_nte.f incFTC.f
	make -f make_sarta
	mv a.out ../bin/iasi_mar2019

#----------------------------------------------------------------------------------------
# File management
#----------------------------------------------------------------------------------------

clean:
	rm -f *.o
