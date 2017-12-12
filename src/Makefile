# Makefile for SARTA.  Most of the work is done by make_sarta.

#
# Makefile for SARTA v1.08 PCLSAM using rtpV201.  Most of the work is
# done by make_sarta_pclsam.
#

# ------
# Builds
# ------

### tuning
### incFTC_airs_apr08.f has FNTMLT = /asl/data/sarta_database/Data_AIRS_apr08/Coef/tunmlt_XCSNAMEX.txt
### incFTC_airs_apr08.f has FNTMLT = /asl/data/sarta_database/Data_AIRS_apr08/Coef/tunmlt_XCSNAMEX.txt
### incFTC_airs_apr08.f has FNTMLT = /asl/data/sarta_database/Data_AIRS_apr08/Coef/tunmlt_XCSNAMEX.txt eg
### incFTC_airs_apr08.f has FNTMLT = /asl/data/sarta_database/Data_AIRS_apr08/Coef/tunmlt_m140.txt

### basic
### incFTC_airs_apr08_wcon_nte.f has FNTMLT = /asl/data/sarta_database/Data_AIRS_apr08/Coef/tunmlt_wcon_nte.txt
### incFTC_airs_apr08_wcon_nte.f has FNTMLT = /asl/data/sarta_database/Data_AIRS_apr08/Coef/tunmlt_wcon_nte.txt
### incFTC_airs_apr08_wcon_nte.f has FNTMLT = /asl/data/sarta_database/Data_AIRS_apr08/Coef/tunmlt_wcon_nte.txt


#----------------------------------------------------------------------------------------
# Builds cloudy
#----------------------------------------------------------------------------------------

# AIRS
m140_icebaumGHM_waterdrop_desertdust:
	rm -f *.o incFTC.f fnmie.f
	sed 's/XCSNAMEX/m140/' incFTC_airs_apr08.f > incFTC.f
	ln -s fnmie_iceGHMbaum_waterdrop_desertdust.f fnmie.f
	make -f make_sarta_pclsam
	mv a.out ../bin/sarta_apr08_m140_iceGHMbaum_waterdrop_desertdust_slabcloud_hg3X

# CrisHiRes
crisg4_hires_dec17_icebaumGHM_waterdrop_desertdust:
	rm -f *.o incFTC.f fnmie.f
	ln -s incFTC_crisg4_oct16.f incFTC.f
	ln -s fnmie_iceGHMbaum_waterdrop_desertdust_cris_hires.f fnmie.f	
	make -f make_sarta_pclsam
	mv a.out ../bin/crisg4_hires_dec17_iceGHMbaum_waterdrop_desertdust_slabcloud_hg3

#----------------------------------------------------------------------------------------
# Builds clear
#----------------------------------------------------------------------------------------

# AIRS
apr08_m140_wcon_nte_2016tuning:
	rm -f *.o incFTC.f
	sed 's/XCSNAMEX/m140/g' incFTC_airs_apr08_template_m130_m140_m150_wcon_nte.f > incFTC.f
	make -f make_sarta
	mv a.out ../bin/apr08_m140_wcon_nte_2016tuning

apr08_m140_pge_v6_tunmlt:
	rm -f *.o incFTC.f
	sed 's/XCSNAMEX/m140/g' incFTC_airs_apr08_template_m130_m140_m150_pge_v6_tunmlt.f > incFTC.f
	make -f make_sarta
	mv a.out ../bin/apr08_m140_pge_v6_tunmlt

# CrIS standard resolution
crisg4_nov09_wcon_nte:
	rm -f *.o incFTC.f
	ln -s incFTC_crisg4_nov09_wcon_nte.f incFTC.f
	make -f make_sarta
#	mv a.out ../bin/crisg4_nov09

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
