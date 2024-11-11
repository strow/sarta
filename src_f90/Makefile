# Makefile for SARTA.  Most of the work is done by make_sarta.

#
# Makefile for SARTA v1.08.  Most of the work is either by
# make_sarta_pclsam (sarta_cloudy)
# or by
# make_sarta (sarta clear only)
#
# Default is sarta_cloudy named m140_icebaumGHM_waterdrop_desertdust

#------------------------------------------------------------------------------------
# 1.0 Clear SARTA Builds 
#------------------------------------------------------------------------------------
# 1.1 Neural net based builds
# -----------------------------------------------------------------------------------
airs_nn:
	rm -f *.o incFTC.f90
	ln -s incFTC_airs_2834.f90 incFTC.f90
	make -f make_sarta_nn
	mv a.out ../bin/airs_nn


# IASI
iasi_nn:
	rm -f *.o incFTC.f90
	ln -sf incFTC_iasi_p2022jul22_dev.f90 incFTC.f90
	make -f make_sarta_nn
	mv a.out ../bin/iasi_nn

# CrIS FSR
cris_nn:
	rm -f *.o incFTC.f90
	ln -sf incFTC_cris_hr_p2022jul22_dev.f90 incFTC.f90
	ifort -c incFTC.f90 -I/home/chepplew/myLib/rtp/include
	make -f make_sarta_nn
	mv a.out ../bin/cris_nn_nte

# -----------------------------------------------------------------
#  1.2 regression only models
#  ----------------------------------------------------------------

# CHIRP
chirp_reg_dev:
	rm -f *.o incFTC.f90
	ln -sf incFTC_chirp_p2022jul22_dev.f90 incFTC.f90
	ifort -c incFTC.f90 -I/home/chepplew/myLib/rtp/include
	make -f make_sarta_reg
##	mv a.out ../bin/chirp_reg_dev
chirp_reg_prod:
	rm -f *.o incFTC.f90
	ln -sf incFTC_chirp_p2022jul22_reg_prod.f90 incFTC.f90
	ifort -c incFTC.f90 -I/home/chepplew/myLib/rtp/include
	make -f make_sarta_reg
	mv a.out ../bin/chirp_reg_prod

#------------------------------------------------------------------------------------
# File management
#------------------------------------------------------------------------------------

clean:
	rm -f *.o
	rm -f *_genmod.f90 *_genmod.mod
