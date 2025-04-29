# Makefile for SARTA.  Most of the work is done by make_sarta.

# Section 1: Clear Sky
# Section 2: Allsky
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
	ln -sf cbplev_airslay.f90 cbplev.f90
	make -f make_sarta_nn
#	mv a.out ../bin/cris_nn_nte

# -----------------------------------------------------------------
#  1.2 regression only models. std airslay pressure levels
#  ----------------------------------------------------------------

# AIRS L1C std plevs 
airs_l1c_p2025jul2022_dev:
	rm -f *.o incFTC.f90
#	ln -sf incFTC_airs_l1c_p2025jul2022_dev.f90 incFTC.f90
	ln -sf incFTC_airs_l1c_jul2022.f90 incFTC.f90
	ifort -c incFTC.f90 -I/home/chepplew/myLib/rtp/include
	ln -sf cbplev_airslay.f90 cbplev.f90
	make -f make_sarta_reg
	mv a.out ../bin/airs_l1c_jul2022
#	mv a.out ../bin/airs_l1c_p2025jul22_std_dev
# -----------------------------------------------------------------
# CrIS FSR 
cris_hr_reg_dev:
	rm -f *.o incFTC.f90
#	ln -sf incFTC_cris_hrg4_p2025jul22_dev.f90 incFTC.f90
	ln -sf incFTC_cris_hr_jul2022.f90 incFTC.f90
	ifort -c incFTC.f90 -I/home/chepplew/myLib/rtp/include
	ln -sf cbplev_airslay.f90 cbplev.f90
	make -f make_sarta_reg
#	mv a.out ../bin/cris_hr_p2022jul22_reg_dev
#	mv a.out ../bin/cris_hr_p2025jul22_reg_dev2
#	mv a.out ../bin/cris_hr_p2025jul22_reg_dev
# ----------------------------------------------------------------
# IASI
iasi_reg_dev:
	rm -f *.o incFTC.f90
#	ln -sf incFTC_iasi_p2022jul22_dev.f90 incFTC.f90
	ln -sf incFTC_iasi_jul2022.f90 incFTC.f90
	ifort -c incFTC.f90 -I/home/chepplew/myLib/rtp/include
	ln -sf cbplev_airslay.f90 cbplev.f90
	make -f make_sarta_reg
##	mv a.out ../bin/iasi_p2022jul22_dev
# ----------------------------------------------------------------
# CHIRP
chirp_reg_dev:
	rm -f *.o incFTC.f90
	ln -sf incFTC_chirp_p2022jul22_dev.f90 incFTC.f90
	ifort -c incFTC.f90 -I/home/chepplew/myLib/rtp/include
	ln -sf cbplev_airslay.f90 cbplev.f90
	make -f make_sarta_reg
##	mv a.out ../bin/chirp_reg_dev
chirp_reg_prod:
	rm -f *.o incFTC.f90
##	ln -sf incFTC_chirp_p2022jul22_reg_prod.f90 incFTC.f90
	ln -sf incFTC_chirp_jul2022.f90 incFTC.f90
	ifort -c incFTC.f90 -I/home/chepplew/myLib/rtp/include
	ln -sf cbplev_airslay.f90 cbplev.f90
	make -f make_sarta_reg
##	mv a.out ../bin/chirp_p2022jul22_prod
# ===========================================================
#  1.3 regression only models. airs-oco2 PBL  pressure levels
#  ----------------------------------------------------------------
# AIRS PBL (planetary boundary layer version for Bill Irion)
airs_pbl_dev:
	rm -f *.o incFTC.f90
	ln -sf incFTC_airs_pbl_p2025jan25a_dev.f90 incFTC.f90
	ifort -c incFTC.f90 -I/home/chepplew/myLib/rtp/include
	ln -sf cbplev_airs_oco2_pbl_v2.f90 cbplev.f90
	make -f make_sarta_reg
#	mv a.out ../bin/airs_pbl_dev_nooptr
#	mv a.out ../bin/airs_pbl_p2025jan25_dev_debug

# CrIS FSR PBL (planetary boundary layer version)
cris_hr_pbl_dev:
	rm -f *.o incFTC.f90
	ln -sf incFTC_cris_hrg4_pbl_p2025jan25a_dev.f90 incFTC.f90
	ifort -c incFTC.f90 -I/home/chepplew/myLib/rtp/include
	ln -sf cbplev_airs_oco2_pbl_v2.f90 cbplev.f90
	make -f make_sarta_reg
#	mv a.out ../bin/cris_hr_p2025Jan25_pbl_dev

# CHIRP PBL (planetary boundary layer version)
chirp_pbl_dev:
	rm -f *.o incFTC.f90
	ln -sf incFTC_chirp_pbl_p2025jan25a_dev.f90 incFTC.f90
	ifort -c incFTC.f90 -I/home/chepplew/myLib/rtp/include
	ln -sf cbplev_airs_oco2_pbl_v2.f90 cbplev.f90
	make -f make_sarta_reg
	mv a.out ../bin/chirp_pbl_p2025jan25a_dev

#------------------------------------------------------------------------------------
# 2.0 ALLSKY SARTA Builds 
#------------------------------------------------------------------------------------
# 2.1 AIRSLAY PLEVS
# -----------------------------------------------------------------------------------
#
# CRIS.FSR. airslay ALLSKY
cris_hr_reg_allsky:
	rm -f *.o incFTC.f90 fnmie.f90
	ln -s incFTC_cris_hr_p2022jul22_pclsam_dev.f90 incFTC.f90
	ifort -c incFTC.f90 -I/home/chepplew/myLib/rtp/include
	ln -s fnmie_iceGHMbaum_waterdrop_desertdust_cris_hires.f90 fnmie.f90
	ln -sf cbplev_airslay.f90 cbplev.f90
	make -f make_sarta_reg_pclsam
#	mv a.out ../bin/cris_hr_p2022jul22_reg_ibaum_wdrop_ddust_dev


# -----------------------------------------------------------------------------------
#  PBL (oco2-pbl) PLEVS
#  ----------------------------------------------------------------------------------
# CRIS.FSR.PBL ALLSKY
cris_hr_pbl_allsky:
	rm -f *.o incFTC.f90 fnmie.f90
	ln -s incFTC_cris_hrg4_pbl_p2025jan25a_pclsam.f90 incFTC.f90
	ifort -c incFTC.f90 -I/home/chepplew/myLib/rtp/include
	ln -s fnmie_iceGHMbaum_waterdrop_desertdust_cris_hires.f90 fnmie.f90
	ln -sf cbplev_airs_oco2_pbl_v2.f90 cbplev.f90
	make -f make_sarta_reg_pclsam
	mv a.out ../bin/cris_hr_pbl_ibaum_wdrop_ddust_dev



#------------------------------------------------------------------------------------
# File management
#------------------------------------------------------------------------------------

clean:
	rm -f *.o
	rm -f *_genmod.f90 *_genmod.mod
