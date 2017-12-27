       SUBROUTINE ColJac(
     $        RAD, IPROF, HEAD, PROF, INDCHN, NCHAN, FREQ, DST, DQ, IOUNJ,
     $    MIETYP, MIENPS, MIEPS, MIEABS, MIEEXT, MIEASY,
     $    DISTES, SUNCOS, SCOS1,
     $    LBLAC1, CTYPE1, CFRAC1, CPSIZ1, CPRTO1, CPRBO1, CNGWA1,
     $    XCEMI1, XCRHO1, CSTMP1, CFRA1X, 
     $    LBLAC2, CTYPE2, CFRAC2, CPSIZ2, CPRTO2, CPRBO2, CNGWA2,
     $    XCEMI2, XCRHO2, CSTMP2, CFRA2X, CFRA12, 
     $        NEMIS, FEMIS, XEMIS, XRHO,
     $    LRHOT, LBOT, INDMI1,INDMI2,
     $    IEMIS, EMIS, RHOSUN, RHOTHR, 
     $                NCHNTE, CLISTN, COEFN, CO2TOP, 
     $                TEMPRAW, TEMP,TAU,TAUZ,TAUSN,TAUZSN,
     $                TSURF,DOSUN, SUNFDG, BLMULT, SECSUN, SECANG, COSDAZ,
     $                SUNFAC,HSUN, LABOVE, COEFF,
     $                FCLEAR, TEMPC1, TEMPC2, 
     $                CEMIS1, CEMIS2, CRHOT1, CRHOT2, CRHOS1, CRHOS2, MASUN1, MASUN2,
     $                LCBOT1, LCTOP1, CLRB1,CLRT1, TCBOT1, TCTOP1, MASEC1, CFRCL1, 
     $                NEXTO1, NSCAO1, G_ASY1, 
     $                LCBOT2, LCTOP2, CLRB2,CLRT2, TCBOT2, TCTOP2, MASEC2, CFRCL2, 
     $                NEXTO2, NSCAO2, G_ASY2,
     $     RAAPLNCK,RASURFE,CLD1EFFOD,CLD2EFFOD,CLD1SUN,CLD2SUN,OMEGA1LAY,OMEGA2LAY)

      IMPLICIT NONE

      include 'incFTC.f'
      include 'rtpdefs.f'

c output
       REAL    RAD(MXCHAN) ! chan radiance

c planck emission
       REAL    RAAPLNCK(MAXLAY,MXCHAN) ! chan radiance at each lay
       REAL    RASURFE(MXCHAN) ! chan radiance at surf
       REAL    CLD1SUN(MAXLAY,MXCHAN)  ! chan solar scat due to cld1 at each lay
       REAL    CLD2SUN(MAXLAY,MXCHAN)  ! chan solar scat due to cld2 at each lay
       REAL    CLD1EFFOD(MXCHAN)       ! chan cld1 effOD
       REAL    CLD2EFFOD(MXCHAN)       ! chan cld2 effOD
       REAL    OMEGA1LAY(MAXLAY,MXCHAN) ! single scat at each lay
       REAL    OMEGA2LAY(MAXLAY,MXCHAN) ! single scat at each lay              

C      Boundary pressure levels
       COMMON /COMLEV/ PLEV
       REAL PLEV(MAXLAY+1)
       REAL TEMPRAW(MAXLAY) ! raw input prof layer average temperature             
       REAL   TEMP(MAXLAY) ! prof layer average temperature
       REAL    TAU(MAXLAY,MXCHAN) ! chan layer effective optical depth
       REAL   TAUZ(MAXLAY,MXCHAN) ! chan surface-to-space trans
       REAL TAUSN(MAXLAY,MXCHAN)  ! sun lay OD
       REAL TAUZSN(MAXLAY,MXCHAN) ! sun space-to-surface-to-space OD       

       REAL     DQ         ! amount of cloud perturb
       REAL     DST        ! amount of stemp perturb       
       
       INTEGER  IPROF      ! profile loop counter
       INTEGER  NCHAN         ! # of selected channels
       INTEGER INDCHN(MXCHAN) ! array indices for all channels       
       REAL   FREQ(MXCHAN)    ! chan center frequency

       INTEGER NCHNTE                    ! number of non-LTE channels
       INTEGER CLISTN(MXCNTE)            ! non-LTE channel list
       REAL  COEFN(NNCOEF,MXCNTE)        ! non-LTE coefficients
       REAL CO2TOP                ! top layers CO2 mixing ratio

       LOGICAL  LRHOT         ! force refl therm rho=(1-emis)/pi?
C      for SETEMS
       INTEGER IEMIS       ! already set??? (-1 NO  +1 YES)
       REAL   EMIS(MXCHAN) ! chan surface emissivity
       REAL CEMIS1(MXCHAN) ! chan surface emissivity cloud1
       REAL CRHOS1(MXCHAN) ! chan solar reflectivity cloud1
       REAL CRHOT1(MXCHAN) ! chan thermal reflectivity cloud1
       REAL CEMIS2(MXCHAN) ! chan surface emissivity cloud2
       REAL CRHOS2(MXCHAN) ! chan solar reflectivity cloud2
       REAL CRHOT2(MXCHAN) ! chan thermal reflectivity cloud2
       
       REAL  TSURF         ! surface temperature
       LOGICAL DOSUN       ! do sun calc?
       REAL    SUNFDG      ! fudge for large sun angles
       INTEGER   LBOT             ! bottom layer index number
       REAL BLMULT                ! bottom layer fractional multiplier

       INTEGER ICLD       ! have we already set the cloud param (-1 NO +1 YES)
       
C      for CCPREP cloud1
       INTEGER LCBOT1         ! layer containing cloud bottom
       INTEGER LCTOP1         ! layer containing cloud top
       REAL  CLRB1            ! frac of layer at bottom of cloud clear
       REAL  CLRT1            ! frac of layer at top of cloud clear
       REAL TCBOT1            ! temperature at cloud bottom
       REAL TCTOP1            ! temperature at cloud top
       REAL MASEC1            ! mean cloud view angle secant
       REAL MASUN1            ! mean cloud sun-only angle secant
       REAL CFRCL1(MAXLAY)    ! fraction of cloud in layer
       REAL G_ASY1(MXCHAN)    ! "g" asymmetry
       REAL NEXTO1(MXCHAN)    ! nadir extinction optical depth
       REAL NSCAO1(MXCHAN)    ! nadir scattering optical depth
C
C      for CCPREP cloud2
       INTEGER LCBOT2         ! layer containing cloud bottom
       INTEGER LCTOP2         ! layer containing cloud top
       REAL  CLRB2            ! frac of layer at bottom of cloud clear
       REAL  CLRT2            ! frac of layer at top of cloud clear
       REAL TCBOT2            ! temperature at cloud bottom
       REAL TCTOP2            ! temperature at cloud top
       REAL MASEC2            ! mean cloud view angle secant
       REAL MASUN2            ! mean cloud sun-only angle secant
       REAL CFRCL2(MAXLAY)    ! fraction of cloud in layer
       REAL G_ASY2(MXCHAN)    ! "g" asymmetry
       REAL NEXTO2(MXCHAN)    ! nadir extinction optical depth
       REAL NSCAO2(MXCHAN)    ! nadir scattering optical depth

C      for RDCLDT
       INTEGER MIENPS(NMIETY)            ! number of particle sizes
       REAL  MIEPS(MXMIEA,NMIETY)        ! Mie particle size for table
       REAL MIEABS(MXCHAN,MXMIEA,NMIETY) ! Mie absorption table
       REAL MIEEXT(MXCHAN,MXMIEA,NMIETY) ! Mie extinction table
       REAL MIEASY(MXCHAN,MXMIEA,NMIETY) ! Mie asymmetry table
       
C      for surface
       INTEGER  NEMIS             ! # of emis pts from rtp
c       REAL  PSURF                ! surface pressure
       REAL  FEMIS(MXEMIS)        ! emis freq pts from rtp
       REAL  XEMIS(MXEMIS)        ! emis pts from rtp 
       REAL   XRHO(MXEMIS)        ! reflec pts from rtp
       
C      for RDRTP
       RECORD /RTPPROF/ PROF            ! profile
       RECORD /RTPHEAD/ HEAD            ! header data

C      Basic cloud info
       REAL XCEMI1(MXEMIS)    ! cloud1 emissivity
       REAL XCEMI2(MXEMIS)    ! cloud2 emissivity
       REAL XCRHO1(MXEMIS)    ! cloud1 reflectivity
       REAL XCRHO2(MXEMIS)    ! cloud2 reflectivity
       REAL CFRAC1            ! cloud1(total) fraction of FOV
       REAL CFRAC2            ! cloud2(total) fraction of FOV
       REAL CFRA1X            ! cloud1(exclusively) fraction of FOV
       REAL CFRA2X            ! cloud2(exclusively) fraction of FOV
       REAL CFRA12            ! cloud1+2(both) fraction of FOV
       REAL CNGWA1            ! cloud1 non-gases water
       REAL CNGWA2            ! cloud1 non-gases water
       REAL CPRBO1            ! cloud1 bottom pressure
       REAL CPRBO2            ! cloud2 bottom pressure
       REAL CPRTO1            ! cloud1 top pressure
       REAL CPRTO2            ! cloud2 top pressure
       REAL CPSIZ1            ! cloud1 particle size
       REAL CPSIZ2            ! cloud2 particle size
       REAL CSTMP1            ! cloud1 top/surf temperature
       REAL CSTMP2            ! cloud2 top/surf temperature
       REAL FCLEAR            ! clear (no cloud) fraction of FOV
       REAL TEMPC1            ! cloud1 frac layer (above cloud) mean temp
       REAL TEMPC2            ! cloud2 frac layer (above cloud) mean temp
       INTEGER CTYPE1         ! cloud1 type code number
       INTEGER CTYPE2         ! cloud2 type code number

C      for GETMIE
       INTEGER MIETYP(NMIETY)      ! mie type
       LOGICAL LBLAC1  ! black cloud1? {Mie cloud if false}
       LOGICAL LBLAC2  ! black cloud2? {Mie cloud if false}
       INTEGER INDMI1  ! index in MIETYP for CTYPE1
       INTEGER INDMI2  ! index in MIETYP for CTYPE2
c       INTEGER  IERR1  ! error level of CTYPE1/MIETYP match
c       INTEGER  IERR2  ! error level of CTYPE2/MIETYP match

       REAL SECANG(MAXLAY)        ! local path angle secant
       REAL SECSUN(MAXLAY) ! secant of effective sun local path angle
       REAL SUNFAC         ! sun solid angles times cosine at surface
       REAL SUNCOS         ! cosine of sun zenith angle
       REAL SCOS1          ! cosine of sun zenith angle at layer1       
       REAL DISTES         ! distance of Earth from the sun

       REAL COSDAZ         ! cosine(solazi - satazi) {COS Delta AZimuth}       
       REAL   HSUN(MXCHAN) ! sun radiance (direct from sun)
       REAL RHOSUN(MXCHAN) ! chan reflectivity for sun
       REAL RHOTHR(MXCHAN) ! chan reflectivity for downwelling thermal
       INTEGER LABOVE(MXCHAN) ! chan downwelling thermal layer above
       REAL  COEFF(NFCOEF,MXCHAN)        ! coefs for chan "F" factor
       
c local
C      For clear/cloudy radiances
c       REAL   RAD0         ! radiance no clouds
c       REAL  RADC1         ! radiance cloud1
c       REAL  RADC2         ! radiance cloud2
c       REAL RADC12         ! radiance cloud1+cloud2
c       REAL RPLNCK(MAXLAY) ! layer Planck
c       REAL  TRANL(MAXLAY) ! clear air layer transmittance
c       REAL  TRANZ(MXCHAN) ! clear air layer-to-space transmittance
c       REAL  TRANS(MXCHAN) ! clear air total reflected solar trans
c       REAL RSURFE         ! surface emission
c       REAL RSURFC         ! black cloud surface emission
c       REAL VSTORE(6)      ! temporary storage for various variables
c       REAL C1V3           ! rad constant c1 times freq^3
c       REAL C2V            ! rad constant c2 times freq

       INTEGER IOUNJ       !outpout unit for jacs
       INTEGER J
       
c************************************************************************

         !!! this is SurfTempJacobian
         CALL SetCldDoRT(
     $        RAD, IPROF, HEAD, PROF, INDCHN, NCHAN, FREQ, 0, DQ,
     $    MIETYP, MIENPS, MIEPS, MIEABS, MIEEXT, MIEASY,
     $    DISTES, SUNCOS, SCOS1,
     $    LBLAC1, CTYPE1, CFRAC1, CPSIZ1, CPRTO1, CPRBO1, CNGWA1,
     $    XCEMI1, XCRHO1, CSTMP1, CFRA1X, 
     $    LBLAC2, CTYPE2, CFRAC2, CPSIZ2, CPRTO2, CPRBO2, CNGWA2,
     $    XCEMI2, XCRHO2, CSTMP2, CFRA2X, CFRA12, 
     $        NEMIS, FEMIS, XEMIS, XRHO,
     $    LRHOT, LBOT, INDMI1,INDMI2,
     $    IEMIS, EMIS, RHOSUN, RHOTHR, 
     $                NCHNTE, CLISTN, COEFN, CO2TOP, 
     $                TEMPRAW,TEMP,TAU,TAUZ,TAUSN,TAUZSN,
     $                TSURF+DST,DOSUN, SUNFDG, BLMULT, SECSUN, SECANG, COSDAZ,
     $                SUNFAC,HSUN, LABOVE, COEFF,
     $                -1, FCLEAR, TEMPC1, TEMPC2, 
     $                CEMIS1, CEMIS2, CRHOT1, CRHOT2, CRHOS1, CRHOS2, MASUN1, MASUN2,
     $                LCBOT1, LCTOP1, CLRB1,CLRT1, TCBOT1, TCTOP1, MASEC1, CFRCL1, 
     $                NEXTO1, NSCAO1, G_ASY1, 
     $                LCBOT2, LCTOP2, CLRB2,CLRT2, TCBOT2, TCTOP2, MASEC2, CFRCL2, 
     $                NEXTO2, NSCAO2, G_ASY2,
     $     RAAPLNCK,RASURFE,CLD1EFFOD,CLD2EFFOD,CLD1SUN,CLD2SUN,OMEGA1LAY,OMEGA2LAY,
     $  )
        write(IOUNJ) IPROF,+1
        write(IOUNJ) (1000.0*RAD(J),J=1,NCHAN)

         !!! this is cloud1 amt jac
         CALL SetCldDoRT(
     $        RAD, IPROF, HEAD, PROF, INDCHN, NCHAN, FREQ, 11, DQ,
     $    MIETYP, MIENPS, MIEPS, MIEABS, MIEEXT, MIEASY,
     $    DISTES, SUNCOS, SCOS1,
     $    LBLAC1, CTYPE1, CFRAC1, CPSIZ1, CPRTO1, CPRBO1, CNGWA1,
     $    XCEMI1, XCRHO1, CSTMP1, CFRA1X, 
     $    LBLAC2, CTYPE2, CFRAC2, CPSIZ2, CPRTO2, CPRBO2, CNGWA2,
     $    XCEMI2, XCRHO2, CSTMP2, CFRA2X, CFRA12, 
     $        NEMIS, FEMIS, XEMIS, XRHO,
     $    LRHOT, LBOT, INDMI1,INDMI2,
     $    IEMIS, EMIS, RHOSUN, RHOTHR, 
     $                NCHNTE, CLISTN, COEFN, CO2TOP, 
     $                TEMPRAW,TEMP,TAU,TAUZ,TAUSN,TAUZSN,
     $                TSURF,DOSUN, SUNFDG, BLMULT, SECSUN, SECANG, COSDAZ,
     $                SUNFAC,HSUN, LABOVE, COEFF,
     $                -1, FCLEAR, TEMPC1, TEMPC2, 
     $                CEMIS1, CEMIS2, CRHOT1, CRHOT2, CRHOS1, CRHOS2, MASUN1, MASUN2,
     $                LCBOT1, LCTOP1, CLRB1,CLRT1, TCBOT1, TCTOP1, MASEC1, CFRCL1, 
     $                NEXTO1, NSCAO1, G_ASY1, 
     $                LCBOT2, LCTOP2, CLRB2,CLRT2, TCBOT2, TCTOP2, MASEC2, CFRCL2, 
     $                NEXTO2, NSCAO2, G_ASY2,
     $     RAAPLNCK,RASURFE,CLD1EFFOD,CLD2EFFOD,CLD1SUN,CLD2SUN,OMEGA1LAY,OMEGA2LAY,
     $  )
        write(IOUNJ) IPROF,+11
        write(IOUNJ) (1000.0*RAD(J),J=1,NCHAN)

         !!! this is cloud2 amt jac
         CALL SetCldDoRT(
     $        RAD, IPROF, HEAD, PROF, INDCHN, NCHAN, FREQ, 12, DQ,
     $    MIETYP, MIENPS, MIEPS, MIEABS, MIEEXT, MIEASY,
     $    DISTES, SUNCOS, SCOS1,
     $    LBLAC1, CTYPE1, CFRAC1, CPSIZ1, CPRTO1, CPRBO1, CNGWA1,
     $    XCEMI1, XCRHO1, CSTMP1, CFRA1X, 
     $    LBLAC2, CTYPE2, CFRAC2, CPSIZ2, CPRTO2, CPRBO2, CNGWA2, 
     $    XCEMI2, XCRHO2, CSTMP2, CFRA2X, CFRA12, 
     $        NEMIS, FEMIS, XEMIS, XRHO,
     $    LRHOT, LBOT, INDMI1,INDMI2,
     $    IEMIS, EMIS, RHOSUN, RHOTHR, 
     $                NCHNTE, CLISTN, COEFN, CO2TOP, 
     $                TEMPRAW,TEMP,TAU,TAUZ,TAUSN,TAUZSN,
     $                TSURF,DOSUN, SUNFDG, BLMULT, SECSUN, SECANG, COSDAZ,
     $                SUNFAC,HSUN, LABOVE, COEFF,
     $                -1, FCLEAR, TEMPC1, TEMPC2, 
     $                CEMIS1, CEMIS2, CRHOT1, CRHOT2, CRHOS1, CRHOS2, MASUN1, MASUN2,
     $                LCBOT1, LCTOP1, CLRB1,CLRT1, TCBOT1, TCTOP1, MASEC1, CFRCL1, 
     $                NEXTO1, NSCAO1, G_ASY1, 
     $                LCBOT2, LCTOP2, CLRB2,CLRT2, TCBOT2, TCTOP2, MASEC2, CFRCL2, 
     $                NEXTO2, NSCAO2, G_ASY2,
     $     RAAPLNCK,RASURFE,CLD1EFFOD,CLD2EFFOD,CLD1SUN,CLD2SUN,OMEGA1LAY,OMEGA2LAY,
     $  )
        write(IOUNJ) IPROF,+12
        write(IOUNJ) (1000.0*RAD(J),J=1,NCHAN)

         !!! this is cloud1 sze jac
         CALL SetCldDoRT(
     $        RAD, IPROF, HEAD, PROF, INDCHN, NCHAN, FREQ, 21, DQ,
     $    MIETYP, MIENPS, MIEPS, MIEABS, MIEEXT, MIEASY,
     $    DISTES, SUNCOS, SCOS1,
     $    LBLAC1, CTYPE1, CFRAC1, CPSIZ1, CPRTO1, CPRBO1, CNGWA1,
     $    XCEMI1, XCRHO1, CSTMP1, CFRA1X, 
     $    LBLAC2, CTYPE2, CFRAC2, CPSIZ2, CPRTO2, CPRBO2, CNGWA2,
     $    XCEMI2, XCRHO2, CSTMP2, CFRA2X, CFRA12, 
     $        NEMIS, FEMIS, XEMIS, XRHO,
     $    LRHOT, LBOT, INDMI1,INDMI2,
     $    IEMIS, EMIS, RHOSUN, RHOTHR, 
     $                NCHNTE, CLISTN, COEFN, CO2TOP, 
     $                TEMPRAW,TEMP,TAU,TAUZ,TAUSN,TAUZSN,
     $                TSURF,DOSUN, SUNFDG, BLMULT, SECSUN, SECANG, COSDAZ,
     $                SUNFAC,HSUN, LABOVE, COEFF,
     $                -1, FCLEAR, TEMPC1, TEMPC2, 
     $                CEMIS1, CEMIS2, CRHOT1, CRHOT2, CRHOS1, CRHOS2, MASUN1, MASUN2,
     $                LCBOT1, LCTOP1, CLRB1,CLRT1, TCBOT1, TCTOP1, MASEC1, CFRCL1, 
     $                NEXTO1, NSCAO1, G_ASY1, 
     $                LCBOT2, LCTOP2, CLRB2,CLRT2, TCBOT2, TCTOP2, MASEC2, CFRCL2, 
     $                NEXTO2, NSCAO2, G_ASY2,
     $     RAAPLNCK,RASURFE,CLD1EFFOD,CLD2EFFOD,CLD1SUN,CLD2SUN,OMEGA1LAY,OMEGA2LAY,
     $  )
        write(IOUNJ) IPROF,+21
        write(IOUNJ) (1000.0*RAD(J),J=1,NCHAN)

         !!! this is cloud2 amt jac
         CALL SetCldDoRT(
     $        RAD, IPROF, HEAD, PROF, INDCHN, NCHAN, FREQ, 22, DQ,
     $    MIETYP, MIENPS, MIEPS, MIEABS, MIEEXT, MIEASY,
     $    DISTES, SUNCOS, SCOS1,
     $    LBLAC1, CTYPE1, CFRAC1, CPSIZ1, CPRTO1, CPRBO1, CNGWA1,
     $    XCEMI1, XCRHO1, CSTMP1, CFRA1X, 
     $    LBLAC2, CTYPE2, CFRAC2, CPSIZ2, CPRTO2, CPRBO2, CNGWA2,
     $    XCEMI2, XCRHO2, CSTMP2, CFRA2X, CFRA12, 
     $        NEMIS, FEMIS, XEMIS, XRHO,
     $    LRHOT, LBOT, INDMI1,INDMI2,
     $    IEMIS, EMIS, RHOSUN, RHOTHR, 
     $                NCHNTE, CLISTN, COEFN, CO2TOP, 
     $                TEMPRAW,TEMP,TAU,TAUZ,TAUSN,TAUZSN,
     $                TSURF,DOSUN, SUNFDG, BLMULT, SECSUN, SECANG, COSDAZ,
     $                SUNFAC,HSUN, LABOVE, COEFF,
     $                -1, FCLEAR, TEMPC1, TEMPC2, 
     $                CEMIS1, CEMIS2, CRHOT1, CRHOT2, CRHOS1, CRHOS2,  MASUN1, MASUN2,
     $                LCBOT1, LCTOP1, CLRB1,CLRT1, TCBOT1, TCTOP1, MASEC1, CFRCL1, 
     $                NEXTO1, NSCAO1, G_ASY1, 
     $                LCBOT2, LCTOP2, CLRB2,CLRT2, TCBOT2, TCTOP2, MASEC2, CFRCL2, 
     $                NEXTO2, NSCAO2, G_ASY2,
     $     RAAPLNCK,RASURFE,CLD1EFFOD,CLD2EFFOD,CLD1SUN,CLD2SUN,OMEGA1LAY,OMEGA2LAY,
     $  )
        write(IOUNJ) IPROF,+22
        write(IOUNJ) (1000.0*RAD(J),J=1,NCHAN)

c************************************************************************
       
       RETURN
       END
       
