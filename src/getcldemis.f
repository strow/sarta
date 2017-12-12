
       SUBROUTINE GetCldEms(
     $        IPROF, HEAD, PROF,
     $    LBLAC1, CTYPE1, CFRAC1, CPSIZ1, CPRTO1, CPRBO1, CNGWA1,
     $    XCEMI1, XCRHO1, CSTMP1,
     $    LBLAC2, CTYPE2, CFRAC2, CPSIZ2, CPRTO2, CPRBO2, CNGWA2,
     $    XCEMI2, XCRHO2, CSTMP2, CFRA12, FCLEAR, CFRA1X, CFRA2X,
     $        NCHAN, NEMIS, FREQ, FEMIS, XEMIS, XRHO,
     $    LRHOT,
     $    EMIS, RHOSUN, RHOTHR, CEMIS1, CRHOS1, CRHOT1,
     $    CEMIS2, CRHOS2, CRHOT2)

      IMPLICIT NONE

      include 'incFTC.f'
      include 'rtpdefs.f'

C      Boundary pressure levels
       COMMON /COMLEV/ PLEV
       REAL PLEV(MAXLAY+1)
       REAL   TEMP(MAXLAY) ! prof layer average temperature
       REAL PLAY(MAXLAY)   ! layer mean pressure
       
       INTEGER  NCHAN         ! # of selected channels
       REAL   FREQ(MXCHAN)    ! chan center frequency
       LOGICAL  LRHOT         ! force refl therm rho=(1-emis)/pi?
C      for SETEMS
       REAL   EMIS(MXCHAN) ! chan surface emissivity
       REAL RHOSUN(MXCHAN) ! chan reflectivity for sun
       REAL RHOTHR(MXCHAN) ! chan reflectivity for downwelling thermal
       REAL CEMIS1(MXCHAN) ! chan surface emissivity cloud1
       REAL CRHOS1(MXCHAN) ! chan solar reflectivity cloud1
       REAL CRHOT1(MXCHAN) ! chan thermal reflectivity cloud1
       REAL CEMIS2(MXCHAN) ! chan surface emissivity cloud2
       REAL CRHOS2(MXCHAN) ! chan solar reflectivity cloud2
       REAL CRHOT2(MXCHAN) ! chan thermal reflectivity cloud2

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
       INTEGER   LBOT             ! bottom layer index number
       INTEGER  NEMIS             ! # of emis pts
       REAL  PSURF                ! surface pressure
       REAL BLMULT                ! bottom layer fractional multiplier
       REAL  FEMIS(MXEMIS)        ! emis freq pts
       REAL  XEMIS(MXEMIS)        ! emis pts
       REAL   XRHO(MXEMIS)        ! reflec pts
       
C      for RDRTP
       INTEGER  IPROF      ! profile loop counter
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
       INTEGER  IERR1  ! error level of CTYPE1/MIETYP match
       INTEGER  IERR2  ! error level of CTYPE2/MIETYP match

       REAL SECANG(MAXLAY)        ! local path angle secant
       REAL SECSUN(MAXLAY) ! secant of effective sun local path angle
       REAL SUNFAC         ! sun solid angles times cosine at surface
       REAL SUNCOS         ! cosine of sun zenith angle
       REAL DISTES         ! distance of Earth from the sun
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>       
C

C      Get basic cloud parameters from input RTP
       CALL GETCLD( IPROF, HEAD, PROF,
     $    LBLAC1, CTYPE1, CFRAC1, CPSIZ1, CPRTO1, CPRBO1, CNGWA1,
     $    XCEMI1, XCRHO1, CSTMP1,
     $    LBLAC2, CTYPE2, CFRAC2, CPSIZ2, CPRTO2, CPRBO2, CNGWA2,
     $    XCEMI2, XCRHO2, CSTMP2, CFRA12, FCLEAR, CFRA1X, CFRA2X )
       print *,'sergio getcld ',IPROF,CTYPE1, CFRAC1, CPSIZ1, CPRTO1,
     $                          CPRBO1, CNGWA1,CFRA1X     

C      ---------------------------------------------------
C      Set the emissivity & reflectivity for every channel
C      ---------------------------------------------------
       CALL SETEMS( NCHAN, NEMIS, FREQ, FEMIS, XEMIS, XRHO,
     $    XCEMI1, XCRHO1, XCEMI2, XCRHO2, LRHOT,
     $    EMIS, RHOSUN, RHOTHR, CEMIS1, CRHOS1, CRHOT1,
     $    CEMIS2, CRHOS2, CRHOT2) 
C
c       print *,CFRAC1,CFRAC2,CFRA12,LBLAC1,LBLAC2

C      Check and prepare (top) cloud1
       IF (CFRAC1 .GT. 0.0) THEN
          IF (LBLAC1) THEN
             CALL BKPREP(IPROF, 1, CTYPE1, CFRAC1, CPRTO1,
     $          LBOT, PSURF, PLEV, PLAY, TEMP, LCTOP1, TCTOP1,
     $          TEMPC1, CLRT1)
             IF (CSTMP1 .GT. 0.0) TCTOP1=CSTMP1
          ELSE
C            Determine which lookup table to use
             CALL GETMIE(CTYPE1,MIETYP,INDMI1,IERR1)
C            Prepare selected lookup table for given cpsize
             CALL CCPREP( NCHAN, LBOT, INDMI1, MIENPS,
     $          CNGWA1, CPSIZ1, CPRTO1, CPRBO1, PLEV, TEMP, SECANG,
     $          SECSUN, MIEPS, MIEABS, MIEEXT, MIEASY, LCBOT1, LCTOP1,
     $          CLRB1, CLRT1, TCBOT1, TCTOP1, MASEC1, MASUN1,
     $          CFRCL1, G_ASY1, NEXTO1, NSCAO1 )
          ENDIF
       ENDIF

C      Check and prepare (bottom) cloud2
       IF (CFRAC2 .GT. 0.0) THEN
          IF (LBLAC2) THEN
             CALL BKPREP(IPROF, 2, CTYPE2, CFRAC2, CPRTO2,
     $          LBOT, PSURF, PLEV, PLAY, TEMP, LCTOP2, TCTOP2,
     $          TEMPC2, CLRT2)
             IF (CSTMP2 .GT. 0.0) TCTOP2=CSTMP2
          ELSE
C            Determine which lookup table to use
             CALL GETMIE(CTYPE2,MIETYP,INDMI2,IERR2)
C            Prepare lookup data for cloud2
             CALL CCPREP( NCHAN, LBOT, INDMI2, MIENPS,
     $          CNGWA2, CPSIZ2, CPRTO2, CPRBO2, PLEV, TEMP, SECANG,
     $          SECSUN, MIEPS, MIEABS, MIEEXT, MIEASY, LCBOT2, LCTOP2,
     $          CLRB2, CLRT2, TCBOT2, TCTOP2, MASEC2, MASUN2,
     $          CFRCL2, G_ASY2, NEXTO2, NSCAO2 )
          ENDIF
       ELSE
C         Safe default for non-existant cloud2
          LCTOP2=1
       ENDIF

cccccccc this block for testing only
c      PROF%udef(19)=TCTOP1
c      PROF%udef(20)=TCTOP2
cccccccccccccccccccccccccccccccccccc

       SUNFAC=SUNCOS*PI*(RADSUN/DISTES)**2
C      Note: PI*(RADSUN/DISTES)^2 = solid angle [steradians] of
C      the sun as seen from Earth for the case DISTES >> RADSUN.

       RETURN
       END
       
