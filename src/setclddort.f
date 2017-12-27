       SUBROUTINE SetCldDoRT(
     $        RAD, IPROF, HEAD, PROF, INDCHN, NCHAN, FREQ, IJACCLD, DQ,
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
     $                TEMPRAW, TEMP, TAU, TAUZ, TAUSN, TAUZSN,
     $                TSURF,DOSUN, SUNFDG, BLMULT, SECSUN, SECANG, COSDAZ,
     $                SUNFAC,HSUN, LABOVE, COEFF,
     $                ICLD, FCLEAR, TEMPC1, TEMPC2, 
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

C      Boundary pressure levels
       COMMON /COMLEV/ PLEV
       REAL PLEV(MAXLAY+1)
       REAL TEMPRAW(MAXLAY) ! raw input prof layer average temperature       
       REAL   TEMP(MAXLAY)  ! prof layer average temperature
       REAL    TAU(MAXLAY,MXCHAN) ! chan layer effective optical depth
       REAL   TAUZ(MAXLAY,MXCHAN) ! chan surface-to-space trans
       REAL TAUSN(MAXLAY,MXCHAN) ! sun OD       
       REAL TAUZSN(MAXLAY,MXCHAN) ! sun space-to-surface-to-space OD       
       REAL PLAY(MAXLAY)   ! layer mean pressure

       INTEGER IJACCLD     !cloud perturb = 0 for none,
                           !11,12 for cngwat1,2  21,22 for cpsize1,2
       REAL     DQ         ! amount of cloud perturb
       
       INTEGER  IPROF      ! profile loop counter
       INTEGER  NCHAN         ! # of selected channels
       INTEGER INDCHN(MXCHAN) ! array indices for all channels selected by user      
       REAL   FREQ(MXCHAN)    ! chan center frequency

       INTEGER NCHNTE                    ! number of non-LTE channels
       INTEGER CLISTN(MXCNTE)            ! non-LTE channel list
       REAL  COEFN(NNCOEF,MXCNTE)        ! non-LTE coefficients
       REAL CO2TOP                ! top layers CO2 mixing ratio

       LOGICAL  LRHOT         ! force refl therm rho=(1-emis)/pi?
C      for SETEMS
       INTEGER IEMIS       ! have we already set the chan emiss (-1 NO +1 YES)
       REAL   EMIS(MXCHAN) ! chan surface emissivity
       REAL CEMIS1(MXCHAN) ! chan surface emissivity cloud1
       REAL CRHOS1(MXCHAN) ! chan solar reflectivity cloud1
       REAL CRHOT1(MXCHAN) ! chan thermal reflectivity cloud1
       REAL CEMIS2(MXCHAN) ! chan surface emissivity cloud2
       REAL CRHOS2(MXCHAN) ! chan solar reflectivity cloud2
       REAL CRHOT2(MXCHAN) ! chan thermal reflectivity cloud2
       
       REAL  TSURF         ! surface temperature
       REAL  SUNFDG        ! sun fudge for large sun angles
       LOGICAL DOSUN       ! do sun calc?
       INTEGER   LBOT      ! bottom layer index number
       REAL BLMULT         ! bottom layer fractional multiplier

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
       REAL  PSURF                ! surface pressure
       INTEGER  NEMIS             ! # of emis pts from rtp       
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
       INTEGER  IERR1  ! error level of CTYPE1/MIETYP match
       INTEGER  IERR2  ! error level of CTYPE2/MIETYP match

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
       REAL    RAAPLNCK(MAXLAY,MXCHAN) ! chan radiance at each lay
       REAL    RASURFE(MXCHAN) ! chan radiance at surf
       REAL    CLD1SUN(MAXLAY,MXCHAN)  ! chan solar scat due to cld1 at each lay
       REAL    CLD2SUN(MAXLAY,MXCHAN)  ! chan solar scat due to cld2 at each lay
       REAL    CLD1EFFOD(MXCHAN)       ! chan cld1 effOD
       REAL    CLD2EFFOD(MXCHAN)       ! chan cld2 effOD
       REAL    OMEGA1LAY(MAXLAY,MXCHAN) ! single scat at each lay
       REAL    OMEGA2LAY(MAXLAY,MXCHAN) ! single scat at each lay       

C      For clear/cloudy radiances
       REAL   RAD0         ! radiance no clouds
       REAL  RADC1         ! radiance cloud1
       REAL  RADC2         ! radiance cloud2
       REAL RADC12         ! radiance cloud1+cloud2

       REAL RJUNK1         ! junk/work
       REAL RJUNK2         ! another junk/work

       REAL RPLNCK(MAXLAY) ! layer Planck
       REAL  TRANL(MAXLAY) ! clear air layer transmittance
       REAL  TRANZ(MXCHAN) ! clear air layer-to-space transmittance
       REAL  TRANS(MXCHAN) ! clear air total reflected solar trans
       REAL RSURFE         ! surface emission
       REAL RSURFC         ! black cloud surface emission
       
       REAL VSTORE(6)      ! temporary storage for various variables
       REAL C1V3           ! rad constant c1 times freq^3
       REAL C2V            ! rad constant c2 times freq

c       INTEGER I,L,J
       INTEGER I,L
       
C      for function QIKEXP
       REAL QIKEXP

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>       
C
c       REAL   TAUZ(MAXLAY,MXCHAN) ! chan surface-to-space trans
       L = 1
       DO I = 1,NCHAN
         TAUZ(1,I)   = TAU(1,I)
       END DO
       DO L = 2,MAXLAY
         DO I = 1,NCHAN
	   TAUZ(L,I) = TAUZ(L-1,I) + TAU(L,I)
	 END DO
       END DO

       IF ((DOSUN .EQ. .TRUE.) .AND. (SUNFDG .GT. 1.0001)) THEN
         L = 1
         DO I = 1,NCHAN
           TAUZSN(1,I)   = TAUSN(1,I)*SUNFDG
         END DO
         DO L = 2,MAXLAY
           DO I = 1,NCHAN
  	     TAUZSN(L,I) = TAUZSN(L-1,I) + TAUSN(L,I)*SUNFDG
 	   END DO
         END DO
       ELSE
         L = 1
         DO I = 1,NCHAN
           TAUZSN(1,I)   = TAUSN(1,I)
         END DO
         DO L = 2,MAXLAY
           DO I = 1,NCHAN
  	     TAUZSN(L,I) = TAUZSN(L-1,I) + TAUSN(L,I)
 	   END DO
         END DO       
       END IF

 1234  FORMAT(3I,2(F12.4),2(F12.4))
c       DO L = 1,100
cc        print *,'tempraw(z),T(z),tau(z)',L,TEMPRAW(L),TEMP(L),TAU(L,757),TAUZSN(L,757)  !! 900 cm-1
cc        print *,'tempraw(z),T(z),tau(z)',L,TEMPRAW(L),TEMP(L),TAU(L,2333),TAUZSN(L,2333) !! 2616 cm-1
c         print *,L,TEMPRAW(L),TEMP(L),TAU(L,2333),TAUSN(L,2333) !! 2616 cm-1                 
c       END DO
       
C      Get basic cloud parameters from input RTP
       IF (ICLD .LT. 0) THEN
         CALL GETCLD( IPROF, HEAD, PROF,
     $     LBLAC1, CTYPE1, CFRAC1, CPSIZ1, CPRTO1, CPRBO1, CNGWA1,
     $     XCEMI1, XCRHO1, CSTMP1,
     $     LBLAC2, CTYPE2, CFRAC2, CPSIZ2, CPRTO2, CPRBO2, CNGWA2,
     $     XCEMI2, XCRHO2, CSTMP2, CFRA12, FCLEAR, CFRA1X, CFRA2X )
       END IF

       IF (IJACCLD .EQ. 11) THEN
         CNGWA1 = CNGWA1*(1.0+DQ)
       ELSEIF (IJACCLD .EQ. 12) THEN
         CNGWA2 = CNGWA2*(1.0+DQ)
       ELSEIF (IJACCLD .EQ. 21) THEN
         CPSIZ1 = CPSIZ1*(1.0+DQ)
       ELSEIF (IJACCLD .EQ. 22) THEN
         CPSIZ2 = CPSIZ2*(1.0+DQ)
       END IF
       
c       print *,'sergio getcld ',IPROF,CTYPE1, CFRAC1, CPSIZ1, CPRTO1,
c     $                          CPRBO1, CNGWA1,CFRA1X     

C      ---------------------------------------------------
C      Set the emissivity & reflectivity for every channel
C      ---------------------------------------------------
       IF (IEMIS .LT. 0) THEN
c         print *,'setting emiss and planck'
         CALL SETEMS( NCHAN, NEMIS, FREQ, FEMIS, XEMIS, XRHO,
     $     XCEMI1, XCRHO1, XCEMI2, XCRHO2, LRHOT,
     $     EMIS, RHOSUN, RHOTHR, CEMIS1, CRHOS1, CRHOT1,
     $     CEMIS2, CRHOS2, CRHOT2)

         CALL planckemis(NCHAN,LBOT,TEMP,FREQ,EMIS,TSURF,RAAPLNCK,RASURFE)
       END IF

       IF (ICLD .LT. 0) THEN
c         print *,'setting cld'       
C        Check and prepare (top) cloud1
         IF (CFRAC1 .GT. 0.0) THEN
           IF (LBLAC1) THEN
             CALL BKPREP(IPROF, 1, CTYPE1, CFRAC1, CPRTO1,
     $          LBOT, PSURF, PLEV, PLAY, TEMP, 
     $          LCTOP1, TCTOP1, TEMPC1, CLRT1)    !! output
             IF (CSTMP1 .GT. 0.0) TCTOP1=CSTMP1   !! output
           ELSE
C            Determine which lookup table to use
             CALL GETMIE(CTYPE1,MIETYP,INDMI1,IERR1)
C            Prepare selected lookup table for given cpsize
             CALL CCPREP( NCHAN, LBOT, INDMI1, MIENPS,
     $          CNGWA1, CPSIZ1, CPRTO1, CPRBO1, PLEV, TEMP, SECANG,
     $          SECSUN, MIEPS, MIEABS, MIEEXT, MIEASY,
     $          LCBOT1, LCTOP1,                               !! output
     $          CLRB1, CLRT1, TCBOT1, TCTOP1, MASEC1, MASUN1, !! output
     $          CFRCL1, G_ASY1, NEXTO1, NSCAO1 )              !! output
           ENDIF
         ENDIF

C        Check and prepare (bottom) cloud2
         IF (CFRAC2 .GT. 0.0) THEN
           IF (LBLAC2) THEN
             CALL BKPREP(IPROF, 2, CTYPE2, CFRAC2, CPRTO2,
     $          LBOT, PSURF, PLEV, PLAY, TEMP, 
     $          LCTOP2, TCTOP2, TEMPC2, CLRT2)               !! output
             IF (CSTMP2 .GT. 0.0) TCTOP2=CSTMP2              !! output
           ELSE
C            Determine which lookup table to use
             CALL GETMIE(CTYPE2,MIETYP,INDMI2,IERR2)
C            Prepare lookup data for cloud2
             CALL CCPREP( NCHAN, LBOT, INDMI2, MIENPS,
     $          CNGWA2, CPSIZ2, CPRTO2, CPRBO2, PLEV, TEMP, SECANG,
     $          SECSUN, MIEPS, MIEABS, MIEEXT, MIEASY,
     $          LCBOT2, LCTOP2,                                !! output
     $          CLRB2, CLRT2, TCBOT2, TCTOP2, MASEC2, MASUN2,  !! output
     $          CFRCL2, G_ASY2, NEXTO2, NSCAO2 )               !! output
c             print *,NCHAN,LBOT,INDMI2,MIENPS,CNGWA2, CPSIZ2, CPRTO2, CPRBO2,
c     $               G_ASY2(1291),NEXTO2(1291),NSCAO2(1291)
c            print *,'ABC=',LCBOT2, LCTOP2,CLRB2, CLRT2, TCBOT2, TCTOP2, MASEC2, MASUN2
c            print *,MIEPS(1,1),MIEPS(1,2),MIEPS(1,3)
c            print *,MIEABS(1,1,1),MIEABS(1,1,2),MIEABS(1,1,3)
c            print *,MIEEXT(1,1,1),MIEEXT(1,1,2),MIEEXT(1,1,3)
c            print *,MIEASY(1,1,1),MIEASY(1,1,2),MIEASY(1,1,3)
            ENDIF
         ELSE
C           Safe default for non-existant cloud2
            LCTOP2=1                                          !! output
	 ENDIF
	 
       ENDIF

cccccccc this block for testing only
c      PROF%udef(19)=TCTOP1
c      PROF%udef(20)=TCTOP2
cccccccccccccccccccccccccccccccccccc

       SUNFAC=SUNCOS*PI*(RADSUN/DISTES)**2
C      Note: PI*(RADSUN/DISTES)^2 = solid angle [steradians] of
C      the sun as seen from Earth for the case DISTES >> RADSUN.

c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
              
         CALL planckemis(NCHAN,LBOT,TEMP,FREQ,EMIS,TSURF,RAAPLNCK,RASURFE)
	 
C      ----------------------
C      Loop over the channels
C      ----------------------
       DO I=1,NCHAN

C         Radiation constants for current channel
          C1V3=C1*(FREQ(I)**3)
          C2V=C2*FREQ(I)

C         Calculate Planck & clear airs trans for full layers
          DO L=1,LBOT-1
             RPLNCK(L)=C1V3/( EXP( C2V/TEMP(L) ) - 1.0 )
             TRANL(L)=QIKEXP( -TAU(L,I) )
          ENDDO
C         Note: TEMP(LBOT) already adjusted for bottom fractional layer
          RPLNCK(LBOT)=C1V3/( EXP( C2V/TEMP(LBOT) ) - 1.0 )

C         Calculate clear airs trans for bottom fractional layer
          RJUNK1=-TAU(LBOT,I)*BLMULT
          TRANL(LBOT)=QIKEXP( RJUNK1 )
          TRANL(LBOT)=QIKEXP( RJUNK1 )
          TRANZ(I)=QIKEXP( RJUNK1 - TAUZ(LBOT-1,I) )
          TRANS(I)=QIKEXP( BLMULT*(TAUZSN(LBOT-1,I)-TAUZSN(LBOT,I)) -
     $       TAUZSN(LBOT-1,I) )

C         Planck for surface
          RSURFE=EMIS(I)*C1V3/( EXP( C2V/TSURF ) - 1.0 )

C         Calculate clear radiance
          IF (FCLEAR .GT. 0.0) THEN
             CALL CALRAD0( DOSUN, I, LBOT, RPLNCK, RSURFE, SECANG,
     $       TRANL, TRANZ, SUNFAC, HSUN, TRANS, RHOSUN,
     $       RHOTHR, LABOVE, COEFF, RAD0 )
          ELSE
             RAD0=0.0
          ENDIF

C         Store original values
          VSTORE(1)=TRANL(LCTOP2)
          VSTORE(2)=TRANZ(I)
          VSTORE(3)=TRANS(I)
          VSTORE(4)=RHOTHR(I)
          VSTORE(5)=RHOSUN(I)
          VSTORE(6)=RPLNCK(LCTOP2)
c	  IF (I .EQ. 1291) print *,(VSTORE(J),J=1,6)
	  
C         Updates for new surface if bottom cloud2 is black
          IF (CFRAC2 .GT. 0.0 .AND. LBLAC2) THEN
             RJUNK1=-TAU(LCTOP2,I)*CLRT2
             TRANL(LCTOP2)=QIKEXP( RJUNK1 )
             TRANZ(I)=QIKEXP( RJUNK1 - TAUZ(LCTOP2-1,I) )
             TRANS(I)=QIKEXP( CLRT2*(TAUZSN(LCTOP2-1,I)-
     $          TAUZSN(LCTOP2,I)) - TAUZSN(LCTOP2-1,I) )
             RSURFC=CEMIS2(I)*C1V3/( EXP( C2V/TCTOP2 ) - 1.0 )
             RHOTHR(I)=CRHOT2(I)
             RHOSUN(I)=CRHOS2(I)
             RPLNCK(LCTOP2)=C1V3/( EXP( C2V/TEMPC2 ) - 1.0 )
c             RSURFC=C1V3/( EXP( C2V/TEMPC2 ) - 1.0 )
          ENDIF

C         Calculate bottom cloud2 radiance
          IF (CFRA2X .GT. 0.0) THEN
             IF (LBLAC2) THEN
                CALL CALRAD0( DOSUN, I, LCTOP2, RPLNCK, RSURFC, SECANG,
     $          TRANL, TRANZ, SUNFAC, HSUN, TRANS, RHOSUN,
     $          RHOTHR, LABOVE, COEFF, RADC2 )
             ELSE
c	        IF (I .EQ. 1291) THEN		
c  	          print *,DOSUN,I,LBOT
c		  print *,TAU(50,I),TRANL(LCTOP2),TRANZ(I)
c		  print *,RPLNCK(50),RSURFE,SECANG(50)
c		  print *,SUNFAC,HSUN(I),TRANS(I),RHOSUN(I),RHOTHR(I)
c		  print *,LABOVE(I),CFRCL2(50),COEFF(1,1)
c		  print *,'XYZ=',MASEC2,MASUN2,COSDAZ
c		  print *,NEXTO2(I),NSCAO2(I),G_ASY2(I),LCTOP2,LCBOT2
c		END IF
               CALL CALRAD1( DOSUN, I, LBOT, RPLNCK, RSURFE, SECANG,
     $          TAU, TRANL, TRANZ, SUNFAC, HSUN, TRANS, RHOSUN,
     $          RHOTHR, LABOVE, COEFF, CFRCL2, MASEC2, MASUN2, COSDAZ,
     $          NEXTO2, NSCAO2, G_ASY2, LCTOP2, LCBOT2,
     $          CLD2EFFOD, CLD2SUN,
     $          RADC2)
                
             ENDIF
          ELSE
             RADC2=0.0
          ENDIF

C         Calculate combined cloud1+cloud2 radiance
          IF (CFRA12 .GT. 0.0) THEN
             IF (LBLAC2) THEN
                CALL CALRAD1( DOSUN, I, LCTOP2, RPLNCK, RSURFC, SECANG,
     $          TAU, TRANL, TRANZ, SUNFAC, HSUN, TRANS, RHOSUN,
     $          RHOTHR, LABOVE, COEFF, CFRCL1, MASEC1, MASUN1, COSDAZ,
     $          NEXTO1, NSCAO1, G_ASY1, LCTOP1, LCBOT1, 
     $          CLD1EFFOD, CLD1SUN,
     $          RADC12)
             ELSE
                CALL CALRAD2( DOSUN, I, LBOT, RPLNCK, RSURFE, SECANG,
     $          TAU, TRANL, TRANZ, SUNFAC, HSUN, TRANS, RHOSUN,
     $          RHOTHR, LABOVE, COEFF, CFRCL1, MASEC1, MASUN1, NEXTO1,
     $          NSCAO1, G_ASY1, LCTOP1, LCBOT1, CFRCL2, MASEC2, MASUN2,
     $          COSDAZ, NEXTO2, NSCAO2, G_ASY2, LCTOP2, LCBOT2, 
     $          CLD1EFFOD,CLD2EFFOD,CLD1SUN,CLD2SUN,
     $          RADC12)
             ENDIF
          ELSE
             RADC12=0.0
          ENDIF

C         Restore original values
          TRANL(LCTOP2)=VSTORE(1)
          TRANZ(I)=VSTORE(2)
          TRANS(I)=VSTORE(3)
          RHOTHR(I)=VSTORE(4)
          RHOSUN(I)=VSTORE(5)
          RPLNCK(LCTOP2)=VSTORE(6)
C         Updates for new surface if top cloud1 is black
          IF (CFRAC1 .GT. 0.0 .AND. LBLAC1) THEN
             RJUNK1=-TAU(LCTOP1,I)*CLRT1
             TRANL(LCTOP1)=QIKEXP( RJUNK1 )
             TRANZ(I)=QIKEXP( RJUNK1 - TAUZ(LCTOP1-1,I) )
             TRANS(I)=QIKEXP( CLRT1*(TAUZSN(LCTOP1-1,I)-
     $          TAUZSN(LCTOP1,I)) - TAUZSN(LCTOP1-1,I) )
             RSURFC=CEMIS1(I)*C1V3/( EXP( C2V/TCTOP1 ) - 1.0 )
             RHOTHR(I)=CRHOT1(I)
             RHOSUN(I)=CRHOS1(I)
             RPLNCK(LCTOP1)=C1V3/( EXP( C2V/TEMPC1 ) - 1.0 )
c             RSURFC=C1V3/( EXP( C2V/TEMPC1 ) - 1.0 )
          ENDIF

C         Calculate top cloud1 radiance
          IF (CFRA1X .GT. 0.0) THEN
             IF (LBLAC1) THEN
                CALL CALRAD0( DOSUN, I, LCTOP1, RPLNCK, RSURFC, SECANG,
     $          TRANL, TRANZ, SUNFAC, HSUN, TRANS, RHOSUN,
     $          RHOTHR, LABOVE, COEFF, RADC1 )
             ELSE
                CALL CALRAD1( DOSUN, I, LBOT, RPLNCK, RSURFE, SECANG,
     $          TAU, TRANL, TRANZ, SUNFAC, HSUN, TRANS, RHOSUN,
     $          RHOTHR, LABOVE, COEFF, CFRCL1, MASEC1, MASUN1, COSDAZ,
     $          NEXTO1, NSCAO1, G_ASY1, LCTOP1, LCBOT1, 
     $          CLD1EFFOD, CLD1SUN,
     $          RADC1)
             ENDIF
          ELSE
             RADC1=0.0
          ENDIF

C         Total the clear & various cloudy radiances
          RAD(I)=RAD0*FCLEAR + RADC1*CFRA1X + RADC2*CFRA2X +
     $       RADC12*CFRA12

ccc this block for testing
c       IF (I .EQ. 1291) THEN
ccc         print *,'chan1291 : iPROF,rad0,radc1,radc2,radc12,FINAL=',
ccc     $      IPROF,RAD0,RADC1,RADC2,RADC12,RAD(I)
c         print *,'1291:I,TS,rF=',IPROF,TSURF,RAD(I)
cc         print *,'1291:I,CLR,C1,C2,C12,TS,rad0,radC1,radC2,radC12,rF=',
cc     $      IPROF,FCLEAR,CFRA1X,CFRA2X,CFRA12,
cc     $      TSURF,rad0,radC1,radC2,radC12,RAD(I)
ccc         PRINT *,'CLOUD1 emis,temp = ',CEMIS1(I),TCTOP1
ccc         PRINT *,'CLOUD2 emis,temp = ',CEMIS2(I),TCTOP2
c       endif
ccc this block is for testing

       ENDDO ! channels


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C      -----------------
C      Calculate non-LTE
C      -----------------
C      comment: the nonLTE calculation does not consider cloud effects,
C      but clouds are generally below the altitude where nonLTE occurs.
       IF (DOSUN) THEN
          CALL CALNTE ( INDCHN, TEMP, SUNCOS, SCOS1, SECANG(1),
     $       NCHNTE, CLISTN, COEFN, CO2TOP, RAD )
       ENDIF

       RETURN
       END
       
