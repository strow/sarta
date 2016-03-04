C=======================================================================
C=======================================================================
C
C    University of Maryland Baltimore Country (UMBC)
C
C    AIRS
C
C    SUMCOF
C
!F77====================================================================


!ROUTINE NAME:
C    SUMCOF


!ABSTRACT:
C    Do a weighted sum of two coef/etc databases ("A" and "B")


!CALL PROTOCOL
C       SUMCOF( NCHAN, NCHNTE,  NCHN1,  NCHN2,  NCHN3,  NCHN4,  NCHN5,
C  $    NCHN6,  NCHN7, NCHCO2, NCHN2O, NCHSO2, NCHHNO, NCHH2O,   YOFF,
C  $    FREQA,  HSUNA, COEFFA, COEFNA, COEF1A, COEF2A, COEF3A, COEF4A,
C  $   COEF5A, COEF6A, COEF7A, COFCOA, COFN2A, COFSOA, COFHNA, COFH2A,
C  $    FREQB,  HSUNB, COEFFB, COEFNB, COEF1B, COEF2B, COEF3B, COEF4B,
C  $   COEF5B, COEF6B, COEF7B, COFCOB, COFN2B, COFSOB, COFHNB, COFH2B,
C  $     FREQ,   HSUN,  COEFF,  COEFN,  COEF1,  COEF2,  COEF3,  COEF4,
C  $    COEF5,  COEF6,  COEF7, COFCO2, COFN2O, COFSO2, COFHNO, COFH2O)


!INPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    INTEGER   NCHAN   number of channels          none
C    INTEGER   NCHNTE  non-LTE number of channels  none
C    INTEGER   NCHN1   set1 number of channels     none
C    INTEGER   NCHN2   set2 number of channels     none
C    INTEGER   NCHN3   set3 number of channels     none
C    INTEGER   NCHN4   set4 number of channels     none
C    INTEGER   NCHN5   set5 number of channels     none
C    INTEGER   NCHN6   set6 number of channels     none
C    INTEGER   NCHN7   set7 number of channels     none
C    INTEGER   NCHCO2  number of CO2 channels      none
C    INTEGER   NCHN2O  number of N2O channels      none
C    INTEGER   NCHSO2  number of SO2 channels      none
C    INTEGER   NCHHNO  number of HNO3 channels     none
C    INTEGER   NCHH2O  number of OPTRAN H2O chans  none
C    REAL      YOFF    yoffset used to calc freqs  um
C    REAL arr  FREQA   channel frequencies         cm^-1
C    REAL arr  HSUNA   solar irradiance            mW/m^2/cm^-1 ?
C    REAL arr  COEFFA  refl thermal "F" coefs      various
C    REAL arr  COEFNA  nonLTE coefs                various
C    REAL arr  COEF1A  set1 fast trans coefs       various
C    REAL arr  COEF2A  set2 fast trans coefs       various
C    REAL arr  COEF3A  set3 fast trans coefs       various
C    REAL arr  COEF4A  set4 fast trans coefs       various
C    REAL arr  COEF5A  set5 fast trans coefs       various
C    REAL arr  COEF6A  set6 fast trans coefs       various
C    REAL arr  COEF7A  set7 fast trans coefs       various
C    REAL arr  COFCOA  CO2 perturbation coefs      various
C    REAL arr  COFN2A  N2O perturbation coefs      various
C    REAL arr  COFSOA  SO2 perturbation coefs      various
C    REAL arr  COFHNA  HNO3 perturbation coefs     various
C    REAL arr  COFH2A  OPTRAN H2O coefs            various
C
C    REAL arr  FREQB   channel frequencies         cm^-1
C    REAL arr  HSUNB   solar irradiance            mW/m^2/cm^-1 ?
C    REAL arr  COEFFB  refl thermal "F" coefs      various
C    REAL arr  COEFNB  nonLTE coefs                various
C    REAL arr  COEF1B  set1 fast trans coefs       various
C    REAL arr  COEF2B  set2 fast trans coefs       various
C    REAL arr  COEF3B  set3 fast trans coefs       various
C    REAL arr  COEF4B  set4 fast trans coefs       various
C    REAL arr  COEF5B  set5 fast trans coefs       various
C    REAL arr  COEF6B  set6 fast trans coefs       various
C    REAL arr  COEF7B  set7 fast trans coefs       various
C    REAL arr  COFCOB  CO2 perturbation coefs      various
C    REAL arr  COFN2B  N2O perturbation coefs      various
C    REAL arr  COFSOB  SO2 perturbation coefs      various
C    REAL arr  COFHNB  HNO3 perturbation coefs     various
C    REAL arr  COFH2B  OPTRAN H2O coefs            various


!INPUT/OUTPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    REAL arr  FREQ    channel frequencies         cm^-1
C    REAL arr  HSUN    solar irradiance            mW/m^2/cm^-1 ?
C    REAL arr  COEFF   refl thermal "F" coefs      various
C    REAL arr  COEFN   nonLTE coefs                various
C    REAL arr  COEF1   set1 fast trans coefs       various
C    REAL arr  COEF2   set2 fast trans coefs       various
C    REAL arr  COEF3   set3 fast trans coefs       various
C    REAL arr  COEF4   set4 fast trans coefs       various
C    REAL arr  COEF5   set5 fast trans coefs       various
C    REAL arr  COEF6   set6 fast trans coefs       various
C    REAL arr  COEF7   set7 fast trans coefs       various
C    REAL arr  COFCO2  CO2 perturbation coefs      various
C    REAL arr  COFN2O  N2O perturbation coefs      various
C    REAL arr  COFSO2  SO2 perturbation coefs      various
C    REAL arr  COFHNO  HNO3 perturbation coefs     various
C    REAL arr  COFH2O  OPTRAN H2O coefs            various


!OUTPUT PARAMETERS:
C    none


!RETURN VALUES:
C    none


!PARENT(S):
C    USEFAST


!ROUTINES CALLED:
C    none


!FILES ACCESSED:
C    incFTC.f : include file of parameter statements accessed during
C       compilation only.
C    unit IOUN : input text file of tuning adjustments.


!COMMON BLOCKS
C    none


!DESCRIPTION:
C    May 2008 version of SARTA v1.07x code by L.Strow/S.Hannon.
C
C    The routine takes a pair of fast transmittace coefficients
C    and sums them.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date        Programmer     Comments
C    ----------- -------------- ----------------------------------------
C    09 May 2008 Scott Hannon   Created


!END====================================================================

C      =================================================================
       SUBROUTINE SUMCOF(NCHAN, NCHNTE, NCHN1, NCHN2, NCHN3,NCHN4,NCHN5,
     $    NCHN6,  NCHN7, NCHCO2, NCHN2O, NCHSO2, NCHHNO, NCHH2O,   YOFF,
     $    FREQA,  HSUNA, COEFFA, COEFNA, COEF1A, COEF2A, COEF3A, COEF4A,
     $   COEF5A, COEF6A, COEF7A, COFCOA, COFN2A, COFSOA, COFHNA, COFH2A,
     $    FREQB,  HSUNB, COEFFB, COEFNB, COEF1B, COEF2B, COEF3B, COEF4B,
     $   COEF5B, COEF6B, COEF7B, COFCOB, COFN2B, COFSOB, COFHNB, COFH2B,
     $     FREQ,   HSUN,  COEFF,  COEFN,  COEF1,  COEF2,  COEF3,  COEF4,
     $    COEF5,  COEF6,  COEF7, COFCO2, COFN2O, COFSO2, COFHNO, COFH2O)

C      =================================================================


C-----------------------------------------------------------------------
C      IMPLICIT NONE
C-----------------------------------------------------------------------
       IMPLICIT NONE


C-----------------------------------------------------------------------
C      INCLUDE FILES
C-----------------------------------------------------------------------
       include 'incFTC.f'


C-----------------------------------------------------------------------
C      EXTERNAL FUNCTIONS
C-----------------------------------------------------------------------
C      none


C-----------------------------------------------------------------------
C      ARGUMENTS
C-----------------------------------------------------------------------
C      INPUT
       INTEGER  NCHAN  ! number of channels in use
       INTEGER NCHNTE  ! number of nonLTE channels in use
       INTEGER  NCHN1  ! number of set1 channels in use
       INTEGER  NCHN2  ! number of set2 channels in use
       INTEGER  NCHN3  ! number of set3 channels in use
       INTEGER  NCHN4  ! number of set4 channels in use
       INTEGER  NCHN5  ! number of set5 channels in use
       INTEGER  NCHN6  ! number of set6 channels in use
       INTEGER  NCHN7  ! number of set7 channels in use
       INTEGER NCHCO2  ! number of CO2 perturbation channels in use
       INTEGER NCHN2O  ! number of N2O perturbation channels in use
       INTEGER NCHSO2  ! number of SO2 perturbation channels in use
       INTEGER NCHHNO  ! number of HNO3 perturbation channels in use
       INTEGER NCHH2O  ! number of OPTRAN H2O channels in use
C
       REAL  YOFF      ! yoffset used to calculate channel freqs
C
C      Database "A"
       REAL  FREQA(MXCHAN)               ! channel frequencies
       REAL  HSUNA(MXCHAN)               ! solar irradiance
       REAL COEFFA(NFCOEF,MXCHAN)        ! reflected thermal "F" coefs
       REAL COEFNA(NNCOEF,MXCNTE)        ! nonLTE coefs
       REAL COEF1A(N1COEF,MAXLAY,MXCHN1) ! set1 coefs
       REAL COEF2A(N2COEF,MAXLAY,MXCHN2) ! set2 coefs
       REAL COEF3A(N3COEF,MAXLAY,MXCHN3) ! set3 coefs
       REAL COEF4A(N4COEF,MAXLAY,MXCHN4) ! set4 coefs
       REAL COEF5A(N5COEF,MAXLAY,MXCHN5) ! set5 coefs
       REAL COEF6A(N6COEF,MAXLAY,MXCHN6) ! set6 coefs
       REAL COEF7A(N7COEF,MAXLAY,MXCHN7) ! set7 coefs
       REAL COFCOA(  NCO2,MAXLAY,MXCHNC) ! CO2 pert coefs
       REAL COFN2A(  NN2O,MAXLAY,MXCHNN) ! N2O pert coefs
       REAL COFSOA(  NSO2,MAXLAY,MXCHNS) ! SO2 pert coefs
       REAL COFHNA( NHNO3,MAXLAY,MXCHNH) ! HNO3 pert coefs
       REAL COFH2A(  NH2O,MXOWLY,MXCHNW) ! H2O OPTRAN coefs
C
C      Database "B"
       REAL  FREQB(MXCHAN)               ! channel frequencies
       REAL  HSUNB(MXCHAN)               ! solar irradiance
       REAL COEFFB(NFCOEF,MXCHAN)        ! reflected thermal "F" coefs
       REAL COEFNB(NNCOEF,MXCNTE)        ! nonLTE coefs
       REAL COEF1B(N1COEF,MAXLAY,MXCHN1) ! set1 coefs
       REAL COEF2B(N2COEF,MAXLAY,MXCHN2) ! set2 coefs
       REAL COEF3B(N3COEF,MAXLAY,MXCHN3) ! set3 coefs
       REAL COEF4B(N4COEF,MAXLAY,MXCHN4) ! set4 coefs
       REAL COEF5B(N5COEF,MAXLAY,MXCHN5) ! set5 coefs
       REAL COEF6B(N6COEF,MAXLAY,MXCHN6) ! set6 coefs
       REAL COEF7B(N7COEF,MAXLAY,MXCHN7) ! set7 coefs
       REAL COFCOB(  NCO2,MAXLAY,MXCHNC) ! CO2 pert coefs
       REAL COFN2B(  NN2O,MAXLAY,MXCHNN) ! N2O pert coefs
       REAL COFSOB(  NSO2,MAXLAY,MXCHNS) ! SO2 pert coefs
       REAL COFHNB( NHNO3,MAXLAY,MXCHNH) ! HNO3 pert coefs
       REAL COFH2B(  NH2O,MXOWLY,MXCHNW) ! H2O OPTRAN coefs


C      INPUT/OUTPUT
C      Weighted sum database to be use for calcs
       REAL   FREQ(MXCHAN)               ! channel frequencies
       REAL   HSUN(MXCHAN)               ! solar irradiance
       REAL  COEFF(NFCOEF,MXCHAN)        ! reflected thermal "F" coefs
       REAL  COEFN(NNCOEF,MXCNTE)        ! nonLTE coefs
       REAL  COEF1(N1COEF,MAXLAY,MXCHN1) ! set1 coefs
       REAL  COEF2(N2COEF,MAXLAY,MXCHN2) ! set2 coefs
       REAL  COEF3(N3COEF,MAXLAY,MXCHN3) ! set3 coefs
       REAL  COEF4(N4COEF,MAXLAY,MXCHN4) ! set4 coefs
       REAL  COEF5(N5COEF,MAXLAY,MXCHN5) ! set5 coefs
       REAL  COEF6(N6COEF,MAXLAY,MXCHN6) ! set6 coefs
       REAL  COEF7(N7COEF,MAXLAY,MXCHN7) ! set7 coefs
       REAL COFCO2(  NCO2,MAXLAY,MXCHNC) ! CO2 pert coefs
       REAL COFN2O(  NN2O,MAXLAY,MXCHNN) ! N2O pert coefs
       REAL COFSO2(  NSO2,MAXLAY,MXCHNS) ! SO2 pert coefs
       REAL COFHNO( NHNO3,MAXLAY,MXCHNH) ! HNO3 pert coefs
       REAL COFH2O(  NH2O,MXOWLY,MXCHNW) ! H2O OPTRAN coefs

C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER      I  ! generic looping variable
       INTEGER      J  ! generic looping variable
       INTEGER      L  ! generic looping variable
       REAL   WGTA     ! fractional weight
       REAL   WGTB     ! fractional weight
C

C-----------------------------------------------------------------------
C      SAVE STATEMENTS
C-----------------------------------------------------------------------
C      none


C***********************************************************************
C***********************************************************************
C                    EXECUTABLE CODE
C***********************************************************************
C***********************************************************************
C
C      Calc summing weights
       WGTA=(YOFF - YOFFB)/(YOFFA - YOFFB)
       WGTB=(YOFF - YOFFA)/(YOFFB - YOFFA)
C
C      Set 1
       DO I=1,NCHN1
          DO L=1,MAXLAY
             DO J=1,N1COEF
                COEF1(J,L,I)=COEF1A(J,L,I)*WGTA + COEF1B(J,L,I)*WGTB
             ENDDO
          ENDDO
       ENDDO

C      Set 2
       DO I=1,NCHN2
          DO L=1,MAXLAY
             DO J=1,N2COEF
                COEF2(J,L,I)=COEF2A(J,L,I)*WGTA + COEF2B(J,L,I)*WGTB
             ENDDO
          ENDDO
       ENDDO

C      Set 3
       DO I=1,NCHN3
          DO L=1,MAXLAY
             DO J=1,N3COEF
                COEF3(J,L,I)=COEF3A(J,L,I)*WGTA + COEF3B(J,L,I)*WGTB
             ENDDO
          ENDDO
       ENDDO

C      Set 4
       DO I=1,NCHN4
          DO L=1,MAXLAY
             DO J=1,N4COEF
                COEF4(J,L,I)=COEF4A(J,L,I)*WGTA + COEF4B(J,L,I)*WGTB
             ENDDO
          ENDDO
       ENDDO

C      Set 5
       DO I=1,NCHN5
          DO L=1,MAXLAY
             DO J=1,N5COEF
                COEF5(J,L,I)=COEF5A(J,L,I)*WGTA + COEF5B(J,L,I)*WGTB
             ENDDO
          ENDDO
       ENDDO

C      Set 6
       DO I=1,NCHN6
          DO L=1,MAXLAY
             DO J=1,N6COEF
                COEF6(J,L,I)=COEF6A(J,L,I)*WGTA + COEF6B(J,L,I)*WGTB
             ENDDO
          ENDDO
       ENDDO

C      Set 7
       DO I=1,NCHN7
          DO L=1,MAXLAY
             DO J=1,N7COEF
                COEF7(J,L,I)=COEF7A(J,L,I)*WGTA + COEF7B(J,L,I)*WGTB
             ENDDO
          ENDDO
       ENDDO

C      OPTRAN
       DO I=1,NCHH2O
          DO L=1,MXOWLY
             DO J=1,NH2O
                COFH2O(J,L,I)=COFH2A(J,L,I)*WGTA + COFH2B(J,L,I)*WGTB
             ENDDO
          ENDDO
       ENDDO

C      CO2 perturbation coefficients
       DO I=1,NCHCO2
          DO L=1,MAXLAY
             DO J=1,NCO2
                COFCO2(J,L,I)=COFCOA(J,L,I)*WGTA + COFCOB(J,L,I)*WGTB
             ENDDO
          ENDDO
       ENDDO

C      N2O perturbation coefficients
       DO I=1,NCHN2O
          DO L=1,MAXLAY
             DO J=1,NN2O
                COFN2O(J,L,I)=COFN2A(J,L,I)*WGTA + COFN2B(J,L,I)*WGTB
             ENDDO
          ENDDO
       ENDDO

C      SO2 perturbation coefficients
       DO I=1,NCHSO2
          DO L=1,MAXLAY
             DO J=1,NSO2
                COFSO2(J,L,I)=COFSOA(J,L,I)*WGTA + COFSOB(J,L,I)*WGTB
             ENDDO
          ENDDO
       ENDDO

C      HNO3 perturbation coefficients
       DO I=1,NCHHNO
          DO L=1,MAXLAY
             DO J=1,NHNO3
                COFHNO(J,L,I)=COFHNA(J,L,I)*WGTA + COFHNB(J,L,I)*WGTB
             ENDDO
          ENDDO
       ENDDO

C      Adjust non-LTE coefs
       DO I=1,NCHNTE
          DO J=1,NNCOEF
             COEFN(J,I)=COEFNA(J,I)*WGTA + COEFNB(J,I)*WGTB
          ENDDO
       ENDDO

C      Adjust freq, reflected thermal coefs, and solar lookup table
       DO I=1,NCHAN
          DO J=1,NFCOEF
             COEFF(J,I)=COEFFA(J,I)*WGTA + COEFFB(J,I)*WGTB
          ENDDO
          FREQ(I)=FREQA(I)*WGTA + FREQB(I)*WGTB
          HSUN(I)=HSUNA(I)*WGTA + HSUNB(I)*WGTB
       ENDDO
C
       RETURN
       END
