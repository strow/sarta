!=======================================================================
 
! Code converted using TO_F90_LOOP by Alan Miller
! Date: 2023-04-04  Time: 16:44:56
 
!=======================================================================

!    University of Maryland Baltimore Country (UMBC)

!    AIRS

!    SUMCOF

!F77====================================================================


!ROUTINE NAME:
!    SUMCOF


!ABSTRACT:
!    Do a weighted sum of two coef/etc databases ("A" and "B")


!CALL PROTOCOL
!       SUMCOF( NCHAN, NCHNTE,  NCHN1,  NCHN2,  NCHN3,  NCHN4,  NCHN5,
!  $    NCHN6,  NCHN7, NCHCO2, NCHN2O, NCHSO2, NCHHNO, NCHH2O,   YOFF,
!  $    FREQA,  HSUNA, COEFFA, COEFNA, COEF1A, COEF2A, COEF3A, COEF4A,
!  $   COEF5A, COEF6A, COEF7A, COFCOA, COFN2A, COFSOA, COFHNA, COFH2A,
!  $    FREQB,  HSUNB, COEFFB, COEFNB, COEF1B, COEF2B, COEF3B, COEF4B,
!  $   COEF5B, COEF6B, COEF7B, COFCOB, COFN2B, COFSOB, COFHNB, COFH2B,
!  $     FREQ,   HSUN,  COEFF,  COEFN,  COEF1,  COEF2,  COEF3,  COEF4,
!  $    COEF5,  COEF6,  COEF7, COFCO2, COFN2O, COFSO2, COFHNO, COFH2O)


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    INTEGER   NCHAN   number of channels          none
!    INTEGER   NCHNTE  non-LTE number of channels  none
!    INTEGER   NCHN1   set1 number of channels     none
!    INTEGER   NCHN2   set2 number of channels     none
!    INTEGER   NCHN3   set3 number of channels     none
!    INTEGER   NCHN4   set4 number of channels     none
!    INTEGER   NCHN5   set5 number of channels     none
!    INTEGER   NCHN6   set6 number of channels     none
!    INTEGER   NCHN7   set7 number of channels     none
!    INTEGER   NCHCO2  number of CO2 channels      none
!    INTEGER   NCHN2O  number of N2O channels      none
!    INTEGER   NCHSO2  number of SO2 channels      none
!    INTEGER   NCHHNO  number of HNO3 channels     none
!    INTEGER   NCHH2O  number of OPTRAN H2O chans  none
!    REAL      YOFF    yoffset used to calc freqs  um
!    REAL arr  FREQA   channel frequencies         cm^-1
!    REAL arr  HSUNA   solar irradiance            mW/m^2/cm^-1 ?
!    REAL arr  COEFFA  refl thermal "F" coefs      various
!    REAL arr  COEFNA  nonLTE coefs                various
!    REAL arr  COEF1A  set1 fast trans coefs       various
!    REAL arr  COEF2A  set2 fast trans coefs       various
!    REAL arr  COEF3A  set3 fast trans coefs       various
!    REAL arr  COEF4A  set4 fast trans coefs       various
!    REAL arr  COEF5A  set5 fast trans coefs       various
!    REAL arr  COEF6A  set6 fast trans coefs       various
!    REAL arr  COEF7A  set7 fast trans coefs       various
!    REAL arr  COFCOA  CO2 perturbation coefs      various
!    REAL arr  COFN2A  N2O perturbation coefs      various
!    REAL arr  COFSOA  SO2 perturbation coefs      various
!    REAL arr  COFHNA  HNO3 perturbation coefs     various
!    REAL arr  COFH2A  OPTRAN H2O coefs            various

!    REAL arr  FREQB   channel frequencies         cm^-1
!    REAL arr  HSUNB   solar irradiance            mW/m^2/cm^-1 ?
!    REAL arr  COEFFB  refl thermal "F" coefs      various
!    REAL arr  COEFNB  nonLTE coefs                various
!    REAL arr  COEF1B  set1 fast trans coefs       various
!    REAL arr  COEF2B  set2 fast trans coefs       various
!    REAL arr  COEF3B  set3 fast trans coefs       various
!    REAL arr  COEF4B  set4 fast trans coefs       various
!    REAL arr  COEF5B  set5 fast trans coefs       various
!    REAL arr  COEF6B  set6 fast trans coefs       various
!    REAL arr  COEF7B  set7 fast trans coefs       various
!    REAL arr  COFCOB  CO2 perturbation coefs      various
!    REAL arr  COFN2B  N2O perturbation coefs      various
!    REAL arr  COFSOB  SO2 perturbation coefs      various
!    REAL arr  COFHNB  HNO3 perturbation coefs     various
!    REAL arr  COFH2B  OPTRAN H2O coefs            various


!INPUT/OUTPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    REAL arr  FREQ    channel frequencies         cm^-1
!    REAL arr  HSUN    solar irradiance            mW/m^2/cm^-1 ?
!    REAL arr  COEFF   refl thermal "F" coefs      various
!    REAL arr  COEFN   nonLTE coefs                various
!    REAL arr  COEF1   set1 fast trans coefs       various
!    REAL arr  COEF2   set2 fast trans coefs       various
!    REAL arr  COEF3   set3 fast trans coefs       various
!    REAL arr  COEF4   set4 fast trans coefs       various
!    REAL arr  COEF5   set5 fast trans coefs       various
!    REAL arr  COEF6   set6 fast trans coefs       various
!    REAL arr  COEF7   set7 fast trans coefs       various
!    REAL arr  COFCO2  CO2 perturbation coefs      various
!    REAL arr  COFN2O  N2O perturbation coefs      various
!    REAL arr  COFSO2  SO2 perturbation coefs      various
!    REAL arr  COFHNO  HNO3 perturbation coefs     various
!    REAL arr  COFH2O  OPTRAN H2O coefs            various


!OUTPUT PARAMETERS:
!    none


!RETURN VALUES:
!    none


!PARENT(S):
!    USEFAST


!ROUTINES CALLED:
!    none


!FILES ACCESSED:
!    incFTC.f : include file of parameter statements accessed during
!       compilation only.
!    unit IOUN : input text file of tuning adjustments.


!COMMON BLOCKS
!    none


!DESCRIPTION:
!    May 2008 version of SARTA v1.07x code by L.Strow/S.Hannon.

!    The routine takes a pair of fast transmittace coefficients
!    and sums them.


!ALGORITHM REFERENCES:
!    none


!KNOWN BUGS AND LIMITATIONS:
!    none


!ROUTINE HISTORY:
!    Date        Programmer     Comments
!    ----------- -------------- ----------------------------------------
!    09 May 2008 Scott Hannon   Created


!END====================================================================

!      =================================================================

SUBROUTINE SUMCOF(NCHAN, NCHNTE, NCHN1, NCHN2, NCHN3,NCHN4,NCHN5,  &
    NCHN6,  NCHN7, NCHCO2, NCHN2O, NCHSO2, NCHHNO, NCHH2O,   YOFF,  &
    FREQA,  HSUNA, COEFFA, COEFNA, COEF1A, COEF2A, COEF3A, COEF4A,  &
    COEF5A, COEF6A, COEF7A, COFCOA, COFN2A, COFSOA, COFHNA, COFH2A,  &
    FREQB,  HSUNB, COEFFB, COEFNB, COEF1B, COEF2B, COEF3B, COEF4B,  &
    COEF5B, COEF6B, COEF7B, COFCOB, COFN2B, COFSOB, COFHNB, COFH2B,  &
    FREQ,   HSUN,  COEFF,  COEFN,  COEF1,  COEF2,  COEF3,  COEF4,  &
    COEF5,  COEF6,  COEF7, COFCO2, COFN2O, COFSO2, COFHNO, COFH2O)

!      =================================================================


!-----------------------------------------------------------------------
!      IMPLICIT NONE
!-----------------------------------------------------------------------

INTEGER, INTENT(IN)                      :: NCHAN
INTEGER, INTENT(IN)                      :: NCHNTE
INTEGER, INTENT(IN)                      :: NCHN1
INTEGER, INTENT(IN)                      :: NCHN2
INTEGER, INTENT(IN)                      :: NCHN3
INTEGER, INTENT(IN)                      :: NCHN4
INTEGER, INTENT(IN)                      :: NCHN5
INTEGER, INTENT(IN)                      :: NCHN6
INTEGER, INTENT(IN)                      :: NCHN7
INTEGER, INTENT(IN)                      :: NCHCO2
INTEGER, INTENT(IN)                      :: NCHN2O
INTEGER, INTENT(IN)                      :: NCHSO2
INTEGER, INTENT(IN)                      :: NCHHNO
INTEGER, INTENT(IN)                      :: NCHH2O
REAL, INTENT(IN OUT)                     :: YOFF
REAL, INTENT(IN)                         :: FREQA(MXCHAN)
REAL, INTENT(IN)                         :: HSUNA(MXCHAN)
REAL, INTENT(IN)                         :: COEFFA(NFCOEF,MXCHAN)
REAL, INTENT(IN)                         :: COEFNA(NNCOEF,MXCNTE)
REAL, INTENT(IN)                         :: COEF1A(N1COEF,MAXLAY,MXCHN1)
REAL, INTENT(IN)                         :: COEF2A(N2COEF,MAXLAY,MXCHN2)
REAL, INTENT(IN)                         :: COEF3A(N3COEF,MAXLAY,MXCHN3)
REAL, INTENT(IN)                         :: COEF4A(N4COEF,MAXLAY,MXCHN4)
REAL, INTENT(IN)                         :: COEF5A(N5COEF,MAXLAY,MXCHN5)
REAL, INTENT(IN)                         :: COEF6A(N6COEF,MAXLAY,MXCHN6)
REAL, INTENT(IN)                         :: COEF7A(N7COEF,MAXLAY,MXCHN7)
REAL, INTENT(IN)                         :: COFCOA(  NCO2,MAXLAY,MXCHNC)
REAL, INTENT(IN)                         :: COFN2A(  NN2O,MAXLAY,MXCHNN)
REAL, INTENT(IN)                         :: COFSOA(  NSO2,MAXLAY,MXCHNS)
REAL, INTENT(IN)                         :: COFHNA( NHNO3,MAXLAY,MXCHNH)
REAL, INTENT(IN)                         :: COFH2A(  NH2O,MXOWLY,MXCHNW)
REAL, INTENT(IN)                         :: FREQB(MXCHAN)
REAL, INTENT(IN)                         :: HSUNB(MXCHAN)
REAL, INTENT(IN)                         :: COEFFB(NFCOEF,MXCHAN)
REAL, INTENT(IN)                         :: COEFNB(NNCOEF,MXCNTE)
REAL, INTENT(IN)                         :: COEF1B(N1COEF,MAXLAY,MXCHN1)
REAL, INTENT(IN)                         :: COEF2B(N2COEF,MAXLAY,MXCHN2)
REAL, INTENT(IN)                         :: COEF3B(N3COEF,MAXLAY,MXCHN3)
REAL, INTENT(IN)                         :: COEF4B(N4COEF,MAXLAY,MXCHN4)
REAL, INTENT(IN)                         :: COEF5B(N5COEF,MAXLAY,MXCHN5)
REAL, INTENT(IN)                         :: COEF6B(N6COEF,MAXLAY,MXCHN6)
REAL, INTENT(IN)                         :: COEF7B(N7COEF,MAXLAY,MXCHN7)
REAL, INTENT(IN)                         :: COFCOB(  NCO2,MAXLAY,MXCHNC)
REAL, INTENT(IN)                         :: COFN2B(  NN2O,MAXLAY,MXCHNN)
REAL, INTENT(IN)                         :: COFSOB(  NSO2,MAXLAY,MXCHNS)
REAL, INTENT(IN)                         :: COFHNB( NHNO3,MAXLAY,MXCHNH)
REAL, INTENT(IN)                         :: COFH2B(  NH2O,MXOWLY,MXCHNW)
REAL, INTENT(OUT)                        :: FREQ(MXCHAN)
REAL, INTENT(OUT)                        :: HSUN(MXCHAN)
REAL, INTENT(OUT)                        :: COEFF(NFCOEF,MXCHAN)
REAL, INTENT(OUT)                        :: COEFN(NNCOEF,MXCNTE)
REAL, INTENT(OUT)                        :: COEF1(N1COEF,MAXLAY,MXCHN1)
REAL, INTENT(OUT)                        :: COEF2(N2COEF,MAXLAY,MXCHN2)
REAL, INTENT(OUT)                        :: COEF3(N3COEF,MAXLAY,MXCHN3)
REAL, INTENT(OUT)                        :: COEF4(N4COEF,MAXLAY,MXCHN4)
REAL, INTENT(OUT)                        :: COEF5(N5COEF,MAXLAY,MXCHN5)
REAL, INTENT(OUT)                        :: COEF6(N6COEF,MAXLAY,MXCHN6)
REAL, INTENT(OUT)                        :: COEF7(N7COEF,MAXLAY,MXCHN7)
REAL, INTENT(OUT)                        :: COFCO2(  NCO2,MAXLAY,MXCHNC)
REAL, INTENT(OUT)                        :: COFN2O(  NN2O,MAXLAY,MXCHNN)
REAL, INTENT(OUT)                        :: COFSO2(  NSO2,MAXLAY,MXCHNS)
REAL, INTENT(OUT)                        :: COFHNO( NHNO3,MAXLAY,MXCHNH)
REAL, INTENT(OUT)                        :: COFH2O(  NH2O,MXOWLY,MXCHNW)
IMPLICIT NONE


!-----------------------------------------------------------------------
!      INCLUDE FILES
!-----------------------------------------------------------------------
INCLUDE 'incFTC.f'


!-----------------------------------------------------------------------
!      EXTERNAL FUNCTIONS
!-----------------------------------------------------------------------
!      none


!-----------------------------------------------------------------------
!      ARGUMENTS
!-----------------------------------------------------------------------
!      INPUT

















!      Database "A"
REAL :: ! channel frequencies
REAL :: ! solar irradiance
REAL :: ! reflected thermal "F" coefs
REAL :: ! nonLTE coefs
REAL :: ! set1 coefs
REAL :: ! set2 coefs
REAL :: ! set3 coefs
REAL :: ! set4 coefs
REAL :: ! set5 coefs
REAL :: ! set6 coefs
REAL :: ! set7 coefs
REAL :: ! CO2 pert coefs
REAL :: ! N2O pert coefs
REAL :: ! SO2 pert coefs
REAL :: ! HNO3 pert coefs
REAL :: ! H2O OPTRAN coefs

!      Database "B"
REAL :: ! channel frequencies
REAL :: ! solar irradiance
REAL :: ! reflected thermal "F" coefs
REAL :: ! nonLTE coefs
REAL :: ! set1 coefs
REAL :: ! set2 coefs
REAL :: ! set3 coefs
REAL :: ! set4 coefs
REAL :: ! set5 coefs
REAL :: ! set6 coefs
REAL :: ! set7 coefs
REAL :: ! CO2 pert coefs
REAL :: ! N2O pert coefs
REAL :: ! SO2 pert coefs
REAL :: ! HNO3 pert coefs
REAL :: ! H2O OPTRAN coefs


!      INPUT/OUTPUT
!      Weighted sum database to be use for calcs
REAL :: ! channel frequencies
REAL :: ! solar irradiance
REAL :: ! reflected thermal "F" coefs
REAL :: ! nonLTE coefs
REAL :: ! set1 coefs
REAL :: ! set2 coefs
REAL :: ! set3 coefs
REAL :: ! set4 coefs
REAL :: ! set5 coefs
REAL :: ! set6 coefs
REAL :: ! set7 coefs
REAL :: ! CO2 pert coefs
REAL :: ! N2O pert coefs
REAL :: ! SO2 pert coefs
REAL :: ! HNO3 pert coefs
REAL :: ! H2O OPTRAN coefs

!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
INTEGER :: I  ! generic looping variable
INTEGER :: J  ! generic looping variable
INTEGER :: L  ! generic looping variable
REAL :: WGTA     ! fractional weight
REAL :: WGTB     ! fractional weight


!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none


!***********************************************************************
!***********************************************************************
!                    EXECUTABLE CODE
!***********************************************************************
!***********************************************************************

!      Calc summing weights
WGTA=(YOFF - YOFFB)/(YOFFA - YOFFB)
WGTB=(YOFF - YOFFA)/(YOFFB - YOFFA)

!      Set 1
DO I=1,NCHN1
  DO L=1,MAXLAY
    DO J=1,N1COEF
      COEF1(J,L,I)=COEF1A(J,L,I)*WGTA + COEF1B(J,L,I)*WGTB
      ENDDO
        ENDDO
          ENDDO
            
!      Set 2
            DO I=1,NCHN2
              DO L=1,MAXLAY
                DO J=1,N2COEF
                  COEF2(J,L,I)=COEF2A(J,L,I)*WGTA + COEF2B(J,L,I)*WGTB
                  ENDDO
                    ENDDO
                      ENDDO
                        
!      Set 3
                        DO I=1,NCHN3
                          DO L=1,MAXLAY
                            DO J=1,N3COEF
                              COEF3(J,L,I)=COEF3A(J,L,I)*WGTA + COEF3B(J,L,I)*WGTB
                              ENDDO
                                ENDDO
                                  ENDDO
                                    
!      Set 4
                                    DO I=1,NCHN4
                                      DO L=1,MAXLAY
                                        DO J=1,N4COEF
                                          COEF4(J,L,I)=COEF4A(J,L,I)*WGTA + COEF4B(J,L,I)*WGTB
                                          ENDDO
                                            ENDDO
                                              ENDDO
                                                
!      Set 5
                                                DO I=1,NCHN5
                                                  DO L=1,MAXLAY
                                                    DO J=1,N5COEF
                                                      COEF5(J,L,I)=COEF5A(J,L,I)*WGTA + COEF5B(J,L,I)*WGTB
                                                      ENDDO
                                                        ENDDO
                                                          ENDDO
                                                            
!      Set 6
                                                            DO I=1,NCHN6
                                                              DO L=1,MAXLAY
                                                                DO J=1,N6COEF
                                                                  COEF6(J,L,I)=COEF6A(J,L,I)*WGTA + COEF6B(J,L,I)*WGTB
                                                                  ENDDO
                                                                   6ENDDO
                                                                   6  ENDDO
                                                                   6   H
!      Set 7
                                                                   6   HDO I=1,NCHN7
                                                                   6   H DO L=1,MAXLAY
                                                                   6   H  $DO J=1,N7COEF
                                                                   6   H  $  COEF7(J,L,I)=COEF7A(J,L,I)*WGTA + COEF7B(J,L,I)*WGTB
                                                                   6   H  $  ENDDO
                                                                   6   H  $   ENDDO
                                                                   6   H  $     ENDDO
                                                                   6   H  $      
!      OPTRAN
                                                                   6   H  $      DO I=1,NCHH2O
                                                                   6   H  $        DO L=1,MXOWLY
                                                                   6   H  $         7DO J=1,NH2O
                                                                   6   H  $         7  COFH2O(J,L,I)=COFH2A(J,L,I)*WGTA + COFH2B(J,L,I)*WGTB
                                                                   6   H  $         7  ENDDO
                                                                   6   H  $         7   6ENDDO
                                                                   6   H  $         7   6  ENDDO
                                                                   6   H  $         7   6   H
!      CO2 perturbation coefficients
                                                                   6   H  $         7   6   HDO I=1,NCHCO2
                                                                   6   H  $         7   6   H DO L=1,MAXLAY
                                                                   6   H  $         7   6   H  $DO J=1,NCO2
                                                                   6   H  $         7   6   H  $  COFCO2(J,L,I)=COFCOA(J,L,I)*WGTA + COFCOB(J,L,I)*WGTB
                                                                   6   H  $         7   6   H  $  ENDDO
                                                                   6   H  $         7   6   H  $   ENDDO
                                                                   6   H  $         7   6   H  $     ENDDO
                                                                   6   H  $         7   6   H  $      
!      N2O perturbation coefficients
                                                                   6   H  $         7   6   H  $      DO I=1,NCHN2O
                                                                   6   H  $         7   6   H  $        DO L=1,MAXLAY
                                                                   6   H  $         7   6   H  $         7DO J=1,NN2O
                                                                   6   H  $         7   6   H  $         7  COFN2O(J,L,I)=COFN2A(J,L,I)*WGTA + COFN2B(J,L,I)*WGTB
                                                                   6   H  $         7   6   H  $         7  ENDDO
                                                                   6   H  $         7   6   H  $         7   6ENDDO
                                                                   6   H  $         7   6   H  $         7   6  ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H
!      SO2 perturbation coefficients
                                                                   6   H  $         7   6   H  $         7   6   HDO I=1,NCHSO2
                                                                   6   H  $         7   6   H  $         7   6   H DO L=1,MAXLAY
                                                                   6   H  $         7   6   H  $         7   6   H  $DO J=1,NSO2
                                                                   6   H  $         7   6   H  $         7   6   H  $  COFSO2(J,L,I)=COFSOA(J,L,I)*WGTA + COFSOB(J,L,I)*WGTB
                                                                   6   H  $         7   6   H  $         7   6   H  $  ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $   ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $     ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $      
!      HNO3 perturbation coefficients
                                                                   6   H  $         7   6   H  $         7   6   H  $      DO I=1,NCHHNO
                                                                   6   H  $         7   6   H  $         7   6   H  $        DO L=1,MAXLAY
                                                                   6   H  $         7   6   H  $         7   6   H  $         7DO J=1,NHNO3
                                                                   6   H  $         7   6   H  $         7   6   H  $         7  COFHNO(J,L,I)=COFHNA(J,L,I)*WGTA + COFHNB(J,L,I)*WGTB
                                                                   6   H  $         7   6   H  $         7   6   H  $         7  ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6  ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H
!      Adjust non-LTE coefs
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   HDO I=1,NCHNTE
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H DO J=1,NNCOEF
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $COEFN(J,I)=COEFNA(J,I)*WGTA + COEFNB(J,I)*WGTB
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $  ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $   
!      Adjust freq, reflected thermal coefs, and solar lookup table
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $   DO I=1,NCHAN
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $     DO J=1,NFCOEF
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $      COEFF(J,I)=COEFFA(J,I)*WGTA + COEFFB(J,I)*WGTB
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $      ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $        FREQ(I)=FREQA(I)*WGTA + FREQB(I)*WGTB
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $        HSUN(I)=HSUNA(I)*WGTA + HSUNB(I)*WGTB
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $        ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7RETURN
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $        END SUBROUTINE SUMCOF
