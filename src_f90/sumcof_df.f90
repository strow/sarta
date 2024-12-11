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
!  $    NCHN6,  NCHN7, NCHCO2, NCHN2O, NCHSO2, NCHHNO, NCHH2O,
!  $   LSTCHN, CLIST1, CLIST2, CLIST3, CLIST4, CLIST5, CLIST6, CLIST7,
!  $   CLISTN, CLICO2, CLIN2O, CLISO2, CLIHNO, CLIH2O,   YOFF,  DFCAL,
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
!    INT arr   LSTCHN  channel list                channel ID
!    INT arr   CLIST1  set1 channel list           channel ID
!    INT arr   CLIST2  set2 channel list           channel ID
!    INT arr   CLIST3  set3 channel list           channel ID
!    INT arr   CLIST4  set4 channel list           channel ID
!    INT arr   CLIST5  set5 channel list           channel ID
!    INT arr   CLIST6  set6 channel list           channel ID
!    INT arr   CLIST7  set7 channel list           channel ID
!    INT arr   CLISTN  non-LTE channel list        channel ID
!    INT arr   CLICO2  CO2 channel list            channel ID
!    INT arr   CLIN2O  N2O channel list            channel ID
!    INT arr   CLISO2  SO2 channel list            channel ID
!    INT arr   CLIHNO  HNO3 channel list           channel ID
!    INT arr   CLIH2O  H2O channel list            channel ID
!    REAL      YOFF    yoffset used to calc freqs  um
!    REAL arr  DFCAL   speccal adjustment to YOFF  um
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
!    August 2009 version of SARTA v1.08 "df" code by L.Strow/S.Hannon.

!    The routine takes a pair of fast transmittace coefficients
!    and sums them.


!ALGORITHM REFERENCES:
!    none


!KNOWN BUGS AND LIMITATIONS:
!    none


!ROUTINE HISTORY:
! Date        Programmer     Comments
! ----------- -------------- -------------------------------------------
! 09 May 2008 Scott Hannon   Created
! 03 Aug 2009 Scott Hannon   Added DFCAL; add LSTCHN & CLI<*> to call

!END====================================================================

!      =================================================================

SUBROUTINE SUMCOF(NCHAN, NCHNTE, NCHN1, NCHN2, NCHN3,NCHN4,NCHN5,  &
    NCHN6,  NCHN7, NCHCO2, NCHN2O, NCHSO2, NCHHNO, NCHH2O,  &
    LSTCHN, CLIST1, CLIST2, CLIST3, CLIST4, CLIST5, CLIST6, CLIST7,  &
    CLISTN, CLICO2, CLIN2O, CLISO2, CLIHNO, CLIH2O,   YOFF,  DFCAL,  &
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
INTEGER, INTENT(IN)                      :: LSTCHN(MXCHAN)
INTEGER, INTENT(IN)                      :: CLIST1(MXCHN1)
INTEGER, INTENT(IN)                      :: CLIST2(MXCHN2)
INTEGER, INTENT(IN)                      :: CLIST3(MXCHN3)
INTEGER, INTENT(IN)                      :: CLIST4(MXCHN4)
INTEGER, INTENT(IN)                      :: CLIST5(MXCHN5)
INTEGER, INTENT(IN)                      :: CLIST6(MXCHN6)
INTEGER, INTENT(IN)                      :: CLIST7(MXCHN7)
INTEGER, INTENT(IN)                      :: CLISTN(MXCNTE)
INTEGER, INTENT(IN)                      :: CLICO2(MXCHNC)
INTEGER, INTENT(IN)                      :: CLIN2O(MXCHNN)
INTEGER, INTENT(IN)                      :: CLISO2(MXCHNS)
INTEGER, INTENT(IN)                      :: CLIHNO(MXCHNH)
INTEGER, INTENT(IN)                      :: CLIH2O(MXCHNW)
REAL, INTENT(IN)                         :: YOFF
REAL, INTENT(IN)                         :: DFCAL(MXCHAN)
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
REAL, INTENT(IN OUT)                     :: COEFFB(NFCOEF,MXCHAN)
REAL, INTENT(IN OUT)                     :: COEFNB(NNCOEF,MXCNTE)
REAL, INTENT(IN OUT)                     :: COEF1B(N1COEF,MAXLAY,MXCHN1)
REAL, INTENT(IN OUT)                     :: COEF2B(N2COEF,MAXLAY,MXCHN2)
REAL, INTENT(IN OUT)                     :: COEF3B(N3COEF,MAXLAY,MXCHN3)
REAL, INTENT(IN OUT)                     :: COEF4B(N4COEF,MAXLAY,MXCHN4)
REAL, INTENT(IN OUT)                     :: COEF5B(N5COEF,MAXLAY,MXCHN5)
REAL, INTENT(IN OUT)                     :: COEF6B(N6COEF,MAXLAY,MXCHN6)
REAL, INTENT(IN OUT)                     :: COEF7B(N7COEF,MAXLAY,MXCHN7)
REAL, INTENT(IN OUT)                     :: COFCOB(  NCO2,MAXLAY,MXCHNC)
REAL, INTENT(IN OUT)                     :: COFN2B(  NN2O,MAXLAY,MXCHNN)
REAL, INTENT(IN OUT)                     :: COFSOB(  NSO2,MAXLAY,MXCHNS)
REAL, INTENT(IN OUT)                     :: COFHNB( NHNO3,MAXLAY,MXCHNH)
REAL, INTENT(IN OUT)                     :: COFH2B(  NH2O,MXOWLY,MXCHNW)
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














INTEGER :: ! channel list
INTEGER :: ! set1 channel list
INTEGER :: ! set2 channel list
INTEGER :: ! set3 channel list
INTEGER :: ! set4 channel list
INTEGER :: ! set5 channel list
INTEGER :: ! set6 channel list
INTEGER :: ! set7 channel list
INTEGER :: ! non-LTE channel list
INTEGER :: ! CO2 channel list
INTEGER :: ! N2O channel list
INTEGER :: ! SO2 channel list
INTEGER :: ! HNO3 channel list
INTEGER :: ! H2O channel list


REAL :: ! speccal adjustment to YOFF

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
INTEGER :: ICHAN  ! channel ID
REAL :: WGTA(MXCHAN) ! fractional weight of database "A"
REAL :: WGTB(MXCHAN) ! fractional weight of database "B"
REAL :: RJUNK         ! generic work variable


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
DO I=1,NCHAN
  ICHAN = LSTCHN(I)
  RJUNK = YOFF + DFCAL(ICHAN)
  WGTA(ICHAN)=(RJUNK - YOFFB)/(YOFFA - YOFFB)
  WGTB(ICHAN)=(RJUNK - YOFFA)/(YOFFB - YOFFA)
  
!         Adjust freq, sun, and reflected thermal "F" coefs
  FREQ(I)=FREQA(I)*WGTA(ICHAN) + FREQB(I)*WGTB(ICHAN)
  HSUN(I)=HSUNA(I)*WGTA(ICHAN) + HSUNB(I)*WGTB(ICHAN)
  DO J=1,NFCOEF
    COEFF(J,I)=COEFFA(J,I)*WGTA(ICHAN) + COEFFB(J,I)*WGTB(ICHAN)
    ENDDO
      ENDDO
        
!      Set 1
        DO I=1,NCHN1
          ICHAN = CLIST1(I)
          DO L=1,MAXLAY
            DO J=1,N1COEF
              COEF1(J,L,I)=COEF1A(J,L,I)*WGTA(ICHAN) +  &
                  COEF1B(J,L,I)*WGTB(ICHAN)
              ENDDO
                ENDDO
                  ENDDO
                    
!      Set 2
                    DO I=1,NCHN2
                      ICHAN = CLIST2(I)
                      DO L=1,MAXLAY
                        DO J=1,N2COEF
                          COEF2(J,L,I)=COEF2A(J,L,I)*WGTA(ICHAN)+  &
                              COEF2B(J,L,I)*WGTB(ICHAN)
                          ENDDO
                            ENDDO
                              ENDDO
                                
!      Set 3
                                DO I=1,NCHN3
                                  ICHAN = CLIST3(I)
                                  DO L=1,MAXLAY
                                    DO J=1,N3COEF
                                      COEF3(J,L,I)=COEF3A(J,L,I)*WGTA(ICHAN) +  &
                                          COEF3B(J,L,I)*WGTB(ICHAN)
                                      ENDDO
                                        ENDDO
                                          ENDDO
                                            
!      Set 4
                                            DO I=1,NCHN4
                                              ICHAN = CLIST4(I)
                                              DO L=1,MAXLAY
                                                DO J=1,N4COEF
                                                  COEF4(J,L,I)=COEF4A(J,L,I)*WGTA(ICHAN) +  &
                                                      COEF4B(J,L,I)*WGTB(ICHAN)
                                                  ENDDO
                                                    ENDDO
                                                      ENDDO
                                                        
!      Set 5
                                                        DO I=1,NCHN5
                                                          ICHAN = CLIST5(I)
                                                          DO L=1,MAXLAY
                                                            DO J=1,N5COEF
                                                              COEF5(J,L,I)=COEF5A(J,L,I)*WGTA(ICHAN) +  &
                                                                  COEF5B(J,L,I)*WGTB(ICHAN)
                                                              ENDDO
                                                                ENDDO
                                                                  ENDDO
                                                                   6
!      Set 6
                                                                   6DO I=1,NCHN6
                                                                   6  ICHAN = CLIST6(I)
                                                                   6  DO L=1,MAXLAY
                                                                   6   HDO J=1,N6COEF
                                                                   6   H COEF6(J,L,I)=COEF6A(J,L,I)*WGTA(ICHAN) +  &
                                                                   6   H  $  COEF6B(J,L,I)*WGTB(ICHAN)
                                                                   6   H ENDDO
                                                                   6   H  $ENDDO
                                                                   6   H  $  ENDDO
                                                                   6   H  $   
!      Set 7
                                                                   6   H  $   DO I=1,NCHN7
                                                                   6   H  $     ICHAN = CLIST7(I)
                                                                   6   H  $     DO L=1,MAXLAY
                                                                   6   H  $      DO J=1,N7COEF
                                                                   6   H  $        COEF7(J,L,I)=COEF7A(J,L,I)*WGTA(ICHAN) +  &
                                                                   6   H  $         7  COEF7B(J,L,I)*WGTB(ICHAN)
                                                                   6   H  $        ENDDO
                                                                   6   H  $         7ENDDO
                                                                   6   H  $         7  ENDDO
                                                                   6   H  $         7   6
!      OPTRAN
                                                                   6   H  $         7   6DO I=1,NCHH2O
                                                                   6   H  $         7   6  ICHAN = CLIH2O(I)
                                                                   6   H  $         7   6  DO L=1,MXOWLY
                                                                   6   H  $         7   6   HDO J=1,NH2O
                                                                   6   H  $         7   6   H COFH2O(J,L,I)=COFH2A(J,L,I)*WGTA(ICHAN) +  &
                                                                   6   H  $         7   6   H  $  COFH2B(J,L,I)*WGTB(ICHAN)
                                                                   6   H  $         7   6   H ENDDO
                                                                   6   H  $         7   6   H  $ENDDO
                                                                   6   H  $         7   6   H  $  ENDDO
                                                                   6   H  $         7   6   H  $   
!      CO2 perturbation coefficients
                                                                   6   H  $         7   6   H  $   DO I=1,NCHCO2
                                                                   6   H  $         7   6   H  $     ICHAN = CLICO2(I)
                                                                   6   H  $         7   6   H  $     DO L=1,MAXLAY
                                                                   6   H  $         7   6   H  $      DO J=1,NCO2
                                                                   6   H  $         7   6   H  $        COFCO2(J,L,I)=COFCOA(J,L,I)*WGTA(ICHAN) +  &
                                                                   6   H  $         7   6   H  $         7  COFCOB(J,L,I)*WGTB(ICHAN)
                                                                   6   H  $         7   6   H  $        ENDDO
                                                                   6   H  $         7   6   H  $         7ENDDO
                                                                   6   H  $         7   6   H  $         7  ENDDO
                                                                   6   H  $         7   6   H  $         7   6
!      N2O perturbation coefficients
                                                                   6   H  $         7   6   H  $         7   6DO I=1,NCHN2O
                                                                   6   H  $         7   6   H  $         7   6  ICHAN = CLIN2O(I)
                                                                   6   H  $         7   6   H  $         7   6  DO L=1,MAXLAY
                                                                   6   H  $         7   6   H  $         7   6   HDO J=1,NN2O
                                                                   6   H  $         7   6   H  $         7   6   H COFN2O(J,L,I)=COFN2A(J,L,I)*WGTA(ICHAN) +  &
                                                                   6   H  $         7   6   H  $         7   6   H  $  COFN2B(J,L,I)*WGTB(ICHAN)
                                                                   6   H  $         7   6   H  $         7   6   H ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $  ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $   
!      SO2 perturbation coefficients
                                                                   6   H  $         7   6   H  $         7   6   H  $   DO I=1,NCHSO2
                                                                   6   H  $         7   6   H  $         7   6   H  $     ICHAN = CLISO2(I)
                                                                   6   H  $         7   6   H  $         7   6   H  $     DO L=1,MAXLAY
                                                                   6   H  $         7   6   H  $         7   6   H  $      DO J=1,NSO2
                                                                   6   H  $         7   6   H  $         7   6   H  $        COFSO2(J,L,I)=COFSOA(J,L,I)*WGTA(ICHAN) +  &
                                                                   6   H  $         7   6   H  $         7   6   H  $         7  COFSOB(J,L,I)*WGTB(ICHAN)
                                                                   6   H  $         7   6   H  $         7   6   H  $        ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7  ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6
!      HNO3 perturbation coefficients
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6DO I=1,NCHHNO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6  ICHAN = CLIHNO(I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6  DO L=1,MAXLAY
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   HDO J=1,NHNO3
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H COFHNO(J,L,I)=COFHNA(J,L,I)*WGTA(ICHAN) +  &
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $  COFHNB(J,L,I)*WGTB(ICHAN)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $  ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $   
!      Adjust non-LTE coefs
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $   DO I=1,NCHNTE
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $     ICHAN = CLISTN(I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $     DO J=1,NNCOEF
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $      COEFN(J,I)=COEFNA(J,I)*WGTA(ICHAN) +  &
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7COEFNB(J,I)*WGTB(ICHAN)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $      ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $        ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7RETURN
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $        END SUBROUTINE SUMCOF
