!=======================================================================
 
! Code converted using TO_F90_LOOP by Alan Miller
! Date: 2023-04-04  Time: 16:44:56
 
!=======================================================================

!    University of Maryland Baltimore Country (UMBC)

!    AIRS

!    RDCOEF

!F77====================================================================


!ROUTINE NAME:
!    RDCOEF


!ABSTRACT:
!    Read in the AIRS fast transmittance coefficients.


!CALL PROTOCOL
!        RDCOEF ( AORB,   IOUN,  NCHAN, INDCHN, SETCHN,
!  $     NCHN1,  NCHN2,  NCHN3,  NCHN4,  NCHN5,  NCHN6,  NCHN7,
!  $    CLIST1, CLIST2, CLIST3, CLIST4, CLIST5, CLIST6, CLIST7,
!  $     COEF1,  COEF2,  COEF3,  COEF4,  COEF5,  COEF6,  COEF7,
!  $    NCHCO2, INDCO2, CLICO2, COFCO2,
!  $    NCHN2O, INDN2O, CLIN2O, COFN2O,
!  $    NCHSO2, INDSO2, CLISO2, COFSO2,
!  $    NCHHNO, INDHNO, CLIHNO, COFHNO,
!  $    NCHH2O, INDH2O, CLIH2O,  WAZOP, WAVGOP, COFH2O,
!  $      FREQ,  COEFF, NCHNTE, CLISTN,  COEFN,     FX)


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    CHAR*1    AORB    specify A or B              none
!    INT arr   INDCHN  indices of channels         none
!    INTEGER   IOUN    I/O unit number             none
!    INTEGER   NCHAN   number of channels          none


!OUTPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    INT arr   CLICO2  CO2 channel list            none
!    INT arr   CLIHNO  HNO3 channel list           none
!    INT arr   CLIH2O  H2O channel list            none
!    INT arr   CLIN2O  N2O channel list            none
!    INT arr   CLISO2  SO2 channel list            none
!    INT arr   CLIST1  set1 channel list           none
!    INT arr   CLIST2  set2 channel list           none
!    INT arr   CLIST3  set3 channel list           none
!    INT arr   CLIST4  set4 channel list           none
!    INT arr   CLIST5  set5 channel list           none
!    INT arr   CLIST6  set6 channel list           none
!    INT arr   CLIST7  set7 channel list           none
!    INT arr   CLISTN  non-LTE channel list        none
!    REAL arr  COEF1   set1 fast trans coefs       various
!    REAL arr  COEF2   set2 fast trans coefs       various
!    REAL arr  COEF3   set3 fast trans coefs       various
!    REAL arr  COEF4   set4 fast trans coefs       various
!    REAL arr  COEF5   set5 fast trans coefs       various
!    REAL arr  COEF6   set6 fast trans coefs       various
!    REAL arr  COEF7   set7 fast trans coefs       various
!    REAL arr  COEFF   thermal "F" factor coefs    various
!    REAL arr  COEFN   non-LTE coefficients        various
!    REAL arr  COFCO2  CO2 perturbation coefs      various
!    REAL arr  COFSO2  SO2 perturbation coefs      various
!    REAL arr  COFHNO  HNO3 perturbation coefs     various
!    REAL arr  COFN2O  N2O perturbation coefs      various
!    REAL arr  COFH2O  OPTRAN H2O coefs            various
!    REAL arr  FREQ    channel freqs               cm-1
!    REAL arr  FX      fixed gases adjustment      none
!    INT arr   INDCO2  CO2 pert channel indices    none
!    INT arr   INDSO2  SO2 pert channel indices    none
!    INT arr   INDHNO  HNO3 pert channel indices   none
!    INT arr   INDN2O  N2O pert channel indices    none
!    INT arr   INDH2O  OPTRAN H2O channel indices  none
!    INTEGER   NCHN1   set1 number of channels     none
!    INTEGER   NCHN2   set2 number of channels     none
!    INTEGER   NCHN3   set3 number of channels     none
!    INTEGER   NCHN4   set4 number of channels     none
!    INTEGER   NCHN5   set5 number of channels     none
!    INTEGER   NCHN6   set6 number of channels     none
!    INTEGER   NCHN7   set7 number of channels     none
!    INTEGER   NCHCO2  number of CO2 pert chans    none
!    INTEGER   NCHH2O  number of OPTRAN H2O chans  none
!    INTEGER   NCHHNO  number of HNO3 pert chans   none
!    INTEGER   NCHN2O  number of N2O pert chans    none
!    INTEGER   NCHNTE  non-LTE number of channels  none
!    INTEGER   NCHSO2  number of SO2 pert chans    none
!    REAL arr  WAZOP   OPTRAN water grid           kiloMoles/cm^2
!    REAL arr  WAVGOP  OPTRAN water pred averges   various
!    INT arr   SETCHN  set# (1-7) chan belongs to  none (integer, 1 - 7)


!INPUT/OUTPUT PARAMETERS:
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
!    unit IOUN : input file, binary FORTRAN data file. The file is
!       opened, read, and closed. This is done 10 times, once per
!       each of the 7 coef sets, and once each for the variable CO2,
!       OPTRAN water, and thermal F factor coefs.


!COMMON BLOCKS
!    none


!DESCRIPTION:
!    August 2009 version of the 100 layer AIRS Fast Transmittance
!    Code by L.Strow/S.Hannon.

!    Seven sets of binary data files containing the main fast
!    transmittance coefficients are opened and read one channel at
!    a time.  The seven sets of coefs are each stored in their own
!    arrays.  Next, preturbation coefficients for four trace gases,
!    (CO2, SO2, HNO3, & N2O) are read in from four binary files.
!    Next, OPTRAN water fast trans coefs for some channels are read
!    in from a binary file file.  The header of the OPTRAN file
!    specifies 300 OPTRAN water levels, and also the mean value of
!    4 predictor terms for each of the levels.  Next, comes the
!    read of the binary file with the reflected downwelling thermal
!    radiance "F factor" coefficients.  Next is a read of the "FX"
!    fixed gases adjustment term from an ASCII text file.  Lastly
!    comes the read of the non-LTE coefficients from a binary file.


!ALGORITHM REFERENCES:
!    none


!KNOWN BUGS AND LIMITATIONS:
!    none


!ROUTINE HISTORY:
! Date        Programmer     Comments
! ----------- -------------- -------------------------------------------
! 01 Dec 1994 Scott Hannon   Created
! 21 Dec 1994 Scott Hannon   Fixed error with IOPF (now assigned)
! 05 Feb 1997 Scott Hannon   Re-wrote for FWO+FOW+FMW+FCOW.
! 28 Aug 1997 Scott Hannon   Re-wrote for sets 1 - 7 and thermal
! 30 Sep 1997 Scott Hannon   Added COFCO2 and INDCO2
! 27 Feb 1998 Scott Hannon   Added COFH2O, INDH2O, WAZOP, & WAVGOP
! 17 Aug 2000 Scott Hannon   Add FX
! 12 Feb 2001 Scott Hannon   hardcoded filenames instead of prompts
! 18 May 2005 Scott Hannon   Add HNO3 based on SO2 code
! 28 Jun 2005 Scott Hannon   "trace" version for CO2,SO2,HNO3,N2O
! 13 Oct 2005 Scott Hannon   Add non-LTE variables
! 09 May 2008 Scott Hannon   Remove LABOVE; add NCHCO2, NCHN2O,
!                               NCHSO2, NCHHNO, NCHH2O; reorder call;
!                               comment out LACHAN
! 03 Aug 2009 Scott Hannon   Add CLI<gas> for CO2,N2O,HNO,H2O,SO2

!END====================================================================

!      =================================================================

SUBROUTINE RDCOEF (  AORB,   IOUN,  NCHAN, INDCHN, SETCHN,  &
    NCHN1,  NCHN2,  NCHN3,  NCHN4,  NCHN5,  NCHN6,  NCHN7,  &
    CLIST1, CLIST2, CLIST3, CLIST4, CLIST5, CLIST6, CLIST7,  &
    COEF1,  COEF2,  COEF3,  COEF4,  COEF5,  COEF6,  COEF7,  &
    NCHCO2, INDCO2, CLICO2, COFCO2, NCHN2O, INDN2O, CLIN2O, COFN2O,  &
    NCHSO2, INDSO2, CLISO2, COFSO2, NCHHNO, INDHNO, CLIHNO, COFHNO,  &
    NCHH2O, INDH2O, CLIH2O,  WAZOP, WAVGOP, COFH2O,  &
    FREQ,  COEFF, NCHNTE, CLISTN,  COEFN,     FX)
!      =================================================================


!-----------------------------------------------------------------------
!      IMPLICIT NONE
!-----------------------------------------------------------------------

CHARACTER (LEN=1), INTENT(IN OUT)        :: AORB
INTEGER, INTENT(OUT)                     :: IOUN
INTEGER, INTENT(IN OUT)                  :: NCHAN
INTEGER, INTENT(OUT)                     :: INDCHN(MXCHAN)
INTEGER, INTENT(OUT)                     :: SETCHN(MXCHAN)
INTEGER, INTENT(OUT)                     :: NCHN1
INTEGER, INTENT(OUT)                     :: NCHN2
INTEGER, INTENT(OUT)                     :: NCHN3
INTEGER, INTENT(OUT)                     :: NCHN4
INTEGER, INTENT(OUT)                     :: NCHN5
INTEGER, INTENT(OUT)                     :: NCHN6
INTEGER, INTENT(OUT)                     :: NCHN7
INTEGER, INTENT(OUT)                     :: CLIST1(MXCHN1)
INTEGER, INTENT(OUT)                     :: CLIST2(MXCHN2)
INTEGER, INTENT(OUT)                     :: CLIST3(MXCHN3)
INTEGER, INTENT(OUT)                     :: CLIST4(MXCHN4)
INTEGER, INTENT(OUT)                     :: CLIST5(MXCHN5)
INTEGER, INTENT(OUT)                     :: CLIST6(MXCHN6)
INTEGER, INTENT(OUT)                     :: CLIST7(MXCHN7)
REAL, INTENT(IN OUT)                     :: COEF1(N1COEF,MAXLAY,MXCHN1)
REAL, INTENT(IN OUT)                     :: COEF2(N2COEF,MAXLAY,MXCHN2)
REAL, INTENT(IN OUT)                     :: COEF3(N3COEF,MAXLAY,MXCHN3)
REAL, INTENT(IN OUT)                     :: COEF4(N4COEF,MAXLAY,MXCHN4)
REAL, INTENT(IN OUT)                     :: COEF5(N5COEF,MAXLAY,MXCHN5)
REAL, INTENT(IN OUT)                     :: COEF6(N6COEF,MAXLAY,MXCHN6)
REAL, INTENT(IN OUT)                     :: COEF7(N7COEF,MAXLAY,MXCHN7)
INTEGER, INTENT(OUT)                     :: NCHCO2
INTEGER, INTENT(OUT)                     :: INDCO2(MXCHAN)
INTEGER, INTENT(OUT)                     :: CLICO2(MXCHNC)
REAL, INTENT(IN OUT)                     :: COFCO2(  NCO2,MAXLAY,MXCHNC)
INTEGER, INTENT(OUT)                     :: NCHN2O
INTEGER, INTENT(OUT)                     :: INDN2O(MXCHAN)
INTEGER, INTENT(OUT)                     :: CLIN2O(MXCHNN)
REAL, INTENT(IN OUT)                     :: COFN2O(  NN2O,MAXLAY,MXCHNN)
INTEGER, INTENT(OUT)                     :: NCHSO2
INTEGER, INTENT(OUT)                     :: INDSO2(MXCHAN)
INTEGER, INTENT(OUT)                     :: CLISO2(MXCHNS)
REAL, INTENT(IN OUT)                     :: COFSO2(  NSO2,MAXLAY,MXCHNS)
INTEGER, INTENT(OUT)                     :: NCHHNO
INTEGER, INTENT(OUT)                     :: INDHNO(MXCHAN)
INTEGER, INTENT(OUT)                     :: CLIHNO(MXCHNH)
REAL, INTENT(IN OUT)                     :: COFHNO( NHNO3,MAXLAY,MXCHNH)
INTEGER, INTENT(OUT)                     :: NCHH2O
INTEGER, INTENT(OUT)                     :: INDH2O(MXCHAN)
INTEGER, INTENT(OUT)                     :: CLIH2O(MXCHNW)
REAL, INTENT(IN OUT)                     :: WAZOP(MXOWLY)
REAL, INTENT(IN OUT)                     :: WAVGOP(NOWAVG,MXOWLY)
REAL, INTENT(IN OUT)                     :: COFH2O(  NH2O,MXOWLY,MXCHNW)
REAL, INTENT(OUT)                        :: FREQ(MXCHAN)
REAL, INTENT(OUT)                        :: COEFF(NFCOEF,MXCHAN)
INTEGER, INTENT(OUT)                     :: NCHNTE
INTEGER, INTENT(OUT)                     :: CLISTN(MXCNTE)
REAL, INTENT(IN OUT)                     :: COEFN(NNCOEF,MXCNTE)
REAL, INTENT(OUT)                        :: FX(MAXLAY)
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
!      Input





!      Output



















































!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
CHARACTER (LEN=80) :: FNCOF1 ! coef set1
CHARACTER (LEN=80) :: FNCOF2 ! coef set2
CHARACTER (LEN=80) :: FNCOF3 ! coef set3
CHARACTER (LEN=80) :: FNCOF4 ! coef set4
CHARACTER (LEN=80) :: FNCOF5 ! coef set5
CHARACTER (LEN=80) :: FNCOF6 ! coef set6
CHARACTER (LEN=80) :: FNCOF7 ! coef set7
CHARACTER (LEN=80) :: FNCO2  ! coef CO2
CHARACTER (LEN=80) :: FNSO2  ! coef SO2
CHARACTER (LEN=80) :: FNHNO3 ! coef HNO3
CHARACTER (LEN=80) :: FNN2O  ! coef N2O
CHARACTER (LEN=80) :: FNOPTR ! coef optran
CHARACTER (LEN=80) :: FNTHER ! coef therm
CHARACTER (LEN=80) :: FNCOFN ! coef non-LTE
CHARACTER (LEN=80) :: CLINE
REAL :: FRQCHN
REAL :: FCHAN(NFCOEF)
REAL :: RJUNK
INTEGER :: I
INTEGER :: IC
INTEGER :: ICHAN
INTEGER :: IERR
INTEGER :: IL
INTEGER :: J
INTEGER :: ICOUNT
!       INTEGER LACHAN


!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none


!***********************************************************************
!***********************************************************************
!                    EXECUTABLE CODE
!***********************************************************************
!***********************************************************************

!      Coefficient filenames
IF (AORB == 'A') THEN
  FNCOF1=FACOF1
  FNCOF2=FACOF2
  FNCOF3=FACOF3
  FNCOF4=FACOF4
  FNCOF5=FACOF5
  FNCOF6=FACOF6
  FNCOF7=FACOF7
  FNCO2 =FACO2
  FNSO2 =FASO2
  FNHNO3=FAHNO3
  FNN2O =FAN2O
  FNOPTR=FAOPTR
  FNTHER=FATHER
  FNCOFN=FACOFN
ELSE
  FNCOF1=FBCOF1
  FNCOF2=FBCOF2
  FNCOF3=FBCOF3
  FNCOF4=FBCOF4
  FNCOF5=FBCOF5
  FNCOF6=FBCOF6
  FNCOF7=FBCOF7
  FNCO2 =FBCO2
  FNSO2 =FBSO2
  FNHNO3=FBHNO3
  FNN2O =FBN2O
  FNOPTR=FBOPTR
  FNTHER=FBTHER
  FNCOFN=FBCOFN
END IF

!      Initialize "set"-independent index arrays
DO I=1,MXCHAN
!         Trace gases
  INDCO2(I)=0
  INDSO2(I)=0
  INDHNO(I)=0
  INDN2O(I)=0
!         OPTRAN water
  INDH2O(I)=0
  ENDDO
    
!      ----------
!      Read set 1
!      ----------
    OPEN(UNIT=IOUN,FILE=FNCOF1,FORM='UNFORMATTED',STATUS='OLD', IOSTAT=IERR)
    IF (IERR /= 0) THEN
      WRITE(6,1020) IERR, FNCOF1
      1020     FORMAT('Error ',I5,' openning file:',/,A80)
      STOP
    END IF
    
    J=1
    DO I=1,MXCHN1
!         Read data for this frequency/channel
      READ(IOUN) ICHAN, FRQCHN, ((COEF1(IC,IL,J),IC=1,N1COEF), IL=1,MAXLAY)
      
      SETCHN(ICHAN)=1
      
!         Keep the data if the current channel is on the list
      IF (INDCHN(ICHAN) /= 0) THEN
        CLIST1(J)=ICHAN
        FREQ( INDCHN(ICHAN) )=FRQCHN
        J=J + 1
      END IF
      ENDDO
        NCHN1=J - 1
        
        CLOSE(IOUN)
        
        
!      ----------
!      Read set 2
!      ----------
        OPEN(UNIT=IOUN,FILE=FNCOF2,FORM='UNFORMATTED',STATUS='OLD',  &
            IOSTAT=IERR)
        IF (IERR /= 0) THEN
          WRITE(6,1020) IERR, FNCOF2
          STOP
        END IF
        
        J=1
        DO I=1,MXCHN2
!         Read data for this frequency/channel
          READ(IOUN) ICHAN, FRQCHN, ((COEF2(IC,IL,J),IC=1,N2COEF),  &
              IL=1,MAXLAY)
          
          SETCHN(ICHAN)=2
          
!         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) /= 0) THEN
            CLIST2(J)=ICHAN
            FREQ( INDCHN(ICHAN) )=FRQCHN
            J=J + 1
          END IF
          ENDDO
            NCHN2=J - 1
            
            CLOSE(IOUN)
            
            
!      ----------
!      Read set 3
!      ----------
            OPEN(UNIT=IOUN,FILE=FNCOF3,FORM='UNFORMATTED',STATUS='OLD',  &
                IOSTAT=IERR)
            IF (IERR /= 0) THEN
              WRITE(6,1020) IERR, FNCOF3
              STOP
            END IF
            
            J=1
            DO I=1,MXCHN3
!         Read data for this frequency/channel
              READ(IOUN) ICHAN, FRQCHN, ((COEF3(IC,IL,J),IC=1,N3COEF),  &
                  IL=1,MAXLAY)
              
              SETCHN(ICHAN)=3
              
!         Keep the data if the current channel is on the list
              IF (INDCHN(ICHAN) /= 0) THEN
                CLIST3(J)=ICHAN
                FREQ( INDCHN(ICHAN) )=FRQCHN
                J=J + 1
              END IF
              ENDDO
                NCHN3=J - 1
                
                CLOSE(IOUN)
                
                
!      ----------
!      Read set 4
!      ----------
                OPEN(UNIT=IOUN,FILE=FNCOF4,FORM='UNFORMATTED',STATUS='OLD',  &
                    IOSTAT=IERR)
                IF (IERR /= 0) THEN
                  WRITE(6,1020) IERR, FNCOF4
                  STOP
                END IF
                
                J=1
                DO I=1,MXCHN4
!         Read data for this frequency/channel
                  READ(IOUN) ICHAN, FRQCHN, ((COEF4(IC,IL,J),IC=1,N4COEF),  &
                      IL=1,MAXLAY)
                  
                  SETCHN(ICHAN)=4
                  
!         Keep the data if the current channel is on the list
                  IF (INDCHN(ICHAN) /= 0) THEN
                    CLIST4(J)=ICHAN
                    FREQ( INDCHN(ICHAN) )=FRQCHN
                    J=J + 1
                  END IF
                  ENDDO
                    NCHN4=J - 1
                    
                    CLOSE(IOUN)
                    
                    
!      ----------
!      Read set 5
!      ----------
                    OPEN(UNIT=IOUN,FILE=FNCOF5,FORM='UNFORMATTED',STATUS='OLD',  &
                        IOSTAT=IERR)
                    IF (IERR /= 0) THEN
                      WRITE(6,1020) IERR, FNCOF5
                      STOP
                    END IF
                    
                    J=1
                    DO I=1,MXCHN5
!         Read data for this frequency/channel
                      READ(IOUN) ICHAN, FRQCHN, ((COEF5(IC,IL,J),IC=1,N5COEF),  &
                          IL=1,MAXLAY)
                      
                      SETCHN(ICHAN)=5
                      
!         Keep the data if the current channel is on the list
                      IF (INDCHN(ICHAN) /= 0) THEN
                        CLIST5(J)=ICHAN
                        FREQ( INDCHN(ICHAN) )=FRQCHN
                        J=J + 1
                      END IF
                      ENDDO
                        NCHN5=J - 1
                        
                        CLOSE(IOUN)
                        
                        
!      ----------
!      Read set 6
!      ----------
                        OPEN(UNIT=IOUN,FILE=FNCOF6,FORM='UNFORMATTED',STATUS='OLD',  &
                            IOSTAT=IERR)
                        IF (IERR /= 0) THEN
                          WRITE(6,1020) IERR, FNCOF6
                          STOP
                        END IF
                        
                        J=1
                        DO I=1,MXCHN6
!         Read data for this frequency/channel
                          READ(IOUN) ICHAN, FRQCHN, ((COEF6(IC,IL,J),IC=1,N6COEF),  &
                              IL=1,MAXLAY)
                          
                          SETCHN(ICHAN)=6
                          
!         Keep the data if the current channel is on the list
                          IF (INDCHN(ICHAN) /= 0) THEN
                            CLIST6(J)=ICHAN
                            FREQ( INDCHN(ICHAN) )=FRQCHN
                            J=J + 1
                          END IF
                          ENDDO
                            NCHN6=J - 1
                            
                            CLOSE(IOUN)
                            
                            
!      ----------
!      Read set 7
!      ----------
                            OPEN(UNIT=IOUN,FILE=FNCOF7,FORM='UNFORMATTED',STATUS='OLD',  &
                                IOSTAT=IERR)
                            IF (IERR /= 0) THEN
                              WRITE(6,1020) IERR, FNCOF7
                              STOP
                            END IF
                            
                            J=1
                            DO I=1,MXCHN7
!         Read data for this frequency/channel
                              READ(IOUN) ICHAN, FRQCHN, ((COEF7(IC,IL,J),IC=1,N7COEF),  &
                                  IL=1,MAXLAY)
                              
                              SETCHN(ICHAN)=7
                              
!         Keep the data if the current channel is on the list
                              IF (INDCHN(ICHAN) /= 0) THEN
                                CLIST7(J)=ICHAN
                                FREQ( INDCHN(ICHAN) )=FRQCHN
                                J=J + 1
                              END IF
                              ENDDO
                                NCHN7=J - 1
                                
                                CLOSE(IOUN)
                                
                                
!      ---------------------------
!      Read CO2 perturbation coefs
!      ---------------------------
                                OPEN(UNIT=IOUN,FILE=FNCO2,FORM='UNFORMATTED',STATUS='OLD',  &
                                    IOSTAT=IERR)
                                IF (IERR /= 0) THEN
                                  WRITE(6,1020) IERR, FNCO2
                                  STOP
                                END IF
                                
                                J=1
                                DO I=1,MXCHNC
!         Read data for this frequency/channel
                                  READ(IOUN) ICHAN, FRQCHN, ((COFCO2(IC,IL,J),IC=1,NCO2),  &
                                      IL=1,MAXLAY)
                                  
!         Keep the data if the current channel is on the list
                                  IF (INDCHN(ICHAN) /= 0) THEN
                                    CLICO2(J)=ICHAN
                                    INDCO2(ICHAN)=J
                                    J=J + 1
                                  END IF
                                  ENDDO
                                    NCHCO2=J - 1
                                    
                                    CLOSE(IOUN)
                                    
                                    
!      ---------------------------
!      Read SO2 perturbation coefs
!      ---------------------------
                                    OPEN(UNIT=IOUN,FILE=FNSO2,FORM='UNFORMATTED',STATUS='OLD',  &
                                        IOSTAT=IERR)
                                    IF (IERR /= 0) THEN
                                      WRITE(6,1020) IERR, FNSO2
                                      STOP
                                    END IF
                                    
                                    J=1
                                    DO I=1,MXCHNS
!         Read data for this frequency/channel
                                      READ(IOUN) ICHAN, FRQCHN, ((COFSO2(IC,IL,J),IC=1,NSO2),  &
                                          IL=1,MAXLAY)
                                      
!         Keep the data if the current channel is on the list
                                      IF (INDCHN(ICHAN) /= 0) THEN
                                        CLISO2(J)=ICHAN
                                        INDSO2(ICHAN)=J
                                        J=J + 1
                                      END IF
                                      ENDDO
                                        NCHSO2=J - 1
                                        
                                        CLOSE(IOUN)
                                        
                                        
!      ---------------------------
!      Read HNO3 perturbation coefs
!      ---------------------------
                                        OPEN(UNIT=IOUN,FILE=FNHNO3,FORM='UNFORMATTED',STATUS='OLD',  &
                                            IOSTAT=IERR)
                                        IF (IERR /= 0) THEN
                                          WRITE(6,1020) IERR, FNHNO3
                                          STOP
                                        END IF
                                        
                                        J=1
                                        DO I=1,MXCHNH
!         Read data for this frequency/channel
                                          READ(IOUN) ICHAN, FRQCHN, ((COFHNO(IC,IL,J),IC=1,NHNO3),  &
                                              IL=1,MAXLAY)
                                          
!         Keep the data if the current channel is on the list
                                          IF (INDCHN(ICHAN) /= 0) THEN
                                            CLIHNO(J)=ICHAN
                                            INDHNO(ICHAN)=J
                                            J=J + 1
                                          END IF
                                          ENDDO
                                            NCHHNO=J - 1
                                            
                                            CLOSE(IOUN)
                                            
                                            
!      ---------------------------
!      Read N2O perturbation coefs
!      ---------------------------
                                            OPEN(UNIT=IOUN,FILE=FNN2O,FORM='UNFORMATTED',STATUS='OLD',  &
                                                IOSTAT=IERR)
                                            IF (IERR /= 0) THEN
                                              WRITE(6,1020) IERR, FNN2O
                                              STOP
                                            END IF
                                            
                                            J=1
                                            DO I=1,MXCHNN
!         Read data for this frequency/channel
                                              READ(IOUN) ICHAN, FRQCHN, ((COFN2O(IC,IL,J),IC=1,NN2O),  &
                                                  IL=1,MAXLAY)
                                              
!         Keep the data if the current channel is on the list
                                              IF (INDCHN(ICHAN) /= 0) THEN
                                                CLIN2O(J)=ICHAN
                                                INDN2O(ICHAN)=J
                                                J=J + 1
                                              END IF
                                              ENDDO
                                                NCHN2O=J - 1
                                                
                                                CLOSE(IOUN)
                                                
                                                
!      ---------------------
!      Read OPTRAN H2O coefs
!      ---------------------
                                                OPEN(UNIT=IOUN,FILE=FNOPTR,FORM='UNFORMATTED',STATUS='OLD',  &
                                                    IOSTAT=IERR)
                                                IF (IERR /= 0) THEN
                                                  WRITE(6,1020) IERR, FNOPTR
                                                  STOP
                                                END IF
                                                
                                                READ(IOUN) (WAZOP(IL),IL=1,MXOWLY)
                                                DO IC=1,NOWAVG
!         Read the header section
                                                  READ(IOUN) (WAVGOP(IC,IL),IL=1,MXOWLY)
                                                  ENDDO
                                                    
                                                    J=1
                                                    DO I=1,MXCHNW
!         Read data for this frequency/channel
                                                      READ(IOUN) ICHAN, FRQCHN, ((COFH2O(IC,IL,J),IC=1,NH2O),  &
                                                          IL=1,MXOWLY)
                                                      
!         Keep the data if the current channel is on the list
                                                      IF (INDCHN(ICHAN) /= 0) THEN
                                                        CLIH2O(J)=ICHAN
                                                        INDH2O(ICHAN)=J
                                                        J=J + 1
                                                      END IF
                                                      ENDDO
                                                        NCHH2O=J - 1
                                                        
                                                        CLOSE(IOUN)
                                                        
                                                        
!      -----------------------------------------------
!      Read the downward thermal F factor coefficients
!      -----------------------------------------------
                                                        OPEN(UNIT=IOUN,FILE=FNTHER,FORM='UNFORMATTED',STATUS='OLD',  &
                                                            IOSTAT=IERR)
                                                        IF (IERR /= 0) THEN
                                                          WRITE(6,1020) IERR, FNTHER
                                                          STOP
                                                        END IF
                                                        
                                                        DO I=1,MXCHAN
!         Read data for this frequency/channel
!cc changed 18 May 2005
!          READ(IOUN) ICHAN, FRQCHN, LACHAN, (FCHAN(IC),IC=1,NFCOEF)
                                                          READ(IOUN) ICHAN, FRQCHN, (FCHAN(IC),IC=1,NFCOEF)
!          LACHAN=-1   ! assign dummy value
!cc
                                                          
!         Keep the data if the current channel is on the list
                                                          IF (INDCHN(ICHAN) /= 0) THEN
                                                            DO IC=1,NFCOEF
                                                              COEFF(IC,INDCHN(ICHAN))=FCHAN(IC)
                                                              ENDDO
                                                              END IF
                                                              ENDDO
                                                                
                                                                CLOSE(IOUN)
                                                                
                                                                
!      -------
!      Read FX
!      -------
                                                                OPEN(UNIT=IOUN,FILE=FNFX,FORM='FORMATTED',STATUS='OLD',  &
                                                                   6IOSTAT=IERR)
                                                                IF (IERR /= 0) THEN
                                                                  WRITE(6,1020) IERR, FNFX
                                                                  STOP
                                                                END IF
                                                                
!      Read the file
                                                                ICOUNT=0
                                                                10    READ(IOUN,9000,END=99) CLINE
                                                                9000  FORMAT(A80)
                                                                IF (CLINE(1:1) /= '!') THEN
!         Note: fx file format is:  layer_number  fx_value
                                                                  READ(CLINE,*) IC, RJUNK
                                                                  ICOUNT=ICOUNT + 1
                                                                  FX(IC)=RJUNK
                                                                END IF
                                                                GO TO 10
                                                                
                                                                99    CLOSE(IOUN)
                                                                
                                                                IF (ICOUNT /= MAXLAY) THEN
                                                                  WRITE(6,1047) MAXLAY, ICOUNT
                                                                  1047     FORMAT('Error! Unexpected number of layers in fx file.',/,  &
                                                                   6  'Expected fx to have ',I4,' layers, but found ',I4)
                                                                END IF
                                                                
                                                                
!      ------------
!      Read non-LTE
!      ------------
                                                                OPEN(UNIT=IOUN,FILE=FNCOFN,FORM='UNFORMATTED',STATUS='OLD',  &
                                                                   6IOSTAT=IERR)
                                                                IF (IERR /= 0) THEN
                                                                  WRITE(6,1020) IERR, FNCOFN
                                                                  STOP
                                                                END IF
                                                                
                                                                J=1
                                                                DO I=1,MXCNTE
!         Read data for this frequency/channel
                                                                  READ(IOUN) ICHAN, FRQCHN, (COEFN(IC,J),IC=1,NNCOEF)
                                                                  
!         Keep the data if the current channel is on the list
                                                                  IF (INDCHN(ICHAN) /= 0) THEN
                                                                   6CLISTN(J)=ICHAN
                                                                   6J=J + 1
                                                                  END IF
                                                                  ENDDO
                                                                   6NCHNTE=J - 1
                                                                   6
                                                                   6CLOSE(IOUN)
                                                                   6
!      ---------------------------------------------
!      Make sure all channels on the list were found
!      ---------------------------------------------
                                                                   6ICOUNT=NCHN1 + NCHN2 + NCHN3 + NCHN4 + NCHN5 + NCHN6 + NCHN7
                                                                   6IF (ICOUNT /= NCHAN) THEN
                                                                   6  WRITE(6,1050) NCHAN, ICOUNT
                                                                   6  1050     FORMAT('Error! Unexpected number of channels found.',/,  &
                                                                   6   H 'The channel list had ',I4,' channels, but found ',I4)
                                                                   6END IF
                                                                   6
!      ----------------------------
!      Show summary of channel sets
!      ----------------------------
!cc
!       WRITE(6,1060) 1, NCHN1
! 1060  FORMAT('Number of channels for set',I1,' = ',I4)
!       WRITE(6,1060) 2, NCHN2
!       WRITE(6,1060) 3, NCHN3
!       WRITE(6,1060) 4, NCHN4
!       WRITE(6,1060) 5, NCHN5
!       WRITE(6,1060) 6, NCHN6
!       WRITE(6,1060) 7, NCHN7
!cc
                                                                   6
                                                                   6RETURN
                                                                  END SUBROUTINE RDCOEF
