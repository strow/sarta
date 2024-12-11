!=======================================================================
 
! Code converted using TO_F90_LOOP by Alan Miller
! Date: 2023-04-04  Time: 16:44:56
 
!=======================================================================

!    University of Maryland Baltimore Country (UMBC)

!    AIRS

!    RDCOEF version with CO2, SO2, HNO3, NH3, N2O, HDO, CH4.

!F77====================================================================


!ROUTINE NAME:
!    RDCOEF_SWCH4


!ABSTRACT:
!    Read in the AIRS fast transmittance coefficients.


!CALL PROTOCOL
!    RDCOEF ( IOUN, NCHAN, INDCHN, SETCHN,
!       NCHN1,  NCHN2,  NCHN3,  NCHN4,  NCHN5,  NCHN6,  NCHN7,
!       CLIST1, CLIST2, CLIST3, CLIST4, CLIST5, CLIST6, CLIST7,
!       COEF1,  COEF2,  COEF3,  COEF4,  COEF5,  COEF6,  COEF7,
!       FREQ,  LABOVE,  COEFF,  INDCO2, COFCO2, INDSO2, COFSO2,
!       INDHNO, COFHNO, INDN2O, COFN2O, INDNH3, COFNH3, INDCH4,
!       COFCH4, INDHDO, COFHDO, INDH2O, WAZOP,  WAVGOP, COFH2O,
!       FX, NCHNTE, CLISTN, COEFN )


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    INT arr   INDCHN  indices of channels         none
!    INTEGER   IOUN    I/O unit number             none
!    INTEGER   NCHAN   number of channels          none


!OUTPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
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
!    REAL arr  COFNH3  NH3 perturbation coefs      various
!    REAL arr  COFHDO  HDO perturbation coefs      various
!    REAL arr  COFH2O  OPTRAN H2O coefs            various
!    REAL arr  COFCH4  s/w CH4 coefs               various
!    REAL arr  FREQ    channel freqs               cm-1
!    REAL arr  FX      fixed gases adjustment      none
!    INT arr   INDCO2  CO2 pert channel indices    none
!    INT arr   INDSO2  SO2 pert channel indices    none
!    INT arr   INDHNO  HNO3 pert channel indices   none
!    INT arr   INDN2O  N2O pert channel indices    none
!    INT arr   INDNH3  NH3 pert channel indices    none
!    INT arr   INDHDO  HDO pert channel indices    none
!    INT arr   INDH2O  OPTRAN H2O channel indices  none
!    INT arr   INDCH4  s/w CH4  channel indices    none
!    INT arr   LABOVE  layer above for thermal     none
!    INTEGER   NCHN1   set1 number of channels     none
!    INTEGER   NCHN2   set2 number of channels     none
!    INTEGER   NCHN3   set3 number of channels     none
!    INTEGER   NCHN4   set4 number of channels     none
!    INTEGER   NCHN5   set5 number of channels     none
!    INTEGER   NCHN6   set6 number of channels     none
!    INTEGER   NCHN7   set7 number of channels     none
!    INTEGER   NCHNTE  non-LTE number of channels  none
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
!    June 2005 version of the 100 layer AIRS Fast Transmittance
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
!    Date        Programmer     Comments
!    ----------- -------------- ----------------------------------------
!    Dec  1 1994 Scott Hannon   Created
!    Dec 21 1994 Scott Hannon   Fixed error with IOPF (now assigned)
!     5 Feb 1997 Scott Hannon   Re-wrote for FWO+FOW+FMW+FCOW.
!    28 Aug 1997 Scott Hannon   Re-wrote for sets 1 - 7 and thermal
!    30 Sep 1997 Scott Hannon   Added COFCO2 and INDCO2
!    27 Feb 1998 Scott Hannon   Added COFH2O, INDH2O, WAZOP, & WAVGOP
!    17 Aug 2000 Scott Hannon   Add FX
!    12 Feb 2001 Scott Hannon   hardcoded filenames instead of prompts
!    18 May 2005 Scott Hannon   Add HNO3 based on SO2 code
!    28 Jun 2005 Scott Hannon   "trace" version for CO2,SO2,HNO3,N2O
!    13 Oct 2005 Scott Hannon   Add non-LTE variables
!    17 Mar 2016 C Hepplewhite  sections ommitted due to absence of
!    coefficients.
!    10 May 2018 C Hepplewhite  Add NH3
!    1  Feb 2019 C Hepplewhite  Add HDO
!                version sets HDO o/d to zero
!    1 Jun 2022  C Hepplewhite. Add shortwave CH4 per Scott's original

!END====================================================================

!      =================================================================

SUBROUTINE RDCOEF ( IOUN, NCHAN, INDCHN, SETCHN,  &
    NCHN1,  NCHN2,  NCHN3,  NCHN4,  NCHN5,  NCHN6,  NCHN7,  &
    CLIST1, CLIST2, CLIST3, CLIST4, CLIST5, CLIST6, CLIST7,  &
    COEF1,  COEF2,  COEF3,  COEF4,  COEF5,  COEF6,  COEF7,  &
    FREQ, LABOVE,  COEFF, INDCO2, COFCO2, INDSO2, COFSO2,  &
    INDHNO, COFHNO, INDN2O, COFN2O, INDNH3, COFNH3, INDCH4,  &
COFCH4, INDHDO  COFHDO, INDH2O,  WAZOP, WAVGOP, COFH2O,  &
      FX, NCHNTE, CLISTN, COEFN )
!      =================================================================
  
  
!-----------------------------------------------------------------------
!      IMPLICIT NONE
!-----------------------------------------------------------------------
  
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
  REAL, INTENT(OUT)                        :: FREQ(MXCHAN)
  INTEGER, INTENT(OUT)                     :: LABOVE(MXCHAN)
  REAL, INTENT(OUT)                        :: COEFF(NFCOEF,MXCHAN)
  INTEGER, INTENT(OUT)                     :: INDCO2(MXCHAN)
  REAL, INTENT(OUT)                        :: COFCO2(  NCO2,MAXLAY,MXCHNC)
  INTEGER, INTENT(OUT)                     :: INDSO2(MXCHAN)
  REAL, INTENT(OUT)                        :: COFSO2(  NSO2,MAXLAY,MXCHNS)
  INTEGER, INTENT(OUT)                     :: INDHNO(MXCHAN)
  REAL, INTENT(OUT)                        :: COFHNO( NHNO3,MAXLAY,MXCHNH)
  INTEGER, INTENT(OUT)                     :: INDN2O(MXCHAN)
  REAL, INTENT(OUT)                        :: COFN2O(  NN2O,MAXLAY,MXCHNN)
  INTEGER, INTENT(OUT)                     :: INDNH3(MXCHAN)
  REAL, INTENT(OUT)                        :: COFNH3(  NNH3,MAXLAY,MXCHNA)
  INTEGER, INTENT(OUT)                     :: INDCH4(MXCHAN)
  REAL, INTENT(OUT)                        :: COFCH4(  NCH4,MAXLAY,MXCHNM)
  NO TYPE, INTENT(IN OUT)                  :: INDHDO  CO
    INTEGER, INTENT(OUT)                     :: INDH2O(MXCHAN)
    REAL, INTENT(IN OUT)                     :: WAZOP(MXOWLY)
    REAL, INTENT(IN OUT)                     :: WAVGOP(NOWAVG,MXOWLY)
    REAL, INTENT(OUT)                        :: COFH2O(  NH2O,MXOWLY,MXCHNW)
    REAL, INTENT(OUT)                        :: FX(MAXLAY)
    INTEGER, INTENT(OUT)                     :: NCHNTE
    INTEGER, INTENT(OUT)                     :: CLISTN(MXCNTE)
    REAL, INTENT(OUT)                        :: COEFN(NNCOEF,MXCNTE)
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
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    INTEGER :: NCHNNH3
    INTEGER :: INDHDO(MXCHAN)
    REAL :: COFHDO(  NHDO,MAXLAY,MXCHND)
    INTEGER :: NCHNHDO
      
      
      INTEGER :: NCHNCH4
      
      
      
      
      
      
      
      
      
      
!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
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
      INTEGER :: LACHAN
      
      
!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none
      
      
!***********************************************************************
!***********************************************************************
!                    EXECUTABLE CODE
!***********************************************************************
!***********************************************************************
      
!      Initialize "set"-independent index arrays
      DO I=1,MXCHAN
!         Trace gases
        INDCO2(I)=0
        INDSO2(I)=0
        INDHNO(I)=0
        INDN2O(I)=0
        INDNH3(I)=0
        INDHDO(I)=0
!         OPTRAN water
        INDH2O(I)=0
        INDCH4(I)=0
        ENDDO
          
!      write(6,*) 'rdcoef: started OK'
!      ----------
!      Read set 1
!      ----------
          OPEN(UNIT=IOUN,FILE=FNCOF1,FORM='UNFORMATTED',STATUS='OLD',  &
              IOSTAT=IERR)
          IF (IERR /= 0) THEN
            WRITE(6,1020) IERR, FNCOF1
            1020     FORMAT('Error ',I5,' opening file:',/,A80)
            STOP
          END IF
          
          J=1
          DO I=1,MXCHN1
!         Read data for this frequency/channel
            READ(IOUN) ICHAN, FRQCHN, ((COEF1(IC,IL,J),IC=1,N1COEF),  &
                IL=1,MAXLAY)
            
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
                                      
!       NCHN7=0
                                      
!      WRITE(6,'(A)') 'Completed rdcoef to set 7'
!      ---------------------------
!      Read CO2 perturbation coefs - placeholder set to zero
!      ---------------------------
                                      IF (CFCO2) THEN
                                        IF (DEBUG) WRITE(6,"('rdcoef:CDCO2=TRUE, read coeff file')")
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
                                            INDCO2(ICHAN)=J
                                            J=J + 1
                                          END IF
                                          ENDDO
                                            
                                            CLOSE(IOUN)
                                            
                                          ELSE
                                            IF (DEBUG) WRITE(6,"('rdcoef:CFCO2=FALSE, null coeffs')")
                                            J=1
                                            DO I=1,MXCHNC
                                              DO IC=1,NCO2
                                                DO IL=1,MAXLAY
                                                  COFCO2(IC,IL,J) = 0.0
                                                  ENDDO
                                                    ENDDO
                                                      IF (INDCHN(ICHAN) /= 0) THEN
                                                        INDCO2(ICHAN)=J
                                                        J=J + 1
                                                      END IF
                                                      ENDDO
                                                        
                                                      END IF
                                                      
!       IF(.NOT. CFCO2) THEN
!         write(6,*) 'rdcoef:not.CFCO2 since CFCO2 is FALSE'
!       ENDIF
                                                      
!      ---------------------
!      Read SO2 pertub coefs - placeholder while no coef file
!      ---------------------
                                                      IF (CFSO2) THEN
                                                        IF (DEBUG) WRITE(6,"('rdcoef:CFSO2=TRUE, read coeff file')")
                                                        OPEN(UNIT=IOUN,FILE=FNSO2,FORM='UNFORMATTED',STATUS='OLD',  &
                                                            IOSTAT=IERR)
                                                        IF (IERR /= 0) THEN
                                                          WRITE(6,1020) IERR, FNSO2
                                                          STOP
                                                        END IF
                                                        
                                                        J=1
                                                        DO I=1,MXCHNS
!         Read data for this frequency/channel
                                                          READ(IOUN) ICHAN, FRQCHN, ((COFSO2(IC,IL,J),IC=1,NSO2),  &
                                                              IL=1,MAXLAY)
!C
!         Keep the data if the current channel is on the list
                                                          IF (INDCHN(ICHAN) /= 0) THEN
                                                            INDSO2(ICHAN)=J
                                                            J=J + 1
                                                          END IF
                                                          ENDDO
                                                            
                                                            CLOSE(IOUN)
                                                            
                                                          ELSE
                                                            IF (DEBUG) WRITE(6,"('rdcoef:CFSO2=FALSE, null coeffs')")
                                                            J=1
                                                            DO I=1,MXCHNS
                                                              DO IC=1,NSO2
                                                                DO IL=1,MAXLAY
                                                                  COFSO2(IC,IL,J) = 0.0
                                                                  ENDDO
                                                                   6ENDDO
                                                                   6  IF (INDCHN(ICHAN) /= 0) THEN
                                                                   6   HINDSO2(ICHAN)=J
                                                                   6   HJ=J + 1
                                                                   6  END IF
                                                                   6  ENDDO
                                                                   6   H
                                                                   6  END IF
                                                                   6  
!      ---------------------
!      Read HNO3 perturb coefs - placeholder while no coef file
!      ---------------------
                                                                   6  IF (CFHNO3) THEN
                                                                   6   HIF (DEBUG) WRITE(6,"('rdcoef:CFHNO3=TRUE, read coeff file')")
                                                                   6   HOPEN(UNIT=IOUN,FILE=FNHNO3,FORM='UNFORMATTED',STATUS='OLD',  &
                                                                   6   H  $IOSTAT=IERR)
                                                                   6   HIF (IERR /= 0) THEN
                                                                   6   H WRITE(6,1020) IERR, FNHNO3
                                                                   6   H STOP
                                                                   6   HEND IF
                                                                   6   H
                                                                   6   HJ=1
                                                                   6   HDO I=1,MXCHNH
!         Read data for this frequency/channel
                                                                   6   H READ(IOUN) ICHAN, FRQCHN, ((COFHNO(IC,IL,J),IC=1,NHNO3),  &
                                                                   6   H  $  IL=1,MAXLAY)
                                                                   6   H 
!         Keep the data if the current channel is on the list
                                                                   6   H IF (INDCHN(ICHAN) /= 0) THEN
                                                                   6   H  $INDHNO(ICHAN)=J
                                                                   6   H  $J=J + 1
                                                                   6   H END IF
                                                                   6   H ENDDO
                                                                   6   H  $
                                                                   6   H  $CLOSE(IOUN)
                                                                   6   H  $
!      ***** no coeff file:
                                                                   6   H ELSE
                                                                   6   H  $IF (DEBUG) WRITE(6,"('rdcoef:CFHNO3=FALSE, null coeffs')")
                                                                   6   H  $J=1
                                                                   6   H  $DO I=1,MXCHNH
                                                                   6   H  $  DO IC=1,NHNO3
                                                                   6   H  $   DO IL=1,MAXLAY
                                                                   6   H  $     COFHNO(IC,IL,J) = 0.0
                                                                   6   H  $     ENDDO
                                                                   6   H  $      ENDDO
!         Keep the data if the current channel is on the list
                                                                   6   H  $        IF (INDCHN(ICHAN) /= 0) THEN
                                                                   6   H  $         7INDHNO(ICHAN)=J
                                                                   6   H  $         7J=J + 1
                                                                   6   H  $        END IF
                                                                   6   H  $        ENDDO
                                                                   6   H  $         7
                                                                   6   H  $         7
                                                                   6   H  $        END IF
!      ---------------------
!      Read N2O perturb coefs - placeholder while no coef file.
!      ---------------------
                                                                   6   H  $        IF (CFN2O) THEN
                                                                   6   H  $         7IF (DEBUG) WRITE(6,*) 'rdcoef:CFN2O=TRUE, read file'
                                                                   6   H  $         7OPEN(UNIT=IOUN,FILE=FNN2O,FORM='UNFORMATTED',STATUS='OLD',  &
                                                                   6   H  $         7   6IOSTAT=IERR)
                                                                   6   H  $         7IF (IERR /= 0) THEN
                                                                   6   H  $         7  WRITE(6,1020) IERR, FNN2O
                                                                   6   H  $         7  STOP
                                                                   6   H  $         7END IF
                                                                   6   H  $         7
                                                                   6   H  $         7J=1
                                                                   6   H  $         7DO I=1,MXCHNN
!         Read data for this frequency/channel
                                                                   6   H  $         7  READ(IOUN) ICHAN, FRQCHN, ((COFN2O(IC,IL,J),IC=1,NN2O),  &
                                                                   6   H  $         7   6  IL=1,MAXLAY)
!         Keep the data if the current channel is on the list
                                                                   6   H  $         7  IF (INDCHN(ICHAN) /= 0) THEN
                                                                   6   H  $         7   6INDN2O(ICHAN)=J
                                                                   6   H  $         7   6J=J + 1
                                                                   6   H  $         7  END IF
                                                                   6   H  $         7  ENDDO
                                                                   6   H  $         7   6CLOSE(IOUN)
                                                                   6   H  $         7   6
! - these lines used as placeholder when no ceofficients are available.
                                                                   6   H  $         7  ELSE
                                                                   6   H  $         7   6IF (DEBUG) WRITE(6,*) 'rdcoef:CFN2O=FALSE null c
                                                                   6   H  $         7   6J=1
                                                                   6   H  $         7   6DO I=1,MXCHNN
                                                                   6   H  $         7   6  DO IC=1,NN2O
                                                                   6   H  $         7   6   HDO IL=1,MAXLAY
                                                                   6   H  $         7   6   H COFN2O(IC,IL,J) = 0.0
                                                                   6   H  $         7   6   H ENDDO
                                                                   6   H  $         7   6   H  $ENDDO
!         Keep the data if the current channel is on the list
                                                                   6   H  $         7   6   H  $  IF (INDCHN(ICHAN) /= 0) THEN
                                                                   6   H  $         7   6   H  $   INDN2O(ICHAN)=J
                                                                   6   H  $         7   6   H  $   J=J + 1
                                                                   6   H  $         7   6   H  $  END IF
                                                                   6   H  $         7   6   H  $  ENDDO
!       write(6,*) 'rdcoef_nh3: read N2O coeffs'
                                                                   6   H  $         7   6   H  $  END IF
                                                                   6   H  $         7   6   H  $  
!      ---------------------------
!      Read NH3 perturbation coefs
!      ---------------------------
                                                                   6   H  $         7   6   H  $  IF (CFNH3) THEN
                                                                   6   H  $         7   6   H  $   IF (DEBUG) WRITE(6,*) 'rdcoef:CFNH3=
                                                                   6   H  $         7   6   H  $   OPEN(UNIT=IOUN,FILE=FNNH3,FORM='UNFORMATTED',STATUS='OLD',  &
                                                                   6   H  $         7   6   H  $      IOSTAT=IERR)
                                                                   6   H  $         7   6   H  $   IF (IERR /= 0) THEN
                                                                   6   H  $         7   6   H  $     WRITE(6,1020) IERR, FNNH3
                                                                   6   H  $         7   6   H  $     STOP
                                                                   6   H  $         7   6   H  $   END IF
                                                                   6   H  $         7   6   H  $   
                                                                   6   H  $         7   6   H  $   J=1
                                                                   6   H  $         7   6   H  $   DO I=1,MXCHNA
!         Read data for this frequency/channel
                                                                   6   H  $         7   6   H  $     READ(IOUN) ICHAN, FRQCHN, ((COFNH3(IC,IL,J),IC=1,NNH3),  &
                                                                   6   H  $         7   6   H  $        IL=1,MAXLAY)
                                                                   6   H  $         7   6   H  $     
!         Keep the data if the current channel is on the list
                                                                   6   H  $         7   6   H  $     IF (INDCHN(ICHAN) /= 0) THEN
                                                                   6   H  $         7   6   H  $      INDNH3(ICHAN)=J
                                                                   6   H  $         7   6   H  $      J=J + 1
                                                                   6   H  $         7   6   H  $     END IF
                                                                   6   H  $         7   6   H  $     ENDDO
                                                                   6   H  $         7   6   H  $      NCHNNH3=J-1
!      write(6,'(A,X,I4)') 'rdcoef.NH3: INDCHN(7235)= ',INDCHN(7235)
                                                                   6   H  $         7   6   H  $      
                                                                   6   H  $         7   6   H  $      CLOSE(IOUN)
                                                                   6   H  $         7   6   H  $      
                                                                   6   H  $         7   6   H  $     ELSE
                                                                   6   H  $         7   6   H  $      IF (DEBUG) WRITE(6,*) 'rdcoef:CF
! - these lines used as placeholder when no ceofficients are available.
                                                                   6   H  $         7   6   H  $      J=1
                                                                   6   H  $         7   6   H  $      DO I=1,MXCHNA
                                                                   6   H  $         7   6   H  $        DO IC=1,NNH3
                                                                   6   H  $         7   6   H  $         7DO IL=1,MAXLAY
                                                                   6   H  $         7   6   H  $         7  COFNH3(IC,IL,J) = 0.0
                                                                   6   H  $         7   6   H  $         7  ENDDO
                                                                   6   H  $         7   6   H  $         7   6ENDDO
!         Keep the data if the current channel is on the list
                                                                   6   H  $         7   6   H  $         7   6  IF (INDCHN(ICHAN) /= 0
                                                                   6   H  $         7   6   H  $         7   6  INDNH3(ICHAN)=J
                                                                   6   H  $         7   6   H  $         7   6  J=J + 1
                                                                   6   H  $         7   6   H  $         7   6END IF
                                                                   6   H  $         7   6   H  $         7   6ENDDO
!       write(6,*) 'rdcoef: completed to NH3'
                                                                   6   H  $         7   6   H  $         7   6END IF
                                                                   6   H  $         7   6   H  $         7   6
!      ---------------------------
!      Read HDO perturbation coefs
!      ---------------------------
                                                                   6   H  $         7   6   H  $         7   6IF (CFHDO) THEN
                                                                   6   H  $         7   6   H  $         7   6  IF (DEBUG) WRITE(6,*)
                                                                   6   H  $         7   6   H  $         7   6  OPEN(UNIT=IOUN,FILE=FNHDO FORM='UNFORMATTED',STATUS='OLD',  &
                                                                   6   H  $         7   6   H  $         7   6   H  $IOSTAT=IERR)
                                                                   6   H  $         7   6   H  $         7   6   HIF (IERR /= 0) THEN
                                                                   6   H  $         7   6   H  $         7   6   H WRITE(6,1020) IERR, FNHDO
                                                                   6   H  $         7   6   H  $         7   6   H  $STOP
                                                                   6   H  $         7   6   H  $         7   6   H END IF
                                                                   6   H  $         7   6   H  $         7   6   H 
!       write(6,'(a,i6,X,i6)') 'rdcoef: MXCHND,NHDO',MXCHND,NHDO
                                                                   6   H  $         7   6   H  $         7   6   H J=1
                                                                   6   H  $         7   6   H  $         7   6   H DO I=1,MXCHND
!         Read data for this frequency/channel
                                                                   6   H  $         7   6   H  $         7   6   H  $READ(IOUN) ICHAN, FRQCHN, ((COFHDO(IC,IL,J),IC=1,NHDO),  &
                                                                   6   H  $         7   6   H  $         7   6   H  $   IL=1,MAXLAY)
                                                                   6   H  $         7   6   H  $         7   6   H  $
!         Keep the data if the current channel is on the list
                                                                   6   H  $         7   6   H  $         7   6   H  $IF (INDCHN(ICHAN
!             write(6,'(A,X,I4,X,I4,X,I5)') 'rdcoef: I,J,INDCHN(ICHAN)= ', I,J,INDCHN(ICHAN)
                                                                   6   H  $         7   6   H  $         7   6   H  $INDHDO(ICHAN)=J
                                                                   6   H  $         7   6   H  $         7   6   H  $J=J + 1
                                                                   6   H  $         7   6   H  $         7   6   H END IF
                                                                   6   H  $         7   6   H  $         7   6   H ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $NCHNHDO=J-1
!       write(6,'(A,X,I4)') 'rdcoef: INDCHN(125)= ',INDCHN(125)
                                                                   6   H  $         7   6   H  $         7   6   H  $
                                                                   6   H  $         7   6   H  $         7   6   H  $CLOSE(IOUN)
                                                                   6   H  $         7   6   H  $         7   6   H  $IF (DEBUG)  WRITE(6,"('rdcoef:completed read HDO coeffs')")
                                                                   6   H  $         7   6   H  $         7   6   H  $  
                                                                   6   H  $         7   6   H  $         7   6   H  $  
                                                                   6   H  $         7   6   H  $         7   6   H  $ELSE
                                                                   6   H  $         7   6   H  $         7   6   H  $  IF (DEBUG) WRI
! - these lines used as placeholder when no ceofficients are available.
                                                                   6   H  $         7   6   H  $         7   6   H  $  J=1
                                                                   6   H  $         7   6   H  $         7   6   H  $  DO I=1,MXCHND
                                                                   6   H  $         7   6   H  $         7   6   H  $   DO IC=1,NHDO
                                                                   6   H  $         7   6   H  $         7   6   H  $     DO IL=1,MAXLAY
                                                                   6   H  $         7   6   H  $         7   6   H  $      COFHDO(IC,IL,J) = 0.0
                                                                   6   H  $         7   6   H  $         7   6   H  $      ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $        ENDDO
!         Keep the data if the current channel is on the list
                                                                   6   H  $         7   6   H  $         7   6   H  $         7IF (
                                                                   6   H  $         7   6   H  $         7   6   H  $         7INDHDO(ICHAN)=J
                                                                   6   H  $         7   6   H  $         7   6   H  $         7J=J + 1
                                                                   6   H  $         7   6   H  $         7   6   H  $        END IF
                                                                   6   H  $         7   6   H  $         7   6   H  $        ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7
                                                                   6   H  $         7   6   H  $         7   6   H  $        END IF
                                                                   6   H  $         7   6   H  $         7   6   H  $        
!      ---------------------------
!      Read CH4 perturbation coefs
!      ---------------------------
                                                                   6   H  $         7   6   H  $         7   6   H  $        IF (CF
                                                                   6   H  $         7   6   H  $         7   6   H  $        IF (DE
                                                                   6   H  $         7   6   H  $         7   6   H  $        OPEN(UNIT=IOUN,FILE=FNCH4,FORM='UNFORMATTED',STATUS='OLD',  &
                                                                   6   H  $         7   6   H  $         7   6   H  $         7  IOSTAT=IERR)
                                                                   6   H  $         7   6   H  $         7   6   H  $        IF (IE
                                                                   6   H  $         7   6   H  $         7   6   H  $        WRITE(6,1020) IERR, FNCH4
                                                                   6   H  $         7   6   H  $         7   6   H  $        STOP
                                                                   6   H  $         7   6   H  $         7   6   H  $      END IF
                                                                   6   H  $         7   6   H  $         7   6   H  $      
                                                                   6   H  $         7   6   H  $         7   6   H  $      J=1
                                                                   6   H  $         7   6   H  $         7   6   H  $      DO I=1,MXCHNM
!         Read data for this frequency/channel
                                                                   6   H  $         7   6   H  $         7   6   H  $        READ(IOUN) ICHAN, FRQCHN, ((COFCH4(IC,IL,J),IC=1,NCH4),  &
                                                                   6   H  $         7   6   H  $         7   6   H  $         7  IL=1,MAXLAY)
                                                                   6   H  $         7   6   H  $         7   6   H  $        
!         Keep the data if the current channel is on the list
                                                                   6   H  $         7   6   H  $         7   6   H  $        IF (IN
                                                                   6   H  $         7   6   H  $         7   6   H  $        INDCH4(ICHAN)=J
                                                                   6   H  $         7   6   H  $         7   6   H  $        J=J + 1
                                                                   6   H  $         7   6   H  $         7   6   H  $      END IF
                                                                   6   H  $         7   6   H  $         7   6   H  $      ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $        
                                                                   6   H  $         7   6   H  $         7   6   H  $        CLOSE(IOUN)
                                                                   6   H  $         7   6   H  $         7   6   H  $        
                                                                   6   H  $         7   6   H  $         7   6   H  $      ELSE
                                                                   6   H  $         7   6   H  $         7   6   H  $        IF (DE
! - these lines used as placeholder when no ceofficients are available.
                                                                   6   H  $         7   6   H  $         7   6   H  $        J=1
                                                                   6   H  $         7   6   H  $         7   6   H  $        DO I=1,MXCHNM
                                                                   6   H  $         7   6   H  $         7   6   H  $         7DO IC=1,NCH4
                                                                   6   H  $         7   6   H  $         7   6   H  $         7  DO IL=1,MAXLAY
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6COFCH4(IC,IL,J) = 0.0
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6  ENDDO
!         Keep the data if the current channel is on the list
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   HINDCH4(ICHAN)=J
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   HJ=J + 1
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6  END IF
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6  ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6  END IF
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6  
!      ---------------------
!      Read OPTRAN H2O coefs - placeholder to disable coefficients
!      ---------------------
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6  OPEN(UNIT=IOUN,FILE=FNOPTR,FORM='UNFORMATTED',STATUS='OLD',  &
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H IOSTAT=IERR)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6  WRITE(6,1020) IERR, FNOPTR
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6  STOP
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6END IF
!       write(6,*) 'rdcoef: opened optran file successfully'
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6READ(IOUN) (WAZOP(IL),IL=1,MXOWLY)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6DO IC=1,NOWAVG
!         Read the header section
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6  READ(IOUN) (WAVGOP(IC,IL),IL=1,MXOWLY)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6  ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H
!       write(6,'(a,X,I6)') 'rdcoef: completed read optran header: MXCHNW',MXCHNW
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   HJ=1
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   HDO I=1,MXCHNW
!         Read data for this frequency/channel
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H READ(IOUN) ICHAN, FRQCHN, ((COFH2O(IC,IL,J),IC=1,NH2O),  &
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $  IL=1,MXOWLY)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H 
!         Keep the data if the current channel is on the list
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H INDH2O(ICHAN)=J
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H J=J + 1
!             write(6,'(a,X,I4,X,I6)') 'rdcoef:J, INDH2O(ICHAN)', J,INDH2O(ICHAN)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   HEND IF
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   HENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H 
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H CLOSE(IOUN)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H 
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   HELSE
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H 
!      these loops for zeroing out optran coefficients
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H J=1
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H DO I=1,MXCHNW
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $DO IC=1,NH2O
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $  DO IL=1,MXOWLY
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $   COFH2O(IC,IL,J) = 0.0
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $   ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $     ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $      ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $        
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $      END IF
!      -----------------------------------------------
!      Read the downward thermal F factor coefficients
!      -----------------------------------------------
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $      OPEN(UNIT=IOUN,FILE=FNTHER,FORM='UNFORMATTED',STATUS='OLD',  &
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7IOSTAT=IERR)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $      WRITE(6,1020) IERR, FNTHER
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $      STOP
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $     END IF
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $     
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $     DO I=1,MXCHAN
!         Read data for this frequency/channel
!cc changed 18 May 2005
!cc          READ(IOUN) ICHAN, FRQCHN, LACHAN, (FCHAN(IC),IC=1,NFCOEF)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $      READ(IOUN) ICHAN, FRQCHN, (FCHAN(IC),IC=1,NFCOEF)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $      LACHAN=-1   ! assign dummy value
!         Keep the data if the current channel is on the list
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $      LABOVE( INDCHN(ICHAN) )=LACHAN
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $      DO IC=1,NFCOEF
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $        COEFF(IC,INDCHN(ICHAN))=FCHAN(IC)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $        ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $        END IF
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $        ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7CLOSE(IOUN)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $        ELSE
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6
! set to zero - to be used when no coeff file available
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7DO I=1,MXCHAN
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7  DO IC=1,NFCOEF
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6COEFF(IC,I)=0.0
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6  ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6  END IF
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6  
!      -------
!      Read FX
!      -------
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6  OPEN(UNIT=IOUN,FILE=FNFX,FORM='FORMATTED',STATUS='OLD',  &
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H IOSTAT=IERR)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6  WRITE(6,1020) IERR, FNFX
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6  STOP
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6END IF
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6
!      Read the file
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6ICOUNT=0
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   610    READ(IOUN,9000,END=99) CLINE
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   69000  FORMAT(A80)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6
!         Note: fx file format is:  layer_number  fx_value
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6READ(CLINE,*) IC, RJUNK
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6ICOUNT=ICOUNT + 1
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6FX(IC)=RJUNK
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7  END IF
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7  GO TO 10
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7  
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7  99    CLOSE(IOUN)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7  
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7  WRITE(6,1047) MAXLAY, ICOUNT
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7  1047     FORMAT('Error! Unexpected number of layers in fx file.',/,  &
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6  'Expected fx to have ',I4,' layers, but found ',I4)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7END IF
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7
!      ------------
!      Read non-LTE
!      ------------
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7OPEN(UNIT=IOUN,FILE=FNCOFN,FORM='UNFORMATTED',STATUS='OLD',  &
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6IOSTAT=IERR)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7WRITE(6,1020) IERR, FNCOFN
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7STOP
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $        END IF
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $        
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $        J=1
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $        DO I=1,MXCNTE
!         Read data for this frequency/channel
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7READ(IOUN) ICHAN, FRQCHN, (COEFN(IC,J),IC=1,NNCOEF)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7
!         Keep the data if the current channel is on the list
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7CLISTN(J)=ICHAN
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7J=J + 1
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $        END IF
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $        ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7NCHNTE=J - 1
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7CLOSE(IOUN)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $        ELSE
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6
! placeholder set to zero
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7DO I=1,MXCNTE
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7  DO IC=1,NNCOEF
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6COEFN(IC,I)=0.0
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6  ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6  END IF
!      ---------------------------------------------
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6
!      ---------------------------------------------
!      Make sure all channels on the list were found
!      ---------------------------------------------
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6  ICOUNT=NCHN1 + NCHN2 + NCHN3 + NCHN4 + NCHN5 + NCHN6 + NCHN7
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6  WRITE(6,1050) NCHAN, ICOUNT
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6  1050     FORMAT('Error! Unexpected number of channels found.',/,  &
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H 'The channel list had ',I4,' channels, but found ',I4)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6END IF
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6
!      ----------------------------
!      Show summary of channel sets
!      ----------------------------
!cc
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6WRITE(6,1060) 1, NCHN1
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   61060    FORMAT('Number of channels for set',I3,' = ',I5)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6WRITE(6,1060) 2, NCHN2
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6WRITE(6,1060) 3, NCHN3
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6WRITE(6,1060) 4, NCHN4
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6WRITE(6,1060) 5, NCHN5
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6WRITE(6,1060) 6, NCHN6
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6WRITE(6,1060) 7, NCHN7
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6WRITE(6,1060) 11,NCHNNH3
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6WRITE(6,1060) 5, NCHNCH4
!         WRITE(6,1060) 162,NCHNHDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7  END IF
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7  
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7  RETURN
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7END SUBROUTINE RDCOEF
