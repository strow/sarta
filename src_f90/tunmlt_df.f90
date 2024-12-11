!=======================================================================
 
! Code converted using TO_F90_LOOP by Alan Miller
! Date: 2023-04-04  Time: 16:44:56
 
!=======================================================================

!    University of Maryland Baltimore Country (UMBC)

!    AIRS

!    TUNMLT

!F77====================================================================


!ROUTINE NAME:
!    TUNMLT


!ABSTRACT:
!    Apply transmittance tuning multipliers to the coefficients


!CALL PROTOCOL
!    note: call is identical to RDCOEF
!        TUNMLT ( AORB,   IOUN,  NCHAN, INDCHN, SETCHN,
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
!    REAL arr  FREQ    channel freqs               cm-1
!    REAL arr  FX      fixed gases adjustment      none
!    INT arr   INDCHN  indices of channels         none
!    INT arr   INDCO2  CO2 pert channel indices    none
!    INT arr   INDH2O  OPTRAN H2O channel indices  none
!    INT arr   INDHNO  HNO3 pert channel indices   none
!    INT arr   INDN2O  N2O pert channel indices    none
!    INT arr   INDSO2  SO2 pert channel indices    none
!    INTEGER   IOUN    I/O unit number             none
!    INTEGER   NCHAN   number of channels          none
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
!    INT arr   SETCHN  set# (1-7) chan belongs to  none (integer, 1 - 7)
!    REAL arr  WAZOP   OPTRAN water grid           kiloMoles/cm^2
!    REAL arr  WAVGOP  OPTRAN water pred averges   various


!INPUT/OUTPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
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
!    REAL arr  COFH2O  OPTRAN H2O coefs            various
!    REAL arr  COFHNO  HNO3 perturbation coefs     various
!    REAL arr  COFN2O  N2O perturbation coefs      various
!    REAL arr  COFSO2  SO2 perturbation coefs      various

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
!    August 2009 version of SARTA v1.08 code by L.Strow/S.Hannon.

!    The routine reads a text file of tuning multipliers and
!    applies them to the fast transmittace coefficients.

!    The tuning multiplier file must consist of MXCHAN lines of data
!    sorted (in ascending order) by channel ID and containing the
!    following 9 columns:
!       1    2   3    4     5    6   7   8    9
!       ID RJUNK XF XH2OL XH2OC XO3 XCO XCH4 XNTE
!    where
!       ID = integer, channel ID number
!       RJUNK = real, value is ignored, eg perhaps channel freq
!       XF,XH2OL,XH2OC,XO3,XCO,XCH4,XNTE = real, tuning multipler
!          for fixed, water lines, water continuum, ozone, carbon
!          monoxide, methane, and non-LTE.  The value should
!          be of the order of one.
!    Comment lines may be included anywhere in the tuning multiplier
!    by using a "!" or "%" as the first character on the line.



!ALGORITHM REFERENCES:
!    none


!KNOWN BUGS AND LIMITATIONS:
!    none


!ROUTINE HISTORY:
! Date        Programmer     Comments
! ----------- -------------- ----------------------------------------
! 06 Feb 2003 Scott Hannon   Created
! 11 Nov 2005 Scott Hannon   Replace CO2pert tuning with non-LTE
! 08 Dec 2005 Scott Hannon   Add SO2, HNO3, & N2O to arguments so
!                            tunmlt call is identical to rdcoef
! 09 May 2008 Scott Hannon   Call changed to match 09 May 2008 rdcoef;
!                            add code for AORB filename
! 16 May 2008 Scott Hannon   Add NJUNK to prevent adjusting 7th nonLTE
!                               coef
! 02 Nov 2008 Scott Hannon   Bug fix for NJUNK
! 03 Aug 2009 Scott Hannon   Add CLI<gas> for CO2,HNO,H2O,N2O,SO2 to
!                               call to match rdcoef; simplify OPTRAN
!                               adjustment using CLIH2O

!END====================================================================

!      =================================================================

SUBROUTINE TUNMLT (  AORB,   IOUN,  NCHAN, INDCHN, SETCHN,  &
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
INTEGER, INTENT(IN OUT)                  :: INDCHN(MXCHAN)
INTEGER, INTENT(IN OUT)                  :: SETCHN(MXCHAN)
INTEGER, INTENT(IN)                      :: NCHN1
INTEGER, INTENT(IN)                      :: NCHN2
INTEGER, INTENT(IN)                      :: NCHN3
INTEGER, INTENT(IN)                      :: NCHN4
INTEGER, INTENT(IN)                      :: NCHN5
INTEGER, INTENT(IN)                      :: NCHN6
INTEGER, INTENT(IN)                      :: NCHN7
INTEGER, INTENT(IN)                      :: CLIST1(MXCHN1)
INTEGER, INTENT(IN)                      :: CLIST2(MXCHN2)
INTEGER, INTENT(IN)                      :: CLIST3(MXCHN3)
INTEGER, INTENT(IN)                      :: CLIST4(MXCHN4)
INTEGER, INTENT(IN)                      :: CLIST5(MXCHN5)
INTEGER, INTENT(IN)                      :: CLIST6(MXCHN6)
INTEGER, INTENT(IN)                      :: CLIST7(MXCHN7)
REAL, INTENT(OUT)                        :: COEF1(N1COEF,MAXLAY,MXCHN1)
REAL, INTENT(OUT)                        :: COEF2(N2COEF,MAXLAY,MXCHN2)
REAL, INTENT(OUT)                        :: COEF3(N3COEF,MAXLAY,MXCHN3)
REAL, INTENT(OUT)                        :: COEF4(N4COEF,MAXLAY,MXCHN4)
REAL, INTENT(OUT)                        :: COEF5(N5COEF,MAXLAY,MXCHN5)
REAL, INTENT(OUT)                        :: COEF6(N6COEF,MAXLAY,MXCHN6)
REAL, INTENT(OUT)                        :: COEF7(N7COEF,MAXLAY,MXCHN7)
INTEGER, INTENT(IN OUT)                  :: NCHCO2
INTEGER, INTENT(IN OUT)                  :: INDCO2(MXCHAN)
INTEGER, INTENT(IN OUT)                  :: CLICO2(MXCHNC)
REAL, INTENT(IN OUT)                     :: COFCO2(  NCO2,MAXLAY,MXCHNC)
INTEGER, INTENT(IN OUT)                  :: NCHN2O
INTEGER, INTENT(IN OUT)                  :: INDN2O(MXCHAN)
INTEGER, INTENT(IN OUT)                  :: CLIN2O(MXCHNN)
REAL, INTENT(IN OUT)                     :: COFN2O(  NN2O,MAXLAY,MXCHNN)
INTEGER, INTENT(IN OUT)                  :: NCHSO2
INTEGER, INTENT(IN OUT)                  :: INDSO2(MXCHAN)
INTEGER, INTENT(IN OUT)                  :: CLISO2(MXCHNS)
REAL, INTENT(IN OUT)                     :: COFSO2(  NSO2,MAXLAY,MXCHNS)
INTEGER, INTENT(IN OUT)                  :: NCHHNO
INTEGER, INTENT(IN OUT)                  :: INDHNO(MXCHAN)
INTEGER, INTENT(IN OUT)                  :: CLIHNO(MXCHNH)
REAL, INTENT(IN OUT)                     :: COFHNO( NHNO3,MAXLAY,MXCHNH)
INTEGER, INTENT(IN)                      :: NCHH2O
INTEGER, INTENT(IN OUT)                  :: INDH2O(MXCHAN)
INTEGER, INTENT(IN)                      :: CLIH2O(MXCHNW)
REAL, INTENT(IN OUT)                     :: WAZOP(MXOWLY)
REAL, INTENT(IN OUT)                     :: WAVGOP(NOWAVG,MXOWLY)
REAL, INTENT(OUT)                        :: COFH2O(  NH2O,MXOWLY,MXCHNW)
REAL, INTENT(IN OUT)                     :: FREQ(MXCHAN)
REAL, INTENT(IN OUT)                     :: COEFF(NFCOEF,MXCHAN)
INTEGER, INTENT(IN)                      :: NCHNTE
INTEGER, INTENT(IN)                      :: CLISTN(MXCNTE)
REAL, INTENT(OUT)                        :: COEFN(NNCOEF,MXCNTE)
REAL, INTENT(IN OUT)                     :: FX(MAXLAY)
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










































!      INPUT/OUTPUT















!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
CHARACTER (LEN=80) :: FNTMLT
CHARACTER (LEN=80) :: CLINE
REAL :: RJUNK
REAL :: XMULTS(7,MXCHAN)
INTEGER :: I
INTEGER :: ICHAN
INTEGER :: IERR
INTEGER :: J
INTEGER :: L
INTEGER :: NJUNK
LOGICAL :: USEMLT(7)


!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none


!***********************************************************************
!***********************************************************************
!                    EXECUTABLE CODE
!***********************************************************************
!***********************************************************************

!      Tuning multiplier filename
IF (AORB == 'A') THEN
  FNTMLT=FATMLT
ELSE
  FNTMLT=FBTMLT
END IF

!      -------------------------------
!      Open the tuning multiplier file
!      -------------------------------
OPEN(UNIT=IOUN,FILE=FNTMLT,FORM='FORMATTED',STATUS='OLD', IOSTAT=IERR)
IF (IERR /= 0) THEN
  WRITE(6,1020) IERR, FNTMLT
  1020     FORMAT('Error ',I5,' opening file:',/,A80)
  STOP
END IF


!      Initialize USEMLT
DO J=1,7
  USEMLT(J)=.FALSE.
  ENDDO
    
!      Initialize the channel counter
    I=0
    
!      -------------------------
!      Read the tuning mult file
!      -------------------------
!      Read a line of text from the file
    10    READ(IOUN,9000,END=910) CLINE
    9000  FORMAT(A80)
    
!      Determine if the text line is data or a comment
    IF (CLINE(1:1) /= '!' .AND. CLINE(1:1) /= '%') THEN
      
!         It's data, so increment the channel counter
      I=I+1
      
!         Read the data from the text line
      READ(CLINE,*)  ICHAN, RJUNK, (XMULTS(J,I),J=1,7)
      
!         Check that ICHAN agrees with I
      IF (ICHAN /= I) THEN
        WRITE(6,1040) I, MXCHAN
        1040        FORMAT('Error reading tuning multipler file:',/,  &
            'Expected channel ID ',I4,' but file has ID ',I4)
        STOP
      END IF
      
!         Update USEMLT
      DO J=1,7
        IF (XMULTS(J,I) < 0.9999 .OR. XMULTS(J,I) > 1.0001) USEMLT(J)=.TRUE.
        ENDDO
          
        END IF
        
        GO TO 10
        910   CLOSE(IOUN)
        
!cc
!       write(6,*) 'usemlt(fixed,H2Oline,H2Ocon,O3,CO,CH4,NTE)=',
!     $    (USEMLT(J),J=1,7)
!cc
        
!      ------------
!      Adjust Fixed
!      ------------
        IF (USEMLT(1)) THEN
          
!         Set 1
          DO I=1,NCHN1
            ICHAN=CLIST1(I)
            DO L=1,MAXLAY
              DO J=8,15
                COEF1(J,L,I)=XMULTS(1,ICHAN)*COEF1(J,L,I)
                ENDDO
                  ENDDO
                    ENDDO
                      
!         Set 2
                      DO I=1,NCHN2
                        ICHAN=CLIST2(I)
                        DO L=1,MAXLAY
                          DO J=8,15
                            COEF2(J,L,I)=XMULTS(1,ICHAN)*COEF2(J,L,I)
                            ENDDO
                              ENDDO
                                ENDDO
                                  
!         Set 3
                                  DO I=1,NCHN3
                                    ICHAN=CLIST3(I)
                                    DO L=1,MAXLAY
                                      DO J=8,15
                                        COEF3(J,L,I)=XMULTS(1,ICHAN)*COEF3(J,L,I)
                                        ENDDO
                                          ENDDO
                                            ENDDO
                                              
!         Set 4
                                              DO I=1,NCHN4
                                                ICHAN=CLIST4(I)
                                                DO L=1,MAXLAY
                                                  DO J=8,18
                                                    COEF4(J,L,I)=XMULTS(1,ICHAN)*COEF4(J,L,I)
                                                    ENDDO
                                                      ENDDO
                                                        ENDDO
                                                          
!         Set 5
                                                          DO I=1,NCHN5
                                                            ICHAN=CLIST5(I)
                                                            DO L=1,MAXLAY
                                                              DO J=8,18
                                                                COEF5(J,L,I)=XMULTS(1,ICHAN)*COEF5(J,L,I)
                                                                ENDDO
                                                                  ENDDO
                                                                   6ENDDO
                                                                   6  
!         Set 6
                                                                   6  DO I=1,NCHN6
                                                                   6   HICHAN=CLIST6(I)
                                                                   6   HDO L=1,MAXLAY
                                                                   6   H DO J=8,15
                                                                   6   H  $COEF6(J,L,I)=XMULTS(1,ICHAN)*COEF6(J,L,I)
                                                                   6   H  $ENDDO
                                                                   6   H  $  ENDDO
                                                                   6   H  $   ENDDO
                                                                   6   H  $     
!         Set 7
                                                                   6   H  $     DO I=1,NCHN7
                                                                   6   H  $      ICHAN=CLIST7(I)
                                                                   6   H  $      DO L=1,MAXLAY
                                                                   6   H  $        DO J=8,15
                                                                   6   H  $         7COEF7(J,L,I)=XMULTS(1,ICHAN)*COEF7(J,L,I)
                                                                   6   H  $         7ENDDO
                                                                   6   H  $         7  ENDDO
                                                                   6   H  $         7   6ENDDO
                                                                   6   H  $         7   6  
                                                                   6   H  $         7   6END IF ! end of Fixed
                                                                   6   H  $         7   6
!      ---------------
!      Adjust H2O line
!      ---------------
                                                                   6   H  $         7   6IF (USEMLT(2)) THEN
                                                                   6   H  $         7   6  
!         Set 1
                                                                   6   H  $         7   6  DO I=1,NCHN1
                                                                   6   H  $         7   6   HICHAN=CLIST1(I)
                                                                   6   H  $         7   6   HDO L=1,MAXLAY
                                                                   6   H  $         7   6   H DO J=16,26
                                                                   6   H  $         7   6   H  $COEF1(J,L,I)=XMULTS(2,ICHAN)*COEF1(J,L,I)
                                                                   6   H  $         7   6   H  $ENDDO
                                                                   6   H  $         7   6   H  $  ENDDO
                                                                   6   H  $         7   6   H  $   ENDDO
                                                                   6   H  $         7   6   H  $     
!         Set 2
                                                                   6   H  $         7   6   H  $     DO I=1,NCHN2
                                                                   6   H  $         7   6   H  $      ICHAN=CLIST2(I)
                                                                   6   H  $         7   6   H  $      DO L=1,MAXLAY
                                                                   6   H  $         7   6   H  $        DO J=26,36
                                                                   6   H  $         7   6   H  $         7COEF2(J,L,I)=XMULTS(2,ICHAN)*COEF2(J,L,I)
                                                                   6   H  $         7   6   H  $         7ENDDO
                                                                   6   H  $         7   6   H  $         7  ENDDO
                                                                   6   H  $         7   6   H  $         7   6ENDDO
                                                                   6   H  $         7   6   H  $         7   6  
!         Set 3
                                                                   6   H  $         7   6   H  $         7   6  DO I=1,NCHN3
                                                                   6   H  $         7   6   H  $         7   6   HICHAN=CLIST3(I)
                                                                   6   H  $         7   6   H  $         7   6   HDO L=1,MAXLAY
                                                                   6   H  $         7   6   H  $         7   6   H DO J=25,35
                                                                   6   H  $         7   6   H  $         7   6   H  $COEF3(J,L,I)=XMULTS(2,ICHAN)*COEF3(J,L,I)
                                                                   6   H  $         7   6   H  $         7   6   H  $ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $  ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $   ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $     
!         Set 4
                                                                   6   H  $         7   6   H  $         7   6   H  $     DO I=1,NCHN4
                                                                   6   H  $         7   6   H  $         7   6   H  $      ICHAN=CLIST4(I)
                                                                   6   H  $         7   6   H  $         7   6   H  $      DO L=1,MAXLAY
                                                                   6   H  $         7   6   H  $         7   6   H  $        DO J=33,45
                                                                   6   H  $         7   6   H  $         7   6   H  $         7COEF4(J,L,I)=XMULTS(2,ICHAN)*COEF4(J,L,I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7  ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6  
!         Set 5
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6  DO I=1,NCHN5
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   HICHAN=CLIST5(I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   HDO L=1,MAXLAY
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H DO J=19,21
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $COEF5(J,L,I)=XMULTS(2,ICHAN)*COEF5(J,L,I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $  ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $   ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $     
!         Set 6
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $     DO I=1,NCHN6
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $      ICHAN=CLIST6(I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $      DO L=1,MAXLAY
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $        DO J=16,22
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7COEF6(J,L,I)=XMULTS(2,ICHAN)*COEF6(J,L,I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7  ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6  
!         Set 7
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6  DO I=1,NCHN7
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   HICHAN=CLIST7(I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   HDO L=1,MAXLAY
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H DO J=16,28
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $COEF7(J,L,I)=XMULTS(2,ICHAN)*COEF7(J,L,I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $  ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $   ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $     
!         OPTRAN
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $     DO I=1,NCHH2O
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $      ICHAN=CLIH2O(I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $      DO L=1,MXOWLY
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $        DO J=1,NH2O
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7COFH2O(J,L,I)=XMULTS(2,ICHAN)*COFH2O(J,L,I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7  ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6  
!cc Old OPTRAN code
!          DO ICHAN=1,MXCHAN
!             IF (INDH2O(ICHAN) .GT. 0) THEN
!                I=INDH2O(ICHAN)
!                DO L=1,MXOWLY
!                   DO J=1,NH2O
!                      COFH2O(J,L,I)=XMULTS(2,ICHAN)*COFH2O(J,L,I)
!                   ENDDO
!                ENDDO
!             ENDIF
!          ENDDO
!cc
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6  
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6END IF ! end of H2O line
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6
!      --------------
!      Adjust H2O con
!      --------------
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6
!         Set 1
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6DO I=1,NCHN1
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6  ICHAN=CLIST1(I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6  DO L=1,MAXLAY
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   HDO J=1,7
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H COEF1(J,L,I)=XMULTS(3,ICHAN)*COEF1(J,L,I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $  ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $   
!         Set 2
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $   DO I=1,NCHN2
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $     ICHAN=CLIST2(I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $     DO L=1,MAXLAY
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $      DO J=1,7
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $        COEF2(J,L,I)=XMULTS(3,ICHAN)*COEF2(J,L,I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $        ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7  ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6
!         Set 3
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6DO I=1,NCHN3
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6  ICHAN=CLIST3(I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6  DO L=1,MAXLAY
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   HDO J=1,7
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H COEF3(J,L,I)=XMULTS(3,ICHAN)*COEF3(J,L,I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $  ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $   
!         Set 4
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $   DO I=1,NCHN4
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $     ICHAN=CLIST4(I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $     DO L=1,MAXLAY
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $      DO J=1,7
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $        COEF4(J,L,I)=XMULTS(3,ICHAN)*COEF4(J,L,I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $        ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7  ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6
!         Set 5
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6DO I=1,NCHN5
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6  ICHAN=CLIST5(I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6  DO L=1,MAXLAY
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   HDO J=1,7
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H COEF5(J,L,I)=XMULTS(3,ICHAN)*COEF5(J,L,I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $  ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $   
!         Set 6
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $   DO I=1,NCHN6
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $     ICHAN=CLIST6(I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $     DO L=1,MAXLAY
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $      DO J=1,7
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $        COEF6(J,L,I)=XMULTS(3,ICHAN)*COEF6(J,L,I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $        ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7  ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6
!         Set 7
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6DO I=1,NCHN7
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6  ICHAN=CLIST7(I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6  DO L=1,MAXLAY
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   HDO J=1,7
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H COEF7(J,L,I)=XMULTS(3,ICHAN)*COEF7(J,L,I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $  ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $   
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $  END IF ! end of H2O con
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $  
!      ------------
!      Adjust ozone
!      ------------
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $  
!         Set 1
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $  DO I=1,NCHN1
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $   ICHAN=CLIST1(I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $   DO L=1,MAXLAY
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $     DO J=27,31
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $      COEF1(J,L,I)=XMULTS(4,ICHAN)*COEF1(J,L,I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $      ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $        ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7  
!         Set 2
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7  DO I=1,NCHN2
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7    ICHAN=CLIST2(I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7    DO L=1,MAXLAY
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7      DO J=16,25
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7        COEF2(J,L,I)=XMULTS(4,ICHAN)*COEF2(J,L,I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7        ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7          ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           AENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS
!         Set 3: none
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS
!         Set 4
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABSDO I=1,NCHN4
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS  ICHAN=CLIST4(I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS  DO L=1,MAXLAY
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS    DO J=30,32
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS      COEF4(J,L,I)=XMULTS(4,ICHAN)*COEF4(J,L,I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS      ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCE
!         Set 5
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCEJ=22
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCEDO I=1,NCHN5
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESSICHAN=CLIST5(I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESSDO L=1,MAXLAY
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS  COEF5(J,L,I)=XMULTS(4,ICHAN)*COEF5(J,L,I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS  ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS    ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     A
!         Set 6
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     AJ=23
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ADO I=1,NCHN6
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOICHAN=CLIST6(I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACODO L=1,MAXLAY
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS COEF6(J,L,I)=XMULTS(4,ICHAN)*COEF6(J,L,I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS   ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS     
!         Set 7
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS     J=29
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS     DO I=1,NCHN7
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       ICHAN=CLIST7(I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       DO L=1,MAXLAY
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AICOEF7(J,L,I)=XMULTS(4,ICHAN)*COEF7(J,L,I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG 
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAEND IF ! end of O3
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMA
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMA
!      ---------
!      Adjust CO
!      ---------
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6
!         Set 4
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMADO I=1,NCHN4
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG ICHAN=CLIST4(I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG DO L=1,MAXLAY
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG   DO J=19,29
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG     COEF4(J,L,I)=XMULTS(5,ICHAN)*COEF4(J,L,I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG     ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG      AENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG      AINENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG      AINEND IF
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG      AIN
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG      AIN
!      --------------
!      Adjust methane
!      --------------
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6
!         Set 3
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG      AINDO I=1,NCHN3
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG      AINT ICHAN=CLIST3(I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG      AINT DO L=1,MAXLAY
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG      AINT   DO J=16,24
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG      AINT     COEF3(J,L,I)=XMULTS(6,ICHAN)*COEF3(J,L,I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG      AINT     ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG      AINT       ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG      AINT       ALENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG      AINT       ALEND IF
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG      AINT       AL
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG      AINT       AL
!      --------------
!      Adjust non-LTE
!      --------------
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6
! wrong          NJUNK=MIN0(NCHNTE,6)
! wrong          DO I=1,NJUNK ! do not adjust 7th coef
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG      AINT       ALNJUNK=MIN0(NNCOEF,6)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG      AINT       ALDO I=1,NCHNTE
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG      AINT       ALOGICHAN=CLISTN(I)
! wrong             DO J=1,NNCOEF
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG      AINT       ALOGDO J=1,NJUNK
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG      AINT       ALOG  COEFN(J,I)=XMULTS(7,ICHAN)*COEFN(J,I)
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG      AINT       ALOG  ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG      AINT       ALOG    ENDDO
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG      AINT       ALOG    END IF
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG      AINT       ALOG    
!cc
!C      ---------------
!C      Adjust CO2 pert
!C      ---------------
!       IF (USEMLT(7)) THEN
!          DO ICHAN=1,MXCHAN
!             IF (INDCO2(ICHAN) .GT. 0) THEN
!                I=INDCO2(ICHAN)
!                DO L=1,MAXLAY
!                   DO J=1,NCO2
!C wrong                      COFCO2(J,L,I)=XMULTS(2,ICHAN)*COFCO2(J,L,I)
!                      COFCO2(J,L,I)=XMULTS(7,ICHAN)*COFCO2(J,L,I)
!                   ENDDO
!                ENDDO
!             ENDIF
!          ENDDO
!       ENDIF
!cc
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG      AINT       ALOG    
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG      AINT       ALOG    RETURN
                                                                   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7   6   H  $         7           ABS        ACCESS     ACOS       AIMAG      AINT       ALOG  END SUBROUTINE TUNMLT
