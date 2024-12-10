C=======================================================================
C=======================================================================
C
C    University of Maryland Baltimore Country (UMBC)
C
C    AIRS
C
C    TUNMLT
C
!F77====================================================================


!ROUTINE NAME:
C    TUNMLT


!ABSTRACT:
C    Apply transmittance tuning multipliers to the coefficients


!CALL PROTOCOL
C    note: call is identical to RDCOEF
C        TUNMLT ( AORB,   IOUN,  NCHAN, INDCHN, SETCHN,
C  $     NCHN1,  NCHN2,  NCHN3,  NCHN4,  NCHN5,  NCHN6,  NCHN7,
C  $    CLIST1, CLIST2, CLIST3, CLIST4, CLIST5, CLIST6, CLIST7,
C  $     COEF1,  COEF2,  COEF3,  COEF4,  COEF5,  COEF6,  COEF7,
C  $    NCHCO2, INDCO2, CLICO2, COFCO2,
C  $    NCHN2O, INDN2O, CLIN2O, COFN2O,
C  $    NCHSO2, INDSO2, CLISO2, COFSO2,
C  $    NCHHNO, INDHNO, CLIHNO, COFHNO,
C  $    NCHH2O, INDH2O, CLIH2O,  WAZOP, WAVGOP, COFH2O,
C  $      FREQ,  COEFF, NCHNTE, CLISTN,  COEFN,     FX)


!INPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    CHAR*1    AORB    specify A or B              none
C    INT arr   CLICO2  CO2 channel list            none
C    INT arr   CLIHNO  HNO3 channel list           none
C    INT arr   CLIH2O  H2O channel list            none
C    INT arr   CLIN2O  N2O channel list            none
C    INT arr   CLISO2  SO2 channel list            none
C    INT arr   CLIST1  set1 channel list           none
C    INT arr   CLIST2  set2 channel list           none
C    INT arr   CLIST3  set3 channel list           none
C    INT arr   CLIST4  set4 channel list           none
C    INT arr   CLIST5  set5 channel list           none
C    INT arr   CLIST6  set6 channel list           none
C    INT arr   CLIST7  set7 channel list           none
C    INT arr   CLISTN  non-LTE channel list        none
C    REAL arr  FREQ    channel freqs               cm-1
C    REAL arr  FX      fixed gases adjustment      none
C    INT arr   INDCHN  indices of channels         none
C    INT arr   INDCO2  CO2 pert channel indices    none
C    INT arr   INDH2O  OPTRAN H2O channel indices  none
C    INT arr   INDHNO  HNO3 pert channel indices   none
C    INT arr   INDN2O  N2O pert channel indices    none
C    INT arr   INDSO2  SO2 pert channel indices    none
C    INTEGER   IOUN    I/O unit number             none
C    INTEGER   NCHAN   number of channels          none
C    INTEGER   NCHN1   set1 number of channels     none
C    INTEGER   NCHN2   set2 number of channels     none
C    INTEGER   NCHN3   set3 number of channels     none
C    INTEGER   NCHN4   set4 number of channels     none
C    INTEGER   NCHN5   set5 number of channels     none
C    INTEGER   NCHN6   set6 number of channels     none
C    INTEGER   NCHN7   set7 number of channels     none
C    INTEGER   NCHCO2  number of CO2 pert chans    none
C    INTEGER   NCHH2O  number of OPTRAN H2O chans  none
C    INTEGER   NCHHNO  number of HNO3 pert chans   none
C    INTEGER   NCHN2O  number of N2O pert chans    none
C    INTEGER   NCHNTE  non-LTE number of channels  none
C    INTEGER   NCHSO2  number of SO2 pert chans    none
C    INT arr   SETCHN  set# (1-7) chan belongs to  none (integer, 1 - 7)
C    REAL arr  WAZOP   OPTRAN water grid           kiloMoles/cm^2
C    REAL arr  WAVGOP  OPTRAN water pred averges   various


!INPUT/OUTPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    REAL arr  COEF1   set1 fast trans coefs       various
C    REAL arr  COEF2   set2 fast trans coefs       various
C    REAL arr  COEF3   set3 fast trans coefs       various
C    REAL arr  COEF4   set4 fast trans coefs       various
C    REAL arr  COEF5   set5 fast trans coefs       various
C    REAL arr  COEF6   set6 fast trans coefs       various
C    REAL arr  COEF7   set7 fast trans coefs       various
C    REAL arr  COEFF   thermal "F" factor coefs    various
C    REAL arr  COEFN   non-LTE coefficients        various
C    REAL arr  COFCO2  CO2 perturbation coefs      various
C    REAL arr  COFH2O  OPTRAN H2O coefs            various
C    REAL arr  COFHNO  HNO3 perturbation coefs     various
C    REAL arr  COFN2O  N2O perturbation coefs      various
C    REAL arr  COFSO2  SO2 perturbation coefs      various

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
C    August 2009 version of SARTA v1.08 code by L.Strow/S.Hannon.
C
C    The routine reads a text file of tuning multipliers and
C    applies them to the fast transmittace coefficients.
C
C    The tuning multiplier file must consist of MXCHAN lines of data
C    sorted (in ascending order) by channel ID and containing the
C    following 9 columns:
C       1    2   3    4     5    6   7   8    9
C       ID RJUNK XF XH2OL XH2OC XO3 XCO XCH4 XNTE
C    where
C       ID = integer, channel ID number
C       RJUNK = real, value is ignored, eg perhaps channel freq
C       XF,XH2OL,XH2OC,XO3,XCO,XCH4,XNTE = real, tuning multipler
C          for fixed, water lines, water continuum, ozone, carbon
C          monoxide, methane, and non-LTE.  The value should
C          be of the order of one.
C    Comment lines may be included anywhere in the tuning multiplier
C    by using a "!" or "%" as the first character on the line.
C


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C Date        Programmer     Comments
C ----------- -------------- ----------------------------------------
C 06 Feb 2003 Scott Hannon   Created
C 11 Nov 2005 Scott Hannon   Replace CO2pert tuning with non-LTE
C 08 Dec 2005 Scott Hannon   Add SO2, HNO3, & N2O to arguments so
C                            tunmlt call is identical to rdcoef
C 09 May 2008 Scott Hannon   Call changed to match 09 May 2008 rdcoef;
C                            add code for AORB filename
C 16 May 2008 Scott Hannon   Add NJUNK to prevent adjusting 7th nonLTE
C                               coef
C 02 Nov 2008 Scott Hannon   Bug fix for NJUNK
C 03 Aug 2009 Scott Hannon   Add CLI<gas> for CO2,HNO,H2O,N2O,SO2 to
C                               call to match rdcoef; simplify OPTRAN
C                               adjustment using CLIH2O

!END====================================================================

C      =================================================================
       SUBROUTINE TUNMLT (  AORB,   IOUN,  NCHAN, INDCHN, SETCHN,
     $     NCHN1,  NCHN2,  NCHN3,  NCHN4,  NCHN5,  NCHN6,  NCHN7,
     $    CLIST1, CLIST2, CLIST3, CLIST4, CLIST5, CLIST6, CLIST7,
     $     COEF1,  COEF2,  COEF3,  COEF4,  COEF5,  COEF6,  COEF7,
     $    NCHCO2, INDCO2, CLICO2, COFCO2,
     $    NCHN2O, INDN2O, CLIN2O, COFN2O,
     $    NCHSO2, INDSO2, CLISO2, COFSO2,
     $    NCHHNO, INDHNO, CLIHNO, COFHNO,
     $    NCHH2O, INDH2O, CLIH2O,  WAZOP, WAVGOP, COFH2O,
     $      FREQ,  COEFF, NCHNTE, CLISTN,  COEFN,     FX)
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
       CHARACTER*1 AORB
       INTEGER   IOUN
       INTEGER  NCHAN
       INTEGER INDCHN(MXCHAN)
       INTEGER SETCHN(MXCHAN)
       INTEGER  NCHN1
       INTEGER  NCHN2
       INTEGER  NCHN3
       INTEGER  NCHN4
       INTEGER  NCHN5
       INTEGER  NCHN6
       INTEGER  NCHN7
       INTEGER CLIST1(MXCHN1)
       INTEGER CLIST2(MXCHN2)
       INTEGER CLIST3(MXCHN3)
       INTEGER CLIST4(MXCHN4)
       INTEGER CLIST5(MXCHN5)
       INTEGER CLIST6(MXCHN6)
       INTEGER CLIST7(MXCHN7)
       INTEGER NCHCO2
       INTEGER INDCO2(MXCHAN)
       INTEGER CLICO2(MXCHNC)
       INTEGER NCHN2O
       INTEGER INDN2O(MXCHAN)
       INTEGER CLIN2O(MXCHNN)
       INTEGER NCHSO2
       INTEGER INDSO2(MXCHAN)
       INTEGER CLISO2(MXCHNS)
       INTEGER NCHHNO
       INTEGER INDHNO(MXCHAN)
       INTEGER CLIHNO(MXCHNH)
       INTEGER NCHH2O
       INTEGER INDH2O(MXCHAN)
       INTEGER CLIH2O(MXCHNW)
       REAL  WAZOP(MXOWLY)
       REAL WAVGOP(NOWAVG,MXOWLY)
       REAL   FREQ(MXCHAN)
       REAL  COEFF(NFCOEF,MXCHAN)
       INTEGER NCHNTE
       INTEGER CLISTN(MXCNTE)
       REAL     FX(MAXLAY)
C
C      INPUT/OUTPUT
       REAL  COEF1(N1COEF,MAXLAY,MXCHN1)
       REAL  COEF2(N2COEF,MAXLAY,MXCHN2)
       REAL  COEF3(N3COEF,MAXLAY,MXCHN3)
       REAL  COEF4(N4COEF,MAXLAY,MXCHN4)
       REAL  COEF5(N5COEF,MAXLAY,MXCHN5)
       REAL  COEF6(N6COEF,MAXLAY,MXCHN6)
       REAL  COEF7(N7COEF,MAXLAY,MXCHN7)
       REAL COFCO2(  NCO2,MAXLAY,MXCHNC)
       REAL COFN2O(  NN2O,MAXLAY,MXCHNN)
       REAL COFSO2(  NSO2,MAXLAY,MXCHNS)
       REAL COFHNO( NHNO3,MAXLAY,MXCHNH)
       REAL COFH2O(  NH2O,MXOWLY,MXCHNW)
       REAL  COEFN(NNCOEF,MXCNTE)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       CHARACTER*80 FNTMLT
       CHARACTER*80  CLINE
       REAL  RJUNK
       REAL XMULTS(7,MXCHAN)
       INTEGER      I
       INTEGER  ICHAN
       INTEGER   IERR
       INTEGER      J
       INTEGER      L
       INTEGER  NJUNK
       LOGICAL USEMLT(7)


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
C      Tuning multiplier filename
       IF (AORB .EQ. 'A') THEN
          FNTMLT=FATMLT
       ELSE
          FNTMLT=FBTMLT
       ENDIF

C      -------------------------------
C      Open the tuning multiplier file
C      -------------------------------
       OPEN(UNIT=IOUN,FILE=FNTMLT,FORM='FORMATTED',STATUS='OLD',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNTMLT
 1020     FORMAT('Error ',I5,' opening file:',/,A80)
          STOP
       ENDIF
C

C      Initialize USEMLT
       DO J=1,7
          USEMLT(J)=.FALSE.
       ENDDO

C      Initialize the channel counter
       I=0

C      -------------------------
C      Read the tuning mult file
C      -------------------------
C      Read a line of text from the file
 10    READ(IOUN,9000,END=910) CLINE
 9000  FORMAT(A80)
C
C      Determine if the text line is data or a comment
       IF (CLINE(1:1) .NE. '!' .AND. CLINE(1:1) .NE. '%') THEN
C
C         It's data, so increment the channel counter
          I=I+1
C
C         Read the data from the text line
          READ(CLINE,*)  ICHAN, RJUNK, (XMULTS(J,I),J=1,7)
C
C         Check that ICHAN agrees with I
          IF (ICHAN .NE. I) THEN
             WRITE(6,1040) I, MXCHAN
 1040        FORMAT('Error reading tuning multipler file:',/,
     $       'Expected channel ID ',I4,' but file has ID ',I4)
             STOP
          ENDIF
C
C         Update USEMLT
          DO J=1,7
             IF (XMULTS(J,I) .LT. 0.9999 .OR.
     $           XMULTS(J,I) .GT. 1.0001) USEMLT(J)=.TRUE.
          ENDDO
C
       ENDIF
C
       GOTO 10
 910   CLOSE(IOUN)

ccc
c       write(6,*) 'usemlt(fixed,H2Oline,H2Ocon,O3,CO,CH4,NTE)=',
c     $    (USEMLT(J),J=1,7)
ccc

C      ------------
C      Adjust Fixed
C      ------------
       IF (USEMLT(1)) THEN

C         Set 1
          DO I=1,NCHN1
             ICHAN=CLIST1(I)
             DO L=1,MAXLAY
                DO J=8,15
                   COEF1(J,L,I)=XMULTS(1,ICHAN)*COEF1(J,L,I)
                ENDDO
             ENDDO
          ENDDO

C         Set 2
          DO I=1,NCHN2
             ICHAN=CLIST2(I)
             DO L=1,MAXLAY
                DO J=8,15
                   COEF2(J,L,I)=XMULTS(1,ICHAN)*COEF2(J,L,I)
                ENDDO
             ENDDO
          ENDDO

C         Set 3
          DO I=1,NCHN3
             ICHAN=CLIST3(I)
             DO L=1,MAXLAY
                DO J=8,15
                   COEF3(J,L,I)=XMULTS(1,ICHAN)*COEF3(J,L,I)
                ENDDO
             ENDDO
          ENDDO

C         Set 4
          DO I=1,NCHN4
             ICHAN=CLIST4(I)
             DO L=1,MAXLAY
                DO J=8,18
                   COEF4(J,L,I)=XMULTS(1,ICHAN)*COEF4(J,L,I)
                ENDDO
             ENDDO
          ENDDO

C         Set 5
          DO I=1,NCHN5
             ICHAN=CLIST5(I)
             DO L=1,MAXLAY
                DO J=8,18
                   COEF5(J,L,I)=XMULTS(1,ICHAN)*COEF5(J,L,I)
                ENDDO
             ENDDO
          ENDDO

C         Set 6
          DO I=1,NCHN6
             ICHAN=CLIST6(I)
             DO L=1,MAXLAY
                DO J=8,15
                   COEF6(J,L,I)=XMULTS(1,ICHAN)*COEF6(J,L,I)
                ENDDO
             ENDDO
          ENDDO

C         Set 7
          DO I=1,NCHN7
             ICHAN=CLIST7(I)
             DO L=1,MAXLAY
                DO J=8,15
                   COEF7(J,L,I)=XMULTS(1,ICHAN)*COEF7(J,L,I)
                ENDDO
             ENDDO
          ENDDO

       ENDIF ! end of Fixed
C
C      ---------------
C      Adjust H2O line
C      ---------------
       IF (USEMLT(2)) THEN

C         Set 1
          DO I=1,NCHN1
             ICHAN=CLIST1(I)
             DO L=1,MAXLAY
                DO J=16,26
                   COEF1(J,L,I)=XMULTS(2,ICHAN)*COEF1(J,L,I)
                ENDDO
             ENDDO
          ENDDO

C         Set 2
          DO I=1,NCHN2
             ICHAN=CLIST2(I)
             DO L=1,MAXLAY
                DO J=26,36
                   COEF2(J,L,I)=XMULTS(2,ICHAN)*COEF2(J,L,I)
                ENDDO
             ENDDO
          ENDDO

C         Set 3
          DO I=1,NCHN3
             ICHAN=CLIST3(I)
             DO L=1,MAXLAY
                DO J=25,35
                   COEF3(J,L,I)=XMULTS(2,ICHAN)*COEF3(J,L,I)
                ENDDO
             ENDDO
          ENDDO

C         Set 4
          DO I=1,NCHN4
             ICHAN=CLIST4(I)
             DO L=1,MAXLAY
                DO J=33,45
                   COEF4(J,L,I)=XMULTS(2,ICHAN)*COEF4(J,L,I)
                ENDDO
             ENDDO
          ENDDO

C         Set 5
          DO I=1,NCHN5
             ICHAN=CLIST5(I)
             DO L=1,MAXLAY
                DO J=19,21
                   COEF5(J,L,I)=XMULTS(2,ICHAN)*COEF5(J,L,I)
                ENDDO
             ENDDO
          ENDDO

C         Set 6
          DO I=1,NCHN6
             ICHAN=CLIST6(I)
             DO L=1,MAXLAY
                DO J=16,22
                   COEF6(J,L,I)=XMULTS(2,ICHAN)*COEF6(J,L,I)
                ENDDO
             ENDDO
          ENDDO

C         Set 7
          DO I=1,NCHN7
             ICHAN=CLIST7(I)
             DO L=1,MAXLAY
                DO J=16,28
                   COEF7(J,L,I)=XMULTS(2,ICHAN)*COEF7(J,L,I)
                ENDDO
             ENDDO
          ENDDO

C         OPTRAN
          DO I=1,NCHH2O
             ICHAN=CLIH2O(I)
             DO L=1,MXOWLY
                DO J=1,NH2O
                   COFH2O(J,L,I)=XMULTS(2,ICHAN)*COFH2O(J,L,I)
                ENDDO
             ENDDO
          ENDDO

ccc Old OPTRAN code
c          DO ICHAN=1,MXCHAN
c             IF (INDH2O(ICHAN) .GT. 0) THEN
c                I=INDH2O(ICHAN)
c                DO L=1,MXOWLY
c                   DO J=1,NH2O
c                      COFH2O(J,L,I)=XMULTS(2,ICHAN)*COFH2O(J,L,I)
c                   ENDDO
c                ENDDO
c             ENDIF
c          ENDDO
ccc

       ENDIF ! end of H2O line

C      --------------
C      Adjust H2O con
C      --------------
       IF (USEMLT(3)) THEN

C         Set 1
          DO I=1,NCHN1
             ICHAN=CLIST1(I)
             DO L=1,MAXLAY
                DO J=1,7
                   COEF1(J,L,I)=XMULTS(3,ICHAN)*COEF1(J,L,I)
                ENDDO
             ENDDO
          ENDDO

C         Set 2
          DO I=1,NCHN2
             ICHAN=CLIST2(I)
             DO L=1,MAXLAY
                DO J=1,7
                   COEF2(J,L,I)=XMULTS(3,ICHAN)*COEF2(J,L,I)
                ENDDO
             ENDDO
          ENDDO

C         Set 3
          DO I=1,NCHN3
             ICHAN=CLIST3(I)
             DO L=1,MAXLAY
                DO J=1,7
                   COEF3(J,L,I)=XMULTS(3,ICHAN)*COEF3(J,L,I)
                ENDDO
             ENDDO
          ENDDO

C         Set 4
          DO I=1,NCHN4
             ICHAN=CLIST4(I)
             DO L=1,MAXLAY
                DO J=1,7
                   COEF4(J,L,I)=XMULTS(3,ICHAN)*COEF4(J,L,I)
                ENDDO
             ENDDO
          ENDDO

C         Set 5
          DO I=1,NCHN5
             ICHAN=CLIST5(I)
             DO L=1,MAXLAY
                DO J=1,7
                   COEF5(J,L,I)=XMULTS(3,ICHAN)*COEF5(J,L,I)
                ENDDO
             ENDDO
          ENDDO

C         Set 6
          DO I=1,NCHN6
             ICHAN=CLIST6(I)
             DO L=1,MAXLAY
                DO J=1,7
                   COEF6(J,L,I)=XMULTS(3,ICHAN)*COEF6(J,L,I)
                ENDDO
             ENDDO
          ENDDO

C         Set 7
          DO I=1,NCHN7
             ICHAN=CLIST7(I)
             DO L=1,MAXLAY
                DO J=1,7
                   COEF7(J,L,I)=XMULTS(3,ICHAN)*COEF7(J,L,I)
                ENDDO
             ENDDO
          ENDDO

       ENDIF ! end of H2O con

C      ------------
C      Adjust ozone
C      ------------
       IF (USEMLT(4)) THEN

C         Set 1
          DO I=1,NCHN1
             ICHAN=CLIST1(I)
             DO L=1,MAXLAY
                DO J=27,31
                   COEF1(J,L,I)=XMULTS(4,ICHAN)*COEF1(J,L,I)
                ENDDO
             ENDDO
          ENDDO

C         Set 2
          DO I=1,NCHN2
             ICHAN=CLIST2(I)
             DO L=1,MAXLAY
                DO J=16,25
                   COEF2(J,L,I)=XMULTS(4,ICHAN)*COEF2(J,L,I)
                ENDDO
             ENDDO
          ENDDO

C         Set 3: none

C         Set 4
          DO I=1,NCHN4
             ICHAN=CLIST4(I)
             DO L=1,MAXLAY
                DO J=30,32
                   COEF4(J,L,I)=XMULTS(4,ICHAN)*COEF4(J,L,I)
                ENDDO
             ENDDO
          ENDDO

C         Set 5
          J=22
          DO I=1,NCHN5
             ICHAN=CLIST5(I)
             DO L=1,MAXLAY
                COEF5(J,L,I)=XMULTS(4,ICHAN)*COEF5(J,L,I)
             ENDDO
          ENDDO

C         Set 6
          J=23
          DO I=1,NCHN6
             ICHAN=CLIST6(I)
             DO L=1,MAXLAY
                COEF6(J,L,I)=XMULTS(4,ICHAN)*COEF6(J,L,I)
             ENDDO
          ENDDO

C         Set 7
          J=29
          DO I=1,NCHN7
             ICHAN=CLIST7(I)
             DO L=1,MAXLAY
                COEF7(J,L,I)=XMULTS(4,ICHAN)*COEF7(J,L,I)
             ENDDO
          ENDDO

       ENDIF ! end of O3


C      ---------
C      Adjust CO
C      ---------
       IF (USEMLT(5)) THEN
C         Set 4
          DO I=1,NCHN4
             ICHAN=CLIST4(I)
             DO L=1,MAXLAY
                DO J=19,29
                   COEF4(J,L,I)=XMULTS(5,ICHAN)*COEF4(J,L,I)
                ENDDO
             ENDDO
          ENDDO
       ENDIF


C      --------------
C      Adjust methane
C      --------------
       IF (USEMLT(6)) THEN
C         Set 3
          DO I=1,NCHN3
             ICHAN=CLIST3(I)
             DO L=1,MAXLAY
                DO J=16,24
                   COEF3(J,L,I)=XMULTS(6,ICHAN)*COEF3(J,L,I)
                ENDDO
             ENDDO
          ENDDO
       ENDIF


C      --------------
C      Adjust non-LTE
C      --------------
       IF (USEMLT(7)) THEN
c wrong          NJUNK=MIN0(NCHNTE,6)
c wrong          DO I=1,NJUNK ! do not adjust 7th coef
          NJUNK=MIN0(NNCOEF,6)
          DO I=1,NCHNTE
             ICHAN=CLISTN(I)
c wrong             DO J=1,NNCOEF
             DO J=1,NJUNK
                COEFN(J,I)=XMULTS(7,ICHAN)*COEFN(J,I)
             ENDDO
          ENDDO
       ENDIF

ccc
cC      ---------------
cC      Adjust CO2 pert
cC      ---------------
c       IF (USEMLT(7)) THEN
c          DO ICHAN=1,MXCHAN
c             IF (INDCO2(ICHAN) .GT. 0) THEN
c                I=INDCO2(ICHAN)
c                DO L=1,MAXLAY
c                   DO J=1,NCO2
cC wrong                      COFCO2(J,L,I)=XMULTS(2,ICHAN)*COFCO2(J,L,I)
c                      COFCO2(J,L,I)=XMULTS(7,ICHAN)*COFCO2(J,L,I)
c                   ENDDO
c                ENDDO
c             ENDIF
c          ENDDO
c       ENDIF
ccc
C
       RETURN
       END
