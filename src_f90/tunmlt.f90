!=======================================================================
!
!    University of Maryland Baltimore Country (UMBC)
!
!    AIRS
!
!    TUNMLT
!
!F90====================================================================


!ROUTINE NAME:
!    TUNMLT


!ABSTRACT:
!    Apply transmittance tuning multipliers to the coefficients


!CALL PROTOCOL
!    note: call is identical to RDCOEF_nte
!    TUNMLT ( IPOPN, NCHAN, INDCHN, SETCHN,
!       NCHN1, NCHN2, NCHN3, NCHN4, NCHN5, NCHN6, NCHN7,
!       CLIST1, CLIST2, CLIST3, CLIST4, CLIST5, CLIST6, CLIST7,
!       COEF1, COEF2, COEF3, COEF4, COEF5, COEF6, COEF7,
!       FREQ, LABOVE, COEFF, INDCO2, COFCO2, INDSO2, COFSO2,
!       INDHNO, COFHNO, INDN2O, COFN2O, INDH2O, WAZOP, WAVGOP,
!       COFH2O, FX, CLISTN, COEFN )


!INPUT PARAMETERS:
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
!    REAL arr  FREQ    channel freqs               cm-1
!    REAL arr  FX      fixed gases adjustment      none
!    INT arr   INDCHN  indices of channels         none
!    INT arr   INDCO2  CO2 pert channel indices    none
!    INT arr   INDH2O  OPTRAN H2O channel indices  none
!    INT arr   INDHNO  HNO3 pert channel indices   none
!    INT arr   INDN2O  N2O pert channel indices    none
!    INT arr   INDSO2  SO2 pert channel indices    none
!    INTEGER   IPOPN    I/O unit number             none
!    INT arr   LABOVE  layer above for thermal     none
!    INTEGER   NCHAN   number of channels          none
!    INTEGER   NCHN1   set1 number of channels     none
!    INTEGER   NCHN2   set2 number of channels     none
!    INTEGER   NCHN3   set3 number of channels     none
!    INTEGER   NCHN4   set4 number of channels     none
!    INTEGER   NCHN5   set5 number of channels     none
!    INTEGER   NCHN6   set6 number of channels     none
!    INTEGER   NCHN7   set7 number of channels     none
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
!    unit IPOPN : input text file of tuning adjustments.


!COMMON BLOCKS
!    none


!DESCRIPTION:
!    December 2005 version of SARTA v1.07 code by L.Strow/S.Hannon.
!
!    The routine reads a text file of tuning multipliers and
!    applies them to the fast transmittace coefficients.
!
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
!


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
!                               tunmlt call is identical to rdcoef
! 16 May 2008 Scott Hannon   Add NJUNK to prevent adjusting 7th nonNTE
!                               coef
! 02 Nov 2008 Scott Hannon   Bug fix: NJUNK wrong

!END====================================================================

!      =================================================================
       SUBROUTINE TUNMLT ( IPOPN, NCHAN, INDCHN, SETCHN, &
          NCHN1,  NCHN2,  NCHN3,  NCHN4,  NCHN5,  NCHN6,  NCHN7, &
         CLIST1, CLIST2, CLIST3, CLIST4, CLIST5, CLIST6, CLIST7, &
          COEF1,  COEF2,  COEF3,  COEF4,  COEF5,  COEF6,  COEF7, &
           FREQ, LABOVE, COEFF,  INDCO2, COFCO2, INDSO2, COFSO2, &
         INDHNO, COFHNO, INDN2O, COFN2O, &
         INDH2O,  WAZOP, WAVGOP, COFH2O, FX)  ! , CLISTN, COEFN )
!      =================================================================


!-----------------------------------------------------------------------
!      INCLUDE FILES
!-----------------------------------------------------------------------
use incFTC

!-----------------------------------------------------------------------
!      IMPLICIT NONE
!-----------------------------------------------------------------------
       IMPLICIT NONE

!-----------------------------------------------------------------------
!      EXTERNAL FUNCTIONS
!-----------------------------------------------------------------------
!      none

!-----------------------------------------------------------------------
!      ARGUMENTS
!-----------------------------------------------------------------------
!      INPUT
integer :: IPOPN, NCHAN, NCHN1, NCHN2, NCHN3, NCHN4, NCHN5, NCHN6, NCHN7
! !integer :: NCHNTE
integer, dimension(MXCHAN) :: INDCHN, SETCHN
real(4), dimension(MXCHAN) :: FREQ
       INTEGER CLIST1(MXCHN1)
       INTEGER CLIST2(MXCHN2)
       INTEGER CLIST3(MXCHN3)
       INTEGER CLIST4(MXCHN4)
       INTEGER CLIST5(MXCHN5)
       INTEGER CLIST6(MXCHN6)
       INTEGER CLIST7(MXCHN7)
       REAL  COEFF(NFCOEF,MXCHAN)
integer, dimension(MXCHAN) :: LABOVE, INDCO2, INDH2O, INDHNO, INDN2O
integer, dimension(MXCHAN) :: INDSO2
       REAL  WAZOP(MXOWLY)
       REAL WAVGOP(NOWAVG,MXOWLY)
       REAL     FX(MAXLAY)
       INTEGER CLISTN(MXCNTE)
!
!      INPUT/OUTPUT
real(4), dimension(N1COEF,MAXLAY,MXCHN1) :: COEF1
real(4), dimension(N2COEF,MAXLAY,MXCHN2) :: COEF2
real(4), dimension(N3COEF,MAXLAY,MXCHN3) :: COEF3
real(4), dimension(N4COEF,MAXLAY,MXCHN4) :: COEF4
real(4), dimension(N5COEF,MAXLAY,MXCHN5) :: COEF5
real(4), dimension(N6COEF,MAXLAY,MXCHN6) :: COEF6
real(4), dimension(N7COEF,MAXLAY,MXCHN7) :: COEF7
       REAL COFCO2(  NCO2,MAXLAY,MXCHNC)
       REAL COFH2O(  NH2O,MXOWLY,MXCHNW)
       REAL COFHNO( NHNO3,MAXLAY,MXCHNH)
       REAL COFN2O(  NN2O,MAXLAY,MXCHNN)
       REAL COFSO2(  NSO2,MAXLAY,MXCHNS)
       REAL  COEFN(NNCOEF,MXCNTE)

!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
character(len=80) :: CLINE
       REAL  RJUNK
       REAL XMULTS(7,MXCHAN)
integer :: I, J, L, ICHAN, IERR, NJUNK
LOGICAL, dimension(7) :: USEMLT


!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none

!***********************************************************************
!                    EXECUTABLE CODE
!***********************************************************************
!

!      -------------------------------
!      Open the tuning multiplier file
!      -------------------------------
       OPEN(UNIT=IPOPN,FILE=FNTMLT,FORM='FORMATTED',STATUS='OLD', &
         IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNTMLT
 1020     FORMAT('Error ',I5,' opening file:',/,A80)
          STOP
       ENDIF
!

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
 10    READ(IPOPN,9000,END=910) CLINE
 9000  FORMAT(A80)
!
!      Determine if the text line is data or a comment
       IF (CLINE(1:1) .NE. '!' .AND. CLINE(1:1) .NE. '%') THEN
!
!         It's data, so increment the channel counter
          I=I+1
!
!         Read the data from the text line
          READ(CLINE,*)  ICHAN, RJUNK, (XMULTS(J,I),J=1,7)
!
!         Check that ICHAN agrees with I
          IF (ICHAN .NE. I) THEN
             WRITE(6,1040) I, MXCHAN
 1040        FORMAT('Error reading tuning multipler file:',/, &
            'Expected channel ID ',I4,' but file has ID ',I4)
             STOP
          ENDIF
!
!         Update USEMLT
          DO J=1,7
             IF (XMULTS(J,I) .LT. 0.9999 .OR. &
                XMULTS(J,I) .GT. 1.0001) USEMLT(J)=.TRUE.
          ENDDO
!
       ENDIF
!
       GOTO 10
 910   CLOSE(IPOPN)

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
                   COEF4(J,L,I)=XMULTS(1,ICHAN)*COEF4(J,L,I)
                ENDDO
             ENDDO
          ENDDO

!         Set 5
          DO I=1,NCHN5
             ICHAN=CLIST5(I)
             DO L=1,MAXLAY
                DO J=8,18
                   COEF5(J,L,I)=XMULTS(1,ICHAN)*COEF5(J,L,I)
                ENDDO
             ENDDO
          ENDDO

!         Set 6
          DO I=1,NCHN6
             ICHAN=CLIST6(I)
             DO L=1,MAXLAY
                DO J=8,15
                   COEF6(J,L,I)=XMULTS(1,ICHAN)*COEF6(J,L,I)
                ENDDO
             ENDDO
          ENDDO

!         Set 7
          DO I=1,NCHN7
             ICHAN=CLIST7(I)
             DO L=1,MAXLAY
                DO J=8,15
                   COEF7(J,L,I)=XMULTS(1,ICHAN)*COEF7(J,L,I)
                ENDDO
             ENDDO
          ENDDO

       ENDIF ! end of Fixed
!
!      ---------------
!      Adjust H2O line
!      ---------------
       IF (USEMLT(2)) THEN

!         Set 1
          DO I=1,NCHN1
             ICHAN=CLIST1(I)
             DO L=1,MAXLAY
                DO J=16,26
                   COEF1(J,L,I)=XMULTS(2,ICHAN)*COEF1(J,L,I)
                ENDDO
             ENDDO
          ENDDO

!         Set 2
          DO I=1,NCHN2
             ICHAN=CLIST2(I)
             DO L=1,MAXLAY
                DO J=26,36
                   COEF2(J,L,I)=XMULTS(2,ICHAN)*COEF2(J,L,I)
                ENDDO
             ENDDO
          ENDDO

!         Set 3
          DO I=1,NCHN3
             ICHAN=CLIST3(I)
             DO L=1,MAXLAY
                DO J=25,35
                   COEF3(J,L,I)=XMULTS(2,ICHAN)*COEF3(J,L,I)
                ENDDO
             ENDDO
          ENDDO

!         Set 4
          DO I=1,NCHN4
             ICHAN=CLIST4(I)
             DO L=1,MAXLAY
                DO J=33,45
                   COEF4(J,L,I)=XMULTS(2,ICHAN)*COEF4(J,L,I)
                ENDDO
             ENDDO
          ENDDO

!         Set 5
          DO I=1,NCHN5
             ICHAN=CLIST5(I)
             DO L=1,MAXLAY
                DO J=19,21
                   COEF5(J,L,I)=XMULTS(2,ICHAN)*COEF5(J,L,I)
                ENDDO
             ENDDO
          ENDDO

!         Set 6
          DO I=1,NCHN6
             ICHAN=CLIST6(I)
             DO L=1,MAXLAY
                DO J=16,22
                   COEF6(J,L,I)=XMULTS(2,ICHAN)*COEF6(J,L,I)
                ENDDO
             ENDDO
          ENDDO

!         Set 7
          DO I=1,NCHN7
             ICHAN=CLIST7(I)
             DO L=1,MAXLAY
                DO J=16,28
                   COEF7(J,L,I)=XMULTS(2,ICHAN)*COEF7(J,L,I)
                ENDDO
             ENDDO
          ENDDO

!         OPTRAN
          DO ICHAN=1,MXCHAN
             IF (INDH2O(ICHAN) .GT. 0) THEN
                I=INDH2O(ICHAN)
                DO L=1,MXOWLY
                   DO J=1,NH2O
                      COFH2O(J,L,I)=XMULTS(2,ICHAN)*COFH2O(J,L,I)
                   ENDDO
                ENDDO
             ENDIF
          ENDDO

       ENDIF ! end of H2O line

!      --------------
!      Adjust H2O con
!      --------------
       IF (USEMLT(3)) THEN

!         Set 1
          DO I=1,NCHN1
             ICHAN=CLIST1(I)
             DO L=1,MAXLAY
                DO J=1,7
                   COEF1(J,L,I)=XMULTS(3,ICHAN)*COEF1(J,L,I)
                ENDDO
             ENDDO
          ENDDO

!         Set 2
          DO I=1,NCHN2
             ICHAN=CLIST2(I)
             DO L=1,MAXLAY
                DO J=1,7
                   COEF2(J,L,I)=XMULTS(3,ICHAN)*COEF2(J,L,I)
                ENDDO
             ENDDO
          ENDDO

!         Set 3
          DO I=1,NCHN3
             ICHAN=CLIST3(I)
             DO L=1,MAXLAY
                DO J=1,7
                   COEF3(J,L,I)=XMULTS(3,ICHAN)*COEF3(J,L,I)
                ENDDO
             ENDDO
          ENDDO

!         Set 4
          DO I=1,NCHN4
             ICHAN=CLIST4(I)
             DO L=1,MAXLAY
                DO J=1,7
                   COEF4(J,L,I)=XMULTS(3,ICHAN)*COEF4(J,L,I)
                ENDDO
             ENDDO
          ENDDO

!         Set 5
          DO I=1,NCHN5
             ICHAN=CLIST5(I)
             DO L=1,MAXLAY
                DO J=1,7
                   COEF5(J,L,I)=XMULTS(3,ICHAN)*COEF5(J,L,I)
                ENDDO
             ENDDO
          ENDDO

!         Set 6
          DO I=1,NCHN6
             ICHAN=CLIST6(I)
             DO L=1,MAXLAY
                DO J=1,7
                   COEF6(J,L,I)=XMULTS(3,ICHAN)*COEF6(J,L,I)
                ENDDO
             ENDDO
          ENDDO

!         Set 7
          DO I=1,NCHN7
             ICHAN=CLIST7(I)
             DO L=1,MAXLAY
                DO J=1,7
                   COEF7(J,L,I)=XMULTS(3,ICHAN)*COEF7(J,L,I)
                ENDDO
             ENDDO
          ENDDO

       ENDIF ! end of H2O con

!      ------------
!      Adjust ozone
!      ------------
       IF (USEMLT(4)) THEN

!         Set 1
          DO I=1,NCHN1
             ICHAN=CLIST1(I)
             DO L=1,MAXLAY
                DO J=27,31
                   COEF1(J,L,I)=XMULTS(4,ICHAN)*COEF1(J,L,I)
                ENDDO
             ENDDO
          ENDDO

!         Set 2
          DO I=1,NCHN2
             ICHAN=CLIST2(I)
             DO L=1,MAXLAY
                DO J=16,25
                   COEF2(J,L,I)=XMULTS(4,ICHAN)*COEF2(J,L,I)
                ENDDO
             ENDDO
          ENDDO

!         Set 3: none

!         Set 4
          DO I=1,NCHN4
             ICHAN=CLIST4(I)
             DO L=1,MAXLAY
                DO J=30,32
                   COEF4(J,L,I)=XMULTS(4,ICHAN)*COEF4(J,L,I)
                ENDDO
             ENDDO
          ENDDO

!         Set 5
          J=22
          DO I=1,NCHN5
             ICHAN=CLIST5(I)
             DO L=1,MAXLAY
                COEF5(J,L,I)=XMULTS(4,ICHAN)*COEF5(J,L,I)
             ENDDO
          ENDDO

!         Set 6
          J=23
          DO I=1,NCHN6
             ICHAN=CLIST6(I)
             DO L=1,MAXLAY
                COEF6(J,L,I)=XMULTS(4,ICHAN)*COEF6(J,L,I)
             ENDDO
          ENDDO

!         Set 7
          J=29
          DO I=1,NCHN7
             ICHAN=CLIST7(I)
             DO L=1,MAXLAY
                COEF7(J,L,I)=XMULTS(4,ICHAN)*COEF7(J,L,I)
             ENDDO
          ENDDO

       ENDIF ! end of O3


!      ---------
!      Adjust CO
!      ---------
       IF (USEMLT(5)) THEN
!         Set 4
          DO I=1,NCHN4
             ICHAN=CLIST4(I)
             DO L=1,MAXLAY
                DO J=19,29
                   COEF4(J,L,I)=XMULTS(5,ICHAN)*COEF4(J,L,I)
                ENDDO
             ENDDO
          ENDDO
       ENDIF


!      --------------
!      Adjust methane
!      --------------
       IF (USEMLT(6)) THEN
!         Set 3
          DO I=1,NCHN3
             ICHAN=CLIST3(I)
             DO L=1,MAXLAY
                DO J=16,24
                   COEF3(J,L,I)=XMULTS(6,ICHAN)*COEF3(J,L,I)
                ENDDO
             ENDDO
          ENDDO
       ENDIF


!      --------------
!      Adjust non-LTE
!      --------------
!!       IF (USEMLT(7)) THEN
! wrong          NJUNK=MIN0(NCHNTE,6)
! wrong          DO I=1,NJUNK ! do not adjust 7th coef
!!          NJUNK=MIN0(NNCOEF,6)
!!          DO I=1,NCHNTE
!!             ICHAN=CLISTN(I)
! wrong            DO J=1,NNCOEF
!!             DO J=1,NJUNK
!!                COEFN(J,I)=XMULTS(7,ICHAN)*COEFN(J,I)
!!             ENDDO
!!          ENDDO
!!       ENDIF

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
!
       RETURN
       END
