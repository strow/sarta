!=======================================================================
!
!    University of Maryland Baltimore Country (UMBC)
!
!    AIRS
!
!    RDCOEF version for CrIS HR G2 with CO2, SO2, HNO3, NH3, HDO.
!
!F90====================================================================


!ROUTINE NAME:
!    RDCOEF_CRIS_HRG4


!ABSTRACT:
!    Read in the AIRS fast transmittance coefficients.


!CALL PROTOCOL
!    RDCOEF ( IPOPN, NCHAN, INDCHN, SETCHN,
!       NCHN1,  NCHN2,  NCHN3,  NCHN4,  NCHN5,  NCHN6,  NCHN7,
!       CLIST1, CLIST2, CLIST3, CLIST4, CLIST5, CLIST6, CLIST7,
!       COEF1,  COEF2,  COEF3,  COEF4,  COEF5,  COEF6,  COEF7,
!       FREQ,  LABOVE,  COEFF,  INDCO2, COFCO2, INDSO2, COFSO2,
!       INDHNO, COFHNO, INDN2O, COFN2O, INDNH3, COFNH3,
!       INDHDO, COFHDO, INDH2O, WAZOP,  WAVGOP, COFH2O, 
!       FX, NCHNTE, CLISTN, COEFN )


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    INT arr   INDCHN  indices of channels         none
!    INTEGER   IPOPN    I/O unit number             none
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
!    REAL arr  FREQ    channel freqs               cm-1
!    REAL arr  FX      fixed gases adjustment      none
!    INT arr   INDCO2  CO2 pert channel indices    none
!    INT arr   INDSO2  SO2 pert channel indices    none
!    INT arr   INDHNO  HNO3 pert channel indices   none
!    INT arr   INDN2O  N2O pert channel indices    none
!    INT arr   INDNH3  NH3 pert channel indices    none
!    INT arr   INDHDO  HDO pert channel indices    none
!    INT arr   INDH2O  OPTRAN H2O channel indices  none
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
!    unit IPOPN : input file, binary FORTRAN data file. The file is
!       opened, read, and closed. This is done 10 times, once per
!       each of the 7 coef sets, and once each for the variable CO2,
!       OPTRAN water, and thermal F factor coefs.


!COMMON BLOCKS
!    none


!DESCRIPTION:
!    June 2005 version of the 100 layer AIRS Fast Transmittance
!    Code by L.Strow/S.Hannon.
!
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
!
!END====================================================================

!      =================================================================
       SUBROUTINE RDCOEF ( IPOPN, NCHAN, INDCHN, SETCHN, &
          NCHN1,  NCHN2,  NCHN3,  NCHN4,  NCHN5,  NCHN6,  NCHN7, &
         CLIST1, CLIST2, CLIST3, CLIST4, CLIST5, CLIST6, CLIST7, &
          COEF1,  COEF2,  COEF3,  COEF4,  COEF5,  COEF6,  COEF7, &
           FREQ, LABOVE,  COEFF, INDCO2, COFCO2, INDSO2, COFSO2, &
         INDHNO, COFHNO, INDN2O, COFN2O, INDNH3, COFNH3, &
         INDHDO, COFHDO, INDH2O,  WAZOP, WAVGOP, COFH2O,  &
         FX, NCHNTE, CLISTN, COEFN )
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
!      Input
integer :: IPOPN, NCHAN
integer, dimension(MXCHAN) :: INDCHN
!      Output
integer :: NCHN1, NCHN2, NCHN3, NCHN4, NCHN5, NCHN6, NCHN7
integer, dimension(MXCHAN) :: SETCHN
integer, dimension(MXCHN1) :: CLIST1
integer, dimension(MXCHN2) :: CLIST2
integer, dimension(MXCHN3) :: CLIST3
integer, dimension(MXCHN4) :: CLIST4
integer, dimension(MXCHN5) :: CLIST5
integer, dimension(MXCHN6) :: CLIST6
integer, dimension(MXCHN7) :: CLIST7
real(4), dimension(N1COEF,MAXLAY,MXCHN1) :: COEF1
real(4), dimension(N2COEF,MAXLAY,MXCHN2) :: COEF2
real(4), dimension(N3COEF,MAXLAY,MXCHN3) :: COEF3
real(4), dimension(N4COEF,MAXLAY,MXCHN4) :: COEF4
real(4), dimension(N5COEF,MAXLAY,MXCHN5) :: COEF5
real(4), dimension(N6COEF,MAXLAY,MXCHN6) :: COEF6
real(4), dimension(N7COEF,MAXLAY,MXCHN7) :: COEF7
       REAL   FREQ(MXCHAN)
       INTEGER LABOVE(MXCHAN)
       REAL  COEFF(NFCOEF,MXCHAN)
       INTEGER INDCO2(MXCHAN)
       REAL COFCO2(  NCO2,MAXLAY,MXCHNC)
       INTEGER INDSO2(MXCHAN)
       REAL COFSO2(  NSO2,MAXLAY,MXCHNS)
       INTEGER INDHNO(MXCHAN)
       REAL COFHNO( NHNO3,MAXLAY,MXCHNH)
       INTEGER INDN2O(MXCHAN)
       REAL COFN2O(  NN2O,MAXLAY,MXCHNN)
       INTEGER INDNH3(MXCHAN)
       REAL COFNH3(  NNH3,MAXLAY,MXCHNA)
       INTEGER NCHNNH3
       INTEGER INDHDO(MXCHAN)
       REAL COFHDO(  NHDO,MAXLAY,MXCHND)
       INTEGER NCHNHDO
       INTEGER INDH2O(MXCHAN)
       REAL   WAZOP(MXOWLY)
       REAL  WAVGOP(NOWAVG,MXOWLY)
       REAL COFH2O(  NH2O,MXOWLY,MXCHNW)
       REAL     FX(MAXLAY)
       INTEGER NCHNTE
       INTEGER  CLISTN(MXCNTE)
       REAL  COEFN(XNCOEF,MXCNTE)


!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------

character(len=80) ::  CLINE
real(4) :: FRQCHN, RJUNK
real(4), dimension(NFCOEF) :: FCHAN
integer :: I, J, IC, IL, ICHAN, IERR, ICOUNT, LACHAN

!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none

!***********************************************************************
!                    EXECUTABLE CODE
!***********************************************************************
!
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
       ENDDO
!
!      write(6,*) 'rdcoef: started OK'
!      ----------
!      Read set 1
!      ----------
       OPEN(UNIT=IPOPN,FILE=FNCOF1,FORM='UNFORMATTED',STATUS='OLD', &
         IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNCOF1
 1020     FORMAT('Error ',I5,' opening file:',/,A80)
          STOP
       ENDIF
!
       J=1
       DO I=1,MXCHN1
!         Read data for this frequency/channel
          READ(IPOPN) ICHAN, FRQCHN, ((COEF1(IC,IL,J),IC=1,N1COEF), &
            IL=1,MAXLAY)
!
          SETCHN(ICHAN)=1
!
!         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) .NE. 0) THEN
             CLIST1(J)=ICHAN
             FREQ( INDCHN(ICHAN) )=FRQCHN
             J=J + 1
          ENDIF
       ENDDO
       NCHN1=J - 1
!!       write(6,*) 'rdcoef:MXCHN1,NCHN1: ', MXCHN1,NCHN1
!
       CLOSE(IPOPN)
!
!      ----------
!      Read set 2
!      ----------
       OPEN(UNIT=IPOPN,FILE=FNCOF2,FORM='UNFORMATTED',STATUS='OLD', &
         IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNCOF2
          STOP
       ENDIF
!
       J=1
       DO I=1,MXCHN2
!         Read data for this frequency/channel
          READ(IPOPN) ICHAN, FRQCHN, ((COEF2(IC,IL,J),IC=1,N2COEF), &
            IL=1,MAXLAY)
!
          SETCHN(ICHAN)=2
!
!         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) .NE. 0) THEN
             CLIST2(J)=ICHAN
             FREQ( INDCHN(ICHAN) )=FRQCHN
             J=J + 1
          ENDIF
       ENDDO
       NCHN2=J - 1
!!       write(6,*) 'rdcoef:MXCHN2,NCHN2: ', MXCHN2,NCHN2
!
       CLOSE(IPOPN)
!
!      ----------
!      Read set 3
!      ----------
       OPEN(UNIT=IPOPN,FILE=FNCOF3,FORM='UNFORMATTED',STATUS='OLD', &
         IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNCOF3
          STOP
       ENDIF
!
       J=1
       DO I=1,MXCHN3
!         Read data for this frequency/channel
          READ(IPOPN) ICHAN, FRQCHN, ((COEF3(IC,IL,J),IC=1,N3COEF), &
            IL=1,MAXLAY)
!
          SETCHN(ICHAN)=3
!
!         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) .NE. 0) THEN
             CLIST3(J)=ICHAN
             FREQ( INDCHN(ICHAN) )=FRQCHN
             J=J + 1
          ENDIF
       ENDDO
       NCHN3=J - 1
!!       write(6,*) 'rdcoef:MXCHN3,NCHN3: ', MXCHN3,NCHN3
!
       CLOSE(IPOPN)
!
!      ----------
!      Read set 4
!      ----------
       OPEN(UNIT=IPOPN,FILE=FNCOF4,FORM='UNFORMATTED',STATUS='OLD', &
         IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNCOF4
          STOP
       ENDIF
!
       J=1
       DO I=1,MXCHN4
!         Read data for this frequency/channel
          READ(IPOPN) ICHAN, FRQCHN, ((COEF4(IC,IL,J),IC=1,N4COEF), &
            IL=1,MAXLAY)
!
          SETCHN(ICHAN)=4
!
!         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) .NE. 0) THEN
             CLIST4(J)=ICHAN
             FREQ( INDCHN(ICHAN) )=FRQCHN
             J=J + 1
          ENDIF
       ENDDO
       NCHN4=J - 1
!
       CLOSE(IPOPN)
!
!      ----------
!      Read set 5
!      ----------
       OPEN(UNIT=IPOPN,FILE=FNCOF5,FORM='UNFORMATTED',STATUS='OLD', &
         IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNCOF5
          STOP
       ENDIF
!
       J=1
       DO I=1,MXCHN5
!         Read data for this frequency/channel
          READ(IPOPN) ICHAN, FRQCHN, ((COEF5(IC,IL,J),IC=1,N5COEF), &
            IL=1,MAXLAY)
!
          SETCHN(ICHAN)=5
!
!         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) .NE. 0) THEN
             CLIST5(J)=ICHAN
             FREQ( INDCHN(ICHAN) )=FRQCHN
             J=J + 1
          ENDIF
       ENDDO
       NCHN5=J - 1
!
       CLOSE(IPOPN)
!
!      ----------
!      Read set 6
!      ----------
       OPEN(UNIT=IPOPN,FILE=FNCOF6,FORM='UNFORMATTED',STATUS='OLD', &
         IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNCOF6
          STOP
       ENDIF
!
       J=1
       DO I=1,MXCHN6
!         Read data for this frequency/channel
          READ(IPOPN) ICHAN, FRQCHN, ((COEF6(IC,IL,J),IC=1,N6COEF), &
            IL=1,MAXLAY)
!
          SETCHN(ICHAN)=6
!
!         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) .NE. 0) THEN
             CLIST6(J)=ICHAN
             FREQ( INDCHN(ICHAN) )=FRQCHN
             J=J + 1
          ENDIF
       ENDDO
       NCHN6=J - 1
!
       CLOSE(IPOPN)
!
!      ----------
!      Read set 7
!      ----------
       OPEN(UNIT=IPOPN,FILE=FNCOF7,FORM='UNFORMATTED',STATUS='OLD', &
         IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNCOF7
          STOP
       ENDIF
!
       J=1
       DO I=1,MXCHN7
!         Read data for this frequency/channel
          READ(IPOPN) ICHAN, FRQCHN, ((COEF7(IC,IL,J),IC=1,N7COEF), &
            IL=1,MAXLAY)
!
          SETCHN(ICHAN)=7
!
!         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) .NE. 0) THEN
             CLIST7(J)=ICHAN
             FREQ( INDCHN(ICHAN) )=FRQCHN
             J=J + 1
          ENDIF
       ENDDO
       NCHN7=J - 1
!
       CLOSE(IPOPN)
!
!       NCHN7=0
!
!      WRITE(6,'(A)') 'Completed rdcoef to set 7'
!      ---------------------------
!      Read CO2 perturbation coefs - placeholder set to zero
!      ---------------------------
       IF (CFCO2) THEN
       IF (DEBUG) write(6,"('rdcoef:CDCO2=TRUE, read coeff file')")
       OPEN(UNIT=IPOPN,FILE=FNCO2,FORM='UNFORMATTED',STATUS='OLD', &
         IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNCO2
          STOP
       ENDIF
!
       J=1
       DO I=1,MXCHNC
!         Read data for this frequency/channel
          READ(IPOPN) ICHAN, FRQCHN, ((COFCO2(IC,IL,J),IC=1,NCO2), &
            IL=1,MAXLAY)
!
!         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) .NE. 0) THEN
             INDCO2(ICHAN)=J
             J=J + 1
          ENDIF
       ENDDO
!
       CLOSE(IPOPN)
!
       ELSE
       IF (DEBUG) write(6,"('rdcoef:CFCO2=FALSE, null coeffs')")
       J=1
       DO I=1,MXCHNC
          DO IC=1,NCO2
             DO IL=1,MAXLAY
                COFCO2(IC,IL,J) = 0.0
             ENDDO
          ENDDO
          IF (INDCHN(ICHAN) .NE. 0) THEN
             INDCO2(ICHAN)=J
             J=J + 1
          ENDIF
       ENDDO
!
       ENDIF  

!       IF(.NOT. CFCO2) THEN
!         write(6,*) 'rdcoef:not.CFCO2 since CFCO2 is FALSE'
!       ENDIF

!      ---------------------
!      Read SO2 pertub coefs - placeholder while no coef file
!      ---------------------
       IF (CFSO2) THEN
       IF (DEBUG) write(6,"('rdcoef:CFSO2=TRUE, read coeff file')")
       OPEN(UNIT=IPOPN,FILE=FNSO2,FORM='UNFORMATTED',STATUS='OLD', &
         IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNSO2
          STOP
       ENDIF
!
       J=1
       DO I=1,MXCHNS
!         Read data for this frequency/channel
          READ(IPOPN) ICHAN, FRQCHN, ((COFSO2(IC,IL,J),IC=1,NSO2), &
            IL=1,MAXLAY)
!C
!         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) .NE. 0) THEN
             INDSO2(ICHAN)=J
             J=J + 1
          ENDIF
       ENDDO
!
       CLOSE(IPOPN)

       ELSE
       IF (DEBUG) write(6,"('rdcoef:CFSO2=FALSE, null coeffs')")
       J=1
       DO I=1,MXCHNS
          DO IC=1,NSO2
             DO IL=1,MAXLAY
                COFSO2(IC,IL,J) = 0.0
             ENDDO
          ENDDO
          IF (INDCHN(ICHAN) .NE. 0) THEN
             INDSO2(ICHAN)=J
             J=J + 1
          ENDIF
       ENDDO
!
       ENDIF

!      ---------------------
!      Read HNO3 perturb coefs - placeholder while no coef file
!      ---------------------
       IF (CFHNO3) THEN
       IF (DEBUG) write(6,"('rdcoef:CFHNO3=TRUE, read coeff file')")
       OPEN(UNIT=IPOPN,FILE=FNHNO3,FORM='UNFORMATTED',STATUS='OLD', &
         IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNHNO3
          STOP
       ENDIF
!
       J=1
       DO I=1,MXCHNH
!         Read data for this frequency/channel
          READ(IPOPN) ICHAN, FRQCHN, ((COFHNO(IC,IL,J),IC=1,NHNO3), &
            IL=1,MAXLAY)
!
!         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) .NE. 0) THEN
             INDHNO(ICHAN)=J
             J=J + 1
          ENDIF
       ENDDO
!
       CLOSE(IPOPN)
!
!      ***** no coeff file:
       ELSE
       IF (DEBUG) write(6,"('rdcoef:CFHNO3=FALSE, null coeffs')")
       J=1
       DO I=1,MXCHNH
          DO IC=1,NHNO3
             DO IL=1,MAXLAY
                COFHNO(IC,IL,J) = 0.0
             ENDDO
          ENDDO
!         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) .NE. 0) THEN
             INDHNO(ICHAN)=J
             J=J + 1
          ENDIF
       ENDDO


       ENDIF
!      ---------------------
!      Read N2O perturb coefs - placeholder while no coef file.
!      ---------------------
       IF (CFN2O) THEN
       IF (DEBUG) write(6,*) 'rdcoef:CFN2O=TRUE, read file'
       OPEN(UNIT=IPOPN,FILE=FNN2O,FORM='UNFORMATTED',STATUS='OLD', &
         IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNN2O
          STOP
       ENDIF
!
       J=1
       DO I=1,MXCHNN
!         Read data for this frequency/channel
          READ(IPOPN) ICHAN, FRQCHN, ((COFN2O(IC,IL,J),IC=1,NN2O), &
            IL=1,MAXLAY)
!         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) .NE. 0) THEN
             INDN2O(ICHAN)=J
             J=J + 1
          ENDIF
       ENDDO
       CLOSE(IPOPN)

! - these lines used as placeholder when no ceofficients are available.
       ELSE
       IF (DEBUG) write(6,*) 'rdcoef:CFN2O=FALSE null coeffs'
       J=1
       DO I=1,MXCHNN
          DO IC=1,NN2O
             DO IL=1,MAXLAY
               COFN2O(IC,IL,J) = 0.0
             ENDDO
          ENDDO
!         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) .NE. 0) THEN
             INDN2O(ICHAN)=J
             J=J + 1
          ENDIF
       ENDDO
!       write(6,*) 'rdcoef_nh3: read N2O coeffs'
       ENDIF

!      ---------------------------
!      Read NH3 perturbation coefs
!      ---------------------------
       IF (CFNH3) THEN
       IF (DEBUG) write(6,*) 'rdcoef:CFNH3=TRUE, read coeff file'
       OPEN(UNIT=IPOPN,FILE=FNNH3,FORM='UNFORMATTED',STATUS='OLD', &
         IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNNH3
          STOP
       ENDIF
!
       J=1
       DO I=1,MXCHNA
!         Read data for this frequency/channel
          READ(IPOPN) ICHAN, FRQCHN, ((COFNH3(IC,IL,J),IC=1,NNH3), &
            IL=1,MAXLAY)
!
!         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) .NE. 0) THEN
             INDNH3(ICHAN)=J
             J=J + 1
          ENDIF
       ENDDO
       NCHNNH3=J-1
!      write(6,'(A,X,I4)') 'rdcoef.NH3: INDCHN(7235)= ',INDCHN(7235)
!
       CLOSE(IPOPN)
!
       ELSE
       IF (DEBUG) write(6,*) 'rdcoef:CFNH3=FALSE, null coeffs'
! - these lines used as placeholder when no ceofficients are available.
       J=1
       DO I=1,MXCHNA
          DO IC=1,NNH3
             DO IL=1,MAXLAY
               COFNH3(IC,IL,J) = 0.0
             ENDDO
          ENDDO
!         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) .NE. 0) THEN
             INDNH3(ICHAN)=J
             J=J + 1
          ENDIF
       ENDDO
!       write(6,*) 'rdcoef: completed to NH3'
       ENDIF

!      ---------------------------
!      Read HDO perturbation coefs
!      ---------------------------
       IF (CFHDO) THEN
       IF (DEBUG) write(6,*) 'rdcoef:CFHDO=TRUE, read coef file'
       OPEN(UNIT=IPOPN,FILE=FNHDO,FORM='UNFORMATTED',STATUS='OLD', &
         IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNHDO
          STOP
       ENDIF
!
!       write(6,'(a,i6,X,i6)') 'rdcoef: MXCHND,NHDO',MXCHND,NHDO
        J=1
        DO I=1,MXCHND
!         Read data for this frequency/channel
          READ(IPOPN) ICHAN, FRQCHN, ((COFHDO(IC,IL,J),IC=1,NHDO), &
            IL=1,MAXLAY)
!
!         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) .NE. 0) THEN
!             write(6,'(A,X,I4,X,I4,X,I5)') 'rdcoef: I,J,INDCHN(ICHAN)= ', I,J,INDCHN(ICHAN)
             INDHDO(ICHAN)=J
             J=J + 1
          ENDIF
        ENDDO
        NCHNHDO=J-1
!       write(6,'(A,X,I4)') 'rdcoef: INDCHN(125)= ',INDCHN(125)
!
       CLOSE(IPOPN)
       IF (DEBUG)  WRITE(6,"('rdcoef:completed read HDO coeffs')")
!

       ELSE
       IF (DEBUG) write(6,*) 'rdcoef:CFHDO=FALSE null coeffs'
! - these lines used as placeholder when no ceofficients are available.
       J=1
       DO I=1,MXCHND
          DO IC=1,NHDO
             DO IL=1,MAXLAY
               COFHDO(IC,IL,J) = 0.0
             ENDDO
          ENDDO
!         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) .NE. 0) THEN
             INDHDO(ICHAN)=J
             J=J + 1
          ENDIF
       ENDDO
!
       ENDIF
!
!      ---------------------
!      Read OPTRAN H2O coefs - placeholder to disable coefficients
!      ---------------------
       IF (CFOPTR) THEN
       OPEN(UNIT=IPOPN,FILE=FNOPTR,FORM='UNFORMATTED',STATUS='OLD', &
         IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNOPTR
          STOP
       ENDIF
!       write(6,*) 'rdcoef: opened optran file successfully'
!
       READ(IPOPN) (WAZOP(IL),IL=1,MXOWLY)
       DO IC=1,NOWAVG
!         Read the header section
          READ(IPOPN) (WAVGOP(IC,IL),IL=1,MXOWLY)
       ENDDO
!
!       write(6,'(a,X,I6)') 'rdcoef: completed read optran header: MXCHNW',MXCHNW
       J=1
       DO I=1,MXCHNW
!         Read data for this frequency/channel
          READ(IPOPN) ICHAN, FRQCHN, ((COFH2O(IC,IL,J),IC=1,NH2O), &
            IL=1,MXOWLY)
!
!         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) .NE. 0) THEN
             INDH2O(ICHAN)=J
             J=J + 1
          ENDIF
!         write(6,'(a,X,I4,X,I4,X,F8.3,X,I6,X,I6)') 'rdcoef:I,J,FRQCHN,ICHAN,INDH2O(ICHAN)', &
!                   I,J,FRQCHN,ICHAN,INDH2O(ICHAN)
       ENDDO
!
       CLOSE(IPOPN)
!
       ELSE
       IF (DEBUG) write(6,*) 'rdcoef:CFOPTR=FALSE null coeffs'
!
!      these loops for zeroing out optran coefficients
       J=1
       DO I=1,MXCHNW
         DO IC=1,NH2O
           DO IL=1,MXOWLY
             COFH2O(IC,IL,J) = 0.0
           ENDDO
         ENDDO
       ENDDO
!
       IF (DEBUG) write(6,*) 'rdcoef: completed optran'
       ENDIF
!      -----------------------------------------------
!      Read the downward thermal F factor coefficients
!      -----------------------------------------------
       IF (CFTHER) THEN
       IF (DEBUG) write(6,*) 'rdcoef:CFTHER=TRUE read coef file'
       OPEN(UNIT=IPOPN,FILE=FNTHER,FORM='UNFORMATTED',STATUS='OLD', &
         IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNTHER
          STOP
       ENDIF
!
       DO I=1,MXCHAN
!         Read data for this frequency/channel
!cc changed 18 May 2005
!cc          READ(IPOPN) ICHAN, FRQCHN, LACHAN, (FCHAN(IC),IC=1,NFCOEF)
          READ(IPOPN) ICHAN, FRQCHN, (FCHAN(IC),IC=1,NFCOEF)
          LACHAN=-1   ! assign dummy value
!         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) .NE. 0) THEN
             LABOVE( INDCHN(ICHAN) )=LACHAN
             DO IC=1,NFCOEF
                COEFF(IC,INDCHN(ICHAN))=FCHAN(IC)
             ENDDO
          ENDIF
       ENDDO
!
       CLOSE(IPOPN)
!
       ELSE
       IF (DEBUG) write(6,*) 'rdcoef:CFTHER=FALSE, null coeffs'
! set to zero - to be used when no coeff file available 
       DO I=1,MXCHAN
          DO IC=1,NFCOEF
             COEFF(IC,I)=0.0
          ENDDO
       ENDDO
!
       ENDIF

!      -------
!      Read FX
!      -------
       IF (DEBUG) write(6,*) 'rdcoef:FX, reading FX'
       OPEN(UNIT=IPOPN,FILE=FNFX,FORM='FORMATTED',STATUS='OLD', &
         IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNFX
          STOP
       ENDIF
!
!      Read the file
       ICOUNT=0
 10    READ(IPOPN,9000,END=99) CLINE
 9000  FORMAT(A80)
       IF (CLINE(1:1) .NE. '!') THEN
!         Note: fx file format is:  layer_number  fx_value 
          READ(CLINE,*) IC, RJUNK
          ICOUNT=ICOUNT + 1
          FX(IC)=RJUNK
       ENDIF
       GOTO 10
!
 99    CLOSE(IPOPN)
!
       IF (ICOUNT .NE. MAXLAY) THEN
          WRITE(6,1047) MAXLAY, ICOUNT
 1047     FORMAT('Error! Unexpected number of layers in fx file.',/, &
         'Expected fx to have ',I4,' layers, but found ',I4)
       ENDIF
!

!      ------------
!      Read non-LTE
!      ------------
       IF (COFNTE) THEN
       IF (DEBUG) write(6,*) 'rdcoef:COFNTE=TRUE read coef file'
! initialize coefficient set to zero (always 2*NNCOEF)
       DO I=1,MXCNTE
          DO IC=1,XNCOEF
             COEFN(IC,I)=0.0
          ENDDO
       ENDDO

       OPEN(UNIT=IPOPN,FILE=FNCOFN,FORM='UNFORMATTED',STATUS='OLD', &
         IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNCOFN
          STOP
       ENDIF
!
       J=1
       DO I=1,MXCNTE
!         Read data for this frequency/channel
          IF( .NOT. LXNTE) THEN       ! FALSE => 0-90 range
             READ(IPOPN) ICHAN, FRQCHN, (COEFN(IC,J),IC=1,NNCOEF)
          ELSEIF( LXNTE)   THEN      ! TRUE => 0-120 extended range
             READ(IPOPN) ICHAN, FRQCHN, (COEFN(IC,J),IC=1,XNCOEF)
          ENDIF
!
!         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) .NE. 0) THEN
             CLISTN(J)=ICHAN
             J=J + 1
          ENDIF
       ENDDO
       NCHNTE=J - 1
!
       CLOSE(IPOPN)
!
       ELSE
       IF (DEBUG) write(6,*) 'rdcoef:COFNTE=FALSE, null coeffs'
! placeholder set to zero
       DO I=1,MXCNTE
          DO IC=1,XNCOEF
             COEFN(IC,I)=0.0
          ENDDO
       ENDDO
       ENDIF
!      ---------------------------------------------
       IF (DEBUG) write(6,*) 'rdcoef: read all coefficients'
!      ---------------------------------------------
!      Make sure all channels on the list were found
!      ---------------------------------------------
       ICOUNT=NCHN1 + NCHN2 + NCHN3 + NCHN4 + NCHN5 + NCHN6 + NCHN7
       IF (ICOUNT .NE. NCHAN) THEN
          WRITE(6,1050) NCHAN, ICOUNT
 1050     FORMAT('Error! Unexpected number of channels found.',/, &
         'The channel list had ',I4,' channels, but found ',I4)
       ENDIF
!
!      ----------------------------
!      Show summary of channel sets
!      ----------------------------
!cc
       IF (DEBUG) THEN
         WRITE(6,1060) 1, NCHN1
 1060    FORMAT('Number of channels for set',I3,' = ',I5)
         WRITE(6,1060) 2, NCHN2
         WRITE(6,1060) 3, NCHN3
         WRITE(6,1060) 4, NCHN4
         WRITE(6,1060) 5, NCHN5
         WRITE(6,1060) 6, NCHN6
         WRITE(6,1060) 7, NCHN7
!         WRITE(6,1060) 11,NCHNNH3
!         WRITE(6,1060) 162,NCHNHDO
       ENDIF
!
       RETURN
       END
