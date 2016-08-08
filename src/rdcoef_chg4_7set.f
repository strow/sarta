C=======================================================================
C=======================================================================
C
C    University of Maryland Baltimore Country (UMBC)
C
C    AIRS
C
C    RDCOEF version for CrIS HR G2 with trace gases CO2.
C
!F77====================================================================


!ROUTINE NAME:
C    RDCOEF_CRIS_HRG4


!ABSTRACT:
C    Read in the AIRS fast transmittance coefficients.


!CALL PROTOCOL
C    RDCOEF ( IOUN, NCHAN, INDCHN, SETCHN,
C       NCHN1, NCHN2, NCHN3, NCHN4, NCHN5, NCHN6, NCHN7,
C       CLIST1, CLIST2, CLIST3, CLIST4, CLIST5, CLIST6, CLIST7,
C       COEF1, COEF2, COEF3, COEF4, COEF5, COEF6, COEF7,
C       FREQ, LABOVE, COEFF, INDCO2, COFCO2, INDSO2, COFSO2,
C       INDHNO, COFHNO, INDN2O, COFN2O,
C       INDH2O, WAZOP, WAVGOP, COFH2O, FX, NCHNTE, CLISTN, COEFN )


!INPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    INT arr   INDCHN  indices of channels         none
C    INTEGER   IOUN    I/O unit number             none
C    INTEGER   NCHAN   number of channels          none


!OUTPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    INT arr   CLIST1  set1 channel list           none
C    INT arr   CLIST2  set2 channel list           none
C    INT arr   CLIST3  set3 channel list           none
C    INT arr   CLIST4  set4 channel list           none
C    INT arr   CLIST5  set5 channel list           none
C    INT arr   CLIST6  set6 channel list           none
C    INT arr   CLIST7  set7 channel list           none
C    INT arr   CLISTN  non-LTE channel list        none
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
C    REAL arr  COFSO2  SO2 perturbation coefs      various
C    REAL arr  COFHNO  HNO3 perturbation coefs     various
C    REAL arr  COFN2O  N2O perturbation coefs      various
C    REAL arr  COFH2O  OPTRAN H2O coefs            various
C    REAL arr  FREQ    channel freqs               cm-1
C    REAL arr  FX      fixed gases adjustment      none
C    INT arr   INDCO2  CO2 pert channel indices    none
C    INT arr   INDSO2  SO2 pert channel indices    none
C    INT arr   INDHNO  HNO3 pert channel indices   none
C    INT arr   INDN2O  N2O pert channel indices    none
C    INT arr   INDH2O  OPTRAN H2O channel indices  none
C    INT arr   LABOVE  layer above for thermal     none
C    INTEGER   NCHN1   set1 number of channels     none
C    INTEGER   NCHN2   set2 number of channels     none
C    INTEGER   NCHN3   set3 number of channels     none
C    INTEGER   NCHN4   set4 number of channels     none
C    INTEGER   NCHN5   set5 number of channels     none
C    INTEGER   NCHN6   set6 number of channels     none
C    INTEGER   NCHN7   set7 number of channels     none
C    INTEGER   NCHNTE  non-LTE number of channels  none
C    REAL arr  WAZOP   OPTRAN water grid           kiloMoles/cm^2
C    REAL arr  WAVGOP  OPTRAN water pred averges   various
C    INT arr   SETCHN  set# (1-7) chan belongs to  none (integer, 1 - 7)


!INPUT/OUTPUT PARAMETERS:
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
C    unit IOUN : input file, binary FORTRAN data file. The file is
C       opened, read, and closed. This is done 10 times, once per
C       each of the 7 coef sets, and once each for the variable CO2,
C       OPTRAN water, and thermal F factor coefs.


!COMMON BLOCKS
C    none


!DESCRIPTION:
C    June 2005 version of the 100 layer AIRS Fast Transmittance
C    Code by L.Strow/S.Hannon.
C
C    Seven sets of binary data files containing the main fast
C    transmittance coefficients are opened and read one channel at
C    a time.  The seven sets of coefs are each stored in their own
C    arrays.  Next, preturbation coefficients for four trace gases,
C    (CO2, SO2, HNO3, & N2O) are read in from four binary files.
C    Next, OPTRAN water fast trans coefs for some channels are read
C    in from a binary file file.  The header of the OPTRAN file
C    specifies 300 OPTRAN water levels, and also the mean value of
C    4 predictor terms for each of the levels.  Next, comes the
C    read of the binary file with the reflected downwelling thermal
C    radiance "F factor" coefficients.  Next is a read of the "FX"
C    fixed gases adjustment term from an ASCII text file.  Lastly
C    comes the read of the non-LTE coefficients from a binary file.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date        Programmer     Comments
C    ----------- -------------- ----------------------------------------
C    Dec  1 1994 Scott Hannon   Created
C    Dec 21 1994 Scott Hannon   Fixed error with IOPF (now assigned)
C     5 Feb 1997 Scott Hannon   Re-wrote for FWO+FOW+FMW+FCOW.
C    28 Aug 1997 Scott Hannon   Re-wrote for sets 1 - 7 and thermal
C    30 Sep 1997 Scott Hannon   Added COFCO2 and INDCO2
C    27 Feb 1998 Scott Hannon   Added COFH2O, INDH2O, WAZOP, & WAVGOP
C    17 Aug 2000 Scott Hannon   Add FX
C    12 Feb 2001 Scott Hannon   hardcoded filenames instead of prompts
C    18 May 2005 Scott Hannon   Add HNO3 based on SO2 code
C    28 Jun 2005 Scott Hannon   "trace" version for CO2,SO2,HNO3,N2O
C    13 Oct 2005 Scott Hannon   Add non-LTE variables
C    17 Mar 2016 C Hepplewhite  sections ommitted due to absence of
C    coefficients.

!END====================================================================

C      =================================================================
       SUBROUTINE RDCOEF_CRIS_HRG4 ( IOUN, NCHAN, INDCHN, SETCHN,
     $     NCHN1,  NCHN2,  NCHN3,  NCHN4,  NCHN5,  NCHN6,  NCHN7,
     $    CLIST1, CLIST2, CLIST3, CLIST4, CLIST5, CLIST6, CLIST7,
     $     COEF1,  COEF2,  COEF3,  COEF4,  COEF5,  COEF6,  COEF7,
     $      FREQ, LABOVE,  COEFF, INDCO2, COFCO2, INDSO2, COFSO2,
     $    INDHNO, COFHNO, INDN2O, COFN2O,
     $    INDH2O,  WAZOP, WAVGOP, COFH2O, FX, NCHNTE, CLISTN, COEFN )
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
C      Input
       INTEGER   IOUN
       INTEGER   NCHAN
       INTEGER   INDCHN(MXCHAN)
C
C      Output
       INTEGER  SETCHN(MXCHAN)
       INTEGER  NCHN1
       INTEGER  NCHN2
       INTEGER  NCHN3
       INTEGER  NCHN4
       INTEGER  NCHN5
       INTEGER  NCHN6
       INTEGER  NCHN7
       INTEGER  CLIST1(MXCHN1)
       INTEGER  CLIST2(MXCHN2)
       INTEGER  CLIST3(MXCHN3)
       INTEGER  CLIST4(MXCHN4)
       INTEGER  CLIST5(MXCHN5)
       INTEGER  CLIST6(MXCHN6)
       INTEGER  CLIST7(MXCHN7)
       REAL  COEF1(N1COEF,MAXLAY,MXCHN1)
       REAL  COEF2(N2COEF,MAXLAY,MXCHN2)
       REAL  COEF3(N3COEF,MAXLAY,MXCHN3)
       REAL  COEF4(N4COEF,MAXLAY,MXCHN4)
       REAL  COEF5(N5COEF,MAXLAY,MXCHN5)
       REAL  COEF6(N6COEF,MAXLAY,MXCHN6)
       REAL  COEF7(N7COEF,MAXLAY,MXCHN7)
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
       INTEGER INDH2O(MXCHAN)
       REAL   WAZOP(MXOWLY)
       REAL  WAVGOP(NOWAVG,MXOWLY)
       REAL COFH2O(  NH2O,MXOWLY,MXCHNW)
       REAL     FX(MAXLAY)
       INTEGER NCHNTE
       INTEGER  CLISTN(MXCNTE)
       REAL  COEFN(NNCOEF,MXCNTE)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       CHARACTER*80  CLINE
       REAL FRQCHN
       REAL  FCHAN(NFCOEF)
       REAL  RJUNK
       INTEGER      I
       INTEGER     IC
       INTEGER  ICHAN
       INTEGER   IERR
       INTEGER     IL
       INTEGER      J
       INTEGER ICOUNT
       INTEGER LACHAN


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
C      Initialize "set"-independent index arrays
       DO I=1,MXCHAN
C         Trace gases
          INDCO2(I)=0
C          INDSO2(I)=0
C          INDHNO(I)=0
          INDN2O(I)=0
C         OPTRAN water
          INDH2O(I)=0
       ENDDO
C
C      ----------
C      Read set 1
C      ----------
       OPEN(UNIT=IOUN,FILE=FNCOF1,FORM='UNFORMATTED',STATUS='OLD',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNCOF1
 1020     FORMAT('Error ',I5,' openning file:',/,A80)
          STOP
       ENDIF
C
       J=1
       DO I=1,MXCHN1
C         Read data for this frequency/channel
          READ(IOUN) ICHAN, FRQCHN, ((COEF1(IC,IL,J),IC=1,N1COEF),
     $       IL=1,MAXLAY)
C
          SETCHN(ICHAN)=1
C
C         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) .NE. 0) THEN
             CLIST1(J)=ICHAN
             FREQ( INDCHN(ICHAN) )=FRQCHN
             J=J + 1
          ENDIF
       ENDDO
       NCHN1=J - 1
C
       CLOSE(IOUN)
C
C
C      ----------
C      Read set 2
C      ----------
       OPEN(UNIT=IOUN,FILE=FNCOF2,FORM='UNFORMATTED',STATUS='OLD',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNCOF2
          STOP
       ENDIF
C
       J=1
       DO I=1,MXCHN2
C         Read data for this frequency/channel
          READ(IOUN) ICHAN, FRQCHN, ((COEF2(IC,IL,J),IC=1,N2COEF),
     $       IL=1,MAXLAY)
C
          SETCHN(ICHAN)=2
C
C         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) .NE. 0) THEN
             CLIST2(J)=ICHAN
             FREQ( INDCHN(ICHAN) )=FRQCHN
             J=J + 1
          ENDIF
       ENDDO
       NCHN2=J - 1
C
       CLOSE(IOUN)
C
C
C      ----------
C      Read set 3
C      ----------
       OPEN(UNIT=IOUN,FILE=FNCOF3,FORM='UNFORMATTED',STATUS='OLD',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNCOF3
          STOP
       ENDIF
C
       J=1
       DO I=1,MXCHN3
C         Read data for this frequency/channel
          READ(IOUN) ICHAN, FRQCHN, ((COEF3(IC,IL,J),IC=1,N3COEF),
     $       IL=1,MAXLAY)
C
          SETCHN(ICHAN)=3
C
C         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) .NE. 0) THEN
             CLIST3(J)=ICHAN
             FREQ( INDCHN(ICHAN) )=FRQCHN
             J=J + 1
          ENDIF
       ENDDO
       NCHN3=J - 1
C
       CLOSE(IOUN)
C
C
C      ----------
C      Read set 4
C      ----------
       OPEN(UNIT=IOUN,FILE=FNCOF4,FORM='UNFORMATTED',STATUS='OLD',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNCOF4
          STOP
       ENDIF
C
       J=1
       DO I=1,MXCHN4
C         Read data for this frequency/channel
          READ(IOUN) ICHAN, FRQCHN, ((COEF4(IC,IL,J),IC=1,N4COEF),
     $       IL=1,MAXLAY)
C
          SETCHN(ICHAN)=4
C
C         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) .NE. 0) THEN
             CLIST4(J)=ICHAN
             FREQ( INDCHN(ICHAN) )=FRQCHN
             J=J + 1
          ENDIF
       ENDDO
       NCHN4=J - 1
C
       CLOSE(IOUN)
C
C

C      ----------
C      Read set 5
C      ----------
       OPEN(UNIT=IOUN,FILE=FNCOF5,FORM='UNFORMATTED',STATUS='OLD',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNCOF5
          STOP
       ENDIF
C
       J=1
       DO I=1,MXCHN5
C         Read data for this frequency/channel
          READ(IOUN) ICHAN, FRQCHN, ((COEF5(IC,IL,J),IC=1,N5COEF),
     $       IL=1,MAXLAY)
C
          SETCHN(ICHAN)=5
C
C         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) .NE. 0) THEN
             CLIST5(J)=ICHAN
             FREQ( INDCHN(ICHAN) )=FRQCHN
             J=J + 1
          ENDIF
       ENDDO
       NCHN5=J - 1
C
       CLOSE(IOUN)
C
C
C      ----------
C      Read set 6
C      ----------
       OPEN(UNIT=IOUN,FILE=FNCOF6,FORM='UNFORMATTED',STATUS='OLD',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNCOF6
          STOP
       ENDIF
C
       J=1
       DO I=1,MXCHN6
C         Read data for this frequency/channel
          READ(IOUN) ICHAN, FRQCHN, ((COEF6(IC,IL,J),IC=1,N6COEF),
     $       IL=1,MAXLAY)
C
          SETCHN(ICHAN)=6
C
C         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) .NE. 0) THEN
             CLIST6(J)=ICHAN
             FREQ( INDCHN(ICHAN) )=FRQCHN
             J=J + 1
          ENDIF
       ENDDO
       NCHN6=J - 1
C
       CLOSE(IOUN)
C
C
C      ----------
C      Read set 7
C      ----------
       OPEN(UNIT=IOUN,FILE=FNCOF7,FORM='UNFORMATTED',STATUS='OLD',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNCOF7
          STOP
       ENDIF
C
       J=1
       DO I=1,MXCHN7
C         Read data for this frequency/channel
          READ(IOUN) ICHAN, FRQCHN, ((COEF7(IC,IL,J),IC=1,N7COEF),
     $       IL=1,MAXLAY)
C
          SETCHN(ICHAN)=7
C
C         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) .NE. 0) THEN
             CLIST7(J)=ICHAN
             FREQ( INDCHN(ICHAN) )=FRQCHN
             J=J + 1
          ENDIF
       ENDDO
       NCHN7=J - 1
C
       CLOSE(IOUN)
C
C
       WRITE(6,'(A)') 'Completed rdcoef to set 7'
C      ---------------------------
C      Read CO2 perturbation coefs - placeholder set to zero
C      ---------------------------
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
C  
C       OPEN(UNIT=IOUN,FILE=FNCO2,FORM='UNFORMATTED',STATUS='OLD',
C     $    IOSTAT=IERR)
C       IF (IERR .NE. 0) THEN
C          WRITE(6,1020) IERR, FNCO2
C          STOP
C       ENDIF
C
C       J=1
C       DO I=1,MXCHNC
C         Read data for this frequency/channel
C          READ(IOUN) ICHAN, FRQCHN, ((COFCO2(IC,IL,J),IC=1,NCO2),
C     $       IL=1,MAXLAY)
C
C         Keep the data if the current channel is on the list
C          IF (INDCHN(ICHAN) .NE. 0) THEN
C             INDCO2(ICHAN)=J
C             J=J + 1
C          ENDIF
C       ENDDO
C
C       CLOSE(IOUN)
C
C      ---------------------
C      Read SO2 pertub coefs - placeholder while no coef file
C      ---------------------
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
C
C       OPEN(UNIT=IOUN,FILE=FNSO2,FORM='UNFORMATTED',STATUS='OLD',
C     $    IOSTAT=IERR)
C       IF (IERR .NE. 0) THEN
C          WRITE(6,1020) IERR, FNSO2
C          STOP
C       ENDIF
C
C       J=1
C       DO I=1,MXCHNS
C         Read data for this frequency/channel
C          READ(IOUN) ICHAN, FRQCHN, ((COFSO2(IC,IL,J),IC=1,NSO2),
C     $       IL=1,MAXLAY)
C
C         Keep the data if the current channel is on the list
C          IF (INDCHN(ICHAN) .NE. 0) THEN
C             INDSO2(ICHAN)=J
C             J=J + 1
C          ENDIF
C       ENDDO
C
C       CLOSE(IOUN)
C      ---------------------
C      Read HNO3 perturb coefs - placeholder while no coef file
C      ---------------------
       J=1
       DO I=1,MXCHNH
          DO IC=1,NHNO3
             DO IL=1,MAXLAY
                COFHNO(IC,IL,J) = 0.0
             ENDDO
          ENDDO
C         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) .NE. 0) THEN
             INDHNO(ICHAN)=J
             J=J + 1
          ENDIF
       ENDDO

C       OPEN(UNIT=IOUN,FILE=FNHNO3,FORM='UNFORMATTED',STATUS='OLD',
C     $    IOSTAT=IERR)
C       IF (IERR .NE. 0) THEN
C          WRITE(6,1020) IERR, FNHNO3
C          STOP
C       ENDIF
C
C       J=1
C       DO I=1,MXCHNH
C         Read data for this frequency/channel
C          READ(IOUN) ICHAN, FRQCHN, ((COFHNO(IC,IL,J),IC=1,NHNO3),
C     $       IL=1,MAXLAY)
C
C         Keep the data if the current channel is on the list
C          IF (INDCHN(ICHAN) .NE. 0) THEN
C             INDHNO(ICHAN)=J
C             J=J + 1
C          ENDIF
C       ENDDO
C
C       CLOSE(IOUN)
C      ---------------------
C      Read N2O perturb coefs - placeholder while no coef file.
C      ---------------------
C       OPEN(UNIT=IOUN,FILE=FNN2O,FORM='UNFORMATTED',STATUS='OLD',
C     $    IOSTAT=IERR)
C       IF (IERR .NE. 0) THEN
C          WRITE(6,1020) IERR, FNN2O
C          STOP
C       ENDIF
C
C       J=1
C       DO I=1,MXCHNN
C         Read data for this frequency/channel
C          READ(IOUN) ICHAN, FRQCHN, ((COFN2O(IC,IL,J),IC=1,NN2O),
C     $       IL=1,MAXLAY)
C         Keep the data if the current channel is on the list
C          IF (INDCHN(ICHAN) .NE. 0) THEN
C             INDN2O(ICHAN)=J
C             J=J + 1
C          ENDIF
C       ENDDO
C       CLOSE(IOUN)
C       WRITE(6,'(A,1X,I4)') 'Completed rdcoef N2O. no chans: ',J

C - these lines used as placeholder when no ceofficients are available.
       J=1
       DO I=1,MXCHNN
          DO IC=1,NN2O
             DO IL=1,MAXLAY
               COFN2O(IC,IL,J) = 0.0
             ENDDO
          ENDDO
C         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) .NE. 0) THEN
             INDN2O(ICHAN)=J
             J=J + 1
          ENDIF
       ENDDO

C      ---------------------
C      Read OPTRAN H2O coefs - placeholder to disable coefficients
C      ---------------------
       OPEN(UNIT=IOUN,FILE=FNOPTR,FORM='UNFORMATTED',STATUS='OLD',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNOPTR
          STOP
       ENDIF
C
       READ(IOUN) (WAZOP(IL),IL=1,MXOWLY)
       DO IC=1,NOWAVG
C         Read the header section
          READ(IOUN) (WAVGOP(IC,IL),IL=1,MXOWLY)
       ENDDO
C
       J=1
       DO I=1,MXCHNW
C         Read data for this frequency/channel
          READ(IOUN) ICHAN, FRQCHN, ((COFH2O(IC,IL,J),IC=1,NH2O),
     $       IL=1,MXOWLY)
C
C         Keep the data if the current channel is on the list
          IF (INDCHN(ICHAN) .NE. 0) THEN
             INDH2O(ICHAN)=J
             J=J + 1
          ENDIF
       ENDDO
C
       CLOSE(IOUN)
C      these loops for zeroing out optran coefficients
C       J=1
C       DO I=1,MXCHNW
C         DO IC=1,NH2O
C           DO IL=1,MXOWLY
C             COFH2O(IC,IL,J) = 0.0
C           ENDDO
C         ENDDO
C       ENDDO
C
C      -----------------------------------------------
C      Read the downward thermal F factor coefficients
C      -----------------------------------------------
C       OPEN(UNIT=IOUN,FILE=FNTHER,FORM='UNFORMATTED',STATUS='OLD',
C     $    IOSTAT=IERR)
C       IF (IERR .NE. 0) THEN
C          WRITE(6,1020) IERR, FNTHER
C          STOP
C       ENDIF
C
C       DO I=1,MXCHAN
C         Read data for this frequency/channel
ccc changed 18 May 2005
ccc          READ(IOUN) ICHAN, FRQCHN, LACHAN, (FCHAN(IC),IC=1,NFCOEF)
C          READ(IOUN) ICHAN, FRQCHN, (FCHAN(IC),IC=1,NFCOEF)
C          LACHAN=-1   ! assign dummy value
C         Keep the data if the current channel is on the list
C          IF (INDCHN(ICHAN) .NE. 0) THEN
C             LABOVE( INDCHN(ICHAN) )=LACHAN
C             DO IC=1,NFCOEF
C                COEFF(IC,INDCHN(ICHAN))=FCHAN(IC)
C             ENDDO
C          ENDIF
C       ENDDO
C
C       CLOSE(IOUN)
C
C set to zero - to be used when no coeff file available 
       DO I=1,MXCHAN
          DO IC=1,NFCOEF
             COEFF(IC,I)=0.0
          ENDDO
       ENDDO
C
C      -------
C      Read FX
C      -------
       OPEN(UNIT=IOUN,FILE=FNFX,FORM='FORMATTED',STATUS='OLD',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(6,1020) IERR, FNFX
          STOP
       ENDIF
C
C      Read the file
       ICOUNT=0
 10    READ(IOUN,9000,END=99) CLINE
 9000  FORMAT(A80)
       IF (CLINE(1:1) .NE. '!') THEN
C         Note: fx file format is:  layer_number  fx_value 
          READ(CLINE,*) IC, RJUNK
          ICOUNT=ICOUNT + 1
          FX(IC)=RJUNK
       ENDIF
       GOTO 10
C
 99    CLOSE(IOUN)
C
       IF (ICOUNT .NE. MAXLAY) THEN
          WRITE(6,1047) MAXLAY, ICOUNT
 1047     FORMAT('Error! Unexpected number of layers in fx file.',/,
     $    'Expected fx to have ',I4,' layers, but found ',I4)
       ENDIF
C

C      ------------
C      Read non-LTE
C      ------------
C       OPEN(UNIT=IOUN,FILE=FNCOFN,FORM='UNFORMATTED',STATUS='OLD',
C     $    IOSTAT=IERR)
C       IF (IERR .NE. 0) THEN
C          WRITE(6,1020) IERR, FNCOFN
C          STOP
C       ENDIF
C
C       J=1
C       DO I=1,MXCNTE
C         Read data for this frequency/channel
C          READ(IOUN) ICHAN, FRQCHN, (COEFN(IC,J),IC=1,NNCOEF)
C
C         Keep the data if the current channel is on the list
C          IF (INDCHN(ICHAN) .NE. 0) THEN
C             CLISTN(J)=ICHAN
C             J=J + 1
C          ENDIF
C       ENDDO
C       NCHNTE=J - 1
C
C       CLOSE(IOUN)
C placeholder set to zero
       DO I=1,MXCNTE
          DO IC=1,NNCOEF
             COEFN(IC,I)=0.0
          ENDDO
       ENDDO
C      ---------------------------------------------

C      ---------------------------------------------
C      Make sure all channels on the list were found
C      ---------------------------------------------
       ICOUNT=NCHN1 + NCHN2 + NCHN3 + NCHN4 + NCHN5 + NCHN6 + NCHN7
       IF (ICOUNT .NE. NCHAN) THEN
          WRITE(6,1050) NCHAN, ICOUNT
 1050     FORMAT('Error! Unexpected number of channels found.',/,
     $    'The channel list had ',I4,' channels, but found ',I4)
       ENDIF
C
C      ----------------------------
C      Show summary of channel sets
C      ----------------------------
ccc
       WRITE(6,1060) 1, NCHN1
 1060  FORMAT('Number of channels for set',I1,' = ',I4)
       WRITE(6,1060) 2, NCHN2
       WRITE(6,1060) 3, NCHN3
       WRITE(6,1060) 4, NCHN4
       WRITE(6,1060) 5, NCHN5
       WRITE(6,1060) 6, NCHN6
       WRITE(6,1060) 7, NCHN7
ccc
C
       RETURN
       END
