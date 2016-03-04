C=======================================================================
C=======================================================================
C
C    University of Maryland Baltimore County [UMBC]
C
C    AIRS
C
C    SUNPAR version with trace gases
C
!F77====================================================================


!ROUTINE NAME:
C    SUNPAR


!ABSTRACT:
C    Calculate the fast transmittance code temperature/amount/angle
C    dependent predictors for a profile at the effective sun angle.


!CALL PROTOCOL:
C    SUNPAR ( LBOT, RTEMP, RWAMNT, ROAMNT, RCAMNT,
C  $                PTEMP, PWAMNT, POAMNT, PCAMNT,
C  $          PRES,   SECANG, CONPRD,
C  $          FPRED4, FPRED5, FPRED6, FPRED7,
C  $          WPRED4, WPRED5, WPRED6, WPRED7,
C  $          OPRED4, OPRED5, OPRED6, OPRED7,
C  $          CPRED4, TRCPRD )


!INPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    INTEGER   LBOT    bottom layer number         none
C    REAL arr  PTEMP   profile temperature         Kelvin
C    REAL arr  PCAMNT  prof carbon monoxide amnt   kiloMoles/cm^2
C    REAL arr  POAMNT  profile ozone amount        kiloMoles/cm^2
C    REAL arr  PRES    layer pressures             atmospheres
C    REAL arr  PWAMNT  profile water amount        kiloMoles/cm^2
C    REAL arr  RTEMP   reference temperature       Kelvin
C    REAL arr  RCAMNT  ref carbon monoxide amount  kiloMoles/cm^2
C    REAL arr  ROAMNT  reference ozone amount      kiloMoles/cm^2
C    REAL arr  RWAMNT  reference water amount      kiloMoles/cm^2
C    REAL arr  SECANG  secant of path angle        none


!OUTPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    REAL arr  CPRED4  carbon monoxide pred set4   various
C    REAL arr  FPRED4  fixed predictors set4       various
C    REAL arr  FPRED5  fixed predictors set5       various
C    REAL arr  FPRED6  fixed predictors set6       various
C    REAL arr  FPRED7  fixed predictors set7       various
C    REAL arr  OPRED4  ozone predictors set4       various
C    REAL arr  OPRED5  ozone predictors set5       various
C    REAL arr  OPRED6  ozone predictors set6       variou
C    REAL arr  OPRED7  ozone predictors set7       various
C    REAL arr  TRCPRD  trace gas pert predictors   various
C    REAL arr  WPRED4  water predictors set4       various
C    REAL arr  WPRED5  water predictors set5       various
C    REAL arr  WPRED6  water predictors set6       various
C    REAL arr  WPRED7  water predictors set7       various


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


!COMMON BLOCKS
C    none


!DESCRIPTION:
C    August 2000 version of the 100 layer AIRS Fast Transmittance
C    Code by L.L.Strow/S.Hannon.
C
C    Rapid transmittace algorithm predictors consisting of various gas
C    amount and temperature ratios and offsets relative to a reference
C    profile are calculated.  Only sets 4 - 7 are calculated, as these
C    are the only sets fit for extreme sun angles (up to secant 9).
C
C    ===================================================================
C    The FTC profile variables computed for each layer are:
C
C    ---------------------------------
C    CONPRD: water continuum predictors (7 terms)
C       1) a*W/Tr^2    2) a*(W/Tr^2)^2   3) a*W/Tr  4) a*W^2/Tr
C       5) a*(W/Tr)^2  6) a*W/Tr^4       7) a*Wr
C
C    -------------------------------
C    Fixed predictors
C
C    FPRED4: FCOW (11 terms):
C       1) a        2) a^2      3) a*Tr    4) a*Tr^2
C       5) Tr       6) Tr^2     7) a*Trz   8) a^2*Trz
C       9) a^2*Tr  10) a^3     11) sqrt(a)
C
C    FPRED5: FWO (11 terms):
C       1) a        2) a^2      3) a*Tr    4) a*Tr^2
C       5) Tr       6) Tr^2     7) a*Trz   8) a*Trz/Tr
C       9) a^2*Tr  10) sqrt(a) 11) Trz
C
C    FPRED6: FWO (8 terms):
C       1) a        2) a^2      3) a*Tr    4) a*Tr^2
C       5) Tr       6) Tr^2     7) a*Trz   8) sqrt(a)
C
C    FPRED7: FWO (8 terms):
C       1) a        2) a^2      3) a*Tr    4) a*Tr^2
C       5) Tr       6) Tr^2     7) a*Trz   8) sqrt(a)
C
C    ---------------------------------
C    Water predictors
C
C    WPRED4: FCOW (13 terms):
C       1) W*a             2) W             3) sqrt(W*a)
C       4) W*a*dT          5) (W*a)^2       6) sqrt(W*a)*dT
C       7) root^4(W*a)     8) W*a*W/Wz      9) W*a^2
C      10) (W*a)^3        11) W*a*Cz*a     12) sqrt(W*a)*W/Wz
C      13) W*a^2*dT
C
C    WPRED5: FWO bfsw (3 terms):
C       1) W*a           2) (W*a)^3/2       3) W*a*dT
C
C    WPRED6: FWO mfmw (7 terms):
C       1) W*a           2) (W*a)^3/2       3) W*a*dT
C       4) (W*a)^2       5) (W*a)^3/2*dT    6) (W*a)^3
C       7) W*a^2
C
C    WPRED7: FWO mfbw (13 terms):
C       1) W*a           2) (W*a)^3/2       3) W*a*dT
C       4) (W*a)^2       5) (W*a)^3/2*dT    6) (W*a)^3
C       7) W*a^2         8) W*a*W/Wz        9) (W*a)^3/2*W/Wz
C      10) (W*a)^5/4    11) (W*a)^2*W/Wz   12) W^2*a
C      13) (W*a)^7/4
C
C    ---------------------------
C    Ozone predictors
C
C    OPRED4: FCOW (3 terms):
C       1) O*a         2) sqrt(O*a)     3) O*a*dT
C
C    OPRED5: FWO bfsw (1 term):
C       1) O*a
C
C    OPRED6: FWO mfmw (1 term):
C       1) O*a
C
C    OPRED7: FWO mfbw (1 term):
C       1) O*a
C
C    ---------------------------
C    CPRED4: carbon monoxide predictors (11 terms):
C       1) C*a           2) sqrt(C*a)       3) C*a*dT
C       4) (C*a)^2       5) C*a*C/Cz        6) sqrt(C*a)*dT
C       7) root^4(C*a)   8) sqrt(C*a)*C/Cz  9) C
C
C    ---------------------------
C    CO2PRD: CO2 perturbation coefs (4 terms):
C       1) a        2) Tr      3) a*Tr    4) a*Tr^2
C
C    -----
C    where:
C    "a" is the secant of the viewing angle SECANG
C    "Tr" is the temperature ratio PTEMP/RTEMP
C    "Trz" is the pressure weighted temperature ratio above, i.e.
C      the sum i=2 to i=L of { P(i) * ( P(i) -  P(i-1) )* Tr(i-1) }
C      where "P" is the pressure PRES and "L" is the layer number, and
C      Trz(L=1)=0
C    "W" is the water amount ratio PWAMNT/RWAMNT
C    "dT" is the temperature offset PTEMP-RTEMP
C    "Wz" is the pressure weighted water amount above ratio, the
C      sum i=1 to i=L of { P(i) * ( (P(i)-P(i-1) ) * PWAMNT(i) },
C      divided by the same sum except using RWAMNT instead of PWAMNT.
C      For these sums, term P(0) is defined as P(0)=2*P(1) - P(2).
C    "O" is the ozone amount ratio POAMNT/ROAMNT
C    "C" is the carbon monoxide amount ratio POAMNT/ROAMNT
C    "Cz" is the pressure weighted CO amount above ratio, the
C      sum i=1 to i=L of { P(i) * ( (P(i)-P(i-1) ) * PCAMNT(i) },
C      divided by the same sum except using RCAMNT instead of PCAMNT.
C      For these sums, term P(0) is defined as P(0)=2*P(1) - P(2).
C
C    ===================================================================


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    Assumes vaguely realistic profile amounts and temperatures, else
C    there might be divide by zero problems, etc.


!ROUTINE HISTORY:
C    Date        Programmer     Comments
C    ----------- -------------- ----------------------------------------
C    27 Aug 1997 Scott Hannon   Created from calpar
C    30 Sep 1997 Scott Hannon   Added variable CO2
C    26 Aug 1998 Scott Hannon   Add LBOT to call; loop on LBOT instead
C                               of MAXLAY
C    24 Aug 2000 Scott Hannon   Remove FIXMUL (calc'ed in CALPAR)
C    18 Sep 2002 Scott Hannon   Add predictors 6 & 7 to H2O con
C    25 Apr 2003 Scott Hannon   Add SO2
C    23 Jun 2005 Scott Hannon   "trace" version for CO2, SO2, & HNO3,
C                               with all using the same predictors.
C    13 Oct 2005 S.Hannon/C.Barnet bug fix: assign TRCPRD 1-7 (was 1-4)

!END====================================================================

C      =================================================================
       SUBROUTINE SUNPAR ( LBOT,
     $    RTEMP,  RWAMNT, ROAMNT, RCAMNT,
     $    PTEMP,  PWAMNT, POAMNT, PCAMNT,
     $    PRES,   SECANG, CONPRD,
     $    FPRED4, FPRED5, FPRED6, FPRED7,
     $    WPRED4, WPRED5, WPRED6, WPRED7,
     $    OPRED4, OPRED5, OPRED6, OPRED7,
     $    CPRED4, TRCPRD )
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
       INTEGER   LBOT
       REAL  RTEMP(MAXLAY)
       REAL RWAMNT(MAXLAY)
       REAL ROAMNT(MAXLAY)
       REAL RCAMNT(MAXLAY)
       REAL  PTEMP(MAXLAY)
       REAL PWAMNT(MAXLAY)
       REAL POAMNT(MAXLAY)
       REAL PCAMNT(MAXLAY)
       REAL   PRES(MAXLAY)
       REAL SECANG(MAXLAY)
C
C      Output
       REAL CONPRD( N1CON,MAXLAY)
       REAL FPRED4( N4FIX,MAXLAY)
       REAL FPRED5( N5FIX,MAXLAY)
       REAL FPRED6( N6FIX,MAXLAY)
       REAL FPRED7( N7FIX,MAXLAY)
       REAL WPRED4( N4H2O,MAXLAY)
       REAL WPRED5( N5H2O,MAXLAY)
       REAL WPRED6( N6H2O,MAXLAY)
       REAL WPRED7( N7H2O,MAXLAY)
       REAL OPRED4(  N4O3,MAXLAY)
       REAL OPRED5(  N5O3,MAXLAY)
       REAL OPRED6(  N6O3,MAXLAY)
       REAL OPRED7(  N7O3,MAXLAY)
       REAL CPRED4(  N4CO,MAXLAY)
       REAL TRCPRD(NTRACE,MAXLAY)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER      L
       REAL    PDP
       REAL  PNORM
       REAL     DT
       REAL     TR
       REAL     TZ
       REAL    TRZ
c       REAL    A_F ! unused so removed 14 Feb 2001
       REAL    A_W
       REAL  WZREF
       REAL     WZ
       REAL   AZ_W
       REAL    A_O
       REAL    A_C
       REAL     CZ
       REAL  CZREF
       REAL   AZ_C
       REAL TJUNKS
       REAL WJUNKA
       REAL WJUNKR
       REAL WJUNKS
       REAL WJUNKZ
       REAL WJUNK4
       REAL OJUNKA
       REAL CJUNKA
       REAL CJUNKR
       REAL CJUNKS
       REAL CJUNKZ


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
C
C      Initialize the sum terms to zero
       PNORM=0.0E+0
       TZ=0.0E+0
       WZREF=0.0E+0
       WZ=0.0E+0
       CZREF=0.0E+0
       CZ=0.0E+0
C
C      --------------------
C      Loop over the layers
C      --------------------
       DO L=1,LBOT
C
C         ---------------------------
C         Calculate the basic profile
C         dependent predictors.
C         ---------------------------
C
          IF (L .EQ. 1) THEN
             PDP=PRES(1)*( PRES(2) - PRES(1))
             TRZ=0.0E+0
          ELSE
             PDP=PRES(L)*( PRES(L) - PRES(L-1) )
             PNORM=PNORM + PDP
C
C            Note: TRZ, TOZ, and TMZ use layer-above terms
             TZ=TZ + PDP*TR
             TRZ=TZ/PNORM
          ENDIF
C
C         Temperature terms
          DT=PTEMP(L) - RTEMP(L)
          TR=PTEMP(L)/RTEMP(L)
C
C         Water terms
          A_W=PWAMNT(L)/RWAMNT(L)
          WZREF=WZREF + PDP*RWAMNT(L)
          WZ=WZ + PDP*PWAMNT(L)
          AZ_W=WZ/WZREF
C
C         Ozone terms
          A_O=POAMNT(L)/ROAMNT(L)
C
C         Carbon monoxide terms
          A_C=PCAMNT(L)/RCAMNT(L)
          CZREF=CZREF + PDP*RCAMNT(L)
          CZ=CZ + PDP*PCAMNT(L)
          AZ_C=CZ/CZREF
C
C         ----------------------
C         Load up the predictors
C         ----------------------
C
C         -----
C         Fixed
C         -----
          TJUNKS=TR*TR
C
          FPRED4(1,L)=SECANG(L)
          FPRED4(2,L)=SECANG(L)*SECANG(L)
          FPRED4(3,L)=SECANG(L)*TR
          FPRED4(4,L)=SECANG(L)*TJUNKS
          FPRED4(5,L)=TR
          FPRED4(6,L)=TJUNKS
          FPRED4(7,L)=SECANG(L)*TRZ
          FPRED4(8,L)=SECANG(L)*SECANG(L)*TRZ
          FPRED4(9,L)=SECANG(L)*SECANG(L)*TR
          FPRED4(10,L)=SECANG(L)*SECANG(L)*SECANG(L)
          FPRED4(11,L)=SQRT(SECANG(L))
C
C         Fixed predictors for FWO sun bfsw = set5
          FPRED5(1,L)=SECANG(L)
          FPRED5(2,L)=SECANG(L)*SECANG(L)
          FPRED5(3,L)=SECANG(L)*TR
          FPRED5(4,L)=SECANG(L)*TJUNKS
          FPRED5(5,L)=TR
          FPRED5(6,L)=TJUNKS
          FPRED5(7,L)=SECANG(L)*TRZ
          FPRED5(8,L)=SECANG(L)*TRZ/TR
          FPRED5(9,L)=SECANG(L)*SECANG(L)*TR
          FPRED5(10,L)=SQRT(SECANG(L))
          FPRED5(11,L)=TRZ
C
C         Fixed predictors for FWO sun mfmw = set6
          FPRED6(1,L)=SECANG(L)
          FPRED6(2,L)=SECANG(L)*SECANG(L)
          FPRED6(3,L)=SECANG(L)*TR
          FPRED6(4,L)=SECANG(L)*TJUNKS
          FPRED6(5,L)=TR
          FPRED6(6,L)=TJUNKS
          FPRED6(7,L)=SECANG(L)*TRZ
          FPRED6(8,L)=SQRT(SECANG(L))
C
C         Fixed predictors for FWO sun mfbw = set7
          FPRED7(1,L)=SECANG(L)
          FPRED7(2,L)=SECANG(L)*SECANG(L)
          FPRED7(3,L)=SECANG(L)*TR
          FPRED7(4,L)=SECANG(L)*TJUNKS
          FPRED7(5,L)=TR
          FPRED7(6,L)=TJUNKS
          FPRED7(7,L)=SECANG(L)*TRZ
          FPRED7(8,L)=SQRT(SECANG(L))
C
C
C         -----
C         Ozone
C         -----
          OJUNKA=SECANG(L)*A_O
C
C         ozone predictors for FCOW = set4
          OPRED4(1,L)=OJUNKA
          OPRED4(2,L)=SQRT( OJUNKA )
          OPRED4(3,L)=OJUNKA*DT
C
C         ozone predictors for FWO sun bfsw = set5
          OPRED5(1,L)=OJUNKA
C
C         ozone predictors for FWO sun mfmw = set6
          OPRED6(1,L)=OJUNKA
C
C         ozone predictors for FWO sun mfbw = set7
          OPRED7(1,L)=OJUNKA
C
C
C         -----
C         Water
C         -----
          WJUNKA=SECANG(L)*A_W
          WJUNKR=SQRT( WJUNKA )
          WJUNKS=WJUNKA*WJUNKA
          WJUNKZ=WJUNKA*A_W/AZ_W
          WJUNK4=SQRT( WJUNKR )
C
C         water predictors for FCOW = set4
          WPRED4( 1,L)=WJUNKA
          WPRED4( 2,L)=A_W
          WPRED4( 3,L)=WJUNKR
          WPRED4( 4,L)=WJUNKA*DT
          WPRED4( 5,L)=WJUNKS
          WPRED4( 6,L)=WJUNKR*DT
          WPRED4( 7,L)=WJUNK4
          WPRED4( 8,L)=WJUNKZ
          WPRED4( 9,L)=WJUNKA*SECANG(L)
          WPRED4(10,L)=WJUNKS*WJUNKA
          WPRED4(11,L)=WJUNKA*AZ_C*SECANG(L)
          WPRED4(12,L)=WJUNKZ/WJUNKR
          WPRED4(13,L)=WJUNKA*DT*SECANG(L)
C
C         Water predictors for FWO sun bfsw = set5
          WPRED5( 1,L)=WJUNKA
          WPRED5( 2,L)=WJUNKA*WJUNKR
          WPRED5( 3,L)=WJUNKA*DT
C
C         Water predictors for FWO sun mfmw = set6
          WPRED6( 1,L)=WJUNKA
          WPRED6( 2,L)=WJUNKA*WJUNKR
          WPRED6( 3,L)=WJUNKA*DT
          WPRED6( 4,L)=WJUNKS
          WPRED6( 5,L)=WJUNKA*WJUNKR*DT
          WPRED6( 6,L)=WJUNKA*WJUNKS
          WPRED6( 7,L)=WJUNKA*SECANG(L)
C
C         Water predictors for FWO sun mfbw = set7
          WPRED7( 1,L)=WJUNKA
          WPRED7( 2,L)=WJUNKA*WJUNKR
          WPRED7( 3,L)=WJUNKA*DT
          WPRED7( 4,L)=WJUNKS
          WPRED7( 5,L)=WJUNKA*WJUNKR*DT
          WPRED7( 6,L)=WJUNKA*WJUNKS
          WPRED7( 7,L)=WJUNKA*SECANG(L)
          WPRED7( 8,L)=WJUNKZ
          WPRED7( 9,L)=WJUNKZ*WJUNKR
          WPRED7(10,L)=WJUNKA*WJUNK4
          WPRED7(11,L)=WJUNKA*WJUNKZ
          WPRED7(12,L)=WJUNKA*A_W
          WPRED7(13,L)=WJUNKS/WJUNK4
C
C         ---------------
C         Water continuum (for FWO, FOW, FMW, FCOW)
C         ---------------
          CONPRD(1,L)=WJUNKA/TJUNKS
          CONPRD(2,L)=CONPRD(1,L)*A_W/TJUNKS
          CONPRD(3,L)=WJUNKA/TR
          CONPRD(4,L)=CONPRD(3,L)*A_W
          CONPRD(5,L)=CONPRD(1,L)*A_W
          CONPRD(6,L)=CONPRD(1,L)/TJUNKS
          CONPRD(7,L)=WJUNKA
C
C
C         ---------------
C         Carbon monoxide for FCOW = set4
C         ---------------
          CJUNKA=SECANG(L)*A_C
          CJUNKR=SQRT( CJUNKA )
          CJUNKS=CJUNKA*CJUNKA
          CJUNKZ=CJUNKA*A_C/AZ_C
C
          CPRED4(1,L)=CJUNKA
          CPRED4(2,L)=CJUNKR
          CPRED4(3,L)=CJUNKA*DT
          CPRED4(4,L)=CJUNKS
          CPRED4(5,L)=CJUNKZ
          CPRED4(6,L)=CJUNKR*DT
          CPRED4(7,L)=SQRT( CJUNKR )
          CPRED4(8,L)=CJUNKZ/CJUNKR
          CPRED4(9,L)=A_C
          CPRED4(10,L)=CJUNKA*SECANG(L)
          CPRED4(11,L)=CJUNKR*SECANG(L)
C
C
C         ---------------
C         trace gas perturbation coefs
C         ---------------
C         The first 4 trace predictors are used by all trace gases
          TRCPRD(1,L)=SECANG(L)
          TRCPRD(2,L)=TR
          TRCPRD(3,L)=SECANG(L)*TR
          TRCPRD(4,L)=SECANG(L)*TJUNKS
C         The last 3 trace predictors are only used by N2O
          TRCPRD(5,L)=SECANG(L)*SECANG(L)
          TRCPRD(6,L)=1.0
          TRCPRD(7,L)=SQRT( SECANG(L) )
C
       ENDDO
C      End loop over layers
C
       RETURN
       END
