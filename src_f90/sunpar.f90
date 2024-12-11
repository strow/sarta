!=======================================================================
!
!    University of Maryland Baltimore County [UMBC]
!
!    AIRS
!
!    SUNPAR version with trace gases
!
!F90====================================================================


!ROUTINE NAME:
!    SUNPAR


!ABSTRACT:
!    Calculate the fast transmittance code temperature/amount/angle
!    dependent predictors for a profile at the effective sun angle.


!CALL PROTOCOL:
!    SUNPAR ( LBOT, RTEMP, RWAMNT, ROAMNT, RCAMNT,
!  $                PTEMP, PWAMNT, POAMNT, PCAMNT,
!  $          PRES,   SECANG, CONPRD,
!  $          FPRED4, FPRED5, FPRED6, FPRED7,
!  $          WPRED4, WPRED5, WPRED6, WPRED7,
!  $          OPRED4, OPRED5, OPRED6, OPRED7,
!  $          CPRED4, TRCPRD )


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    INTEGER   LBOT    bottom layer number         none
!    REAL arr  PTEMP   profile temperature         Kelvin
!    REAL arr  PCAMNT  prof carbon monoxide amnt   kiloMoles/cm^2
!    REAL arr  POAMNT  profile ozone amount        kiloMoles/cm^2
!    REAL arr  PRES    layer pressures             atmospheres
!    REAL arr  PWAMNT  profile water amount        kiloMoles/cm^2
!    REAL arr  RTEMP   reference temperature       Kelvin
!    REAL arr  RCAMNT  ref carbon monoxide amount  kiloMoles/cm^2
!    REAL arr  ROAMNT  reference ozone amount      kiloMoles/cm^2
!    REAL arr  RWAMNT  reference water amount      kiloMoles/cm^2
!    REAL arr  SECANG  secant of path angle        none


!OUTPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    REAL arr  CPRED4  carbon monoxide pred set4   various
!    REAL arr  FPRED4  fixed predictors set4       various
!    REAL arr  FPRED5  fixed predictors set5       various
!    REAL arr  FPRED6  fixed predictors set6       various
!    REAL arr  FPRED7  fixed predictors set7       various
!    REAL arr  OPRED4  ozone predictors set4       various
!    REAL arr  OPRED5  ozone predictors set5       various
!    REAL arr  OPRED6  ozone predictors set6       variou
!    REAL arr  OPRED7  ozone predictors set7       various
!    REAL arr  TRCPRD  trace gas pert predictors   various
!    REAL arr  WPRED4  water predictors set4       various
!    REAL arr  WPRED5  water predictors set5       various
!    REAL arr  WPRED6  water predictors set6       various
!    REAL arr  WPRED7  water predictors set7       various


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


!COMMON BLOCKS
!    none


!DESCRIPTION:
!    August 2000 version of the 100 layer AIRS Fast Transmittance
!    Code by L.L.Strow/S.Hannon.
!
!    Rapid transmittace algorithm predictors consisting of various gas
!    amount and temperature ratios and offsets relative to a reference
!    profile are calculated.  Only sets 4 - 7 are calculated, as these
!    are the only sets fit for extreme sun angles (up to secant 9).
!
!    ===================================================================
!    The FTC profile variables computed for each layer are:
!
!    ---------------------------------
!    CONPRD: water continuum predictors (7 terms)
!       1) a*W/Tr^2    2) a*(W/Tr^2)^2   3) a*W/Tr  4) a*W^2/Tr
!       5) a*(W/Tr)^2  6) a*W/Tr^4       7) a*Wr
!
!    -------------------------------
!    Fixed predictors
!
!    FPRED4: FCOW (11 terms):
!       1) a        2) a^2      3) a*Tr    4) a*Tr^2
!       5) Tr       6) Tr^2     7) a*Trz   8) a^2*Trz
!       9) a^2*Tr  10) a^3     11) sqrt(a)
!
!    FPRED5: FWO (11 terms):
!       1) a        2) a^2      3) a*Tr    4) a*Tr^2
!       5) Tr       6) Tr^2     7) a*Trz   8) a*Trz/Tr
!       9) a^2*Tr  10) sqrt(a) 11) Trz
!
!    FPRED6: FWO (8 terms):
!       1) a        2) a^2      3) a*Tr    4) a*Tr^2
!       5) Tr       6) Tr^2     7) a*Trz   8) sqrt(a)
!
!    FPRED7: FWO (8 terms):
!       1) a        2) a^2      3) a*Tr    4) a*Tr^2
!       5) Tr       6) Tr^2     7) a*Trz   8) sqrt(a)
!
!    ---------------------------------
!    Water predictors
!
!    WPRED4: FCOW (13 terms):
!       1) W*a             2) W             3) sqrt(W*a)
!       4) W*a*dT          5) (W*a)^2       6) sqrt(W*a)*dT
!       7) root^4(W*a)     8) W*a*W/Wz      9) W*a^2
!      10) (W*a)^3        11) W*a*Cz*a     12) sqrt(W*a)*W/Wz
!      13) W*a^2*dT
!
!    WPRED5: FWO bfsw (3 terms):
!       1) W*a           2) (W*a)^3/2       3) W*a*dT
!
!    WPRED6: FWO mfmw (7 terms):
!       1) W*a           2) (W*a)^3/2       3) W*a*dT
!       4) (W*a)^2       5) (W*a)^3/2*dT    6) (W*a)^3
!       7) W*a^2
!
!    WPRED7: FWO mfbw (13 terms):
!       1) W*a           2) (W*a)^3/2       3) W*a*dT
!       4) (W*a)^2       5) (W*a)^3/2*dT    6) (W*a)^3
!       7) W*a^2         8) W*a*W/Wz        9) (W*a)^3/2*W/Wz
!      10) (W*a)^5/4    11) (W*a)^2*W/Wz   12) W^2*a
!      13) (W*a)^7/4
!
!    ---------------------------
!    Ozone predictors
!
!    OPRED4: FCOW (3 terms):
!       1) O*a         2) sqrt(O*a)     3) O*a*dT
!
!    OPRED5: FWO bfsw (1 term):
!       1) O*a
!
!    OPRED6: FWO mfmw (1 term):
!       1) O*a
!
!    OPRED7: FWO mfbw (1 term):
!       1) O*a
!
!    ---------------------------
!    CPRED4: carbon monoxide predictors (11 terms):
!       1) C*a           2) sqrt(C*a)       3) C*a*dT
!       4) (C*a)^2       5) C*a*C/Cz        6) sqrt(C*a)*dT
!       7) root^4(C*a)   8) sqrt(C*a)*C/Cz  9) C
!
!    ---------------------------
!    CO2PRD: CO2 perturbation coefs (4 terms):
!       1) a        2) Tr      3) a*Tr    4) a*Tr^2
!
!    -----
!    where:
!    "a" is the secant of the viewing angle SECANG
!    "Tr" is the temperature ratio PTEMP/RTEMP
!    "Trz" is the pressure weighted temperature ratio above, i.e.
!      the sum i=2 to i=L of { P(i) * ( P(i) -  P(i-1) )* Tr(i-1) }
!      where "P" is the pressure PRES and "L" is the layer number, and
!      Trz(L=1)=0
!    "W" is the water amount ratio PWAMNT/RWAMNT
!    "dT" is the temperature offset PTEMP-RTEMP
!    "Wz" is the pressure weighted water amount above ratio, the
!      sum i=1 to i=L of { P(i) * ( (P(i)-P(i-1) ) * PWAMNT(i) },
!      divided by the same sum except using RWAMNT instead of PWAMNT.
!      For these sums, term P(0) is defined as P(0)=2*P(1) - P(2).
!    "O" is the ozone amount ratio POAMNT/ROAMNT
!    "C" is the carbon monoxide amount ratio POAMNT/ROAMNT
!    "Cz" is the pressure weighted CO amount above ratio, the
!      sum i=1 to i=L of { P(i) * ( (P(i)-P(i-1) ) * PCAMNT(i) },
!      divided by the same sum except using RCAMNT instead of PCAMNT.
!      For these sums, term P(0) is defined as P(0)=2*P(1) - P(2).
!
!    ===================================================================


!ALGORITHM REFERENCES:
!    none


!KNOWN BUGS AND LIMITATIONS:
!    Assumes vaguely realistic profile amounts and temperatures, else
!    there might be divide by zero problems, etc.


!ROUTINE HISTORY:
!    Date        Programmer     Comments
!    ----------- -------------- ----------------------------------------
!    27 Aug 1997 Scott Hannon   Created from calpar
!    30 Sep 1997 Scott Hannon   Added variable CO2
!    26 Aug 1998 Scott Hannon   Add LBOT to call; loop on LBOT instead
!                               of MAXLAY
!    24 Aug 2000 Scott Hannon   Remove FIXMUL (calc'ed in CALPAR)
!    18 Sep 2002 Scott Hannon   Add predictors 6 & 7 to H2O con
!    25 Apr 2003 Scott Hannon   Add SO2
!    23 Jun 2005 Scott Hannon   "trace" version for CO2, SO2, & HNO3,
!                               with all using the same predictors.
!    13 Oct 2005 S.Hannon/C.Barnet bug fix: assign TRCPRD 1-7 (was 1-4)

!END====================================================================

!      =================================================================
       SUBROUTINE SUNPAR ( LBOT, &
         RTEMP,  RWAMNT, ROAMNT, RCAMNT, &
         PTEMP,  PWAMNT, POAMNT, PCAMNT, &
         PRES,   SECANG, CONPRD, &
         FPRED4, FPRED5, FPRED6, FPRED7, &
         WPRED4, WPRED5, WPRED6, WPRED7, &
         OPRED4, OPRED5, OPRED6, OPRED7, &
         CPRED4, TRCPRD )
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
       INTEGER   LBOT
real(4), dimension(MAXLAY) :: RTEMP, RWAMNT, ROAMNT, RCAMNT, PTEMP
real(4), dimension(MAXLAY) :: PWAMNT, POAMNT, PCAMNT, PRES, SECANG
!
!      Output
real(4), dimension( N1CON,MAXLAY) :: CONPRD
real(4), dimension( N4FIX,MAXLAY) :: FPRED4
real(4), dimension( N5FIX,MAXLAY) :: FPRED5
real(4), dimension( N6FIX,MAXLAY) :: FPRED6
real(4), dimension( N7FIX,MAXLAY) :: FPRED7
real(4), dimension( N4H2O,MAXLAY) :: WPRED4
real(4), dimension( N5H2O,MAXLAY) :: WPRED5
real(4), dimension( N6H2O,MAXLAY) :: WPRED6
real(4), dimension( N7H2O,MAXLAY) :: WPRED7
real(4), dimension(  N4O3,MAXLAY) :: OPRED4
real(4), dimension(  N5O3,MAXLAY) :: OPRED5
real(4), dimension(  N6O3,MAXLAY) :: OPRED6
real(4), dimension(  N7O3,MAXLAY) :: OPRED7
real(4), dimension(N4CO,MAXLAY)   :: CPRED4
real(4), dimension(NTRACE,MAXLAY) :: TRCPRD


!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
       INTEGER      L
real(4) :: PDP, PNORM, DT, TR, TZ, TRZ
!       REAL    A_F ! unused so removed 14 Feb 2001
real(4) :: A_W, WZREF, WZ, AZ_W, A_O, A_C, CZ, CZREF, AZ_C
real(4) :: TJUNKS, WJUNKA, WJUNKR, WJUNKS, WJUNKZ, WJUNK4
real(4) :: OJUNKA, CJUNKA, CJUNKR, CJUNKS, CJUNKZ

!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none

!***********************************************************************
!                    EXECUTABLE CODE
!***********************************************************************
!
!
!      Initialize the sum terms to zero
       PNORM=0.0E+0
       TZ=0.0E+0
       WZREF=0.0E+0
       WZ=0.0E+0
       CZREF=0.0E+0
       CZ=0.0E+0
!
!      --------------------
!      Loop over the layers
!      --------------------
       DO L=1,LBOT
!
!         ---------------------------
!         Calculate the basic profile
!         dependent predictors.
!         ---------------------------
!
          IF (L .EQ. 1) THEN
             PDP=PRES(1)*( PRES(2) - PRES(1))
             TRZ=0.0E+0
          ELSE
             PDP=PRES(L)*( PRES(L) - PRES(L-1) )
             PNORM=PNORM + PDP
!
!            Note: TRZ, TOZ, and TMZ use layer-above terms
             TZ=TZ + PDP*TR
             TRZ=TZ/PNORM
          ENDIF
!
!         Temperature terms
          DT=PTEMP(L) - RTEMP(L)
          TR=PTEMP(L)/RTEMP(L)
!
!         Water terms
          A_W=PWAMNT(L)/RWAMNT(L)
          WZREF=WZREF + PDP*RWAMNT(L)
          WZ=WZ + PDP*PWAMNT(L)
          AZ_W=WZ/WZREF
!
!         Ozone terms
          A_O=POAMNT(L)/ROAMNT(L)
!
!         Carbon monoxide terms
          A_C=PCAMNT(L)/RCAMNT(L)
          CZREF=CZREF + PDP*RCAMNT(L)
          CZ=CZ + PDP*PCAMNT(L)
          AZ_C=CZ/CZREF
!
!         ----------------------
!         Load up the predictors
!         ----------------------
!
!         -----
!         Fixed
!         -----
          TJUNKS=TR*TR
!
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
!
!         Fixed predictors for FWO sun bfsw = set5
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
!
!         Fixed predictors for FWO sun mfmw = set6
          FPRED6(1,L)=SECANG(L)
          FPRED6(2,L)=SECANG(L)*SECANG(L)
          FPRED6(3,L)=SECANG(L)*TR
          FPRED6(4,L)=SECANG(L)*TJUNKS
          FPRED6(5,L)=TR
          FPRED6(6,L)=TJUNKS
          FPRED6(7,L)=SECANG(L)*TRZ
          FPRED6(8,L)=SQRT(SECANG(L))
!
!         Fixed predictors for FWO sun mfbw = set7
          FPRED7(1,L)=SECANG(L)
          FPRED7(2,L)=SECANG(L)*SECANG(L)
          FPRED7(3,L)=SECANG(L)*TR
          FPRED7(4,L)=SECANG(L)*TJUNKS
          FPRED7(5,L)=TR
          FPRED7(6,L)=TJUNKS
          FPRED7(7,L)=SECANG(L)*TRZ
          FPRED7(8,L)=SQRT(SECANG(L))
!
!
!         -----
!         Ozone
!         -----
          OJUNKA=SECANG(L)*A_O
!
!         ozone predictors for FCOW = set4
          OPRED4(1,L)=OJUNKA
          OPRED4(2,L)=SQRT( OJUNKA )
          OPRED4(3,L)=OJUNKA*DT
!
!         ozone predictors for FWO sun bfsw = set5
          OPRED5(1,L)=OJUNKA
!
!         ozone predictors for FWO sun mfmw = set6
          OPRED6(1,L)=OJUNKA
!
!         ozone predictors for FWO sun mfbw = set7
          OPRED7(1,L)=OJUNKA
!
!
!         -----
!         Water
!         -----
          WJUNKA=SECANG(L)*A_W
          WJUNKR=SQRT( WJUNKA )
          WJUNKS=WJUNKA*WJUNKA
          WJUNKZ=WJUNKA*A_W/AZ_W
          WJUNK4=SQRT( WJUNKR )
!
!         water predictors for FCOW = set4
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
!
!         Water predictors for FWO sun bfsw = set5
          WPRED5( 1,L)=WJUNKA
          WPRED5( 2,L)=WJUNKA*WJUNKR
          WPRED5( 3,L)=WJUNKA*DT
!
!         Water predictors for FWO sun mfmw = set6
          WPRED6( 1,L)=WJUNKA
          WPRED6( 2,L)=WJUNKA*WJUNKR
          WPRED6( 3,L)=WJUNKA*DT
          WPRED6( 4,L)=WJUNKS
          WPRED6( 5,L)=WJUNKA*WJUNKR*DT
          WPRED6( 6,L)=WJUNKA*WJUNKS
          WPRED6( 7,L)=WJUNKA*SECANG(L)
!
!         Water predictors for FWO sun mfbw = set7
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
!
!         ---------------
!         Water continuum (for FWO, FOW, FMW, FCOW)
!         ---------------
          CONPRD(1,L)=WJUNKA/TJUNKS
          CONPRD(2,L)=CONPRD(1,L)*A_W/TJUNKS
          CONPRD(3,L)=WJUNKA/TR
          CONPRD(4,L)=CONPRD(3,L)*A_W
          CONPRD(5,L)=CONPRD(1,L)*A_W
          CONPRD(6,L)=CONPRD(1,L)/TJUNKS
          CONPRD(7,L)=WJUNKA
!
!
!         ---------------
!         Carbon monoxide for FCOW = set4
!         ---------------
          CJUNKA=SECANG(L)*A_C
          CJUNKR=SQRT( CJUNKA )
          CJUNKS=CJUNKA*CJUNKA
          CJUNKZ=CJUNKA*A_C/AZ_C
!
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
!
!
!         ---------------
!         trace gas perturbation coefs
!         ---------------
!         The first 4 trace predictors are used by all trace gases
          TRCPRD(1,L)=SECANG(L)
          TRCPRD(2,L)=TR
          TRCPRD(3,L)=SECANG(L)*TR
          TRCPRD(4,L)=SECANG(L)*TJUNKS
!         The last 3 trace predictors are only used by N2O
          TRCPRD(5,L)=SECANG(L)*SECANG(L)
          TRCPRD(6,L)=1.0
          TRCPRD(7,L)=SQRT( SECANG(L) )
!
       ENDDO
!      End loop over layers
!
       RETURN
       END
