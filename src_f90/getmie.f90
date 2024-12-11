!=======================================================================
 
! Code converted using TO_F90_LOOP by Alan Miller
! Date: 2023-04-04  Time: 16:44:55
 
!=======================================================================

!              University of Maryland Baltimore County [UMBC]

!              AIRS

!              GETMIE

!F77====================================================================


!ROUTINE NAME: GETMIE


!ABSTRACT:
!    Get index of CTYPE in MIETYP


!CALL PROTOCOL:
!    GETMIE(CTYPE, MIETYP, INDMIE, IERR)


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    INTEGER   CTYPE   desired code number         none
!    INT arr   MIETYP  available code numbers      none


!OUTPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    INTEGER   INDMIE  index of CTYPE in MIETYP    none
!    INTEGER   IERR    error level                 none


!INPUT/OUTPUT PARAMETERS: none


!RETURN VALUES: none


!PARENT(S): KLAYERS


!ROUTINES CALLED: none


!FILES ACCESSED:
!    none


!COMMON BLOCKS: none


!DESCRIPTION:
!    If CTYPE >= 100, then this routine finds
!    the index of CTYPE in MIETYP.  It looks for an exact match, but
!    otherwise finds the nearest match within the same century/class
!    as CTYPE {ie same value of rounddown(ctype/100)}. If an exact
!    match is found, then IERR=0.  If an inexact but within-class
!    match is found, then IERR=1.  If no match is found then IERR=2.

!    Cloud/particle type classes:

!       CTYPE   | class description
!       --------|------------------
!       000-099 : black clouds (non-Mie)
!       --------|------------------
!       100-199 : liquid water droplets
!       200-299 : ice/cirrus
!       300-399 : mineral/dust
!       400-499 : sea salt (water with salt)
!       500-599 : soot/smoke
!       600-699 : sulfate/pollutants



!ALGORITHM REFERENCES: see DESCRIPTION


!KNOWN BUGS AND LIMITATIONS:
!    none


!ROUTINE HISTORY:
!    Date     Programmer        Comments
!------------ ----------------- ----------------------------------------
! 30 Mar 2006 Scott Hannon      created
! 05 Jan 2007 Scott Hannon      Debug. Add LBLACK.
! 20 Feb 2007 Scott Hannon      Remove LBLACK


!END====================================================================

!      =================================================================

SUBROUTINE GETMIE(CTYPE, MIETYP, INDMIE, IERR)
!      =================================================================


!-----------------------------------------------------------------------
!      IMPLICIT NONE
!-----------------------------------------------------------------------

INTEGER, INTENT(IN)                      :: CTYPE
INTEGER, INTENT(IN)                      :: MIETYP(NMIETY)
INTEGER, INTENT(OUT)                     :: INDMIE
INTEGER, INTENT(OUT)                     :: IERR
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
!      Input parameters:

INTEGER :: ! available mie lookup table code numbers

!      Output parameters




!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
INTEGER :: ICLASS
INTEGER :: IDIFF
INTEGER :: IMIN
INTEGER :: K
INTEGER :: MCLASS

!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none


!***********************************************************************
!***********************************************************************
!      EXECUTABLE CODE begins below
!***********************************************************************
!***********************************************************************

!      Initialize variables
INDMIE=-9999
IERR=2
!$$$       IMIN=1E+16
IMIN=1E+6

IF (CTYPE > 100) THEN
  
!         CTYPE class/century
  ICLASS=CTYPE/100  ! integer division
  
!cc this line for testing
!      write(6,*) 'TEST: GETMIE iclass=',ICLASS
!cc
  
!         Mie cloud; loop over the available mie types
  DO K=1,NMIETY
    MCLASS=MIETYP(K)/100  ! integer division
    IDIFF=ABS( CTYPE - MIETYP(K) )
    
!            Look for an exact match
    IF ( CTYPE == MIETYP(K) ) THEN
      INDMIE=K
      IMIN=0
      IERR=0
    ELSE
!               Look for the nearest class/century match
      IF ((ICLASS == MCLASS) .AND. (IDIFF < IMIN)) THEN
        INDMIE=K
        IMIN=IDIFF
        IERR=1
      END IF
    END IF
    
    ENDDO
    END IF
    
!cc uncomment for testing
!      print *, 'getmie: mietyp=',(MIETYP(K),K=1,NMIETY)
!      print *, 'getmie: ctype, indmie, ierr=',CTYPE,INDMIE,IERR
!cc
    
    RETURN
  END SUBROUTINE GETMIE
