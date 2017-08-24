C=======================================================================
C=======================================================================
C
C              University of Maryland Baltimore County [UMBC]
C
C              AIRS
C
C              GETMIE
C
!F77====================================================================


!ROUTINE NAME: GETMIE


!ABSTRACT:
C    Get index of CTYPE in MIETYP


!CALL PROTOCOL:
C    GETMIE(CTYPE, MIETYP, INDMIE, IERR)


!INPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    INTEGER   CTYPE   desired code number         none
C    INT arr   MIETYP  available code numbers      none


!OUTPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    INTEGER   INDMIE  index of CTYPE in MIETYP    none
C    INTEGER   IERR    error level                 none


!INPUT/OUTPUT PARAMETERS: none


!RETURN VALUES: none


!PARENT(S): KLAYERS


!ROUTINES CALLED: none


!FILES ACCESSED:
C    none


!COMMON BLOCKS: none


!DESCRIPTION:
C    If CTYPE >= 100, then this routine finds
C    the index of CTYPE in MIETYP.  It looks for an exact match, but
C    otherwise finds the nearest match within the same century/class
C    as CTYPE {ie same value of rounddown(ctype/100)}. If an exact
C    match is found, then IERR=0.  If an inexact but within-class
C    match is found, then IERR=1.  If no match is found then IERR=2.
C
C    Cloud/particle type classes:
C
C       CTYPE   | class description
C       --------|------------------
C       000-099 : black clouds (non-Mie)
C       --------|------------------
C       100-199 : liquid water droplets
C       200-299 : ice/cirrus
C       300-399 : mineral/dust
C       400-499 : sea salt (water with salt)
C       500-599 : soot/smoke
C       600-699 : sulfate/pollutants
C


!ALGORITHM REFERENCES: see DESCRIPTION


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date     Programmer        Comments
C------------ ----------------- ----------------------------------------
C 30 Mar 2006 Scott Hannon      created
C 05 Jan 2007 Scott Hannon      Debug. Add LBLACK.
C 20 Feb 2007 Scott Hannon      Remove LBLACK


!END====================================================================

C      =================================================================
       SUBROUTINE GETMIE(CTYPE, MIETYP, INDMIE, IERR)
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
C      Input parameters:
       INTEGER  CTYPE         ! desired cloud type code number
       INTEGER MIETYP(NMIETY) ! available mie lookup table code numbers
C
C      Output parameters
       INTEGER INDMIE  ! index of CTYPE in MIETYP
       INTEGER IERR    ! error level


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER ICLASS
       INTEGER IDIFF
       INTEGER IMIN
       INTEGER K
       INTEGER MCLASS

C-----------------------------------------------------------------------
C      SAVE STATEMENTS
C-----------------------------------------------------------------------
C      none


C***********************************************************************
C***********************************************************************
C      EXECUTABLE CODE begins below
C***********************************************************************
C***********************************************************************

C      Initialize variables
       INDMIE=-9999
       IERR=2
       IMIN=1E+16
C
       IF (CTYPE .GT. 100) THEN

C         CTYPE class/century
          ICLASS=CTYPE/100  ! integer division

ccc this line for testing
c      write(6,*) 'TEST: GETMIE iclass=',ICLASS
ccc

C         Mie cloud; loop over the available mie types
          DO K=1,NMIETY
             MCLASS=MIETYP(K)/100  ! integer division
             IDIFF=ABS( CTYPE - MIETYP(K) )
C
C            Look for an exact match
             IF ( CTYPE .EQ. MIETYP(K) ) THEN
                INDMIE=K
                IMIN=0
                IERR=0
             ELSE
C               Look for the nearest class/century match
                IF ((ICLASS .EQ. MCLASS) .AND. (IDIFF .LT. IMIN)) THEN
                   INDMIE=K
                   IMIN=IDIFF
                   IERR=1
                ENDIF
             ENDIF
C
          ENDDO
       ENDIF

ccc uncomment for testing
c      print *, 'getmie: mietyp=',(MIETYP(K),K=1,NMIETY)
c      print *, 'getmie: ctype, indmie, ierr=',CTYPE,INDMIE,IERR
ccc
C
       RETURN
       END
