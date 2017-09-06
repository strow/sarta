C=======================================================================
C=======================================================================
C
C    University of Maryland Baltimore Country (UMBC)
C
C    AIRS
C
C    RDCLDT
C
!F77====================================================================


!ROUTINE NAME:
C    RDCLDT


!ABSTRACT:
C    Read in the cloud lookup tables


!CALL PROTOCOL
C    RDCLDT ( IOUN, INDCHN, MIETYP, FNMIEA, FNMIEE, FNMIEG,
C       MIENPS, MIEPS, MIEABS, MIEEXT, MIEASY )


!INPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    INTEGER   IOUN    I/O unit number             none
C    INT arr   INDCHN  indices of channels         none
C    INT arr   MIETYP  Mie particle type code #    none
C    CHAR arr  FNMIEA  Mie absorption filenames    none
C    CHAR arr  FNMIEE  Mie extinction filenames    none
C    CHAR arr  FNMIEG  Mie asymmetry filenames     none

!OUTPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    INT arr   MIENPS  Mie # of particle sizes     none
C    REAL arr  MIEPS   Mie cloud particle size     um
C    REAL arr  MIEABS  Mie cloud absorption        m^2/g
C    REAL arr  MIEEXT  Mie cloud extinction        m^2/g
C    REAL arr  MIEASY  Mie cloud asymmetry         none


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
C       opened, read, and closed. This is done 3*NMIETY times.


!COMMON BLOCKS
C    none


!DESCRIPTION:
C    May 2009 version of the cloudy SARTA code by L.Strow/S.Hannon.
C
C    Cloud lookup tables of absorption, extinction, and asymmetry
C    are read in from FORTRAN binary data files.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C Date        Programmer     Comments
C ----------- -------------- -------------------------------------------
C 12 May 2009 Scott Hannon   Created from rdcoef_pclsam.f


!END====================================================================

C      =================================================================
       SUBROUTINE RDCLDT( IOUN, INDCHN, MIETYP, FNMIEA, FNMIEE, FNMIEG,
     $    MIENPS, MIEPS, MIEABS, MIEEXT, MIEASY )
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
       INTEGER   IOUN                    ! I/O unit number
       INTEGER INDCHN(MXCHAN)            ! channel use/index
       INTEGER MIETYP(NMIETY)            ! particle type code number
       CHARACTER*79 FNMIEA(NMIETY)       ! absorption filenames
       CHARACTER*79 FNMIEE(NMIETY)       ! extinction filenames
       CHARACTER*79 FNMIEG(NMIETY)       ! asymmetry filenames

C      Output
       INTEGER MIENPS(NMIETY)            ! number of particle sizes
       REAL  MIEPS(MXMIEA,NMIETY)        ! particle size
       REAL MIEABS(MXCHAN,MXMIEA,NMIETY) ! absorption
       REAL MIEEXT(MXCHAN,MXMIEA,NMIETY) ! extinction
       REAL MIEASY(MXCHAN,MXMIEA,NMIETY) ! asymmetry


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
C
C      local
       REAL  RJUNK
       REAL  XJUNK(MXMIEA)
       INTEGER      I
       INTEGER     IC
       INTEGER   IERR
       INTEGER     IL
       INTEGER      J
       INTEGER      K


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
C      Read Mie lookup tables
       DO K=1,NMIETY
C
C         -------------------------
C         Read Mie absorption table
C         -------------------------
          OPEN(UNIT=IOUN,FILE=FNMIEA(K),FORM='UNFORMATTED',STATUS='OLD',
     $       IOSTAT=IERR)
          IF (IERR .NE. 0) THEN
             WRITE(6,1020) IERR, FNMIEA(K)
1020     FORMAT('Error ',I5,' opening file:',/,A80)
             STOP
          ENDIF
C
C         Read the number of channels and mie types
          READ(IOUN) I, IC
          IF (I .NE. MXCHAN) THEN
             WRITE(6,1071) FNMIEA(K), I, MXCHAN
 1071        FORMAT('ERROR! unexpected number of channels in mie file',
     $       /,A80,/,'File has',I6,' channels, but MXCHAN=',I5)
             STOP
          ENDIF
          IF (IC .GT. MXMIEA) THEN
             WRITE(6,1072) FNMIEA(K), IC, MXMIEA
 1072        FORMAT('ERROR! too many particle sizes in mie file',
     $       /,A80,/,'File has',I6,' sizes, but MXMIEA=',I3)
             STOP
          ENDIF
          IF (IC .LT. 1) THEN
             WRITE(6,1073) FNMIEA(K), IC
 1073        FORMAT('ERROR! invalid # of particle sizes in mie file',
     $       /,A80,/,'File has',I12,' sizes')
             STOP
          ENDIF
          MIENPS(K)=IC
C
C         Read mie particle sizes
          READ(IOUN) (MIEPS(IL,K),IL=1,MIENPS(K))
C
C         Read mie abs data for required channels
          DO I=1,MXCHAN
             IF (INDCHN(I) .NE. 0) THEN
                READ(IOUN) (MIEABS(INDCHN(I),IL,K),IL=1,MIENPS(K))
             ELSE
                READ(IOUN) (XJUNK(IL),IL=1,MIENPS(K))
             ENDIF
          ENDDO
C
          CLOSE(IOUN)
C
C
C         -------------------------
C         Read Mie extinction table
C         -------------------------
          OPEN(UNIT=IOUN,FILE=FNMIEE(K),FORM='UNFORMATTED',STATUS='OLD',
     $       IOSTAT=IERR)
          IF (IERR .NE. 0) THEN
             WRITE(6,1020) IERR, FNMIEE(K)
             STOP
          ENDIF
C
C         Read the number of channels and mie types
          READ(IOUN) I, IC
          IF (I .NE. MXCHAN) THEN
             WRITE(6,1071) FNMIEE(K), I, MXCHAN
             STOP
          ENDIF
          IF (IC .NE. MIENPS(K)) THEN
             WRITE(6,1074) FNMIEE(K), IC, MIENPS(K)
 1074        FORMAT('ERROR! unexpected # of particle sizes in mie file',
     $          /,A80,/,'File has',I12,' sizes, but MIENPS=',I3)
             STOP
          ENDIF
C
C         Read mie particle sizes
          READ(IOUN) (XJUNK(I),I=1,MIENPS(K))
C         Check particle sizes are consistent
          DO I=1,MIENPS(K)
             RJUNK=ABS( MIEPS(I,K) - XJUNK(I) )
             IF (RJUNK .GT. 1E-4*MIEPS(I,K)) THEN
                WRITE(6,1075) FNMIEE(K)
 1075           FORMAT('ERROR! unexpected particle sizes in mie file',
     $             /,A80)
                STOP
             ENDIF
          ENDDO
C
C         Read mie ext data for required channels
          J=1
          DO I=1,MXCHAN
             IF (INDCHN(I) .NE. 0) THEN
                READ(IOUN) (MIEEXT(INDCHN(I),IL,K),IL=1,MIENPS(K))
             ELSE
                READ(IOUN) (XJUNK(IL),IL=1,MIENPS(K))
             ENDIF
          ENDDO
C
          CLOSE(IOUN)
C
C
C         ------------------------------
C         Read Mie asymmetry ("g") table
C         ------------------------------
          OPEN(UNIT=IOUN,FILE=FNMIEG(K),FORM='UNFORMATTED',STATUS='OLD',
     $       IOSTAT=IERR)
          IF (IERR .NE. 0) THEN
             WRITE(6,1020) IERR, FNMIEG(K)
             STOP
          ENDIF
C
C         Read the number of channels and mie types
          READ(IOUN) I, IC
          IF (I .NE. MXCHAN) THEN
             WRITE(6,1071) FNMIEG(K), I, MXCHAN
             STOP
          ENDIF
          IF (IC .NE. MIENPS(K)) THEN
             WRITE(6,1074) FNMIEG(K), IC, MIENPS(K)
             STOP
          ENDIF
C
C         Read mie particle sizes
          READ(IOUN) (XJUNK(I),I=1,MIENPS(K))
C         Check particle sizes are consistent
          DO I=1,MIENPS(K)
             RJUNK=ABS( MIEPS(I,K) - XJUNK(I) )
             IF (RJUNK .GT. 1E-4*MIEPS(I,K)) THEN
                WRITE(6,1075) FNMIEG(K)
                STOP
             ENDIF
          ENDDO
C
C         Read mie data for required channels
          J=1
          DO I=1,MXCHAN
             IF (INDCHN(I) .NE. 0) THEN
                READ(IOUN) (MIEASY(INDCHN(I),IL,K),IL=1,MIENPS(K))
             ELSE
                READ(IOUN) (XJUNK(IL),IL=1,MIENPS(K))
             ENDIF
          ENDDO
C
          CLOSE(IOUN)
C
       ENDDO
C
       RETURN
       END
