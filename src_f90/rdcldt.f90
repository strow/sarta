!=======================================================================
!    University of Maryland Baltimore Country (UMBC)
!    AIRS
!    RDCLDT
!F90====================================================================

!ROUTINE NAME:
!    RDCLDT

!ABSTRACT:
!    Read in the cloud lookup tables

!CALL PROTOCOL
!    RDCLDT ( IPOPN, INDCHN, MIETYP, FNMIEA, FNMIEE, FNMIEG,
!       MIENPS, MIEPS, MIEABS, MIEEXT, MIEASY )

!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    INTEGER   IPOPN    I/O unit number             none
!    INT arr   INDCHN  indices of channels         none
!    INT arr   MIETYP  Mie particle type code #    none
!    CHAR arr  FNMIEA  Mie absorption filenames    none
!    CHAR arr  FNMIEE  Mie extinction filenames    none
!    CHAR arr  FNMIEG  Mie asymmetry filenames     none

!OUTPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    INT arr   MIENPS  Mie # of particle sizes     none
!    REAL arr  MIEPS   Mie cloud particle size     um
!    REAL arr  MIEABS  Mie cloud absorption        m^2/g
!    REAL arr  MIEEXT  Mie cloud extinction        m^2/g
!    REAL arr  MIEASY  Mie cloud asymmetry         none

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
!       opened, read, and closed. This is done 3*NMIETY times.

!COMMON BLOCKS
!    none

!DESCRIPTION:
!    May 2009 version of the cloudy SARTA code by L.Strow/S.Hannon.

!    Cloud lookup tables of absorption, extinction, and asymmetry
!    are read in from FORTRAN binary data files.

!ALGORITHM REFERENCES:
!    none

!KNOWN BUGS AND LIMITATIONS:
!    none

!ROUTINE HISTORY:
! Date        Programmer     Comments
! ----------- -------------- -------------------------------------------
! 12 May 2009 Scott Hannon   Created from rdcoef_pclsam.f

!END====================================================================

!      =================================================================

SUBROUTINE RDCLDT( IPOPN, INDCHN, MIETYP, FNMIEA, FNMIEE, FNMIEG,  &
    MIENPS, MIEPS, MIEABS, MIEEXT, MIEASY )
!      =================================================================


!-----------------------------------------------------------------------
!      INCLUDE FILES
!-----------------------------------------------------------------------
USE incFTC

!-----------------------------------------------------------------------
 IMPLICIT NONE
!-----------------------------------------------------------------------

!INTEGER, INTENT(OUT)                     :: IOUN
!INTEGER                                  :: IOUN
integer, intent (in) :: IPOPN
INTEGER, INTENT(IN OUT)                  :: INDCHN(MXCHAN)
INTEGER, INTENT(IN OUT)                  :: MIETYP(NMIETY)
CHARACTER (LEN=79), INTENT(IN)          :: FNMIEA(NMIETY)
CHARACTER (LEN=79), INTENT(IN)          :: FNMIEE(NMIETY)
CHARACTER (LEN=79), INTENT(IN)          :: FNMIEG(NMIETY)
INTEGER, INTENT(OUT)                     :: MIENPS(NMIETY)
REAL, INTENT(OUT)                        :: MIEPS(MXMIEA,NMIETY)
REAL, INTENT(OUT)                        :: MIEABS(MXCHAN,MXMIEA,NMIETY)
REAL, INTENT(OUT)                        :: MIEEXT(MXCHAN,MXMIEA,NMIETY)
REAL, INTENT(OUT)                        :: MIEASY(MXCHAN,MXMIEA,NMIETY)

!-----------------------------------------------------------------------
!      EXTERNAL FUNCTIONS
!-----------------------------------------------------------------------
!      none

!-----------------------------------------------------------------------
!      ARGUMENTS
!-----------------------------------------------------------------------
!      Input

!INTEGER :: ! channel use/index
!INTEGER :: ! particle type code number
!CHARACTER (LEN=79) :: ! absorption filenames
!CHARACTER (LEN=79) :: ! extinction filenames
!CHARACTER (LEN=79) :: ! asymmetry filenames

!      Output
!INTEGER :: ! number of particle sizes
!REAL :: ! particle size
!REAL :: ! absorption
!REAL :: ! extinction
!REAL :: ! asymmetry

!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------

!      local
REAL :: RJUNK
REAL :: XJUNK(MXMIEA)
INTEGER :: I
INTEGER :: IC
INTEGER :: IERR
INTEGER :: IL
INTEGER :: J
INTEGER :: K


!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none

!***********************************************************************
!***********************************************************************
!                    EXECUTABLE CODE
!***********************************************************************
!***********************************************************************

!      Read Mie lookup tables
DO K=1,NMIETY
  
!         -------------------------
!         Read Mie absorption table
!         -------------------------
  OPEN(UNIT=IPOPN,FILE=FNMIEA(K),FORM='UNFORMATTED',STATUS='OLD', IOSTAT=IERR)
  IF (IERR /= 0) THEN
    WRITE(6,1020) IERR, FNMIEA(K)
    1020     FORMAT('Error ',I5,' opening file:',/,A80)
    STOP
  END IF
  
!         Read the number of channels and mie types
  READ(IPOPN) I, IC
  IF (I /= MXCHAN) THEN
    WRITE(6,1071) FNMIEA(K), I, MXCHAN
    1071        FORMAT('ERROR! unexpected number of channels in mie file',  &
        /,A80,/,'File has',I6,' channels, but MXCHAN=',I5)
    STOP
  END IF
  IF (IC > MXMIEA) THEN
    WRITE(6,1072) FNMIEA(K), IC, MXMIEA
    1072        FORMAT('ERROR! too many particle sizes in mie file',  &
        /,A80,/,'File has',I6,' sizes, but MXMIEA=',I3)
    STOP
  END IF
  IF (IC < 1) THEN
    WRITE(6,1073) FNMIEA(K), IC
    1073        FORMAT('ERROR! invalid # of particle sizes in mie file',  &
        /,A80,/,'File has',I12,' sizes')
    STOP
  END IF
  MIENPS(K)=IC
  
!         Read mie particle sizes
  READ(IPOPN) (MIEPS(IL,K),IL=1,MIENPS(K))
  
!         Read mie abs data for required channels
  DO I=1,MXCHAN
    IF (INDCHN(I) /= 0) THEN
      READ(IPOPN) (MIEABS(INDCHN(I),IL,K),IL=1,MIENPS(K))
    ELSE
      READ(IPOPN) (XJUNK(IL),IL=1,MIENPS(K))
    END IF
    ENDDO
      
      CLOSE(IPOPN)
      
      
!         -------------------------
!         Read Mie extinction table
!         -------------------------
      OPEN(UNIT=IPOPN,FILE=FNMIEE(K),FORM='UNFORMATTED',STATUS='OLD',  &
          IOSTAT=IERR)
      IF (IERR /= 0) THEN
        WRITE(6,1020) IERR, FNMIEE(K)
        STOP
      END IF
      
!         Read the number of channels and mie types
      READ(IPOPN) I, IC
      IF (I /= MXCHAN) THEN
        WRITE(6,1071) FNMIEE(K), I, MXCHAN
        STOP
      END IF
      IF (IC /= MIENPS(K)) THEN
        WRITE(6,1074) FNMIEE(K), IC, MIENPS(K)
        1074        FORMAT('ERROR! unexpected # of particle sizes in mie file',  &
            /,A80,/,'File has',I12,' sizes, but MIENPS=',I3)
        STOP
      END IF
      
!         Read mie particle sizes
      READ(IPOPN) (XJUNK(I),I=1,MIENPS(K))
!         Check particle sizes are consistent
      DO I=1,MIENPS(K)
        RJUNK=ABS( MIEPS(I,K) - XJUNK(I) )
        IF (RJUNK > 1E-4*MIEPS(I,K)) THEN
          WRITE(6,1075) FNMIEE(K)
          1075           FORMAT('ERROR! unexpected particle sizes in mie file',  &
              /,A80)
          STOP
        END IF
        ENDDO
          
!         Read mie ext data for required channels
          J=1
          DO I=1,MXCHAN
            IF (INDCHN(I) /= 0) THEN
              READ(IPOPN) (MIEEXT(INDCHN(I),IL,K),IL=1,MIENPS(K))
            ELSE
              READ(IPOPN) (XJUNK(IL),IL=1,MIENPS(K))
            END IF
            ENDDO
              
              CLOSE(IPOPN)
              
              
!         ------------------------------
!         Read Mie asymmetry ("g") table
!         ------------------------------
              OPEN(UNIT=IPOPN,FILE=FNMIEG(K),FORM='UNFORMATTED',STATUS='OLD',  &
                  IOSTAT=IERR)
              IF (IERR /= 0) THEN
                WRITE(6,1020) IERR, FNMIEG(K)
                STOP
              END IF
              
!         Read the number of channels and mie types
              READ(IPOPN) I, IC
              IF (I /= MXCHAN) THEN
                WRITE(6,1071) FNMIEG(K), I, MXCHAN
                STOP
              END IF
              IF (IC /= MIENPS(K)) THEN
                WRITE(6,1074) FNMIEG(K), IC, MIENPS(K)
                STOP
              END IF
              
!         Read mie particle sizes
              READ(IPOPN) (XJUNK(I),I=1,MIENPS(K))
!         Check particle sizes are consistent
              DO I=1,MIENPS(K)
                RJUNK=ABS( MIEPS(I,K) - XJUNK(I) )
                IF (RJUNK > 1E-4*MIEPS(I,K)) THEN
                  WRITE(6,1075) FNMIEG(K)
                  STOP
                END IF
                ENDDO
                  
!         Read mie data for required channels
                  J=1
                  DO I=1,MXCHAN
                    IF (INDCHN(I) /= 0) THEN
                      READ(IPOPN) (MIEASY(INDCHN(I),IL,K),IL=1,MIENPS(K))
                    ELSE
                      READ(IPOPN) (XJUNK(IL),IL=1,MIENPS(K))
                    END IF
                    ENDDO
                      
                      CLOSE(IPOPN)
                      
                      ENDDO
                        
                        RETURN
                      END SUBROUTINE RDCLDT
