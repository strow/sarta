!=======================================================================
!
!    University of Maryland Baltimore County [UMBC]
!    AIRS
!    CHECK_PLEVS
!
!F90====================================================================

!ROUTINE NAME:
!    CHECK_PLEVS

!ABSTRACT:
!    Check that the pressure levels used to build this SARTA are
!    compatible with the values in the atmospheric profile.
!    SARTA pressure levels are read from the reference profile.
!
!    Called from main AFTER rdpref() and opnrtp() IN IPPROF loop

!CALL PROTOCOL:
!    CHECK_PLEVS ( ALT, PRES        )


!INPUT PARAMETERS:
!    type      name    purpose                     units       from
!    --------  ------  --------------------------  ----------  ---------
!    REAL arr  ALT     layer altitudes             m           rdpref
!    REAL arr  PRES    layer pressures             atm         rdpref

     SUBROUTINE CHECK_PLEVS(PROF, NLAY)
!
use incFTC
!-----------------------------------------------------------------------
!      IMPLICIT NONE
!-----------------------------------------------------------------------
       IMPLICIT NONE

!-----------------------------------------------------------------------
!      INCLUDE FILES
!-----------------------------------------------------------------------
include 'rtpdefs.f90'

 
!-----------------------------------------------------------------------
!      ARGUMENTS
!-----------------------------------------------------------------------
!      INPUT
 LOGICAL :: LWANT      ! do we want this profile?
 INTEGER :: IPROF      ! number of current profile
 INTEGER :: IOPCI      ! input RTP unit
 INTEGER :: NLAY
 real(4), dimension(MAXLAY) :: ALT,DZ,PRES
 !!integer, intent (in) :: IPOPN
 !!character(len=90) ::  PFILE

!      OUTPUT
!      Profile data structure
       RECORD /RTPPROF/ PROF
       INTEGER :: ISTAT

!      LOCAL VARIABLES
integer ::  IERR, IJUNK, L, LR, NLEV
character(len=80) ::  CLINE
REAL(4), dimension(MAXLAY) :: PLEVS

!-----------------------------------------------------------------------

!      ------------------------
!      Read the current profile
!      ------------------------
!      ISTAT=rtpread(IOPCI, PROF)
!
!      IF (ISTAT .EQ. -1) GOTO 9999  ! reached end of file
!
!      IF (.NOT. LWANT) GOTO 9999    ! skip prof if not wanted
!
!     --------------------
!     Pull out prof.plevs 
!     --------------------
!      Number of levels
       NLEV=PROF%nlevs

! Get pressure levels PLEVS
      IF (PROF%plevs(1) .LT. PROF%plevs(NLEV)) THEN
!        Prof is in top-down order
         DO L=1,NLAY
            PLEVS(L)=PROF%plevs(L)
         ENDDO
      ELSE
!        Prof is in bottom-up order
         DO L=1,NLAY
            LR=1 + NLAY - L  ! reversed layer index
            PLEVS(L)=PROF%plevs(LR)
         ENDDO
      ENDIF
print*,'check_plevs: plevs(10) ', PLEVS(10) 
! ---------------------------------------
! COmpare with reference profile pressures
! ----------------------------------------

!
 9999  RETURN
       END
