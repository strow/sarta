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
!
       COMMON /COMLEV/ PLEV
!
!      INPUT
       INTEGER :: NLAY
       real(4), dimension(MAXLAY) :: ALT,DZ,PRES
!
!      OUTPUT
!      Profile data structure
       RECORD /RTPPROF/ PROF
!
!      LOCAL VARIABLES
       integer ::  I, IERR, IJUNK, L, LR, NLEV
       character(len=80) ::  CLINE
       REAL(4), dimension(MAXLAY+1) :: PLEV, PLEVS_RTP
       real(4) :: RJUNK1

!-----------------------------------------------------------------------
!   Execution
! ----------------------------------------------------------------------
      if(DEBUG) print*,'check_plevs:prof.nlevs: ', PROF%nlevs,PROF%plevs(1)
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
!        print*,'Prof is in top-down order'
         DO L=1,NLEV
            PLEVS_RTP(L)=PROF%plevs(L)
         ENDDO
      ELSE
!       print*,'Prof is in bottom-up order'
         DO L=1,NLEV
            LR=1 + NLEV - L  ! reversed layer index
            PLEVS_RTP(L)=PROF%plevs(LR)
         ENDDO
      ENDIF
      print*,'check_plevs: NLEV,COMLEV(30),PLEVS_RTP(101-30+1) ', &
           NLEV, PLEV(30),PLEVS_RTP(101-30+1) 
! ---------------------------------------
! COmpare with reference profile pressures
! ----------------------------------------
       RJUNK1 = 0.0
       DO I=1,NLEV
!         write(*,'(I3,5(F12.5))') I, PLEV(I), PLEVS_RTP(I), PLEV(I) - PLEVS_RTP(I), &
!                   PLEV(I)-PLEV(I+1),PLEV_RTP(I)-PLEV_RTP(I+1)
         RJUNK1 = RJUNK1 + abs(PLEV(101-I+1) - PLEVS_RTP(I))
       ENDDO
       RJUNK1 = RJUNK1/(NLEV+1)
       IF (RJUNK1 .GT. 1.0) THEN
         print *,'oh no : difference between plevs in rtp and plevs in cbplev = ',RJUNK1
         STOP
       END IF

!
 9999  RETURN
       END
