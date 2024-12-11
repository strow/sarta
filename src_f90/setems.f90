!=======================================================================
!
!    University of Maryland Baltimore Country (UMBC)
!
!    AIRS
!
!    SETEMS
!
!F90====================================================================


!ROUTINE NAME:
!    SETEMS

!ABSTRACT:
!    Assign the emissivity and reflectivity data for every channel
!    being used.


!CALL PROTOCOL
!    RDEMIS ( NCHAN, NEMIS, FREQ, FEMIS, XEMIS, XRHO,
!       LRHOT, EMIS, RHOSUN, RHOTHR )


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    INTEGER   NCHAN   number of channels          none
!    INTEGER   NEMIS   number of emis pts          none
!    REAL arr  FREQ    channel frequencies         cm^-1
!    REAL arr  FEMIS   raw emis freq points        cm^-1
!    REAL arr  XEMIS   raw emis points             none (0 to 1)
!    REAL arr  XRHO    raw reflec points           1/steradian
!    LOGICAL   LRHOT   force refl therm rho?       none

!OUTPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    REAL arr  EMIS    surface emissivity          none
!    REAL arr  RHOSUN  reflectivity for solar      1/steradian
!    REAL arr  RHOTHR  reflectivity for thermal    1/steradian


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
!    Code by L.Strow/S.Hannon.
!
!    Loops over the passed in EMIS & RHO points and interpolates
!    or extrapolates them onto the channel freqs being used.
!    Interpolations are linear in wavelength (not frequency).


!ALGORITHM REFERENCES:
!    none


!KNOWN BUGS AND LIMITATIONS:
!    none


!ROUTINE HISTORY:
! Date        Programmer     Comments
! ----------- -------------- -------------------------------------------
! 14 Feb 2001 Scott Hannon   Created based on rdemis.f
! 06 Feb 2004 Scott Hannon   Add LRHOT argument and associated code
! 24 Oct 2008 Scott Hannon   Update for rtpV201: input emis & rho is
!                               now specified with same freq points


!END====================================================================

!      =================================================================
       SUBROUTINE SETEMS ( NCHAN, NEMIS, FREQ, FEMIS, XEMIS, &
         XRHO, LRHOT, EMIS, RHOSUN, RHOTHR )
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
integer ::  NCHAN, NEMIS
real(4), dimension(MXCHAN) :: FREQ
real(4), dimension(MXEMIS) :: FEMIS, XEMIS, XRHO
logical :: LRHOT
!      Output
real(4), dimension(MXCHAN) :: EMIS, RHOSUN, RHOTHR

!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
integer :: I, J               ! generic looping variable
integer :: SORTED             ! conrol flag for sorts
!  DELELM                     ! delta for emis wavelength
!  DELEMS                     ! delta for emis
!  DELRHO                     ! delta for rho
!  ELAM                       ! emis wavelength
real(4), dimension(MXEMIS) :: DELELM, DELEMS, DELRHO, ELAM
real(4) ::  DX, RJUNK1, RJUNK2, RJUNK3  ! deltaX, generic junk variable


!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none


!***********************************************************************
!                    EXECUTABLE CODE
!***********************************************************************
!
!      ---------------------------
!      Sort the emis & rho by freq
!      ---------------------------
       SORTED=1
 10    IF (SORTED .EQ. 1) THEN
          SORTED=0
          DO I=1,NEMIS-1
             IF (FEMIS(I) .GT. FEMIS(I+1)) THEN
                RJUNK1=FEMIS(I)
                RJUNK2=XEMIS(I)
                RJUNK3=XRHO(I)
                FEMIS(I)=FEMIS(I+1)
                XEMIS(I)=XEMIS(I+1)
                XRHO(I)=XRHO(I+1)
                FEMIS(I+1)=RJUNK1
                XEMIS(I+1)=RJUNK2
                XRHO(I+1)=RJUNK3
                SORTED=1
             ENDIF
          ENDDO
          GOTO 10
       ENDIF
!
!      ----------------------------------------
!      Calc wavelength and interpolation deltas
!      ----------------------------------------
       ELAM(1)=1.0E+4/FEMIS(1) ! wavelength in microns
       DO J=2,NEMIS
          ELAM(J)=1.0E+4/FEMIS(J)
          DELELM(J-1)=ELAM(J) - ELAM(J-1)
          DELEMS(J-1)=XEMIS(J) - XEMIS(J-1)
          DELRHO(J-1)=XRHO(J) - XRHO(J-1)
       ENDDO
!
!      ------------------------------------
!      Interpolate emis & rho onto channels
!      ------------------------------------
!      Loop over channels (note: chan freqs may be in any order)
       DO I=1,NCHAN
!
!         Emissivity
          IF ( FREQ(I) .LE. FEMIS(1) ) THEN
             EMIS(I)=XEMIS(1)
             RHOSUN(I)=XRHO(1)
          ELSEIF ( FREQ(I) .GE. FEMIS(NEMIS) ) THEN
             EMIS(I)=XEMIS(NEMIS)
             RHOSUN(I)=XRHO(NEMIS)
          ELSE
!            Determine the index of the upper bounding FEMIS
             J=2
 20          IF ( FEMIS(J) .LT. FREQ(I) ) THEN
                J=J + 1
                GOTO 20
             ENDIF
!            Convert to lower boundary index
             J=J - 1
!
             DX=(( 1.0E+4/FREQ(I) - ELAM(J) )/DELELM(J))
             EMIS(I) = XEMIS(J) + DX*DELEMS(J)
             RHOSUN(I) = XRHO(J) + DX*DELRHO(J)
          ENDIF
!
          IF (LRHOT) THEN
!            Force reflected thermal rho = (1-emis)/pi
             RHOTHR(I)=(1.0 - EMIS(I))/PI
          ELSE
!            Reflected thermal uses the same rho as solar
             RHOTHR(I)=RHOSUN(I)
          ENDIF
!
       ENDDO

       RETURN
       END
