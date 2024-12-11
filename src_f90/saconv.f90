!=======================================================================
!
!    University of Maryland Baltimore Country (UMBC)
!
!    AIRS
!
!    SACONV
!
!F90====================================================================


!ROUTINE NAME:
!    SACONV (real function)


!ABSTRACT:
!    Function to convert the surface solar zenith angle SZA into the
!    local solar angle at altitude ALT.


!CALL PROTOCOL
!    SACONV( SZA, ALT )


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    REAL      ALT     Average layer altitude      meters
!    REAL      SZA     Solar Zenith Angle          degrees


!OUTPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    REAL fun  SACONV  local solar zenith angle    radians


!INPUT/OUTPUT PARAMETERS:
!    none


!RETURN VALUES:
!    none


!PARENT(S):
!    USEFAST


!ROUTINES CALLED:
!    none


!FILES ACCESSED:
!    none


!COMMON BLOCKS
!    none


!DESCRIPTION:
!    March 1998 version of the 100 layer AIRS Fast Transmittance
!    Code by L.Strow/S.Hannon.
!
!    ===================================================================
!    Function to convert the Solar Zenith Angle SZA at the Earth's
!    surface into a the local solar angle at altitude ALT.
!    The local solar angle generally varies slightly with altitude
!    due to the curvature of the Earth and its atmosphere.
!    The effect is largest at the maximum solar zenith angle, and
!    disappears as the solar zenith angle approaches 0 degrees.
!
!    Currently this function only considers the geometry of the
!    situation, and no refractive effects are included.
!
!    The layers of the atmosphere may be considered as concentric
!    rings with some average altitude. A ray traced thru these rings
!    at any viewing angle other than nadir will have a slightly
!    different angle (relative to the outward radial at the point
!    of intersection) in each ring. 
!
!    The local solar angle may be calculated (using The Law of
!    Sines) if we know:
!       The solar zenith angle, SZA, at the Earth's surface (ALT=0)
!       The layer altitude, ALT.
!       The radius of the Earth, RE.
!
!    The solution uses the law of sines and sin(180 - x) = sin(x)
!    ===================================================================


!ALGORITHM REFERENCES:
!    none


!KNOWN BUGS AND LIMITATIONS:
!    No refractive effects have been included.


!ROUTINE HISTORY:
!    Date        Programmer     Comments
!    ----------- -------------- ----------------------------------------
!    25 Feb 1998 Scott Hannon   Created


!END====================================================================


!      =================================================================
       REAL FUNCTION SACONV( SZA, ALT )
!      =================================================================


!-----------------------------------------------------------------------
!      IMPLICIT NONE
!-----------------------------------------------------------------------
       IMPLICIT NONE


!-----------------------------------------------------------------------
!      INCLUDE FILES
!-----------------------------------------------------------------------
!      none


!-----------------------------------------------------------------------
!      EXTERNAL FUNCTIONS
!-----------------------------------------------------------------------
!      none


!-----------------------------------------------------------------------
!      ARGUMENTS
!-----------------------------------------------------------------------
       REAL    SZA
       REAL    ALT


!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
       REAL   CONV
       REAL     RE
       REAL     RA


!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none


!***********************************************************************
!***********************************************************************
!                    EXECUTABLE CODE
!***********************************************************************
!***********************************************************************
!
!      ------------------
!      Assign some values
!      ------------------
!      CONV = pi/180 = degrees to radians conversion factor
       CONV=1.7453292E-02
!
!      RE = radius of the Earth (in km)
       RE=6.37E+03
!
!      RA = radius of the point to calc the angle at (in km)
!      Note: need to convert altitude in meters to kilometers
       RA=RE + (ALT/1000.0)
!
!      -----------------
!      Do the conversion
!      -----------------
!
       SACONV=ASIN( (RE/RA) * SIN(CONV*SZA) )
!
       RETURN
       END
