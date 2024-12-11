!=======================================================================
!
!    University of Maryland Baltimore Country (UMBC)
!
!    AIRS
!
!    VACONV
!
!F90====================================================================


!ROUTINE NAME:
!    VACONV (real function)


!ABSTRACT:
!    Function to convert the AIRS satellite viewing angle into the
!    local path angle.


!CALL PROTOCOL
!    VACONV( SVA, SALT, ALT )


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    REAL      SVA     Satellite viewing angle     degrees
!    REAL      SALT    Satellite altitude          kilometers
!    REAL      ALT     Average layer altitude      meters


!OUTPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    REAL fun  VACONV  local path angle            radians


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
!    Function to convert the AIRS satellite viewing angle into a local
!    path angle.  The local path angle generally varies slightly with
!    altitude due to the curvature of the Earth and its atmosphere.
!    The effect is largest at the maximum satellite viewing angle,
!    and goes to zero as the viewing angle approaches 0 degrees.
!
!    For AIRS, the maximum difference in the local path angle secant
!    between the bottom and top layers is around 3 percent.
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
!    If the Earth is treated as a perfect sphere of radius RE (hard
!    coded into this routine), then the local angle may be calculated
!    using trigonometry if we know:
!       The satellite viewing angle
!       The satellite's altitude above the Earth's surface
!       The layer's altitude above the Earth's surface.
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
!    10 Apr 1995 Scott Hannon   Created
!     1 Apr 1997 Scott Hannon   Fix error in Earth radius (was 3.67E+3)
!    27 Feb 1998 Scott Hannon   Simplified; made SALT an input var


!END====================================================================

!      =================================================================
       REAL FUNCTION VACONV( SVA, SALT, ALT )
!      =================================================================
!
!      Viewing Angle CONVersion
!
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
       REAL    SVA
       REAL   SALT
       REAL    ALT


!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
       REAL   CONV
       REAL     RE
       REAL     RS
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
       RA=RE + (ALT/1000.0)
!
!      RS = radius of the satellite orbit (in km)
       RS=RE + SALT
!
!      -----------------
!      Do the conversion
!      -----------------
!
       VACONV=ASIN( (RS/RA) * SIN(CONV*SVA) )
!
       RETURN
       END
