C=======================================================================
C=======================================================================
C
C    University of Maryland Baltimore Country (UMBC)
C
C    AIRS
C
C    VACONV
C
!F77====================================================================


!ROUTINE NAME:
C    VACONV (real function)


!ABSTRACT:
C    Function to convert the AIRS satellite viewing angle into the
C    local path angle.


!CALL PROTOCOL
C    VACONV( SVA, SALT, ALT )


!INPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    REAL      SVA     Satellite viewing angle     degrees
C    REAL      SALT    Satellite altitude          kilometers
C    REAL      ALT     Average layer altitude      meters


!OUTPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    REAL fun  VACONV  local path angle            radians


!INPUT/OUTPUT PARAMETERS:
C    none


!RETURN VALUES:
C    none


!PARENT(S):
C    USEFAST


!ROUTINES CALLED:
C    none


!FILES ACCESSED:
C    none


!COMMON BLOCKS
C    none


!DESCRIPTION:
C    March 1998 version of the 100 layer AIRS Fast Transmittance
C    Code by L.Strow/S.Hannon.
C
C    ===================================================================
C    Function to convert the AIRS satellite viewing angle into a local
C    path angle.  The local path angle generally varies slightly with
C    altitude due to the curvature of the Earth and its atmosphere.
C    The effect is largest at the maximum satellite viewing angle,
C    and goes to zero as the viewing angle approaches 0 degrees.
C
C    For AIRS, the maximum difference in the local path angle secant
C    between the bottom and top layers is around 3 percent.
C
C    Currently this function only considers the geometry of the
C    situation, and no refractive effects are included.
C
C    The layers of the atmosphere may be considered as concentric
C    rings with some average altitude. A ray traced thru these rings
C    at any viewing angle other than nadir will have a slightly
C    different angle (relative to the outward radial at the point
C    of intersection) in each ring. 
C
C    If the Earth is treated as a perfect sphere of radius RE (hard
C    coded into this routine), then the local angle may be calculated
C    using trigonometry if we know:
C       The satellite viewing angle
C       The satellite's altitude above the Earth's surface
C       The layer's altitude above the Earth's surface.
C
C    The solution uses the law of sines and sin(180 - x) = sin(x)
C    ===================================================================


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    No refractive effects have been included.


!ROUTINE HISTORY:
C    Date        Programmer     Comments
C    ----------- -------------- ----------------------------------------
C    10 Apr 1995 Scott Hannon   Created
C     1 Apr 1997 Scott Hannon   Fix error in Earth radius (was 3.67E+3)
C    27 Feb 1998 Scott Hannon   Simplified; made SALT an input var


!END====================================================================

C      =================================================================
       REAL FUNCTION VACONV( SVA, SALT, ALT )
C      =================================================================
C
C      Viewing Angle CONVersion
C
C-----------------------------------------------------------------------
C      IMPLICIT NONE
C-----------------------------------------------------------------------
       IMPLICIT NONE


C-----------------------------------------------------------------------
C      INCLUDE FILES
C-----------------------------------------------------------------------
C      none


C-----------------------------------------------------------------------
C      EXTERNAL FUNCTIONS
C-----------------------------------------------------------------------
C      none


C-----------------------------------------------------------------------
C      ARGUMENTS
C-----------------------------------------------------------------------
       REAL    SVA
       REAL   SALT
       REAL    ALT


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       REAL   CONV
       REAL     RE
       REAL     RS
       REAL     RA


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
C      ------------------
C      Assign some values
C      ------------------
C      CONV = pi/180 = degrees to radians conversion factor
       CONV=1.7453292E-02
C
C      RE = radius of the Earth (in km)
       RE=6.37E+03
C
C      RA = radius of the point to calc the angle at (in km)
       RA=RE + (ALT/1000.0)
C
C      RS = radius of the satellite orbit (in km)
       RS=RE + SALT
C
C      -----------------
C      Do the conversion
C      -----------------
C
       VACONV=ASIN( (RS/RA) * SIN(CONV*SVA) )
C
       RETURN
       END
