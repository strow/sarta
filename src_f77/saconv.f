C=======================================================================
C=======================================================================
C
C    University of Maryland Baltimore Country (UMBC)
C
C    AIRS
C
C    SACONV
C
!F77====================================================================


!ROUTINE NAME:
C    SACONV (real function)


!ABSTRACT:
C    Function to convert the surface solar zenith angle SZA into the
C    local solar angle at altitude ALT.


!CALL PROTOCOL
C    SACONV( SZA, ALT )


!INPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    REAL      ALT     Average layer altitude      meters
C    REAL      SZA     Solar Zenith Angle          degrees


!OUTPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    REAL fun  SACONV  local solar zenith angle    radians


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
C    Function to convert the Solar Zenith Angle SZA at the Earth's
C    surface into a the local solar angle at altitude ALT.
C    The local solar angle generally varies slightly with altitude
C    due to the curvature of the Earth and its atmosphere.
C    The effect is largest at the maximum solar zenith angle, and
C    disappears as the solar zenith angle approaches 0 degrees.
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
C    The local solar angle may be calculated (using The Law of
C    Sines) if we know:
C       The solar zenith angle, SZA, at the Earth's surface (ALT=0)
C       The layer altitude, ALT.
C       The radius of the Earth, RE.
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
C    25 Feb 1998 Scott Hannon   Created


!END====================================================================


C      =================================================================
       REAL FUNCTION SACONV( SZA, ALT )
C      =================================================================


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
       REAL    SZA
       REAL    ALT


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       REAL   CONV
       REAL     RE
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
C      Note: need to convert altitude in meters to kilometers
       RA=RE + (ALT/1000.0)
C
C      -----------------
C      Do the conversion
C      -----------------
C
       SACONV=ASIN( (RE/RA) * SIN(CONV*SZA) )
C
       RETURN
       END
