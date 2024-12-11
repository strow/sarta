!=======================================================================
 
! Code converted using TO_F90_LOOP by Alan Miller
! Date: 2023-04-04  Time: 16:44:55
 
!=======================================================================

!    University of Maryland Baltimore County [UMBC]

!    AIRS

!    CCPREP

!F77====================================================================


!ROUTINE NAME:
!    CCPREP


!ABSTRACT:
!    Prepapre lookup table etc for a complex cloud calculation.


!CALL PROTOCOL:
!    CCPREP( NCHAN, LBOT, INDMIE, MIENPS,
!       CNGWAT, CPSIZE, CPRTOP, CPRBOT, PLEV, TEMP, SECANG, SECSUN,
!       MIEPS, MIEABS, MIEEXT, MIEASY, LCBOT, LCTOP, CLEARB, CLEART,
!       TCBOT, TCTOP, MASEC, MSSEC, CFRCL, G_ASYM, NEXTOD, NSCAOD )


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    INTEGER   NCHAN   number of channels          none
!    INTEGER   LBOT    bottom layer                none
!    INTEGER   INDMIE  index into MIE arrays       none
!    INT arr   MIENPS  # of particle sizes         none
!    REAL      CNGWAT  cloud non-gas water         g/m^2
!    REAL      CPSIZE  cloud particle size         um
!    REAL      CPRTOP  cloud top pressure          mb
!    REAL      CPRBOT  cloud bottom pressure       mb
!    REAL arr  PLEV    layer pres boundary levels  mb
!    REAL arr  TEMP    average layer temperature   K
!    REAL arr  SECANG  path secant angles          none
!    REAL arr  SECSUN  sun path secant angles      none
!    REAL arr  MIEPS   Mie table particle sizes    um
!    REAL arr  MIEABS  Mie table absorption data   m^2/g
!    REAL arr  MIEEXT  Mie table extinction data   ?
!    REAL arr  MIEASY  Mie table asymmetry data    ?


!OUTPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    INTEGER  LCBOT    layer containing cloud bottom
!    INTEGER  LCTOP    layer containing cloud top
!    REAL     CLEARB   frac of layer at bottom of cloud clear
!    REAL     CLEART   frac of layer at top of cloud clear
!    REAL     TCBOT    temperature at cloud bottom
!    REAL     TCTOP    temperature at cloud top
!    REAL     MASEC    mean cloud view angle secant
!    REAL     MSSEC    mean cloud sun-only angle secant
!    REAL arr CFRCL    fraction of cloud in layer
!    REAL arr G_ASYM   "g" asymmetry
!    REAL arr NEXTOD   nadir extinction optical depth
!    REAL arr NSCAOD   nadir scattering optical depth


!INPUT/OUTPUT PARAMETERS:
!    none


!RETURN VALUES:
!    none


!PARENT(S):
!    SARTA


!ROUTINES CALLED:
!    none


!FILES ACCESSED:
!    incFTC.f : include file of parameter statements accessed during
!       compilation only.


!COMMON BLOCKS
!    none


!DESCRIPTION:
!    Calculates the transmission thru a cloud



!ALGORITHM REFERENCES:
!    none


!KNOWN BUGS AND LIMITATIONS:
!    none


!ROUTINE HISTORY:
!    Date        Programmer     Comments
!    ----------- -------------- ----------------------------------------
!    23 Jan 2004 Scott Hannon   Created from a re-write of calcc1 to
!                                  output results and not call calcc2.
!    31 Mar 2006 Scott Hannon   Revised for flexible CTYPE; add INDMIE
!                               and MIENPS.
!    26 Apr 2006 Scott Hannon   Add LBLACK argument and "if" block.
!    14 Nov 2007 Scott Hannon   Remove LBLACK

!END====================================================================

!      =================================================================

SUBROUTINE CCPREP( NCHAN, LBOT, INDMIE, MIENPS,  &
    CNGWAT, CPSIZE, CPRTOP, CPRBOT, PLEV, TEMP, SECANG, SECSUN,  &
    MIEPS, MIEABS, MIEEXT, MIEASY,  &
    LCBOT, LCTOP, CLEARB, CLEART, TCBOT, TCTOP, MASEC, MSSEC,  &
    CFRCL, G_ASYM, NEXTOD, NSCAOD )
!      =================================================================

!-----------------------------------------------------------------------
!      IMPLICIT NONE
!-----------------------------------------------------------------------

INTEGER, INTENT(IN)                      :: NCHAN
INTEGER, INTENT(IN)                      :: LBOT
INTEGER, INTENT(IN OUT)                  :: INDMIE
INTEGER, INTENT(IN)                      :: MIENPS(NMIETY)
REAL, INTENT(IN)                         :: CNGWAT
REAL, INTENT(IN OUT)                     :: CPSIZE
REAL, INTENT(IN)                         :: CPRTOP
REAL, INTENT(IN)                         :: CPRBOT
REAL, INTENT(IN)                         :: PLEV(MAXLAY+1)
REAL, INTENT(IN)                         :: TEMP(MAXLAY)
REAL, INTENT(IN)                         :: SECANG(MAXLAY)
REAL, INTENT(IN)                         :: SECSUN(MAXLAY)
REAL, INTENT(IN)                         :: MIEPS(MXMIEA,NMIETY)
REAL, INTENT(IN)                         :: MIEABS(MXCHAN,MXMIEA,NMIETY)
REAL, INTENT(IN)                         :: MIEEXT(MXCHAN,MXMIEA,NMIETY)
REAL, INTENT(IN)                         :: MIEASY(MXCHAN,MXMIEA,NMIETY)
INTEGER, INTENT(OUT)                     :: LCBOT
INTEGER, INTENT(OUT)                     :: LCTOP
REAL, INTENT(OUT)                        :: CLEARB
REAL, INTENT(OUT)                        :: CLEART
REAL, INTENT(OUT)                        :: TCBOT
REAL, INTENT(OUT)                        :: TCTOP
REAL, INTENT(OUT)                        :: MASEC
REAL, INTENT(OUT)                        :: MSSEC
REAL, INTENT(OUT)                        :: CFRCL(MAXLAY)
REAL, INTENT(OUT)                        :: G_ASYM(MXCHAN)
REAL, INTENT(OUT)                        :: NEXTOD(MXCHAN)
REAL, INTENT(OUT)                        :: NSCAOD(MXCHAN)
IMPLICIT NONE


!-----------------------------------------------------------------------
!      INCLUDE FILES
!-----------------------------------------------------------------------
INCLUDE 'incFTC.f'


!-----------------------------------------------------------------------
!      EXTERNAL FUNCTIONS
!-----------------------------------------------------------------------
!      QIKEXP


!-----------------------------------------------------------------------
!      ARGUMENTS
!-----------------------------------------------------------------------
!      Input



INTEGER :: ! # of particle sizes




REAL :: ! pressure levels
REAL :: ! temperature
REAL :: ! secant of view path
REAL :: ! secant of total sun path
REAL :: ! particle size
REAL :: ! scattering absorption
REAL :: ! scattering extinction
REAL :: ! scattering asymmetry

!      Output








REAL :: ! fraction of cloud in layer
REAL :: ! "g" asymmetry
REAL :: ! nadir extinction optical depth
REAL :: ! nadir scattering optical depth


!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
INTEGER :: I         ! looping variable for channel
INTEGER :: IHI         ! high index
INTEGER :: ILO         ! low index
INTEGER :: L         ! looping variable for layer
INTEGER :: LR         ! reversed layer index
INTEGER :: NPS         ! # of particle sizes for this CTYPE
REAL :: ABSOD            ! interpolated absorption optical depth
REAL :: PAVG            ! layer average pressure
REAL :: PAVG2            ! adjacent layer average pressure
REAL :: X            ! generic junk real variable


!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none


!***********************************************************************
!***********************************************************************
!                    EXECUTABLE CODE
!***********************************************************************
!***********************************************************************

!      --------------------------------
!      Find top and bottom cloud layers
!      --------------------------------
DO L=1,LBOT
  CFRCL(L)=0.0 ! initialize to zero
!          LR = MAXLAY + 1 - L ! replaced by line below 21 July 2003
  LR = LBOT + 1 - L
  IF (PLEV(L) <= CPRTOP) LCTOP=L
  IF (PLEV(LR+1) >= CPRBOT) LCBOT=LR
  ENDDO
    
!      Calc fraction of layer at top & bottom of cloud that is
!      clear (of this cloud; there may be another cloud there).
    CLEART=(CPRTOP   - PLEV(LCTOP))/(PLEV(LCTOP+1) - PLEV(LCTOP))
    CLEARB=(PLEV(LCBOT+1) - CPRBOT)/(PLEV(LCBOT+1) - PLEV(LCBOT))
    
    
!      --------------------------
!      Calc cloud top temperature
!      --------------------------
    L=LCTOP
    PAVG=(PLEV(L+1) - PLEV(L))/LOG( PLEV(L+1)/PLEV(L) )
    IF (PAVG > CPRTOP .OR. L == LBOT) THEN
      PAVG2=(PLEV(L) - PLEV(L-1))/LOG( PLEV(L)/PLEV(L-1) )
      TCTOP=TEMP(L) + LOG(CPRTOP/PAVG)*  &
          (TEMP(L-1) - TEMP(L))/LOG( PAVG2/PAVG )
    ELSE
      PAVG2=(PLEV(L+2) - PLEV(L+1))/LOG( PLEV(L+2)/PLEV(L+1) )
      TCTOP=TEMP(L) + LOG(CPRTOP/PAVG)*  &
          (TEMP(L+1) - TEMP(L))/LOG( PAVG2/PAVG )
    END IF
    
!cccccccc
!      print *, 'top PLEV(L-1)=', PLEV(L-1)
!      print *, 'top PLEV(L  )=', PLEV(L)
!      print *, 'top PLEV(L+1)=', PLEV(L+1)
!      print *, 'top PLEV(L+2)=', PLEV(L+2)
!      print *, 'top pavg=', PAVG
!      print *, 'top pavg2=', PAVG2
!      print *, 'top TEMP(L-1)=',TEMP(L-1)
!      print *, 'top TEMP(L  )=',TEMP(L)
!      print *, 'top TEMP(L+1)=',TEMP(L+1)
!cccccccc
    
!      -----------------------------
!      Calc cloud bottom temperature
!      -----------------------------
    L=LCBOT
    PAVG=(PLEV(L+1) - PLEV(L))/LOG( PLEV(L+1)/PLEV(L) )
    IF (PAVG > CPRBOT .OR. L == LBOT) THEN
      PAVG2=(PLEV(L) - PLEV(L-1))/LOG( PLEV(L)/PLEV(L-1) )
      TCBOT=TEMP(L) + LOG(CPRBOT/PAVG)*  &
          (TEMP(L-1) - TEMP(L))/LOG( PAVG2/PAVG )
    ELSE
      PAVG2=(PLEV(L+2) - PLEV(L+1))/LOG( PLEV(L+2)/PLEV(L+1) )
      TCBOT=TEMP(L) + LOG(CPRBOT/PAVG)*  &
          (TEMP(L+1) - TEMP(L))/LOG( PAVG2/PAVG )
    END IF
    
!cccccccc
!      print *, 'bot PLEV(L-1)=', PLEV(L-1)
!      print *, 'bot PLEV(L  )=', PLEV(L)
!      print *, 'bot PLEV(L+1)=', PLEV(L+1)
!      print *, 'bot PLEV(L+2)=', PLEV(L+2)
!      print *, 'bot pavg=', PAVG
!      print *, 'bot pavg2=', PAVG2
!      print *, 'bot TEMP(L-1)=',TEMP(L-1)
!      print *, 'bot TEMP(L  )=',TEMP(L)
!      print *, 'bot TEMP(L+1)=',TEMP(L+1)
!cccccccc
    
!cccccccc
!      print *, 'lctop, plev(lctop) = ', LCTOP, PLEV(LCTOP)
!      print *, 'lcbot, plev(lcbot+1) = ', LCBOT, PLEV(LCBOT+1)
!      print *, 'tctop=', TCTOP
!      print *, 'tcbot=', TCBOT
!      print *, 'cleart=', CLEART
!      print *, 'clearb=', CLEARB
!cccccccc
    
!      -----------------------------------------------------------------
!      Calc mean secant angles thru cloud and fraction of cloud in layer
!      -----------------------------------------------------------------
    IF (LCTOP == LCBOT) THEN
      MASEC=SECANG(LCTOP)
      MSSEC=SECSUN(LCTOP)
      CFRCL(LCTOP)=1.0
    ELSE
!         top & bottom layers
      MASEC=SECANG(LCTOP)*(1-CLEART) + SECANG(LCBOT)*(1-CLEARB)
      MSSEC=SECSUN(LCTOP)*(1-CLEART) + SECSUN(LCBOT)*(1-CLEARB)
      X=(1-CLEART) + (1-CLEARB)
      CFRCL(LCTOP)=(PLEV(LCTOP+1)-CPRTOP)/(CPRBOT-CPRTOP)
      CFRCL(LCBOT)=(CPRBOT-PLEV(LCBOT))/(CPRBOT-CPRTOP)
!         other layers
      DO L=LCTOP+1,LCBOT-1
        MASEC=MASEC + SECANG(L)
        MSSEC=MSSEC + SECSUN(L)
        X=X + 1
        CFRCL(L)=(PLEV(L+1)-PLEV(L))/(CPRBOT-CPRTOP)
        ENDDO
!         Divide secant sum by weight sum to get mean secant
          MASEC=MASEC/X
          MSSEC=MSSEC/X
        END IF
        MSSEC=MSSEC - MASEC ! convert total secant to sun-only secant
        
        
!      --------------------------------------------------
!      Interpolate tables for particle size and scale for
!      nadir total cloud water
!      -----------------------
!      Note: extinction = scattering + absorption, so sca=ext - abs
        
!      Number of particle sizes for current CTYPE
        NPS=MIENPS(INDMIE)
        
!      Minimum particle size
        IF (CPSIZE <= MIEPS(1,INDMIE)) THEN
          DO I=1,NCHAN
            NEXTOD(I)=CNGWAT*MIEEXT(I,1,INDMIE)
            NSCAOD(I)=CNGWAT*(MIEEXT(I,1,INDMIE) - MIEABS(I,1,INDMIE))
            G_ASYM(I)=MIEASY(I,1,INDMIE)
            ENDDO
              
!      Maximum particle size
            ELSE IF (CPSIZE >= MIEPS(NPS,INDMIE)) THEN
              DO I=1,NCHAN
                NEXTOD(I)=CNGWAT*MIEEXT(I,NPS,INDMIE)
                NSCAOD(I)=CNGWAT*(MIEEXT(I,NPS,INDMIE) - MIEABS(I,NPS,INDMIE))
                G_ASYM(I)=MIEASY(I,NPS,INDMIE)
                ENDDO
                  
!      Intermediate particle size
                ELSE
                  IHI=1
                  10       IF (MIEPS(IHI,INDMIE) < CPSIZE .AND. IHI < NPS) THEN
                    IHI=IHI + 1
                    GO TO 10
                  END IF
                  ILO=IHI - 1
                  
                  X=( LOG(CPSIZE) - LOG(MIEPS(ILO,INDMIE)) ) /  &
                      ( LOG(MIEPS(IHI,INDMIE)) - LOG(MIEPS(ILO,INDMIE)) )
                  
                  DO I=1,NCHAN
                    NEXTOD(I)=CNGWAT*( MIEEXT(I,ILO,INDMIE) +  &
                        X*(MIEEXT(I,IHI,INDMIE) - MIEEXT(I,ILO,INDMIE)) )
                    ABSOD    =CNGWAT*( MIEABS(I,ILO,INDMIE) +  &
                        X*(MIEABS(I,IHI,INDMIE) - MIEABS(I,ILO,INDMIE)) )
                    NSCAOD(I)=NEXTOD(I) - ABSOD
                    G_ASYM(I)=MIEASY(I,ILO,INDMIE) +  &
                        X*(MIEASY(I,IHI,INDMIE) - MIEASY(I,ILO,INDMIE))
                    ENDDO
                    END IF
                    
                    
                    RETURN
                  END SUBROUTINE CCPREP
