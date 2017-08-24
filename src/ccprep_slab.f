C=======================================================================
C=======================================================================
C
C    University of Maryland Baltimore County [UMBC]
C
C    AIRS
C
C    CCPREP
C
!F77====================================================================


!ROUTINE NAME:
C    CCPREP


!ABSTRACT:
C    Prepapre lookup table etc for a complex cloud calculation.


!CALL PROTOCOL:
C    CCPREP( NCHAN, LBOT, INDMIE, MIENPS,
C       CNGWAT, CPSIZE, CPRTOP, CPRBOT, PLEV, TEMP, SECANG, SECSUN,
C       MIEPS, MIEABS, MIEEXT, MIEASY, LCBOT, LCTOP, CLEARB, CLEART,
C       TCBOT, TCTOP, MASEC, MSSEC, CFRCL, G_ASYM, NEXTOD, NSCAOD )


!INPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    INTEGER   NCHAN   number of channels          none
C    INTEGER   LBOT    bottom layer                none
C    INTEGER   INDMIE  index into MIE arrays       none
C    INT arr   MIENPS  # of particle sizes         none
C    REAL      CNGWAT  cloud non-gas water         g/m^2
C    REAL      CPSIZE  cloud particle size         um
C    REAL      CPRTOP  cloud top pressure          mb
C    REAL      CPRBOT  cloud bottom pressure       mb
C    REAL arr  PLEV    layer pres boundary levels  mb
C    REAL arr  TEMP    average layer temperature   K
C    REAL arr  SECANG  path secant angles          none
C    REAL arr  SECSUN  sun path secant angles      none
C    REAL arr  MIEPS   Mie table particle sizes    um
C    REAL arr  MIEABS  Mie table absorption data   m^2/g
C    REAL arr  MIEEXT  Mie table extinction data   ?
C    REAL arr  MIEASY  Mie table asymmetry data    ?


!OUTPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    INTEGER  LCBOT    layer containing cloud bottom
C    INTEGER  LCTOP    layer containing cloud top
C    REAL     CLEARB   frac of layer at bottom of cloud clear
C    REAL     CLEART   frac of layer at top of cloud clear
C    REAL     TCBOT    temperature at cloud bottom
C    REAL     TCTOP    temperature at cloud top
C    REAL     MASEC    mean cloud view angle secant
C    REAL     MSSEC    mean cloud sun-only angle secant
C    REAL arr CFRCL    fraction of cloud in layer
C    REAL arr G_ASYM   "g" asymmetry
C    REAL arr NEXTOD   nadir extinction optical depth
C    REAL arr NSCAOD   nadir scattering optical depth


!INPUT/OUTPUT PARAMETERS:
C    none


!RETURN VALUES:
C    none


!PARENT(S):
C    SARTA


!ROUTINES CALLED:
C    none


!FILES ACCESSED:
C    incFTC.f : include file of parameter statements accessed during
C       compilation only.


!COMMON BLOCKS
C    none


!DESCRIPTION:
C    Calculates the transmission thru a cloud



!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date        Programmer     Comments
C    ----------- -------------- ----------------------------------------
C    23 Jan 2004 Scott Hannon   Created from a re-write of calcc1 to
C                                  output results and not call calcc2.
C    31 Mar 2006 Scott Hannon   Revised for flexible CTYPE; add INDMIE
C                               and MIENPS.
C    26 Apr 2006 Scott Hannon   Add LBLACK argument and "if" block.
C    14 Nov 2007 Scott Hannon   Remove LBLACK

!END====================================================================

C      =================================================================
       SUBROUTINE CCPREP( NCHAN, LBOT, INDMIE, MIENPS,
     $    CNGWAT, CPSIZE, CPRTOP, CPRBOT, PLEV, TEMP, SECANG, SECSUN,
     $    MIEPS, MIEABS, MIEEXT, MIEASY,
     $    LCBOT, LCTOP, CLEARB, CLEART, TCBOT, TCTOP, MASEC, MSSEC,
     $    CFRCL, G_ASYM, NEXTOD, NSCAOD )
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
C      QIKEXP


C-----------------------------------------------------------------------
C      ARGUMENTS
C-----------------------------------------------------------------------
C      Input
       INTEGER  NCHAN              ! number of channels
       INTEGER   LBOT              ! bottom layer
       INTEGER INDMIE              ! index of CTYPE in MIE arrays
       INTEGER MIENPS(NMIETY)      ! # of particle sizes
       REAL CNGWAT                 ! cloud non-gas water
       REAL CPSIZE                 ! cloud particle size
       REAL CPRTOP                 ! cloud top pressure
       REAL CPRBOT                 ! cloud bottom pressure
       REAL   PLEV(MAXLAY+1)       ! pressure levels
       REAL   TEMP(MAXLAY)         ! temperature
       REAL SECANG(MAXLAY)         ! secant of view path
       REAL SECSUN(MAXLAY)         ! secant of total sun path
       REAL  MIEPS(MXMIEA,NMIETY)  ! particle size
       REAL MIEABS(MXCHAN,MXMIEA,NMIETY) ! scattering absorption
       REAL MIEEXT(MXCHAN,MXMIEA,NMIETY) ! scattering extinction
       REAL MIEASY(MXCHAN,MXMIEA,NMIETY) ! scattering asymmetry

C      Output
       INTEGER  LCBOT         ! layer containing cloud bottom
       INTEGER  LCTOP         ! layer containing cloud top
       REAL CLEARB            ! frac of layer at bottom of cloud clear
       REAL CLEART            ! frac of layer at top of cloud clear
       REAL  TCBOT            ! temperature at cloud bottom
       REAL  TCTOP            ! temperature at cloud top
       REAL  MASEC            ! mean cloud view angle secant
       REAL  MSSEC            ! mean cloud sun-only angle secant
       REAL  CFRCL(MAXLAY)    ! fraction of cloud in layer
       REAL G_ASYM(MXCHAN)    ! "g" asymmetry
       REAL NEXTOD(MXCHAN)    ! nadir extinction optical depth
       REAL NSCAOD(MXCHAN)    ! nadir scattering optical depth


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER      I         ! looping variable for channel
       INTEGER    IHI         ! high index
       INTEGER    ILO         ! low index
       INTEGER      L         ! looping variable for layer
       INTEGER     LR         ! reversed layer index
       INTEGER    NPS         ! # of particle sizes for this CTYPE
       REAL  ABSOD            ! interpolated absorption optical depth
       REAL   PAVG            ! layer average pressure
       REAL  PAVG2            ! adjacent layer average pressure
       REAL      X            ! generic junk real variable


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
C      --------------------------------
C      Find top and bottom cloud layers
C      --------------------------------
       DO L=1,LBOT
          CFRCL(L)=0.0 ! initialize to zero
C          LR = MAXLAY + 1 - L ! replaced by line below 21 July 2003
          LR = LBOT + 1 - L
          IF (PLEV(L) .LE. CPRTOP) LCTOP=L
          IF (PLEV(LR+1) .GE. CPRBOT) LCBOT=LR
       ENDDO
C
C      Calc fraction of layer at top & bottom of cloud that is
C      clear (of this cloud; there may be another cloud there).
       CLEART=(CPRTOP   - PLEV(LCTOP))/(PLEV(LCTOP+1) - PLEV(LCTOP))
       CLEARB=(PLEV(LCBOT+1) - CPRBOT)/(PLEV(LCBOT+1) - PLEV(LCBOT))
C

C      --------------------------
C      Calc cloud top temperature
C      --------------------------
       L=LCTOP
       PAVG=(PLEV(L+1) - PLEV(L))/LOG( PLEV(L+1)/PLEV(L) )
       IF (PAVG .GT. CPRTOP .OR. L .EQ. LBOT) THEN
          PAVG2=(PLEV(L) - PLEV(L-1))/LOG( PLEV(L)/PLEV(L-1) )
          TCTOP=TEMP(L) + LOG(CPRTOP/PAVG)*
     $       (TEMP(L-1) - TEMP(L))/LOG( PAVG2/PAVG )
       ELSE
          PAVG2=(PLEV(L+2) - PLEV(L+1))/LOG( PLEV(L+2)/PLEV(L+1) )
          TCTOP=TEMP(L) + LOG(CPRTOP/PAVG)*
     $       (TEMP(L+1) - TEMP(L))/LOG( PAVG2/PAVG )
       ENDIF
C
ccccccccc
c      print *, 'top PLEV(L-1)=', PLEV(L-1)
c      print *, 'top PLEV(L  )=', PLEV(L)
c      print *, 'top PLEV(L+1)=', PLEV(L+1)
c      print *, 'top PLEV(L+2)=', PLEV(L+2)
c      print *, 'top pavg=', PAVG
c      print *, 'top pavg2=', PAVG2
c      print *, 'top TEMP(L-1)=',TEMP(L-1)
c      print *, 'top TEMP(L  )=',TEMP(L)
c      print *, 'top TEMP(L+1)=',TEMP(L+1)
ccccccccc
C
C      -----------------------------
C      Calc cloud bottom temperature
C      -----------------------------
       L=LCBOT
       PAVG=(PLEV(L+1) - PLEV(L))/LOG( PLEV(L+1)/PLEV(L) )
       IF (PAVG .GT. CPRBOT .OR. L .EQ. LBOT) THEN
          PAVG2=(PLEV(L) - PLEV(L-1))/LOG( PLEV(L)/PLEV(L-1) )
          TCBOT=TEMP(L) + LOG(CPRBOT/PAVG)*
     $       (TEMP(L-1) - TEMP(L))/LOG( PAVG2/PAVG )
       ELSE
          PAVG2=(PLEV(L+2) - PLEV(L+1))/LOG( PLEV(L+2)/PLEV(L+1) )
          TCBOT=TEMP(L) + LOG(CPRBOT/PAVG)*
     $       (TEMP(L+1) - TEMP(L))/LOG( PAVG2/PAVG )
       ENDIF
C
ccccccccc
c      print *, 'bot PLEV(L-1)=', PLEV(L-1)
c      print *, 'bot PLEV(L  )=', PLEV(L)
c      print *, 'bot PLEV(L+1)=', PLEV(L+1)
c      print *, 'bot PLEV(L+2)=', PLEV(L+2)
c      print *, 'bot pavg=', PAVG
c      print *, 'bot pavg2=', PAVG2
c      print *, 'bot TEMP(L-1)=',TEMP(L-1)
c      print *, 'bot TEMP(L  )=',TEMP(L)
c      print *, 'bot TEMP(L+1)=',TEMP(L+1)
ccccccccc

ccccccccc
c      print *, 'lctop, plev(lctop) = ', LCTOP, PLEV(LCTOP)
c      print *, 'lcbot, plev(lcbot+1) = ', LCBOT, PLEV(LCBOT+1)
c      print *, 'tctop=', TCTOP
c      print *, 'tcbot=', TCBOT
c      print *, 'cleart=', CLEART
c      print *, 'clearb=', CLEARB
ccccccccc

C      -----------------------------------------------------------------
C      Calc mean secant angles thru cloud and fraction of cloud in layer
C      -----------------------------------------------------------------
       IF (LCTOP .EQ. LCBOT) THEN
          MASEC=SECANG(LCTOP)
          MSSEC=SECSUN(LCTOP)
          CFRCL(LCTOP)=1.0
       ELSE
C         top & bottom layers
          MASEC=SECANG(LCTOP)*(1-CLEART) + SECANG(LCBOT)*(1-CLEARB)
          MSSEC=SECSUN(LCTOP)*(1-CLEART) + SECSUN(LCBOT)*(1-CLEARB)
          X=(1-CLEART) + (1-CLEARB)
          CFRCL(LCTOP)=(PLEV(LCTOP+1)-CPRTOP)/(CPRBOT-CPRTOP)
          CFRCL(LCBOT)=(CPRBOT-PLEV(LCBOT))/(CPRBOT-CPRTOP)
C         other layers
          DO L=LCTOP+1,LCBOT-1
             MASEC=MASEC + SECANG(L)
             MSSEC=MSSEC + SECSUN(L)
             X=X + 1
             CFRCL(L)=(PLEV(L+1)-PLEV(L))/(CPRBOT-CPRTOP)
          ENDDO
C         Divide secant sum by weight sum to get mean secant
          MASEC=MASEC/X
          MSSEC=MSSEC/X
       ENDIF
       MSSEC=MSSEC - MASEC ! convert total secant to sun-only secant
C

C      --------------------------------------------------
C      Interpolate tables for particle size and scale for
C      nadir total cloud water
C      -----------------------
C      Note: extinction = scattering + absorption, so sca=ext - abs
C
C      Number of particle sizes for current CTYPE
       NPS=MIENPS(INDMIE)
C
C      Minimum particle size
       IF (CPSIZE .LE. MIEPS(1,INDMIE)) THEN
          DO I=1,NCHAN
             NEXTOD(I)=CNGWAT*MIEEXT(I,1,INDMIE)
             NSCAOD(I)=CNGWAT*(MIEEXT(I,1,INDMIE) - MIEABS(I,1,INDMIE))
             G_ASYM(I)=MIEASY(I,1,INDMIE)
          ENDDO
C
C      Maximum particle size
       ELSEIF (CPSIZE .GE. MIEPS(NPS,INDMIE)) THEN
          DO I=1,NCHAN
             NEXTOD(I)=CNGWAT*MIEEXT(I,NPS,INDMIE)
             NSCAOD(I)=CNGWAT*(MIEEXT(I,NPS,INDMIE) - 
     $                         MIEABS(I,NPS,INDMIE))
             G_ASYM(I)=MIEASY(I,NPS,INDMIE)
          ENDDO
C
C      Intermediate particle size
       ELSE
          IHI=1
 10       IF (MIEPS(IHI,INDMIE) .LT. CPSIZE .AND. IHI .LT. NPS) THEN
             IHI=IHI + 1
             GOTO 10
          ENDIF
          ILO=IHI - 1
C
          X=( LOG(CPSIZE) - LOG(MIEPS(ILO,INDMIE)) ) /
     $      ( LOG(MIEPS(IHI,INDMIE)) - LOG(MIEPS(ILO,INDMIE)) )
C
          DO I=1,NCHAN
             NEXTOD(I)=CNGWAT*( MIEEXT(I,ILO,INDMIE) +
     $          X*(MIEEXT(I,IHI,INDMIE) - MIEEXT(I,ILO,INDMIE)) )
             ABSOD    =CNGWAT*( MIEABS(I,ILO,INDMIE) +
     $          X*(MIEABS(I,IHI,INDMIE) - MIEABS(I,ILO,INDMIE)) )
             NSCAOD(I)=NEXTOD(I) - ABSOD
             G_ASYM(I)=MIEASY(I,ILO,INDMIE) +
     $          X*(MIEASY(I,IHI,INDMIE) - MIEASY(I,ILO,INDMIE))
          ENDDO
       ENDIF
C
C
       RETURN
       END
