!=======================================================================
 
! Code converted using TO_F90_LOOP by Alan Miller
! Date: 2023-04-04  Time: 16:44:55
 
!=======================================================================

!              University of Maryland Baltimore County [UMBC]

!              AIRS

!              GETCLD

!F77====================================================================


!ROUTINE NAME: GETCLD


!ABSTRACT:
!    Get basic cloud info

!CALL PROTOCOL:
!    GETCLD( IPROF, HEAD, PROF,
!    LBLAC1, CTYPE1, CFRAC1, CPSIZ1, CPRTO1, CPRBO1, CNGWA1, ...
!    XCEMI1, XCRHO1, CSTMP1,
!    LBLAC2, CTYPE2, CFRAC2, CPSIZ2, CPRTO2, CPRBO2, CNGWA2,
!    XCEMI2, XCRHO2, CSTMP2, CFRA12, FCLEAR, CFRA1X, CFRA2X )


!INPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    INTEGER   IPROF   profile loop counter        none
!    STRUCT    HEAD    RTP header structure        various
!    STRUCT    PROF    RTP profile structure       various

!OUTPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    LOGICAL   LBLAC1  cloud1 is black?            none
!    INTEGER   CTYPE1  cloud type code number      none
!    REAL      CFRAC1  total cloud1 fraction       none (0.0 to 1.0)
!    REAL      CPSIZ1  particle size               um
!    REAL      CPRTO1  cloud top pressure          mb
!    REAL      CPRBO1  cloud bottom pressure       mb
!    REAL      CNGWA1  layer integrated profile    g/m^2
!    REAL arr  XCEMI1  emissivity cloud1           none (0.0 to 1.0)
!    REAL arr  XCRHO1  reflectivity cloud1         none
!    REAL      CSTMP1  cloud top temperature       Kelvin
!    LOGICAL   LBLAC2  cloud2 is black?            none
!    INTEGER   CTYPE2  cloud type code number      none
!    REAL      CFRAC2  total cloud2 fraction       none (0.0 to 1.0)
!    REAL      CPSIZ2  particle size               um
!    REAL      CPRTO2  cloud top pressure          mb
!    REAL      CPRBO2  cloud bottom pressure       mb
!    REAL      CNGWA2  layer integrated profile    g/m^2
!    REAL arr  XCEMI2  emissivity cloud2           none (0.0 to 1.0)
!    REAL arr  XCRHO2  reflectivity cloud2         none
!    REAL      CSTMP2  cloud top temperature       Kelvin
!    REAL      CFRA12  both clouds fraction        none (0.0 to 1.0)
!    REAL      FCLEAR  clear fraction              none (0.0 to 1.0)
!    REAL      CFRA1X  exclusive cloud1 fraction   none (0.0 to 1.0)
!    REAL      CFRA2X  exclusive cloud2 fraction   none (0.0 to 1.0)


!INPUT/OUTPUT PARAMETERS: none


!RETURN VALUES: none


!PARENT(S): SARTA


!ROUTINES CALLED: none


!FILES ACCESSED:
!    none


!COMMON BLOCKS: none


!DESCRIPTION:
!    Pulls out and checks cloud profile paramters.


!ALGORITHM REFERENCES: see DESCRIPTION


!KNOWN BUGS AND LIMITATIONS:
!    none


!ROUTINE HISTORY:
!    Date     Programmer        Comments
!------------ ----------------- ----------------------------------------
! 22 Feb 2007 Scott Hannon      created
! 10 Sep 2007 Scott Hannon      Mods for 100 layer particle size
! 14 Sep 2007 Scott Hannon      Add CPMIN check
! 15 Nov 2007 Scott Hannon      re-written for slab cloud
! 26 Nov 2008 Scott Hannon      Partial re-write for rtpV201 including
!                               spectral XCEMI1/2 & XCRHO1/2
! 01 Dec 2008 Scott Hannon      Add CSTMP1/2
! 07 May 2009 Scott Hannon      Bug fix: initialize CFRA1X=CFRA2X=0

!END====================================================================

!      =================================================================

SUBROUTINE GETCLD( IPROF, HEAD, PROF,  &
    LBLAC1, CTYPE1, CFRAC1, CPSIZ1, CPRTO1, CPRBO1, CNGWA1,  &
    XCEMI1, XCRHO1, CSTMP1,  &
    LBLAC2, CTYPE2, CFRAC2, CPSIZ2, CPRTO2, CPRBO2, CNGWA2,  &
    XCEMI2, XCRHO2, CSTMP2, CFRA12, FCLEAR, CFRA1X, CFRA2X )
!      =================================================================


!-----------------------------------------------------------------------
!      IMPLICIT NONE
!-----------------------------------------------------------------------

INTEGER, INTENT(IN OUT)                  :: IPROF
NO TYPE, INTENT(IN OUT)                  :: HEAD
NO TYPE, INTENT(IN OUT)                  :: PROF
LOGICAL, INTENT(OUT)                     :: LBLAC1
INTEGER, INTENT(OUT)                     :: CTYPE1
REAL, INTENT(OUT)                        :: CFRAC1
NO TYPE, INTENT(OUT)                     :: CPSIZ1
NO TYPE, INTENT(OUT)                     :: CPRTO1
NO TYPE, INTENT(OUT)                     :: CPRBO1
NO TYPE, INTENT(OUT)                     :: CNGWA1
REAL, INTENT(OUT)                        :: XCEMI1(MXEMIS)
REAL, INTENT(OUT)                        :: XCRHO1(MXEMIS)
NO TYPE, INTENT(OUT)                     :: CSTMP1
LOGICAL, INTENT(OUT)                     :: LBLAC2
INTEGER, INTENT(OUT)                     :: CTYPE2
REAL, INTENT(OUT)                        :: CFRAC2
NO TYPE, INTENT(OUT)                     :: CPSIZ2
NO TYPE, INTENT(OUT)                     :: CPRTO2
NO TYPE, INTENT(OUT)                     :: CPRBO2
NO TYPE, INTENT(OUT)                     :: CNGWA2
REAL, INTENT(OUT)                        :: XCEMI2(MXEMIS)
REAL, INTENT(OUT)                        :: XCRHO2(MXEMIS)
NO TYPE, INTENT(OUT)                     :: CSTMP2
REAL, INTENT(OUT)                        :: CFRA12
REAL, INTENT(OUT)                        :: FCLEAR
REAL, INTENT(OUT)                        :: CFRA1X
REAL, INTENT(OUT)                        :: CFRA2X
IMPLICIT NONE


!-----------------------------------------------------------------------
!      INCLUDE FILES
!-----------------------------------------------------------------------
INCLUDE 'incFTC.f'
INCLUDE 'rtpdefs.f'


!-----------------------------------------------------------------------
!      EXTERNAL FUNCTIONS
!-----------------------------------------------------------------------
!      none


!-----------------------------------------------------------------------
!      ARGUMENTS
!-----------------------------------------------------------------------
!      Input parameters:

RECORD /RTPHEAD/ HEAD   ! header data
RECORD /RTPPROF/ PROF   ! profile data

!      Output parameters:
!      cloud1



REAL :: CPSIZ1         ! particle size (um)
REAL :: CPRTO1         ! cloud top pressure (mb)
REAL :: CPRBO1         ! cloud bottom pressure (mb)
REAL :: CNGWA1         ! cloud amount (g/m^2)
REAL :: ! emissivity
REAL :: ! reflectivity
REAL :: CSTMP1         ! cloud/surf temperature (K)
!      cloud2



REAL :: CPSIZ2         ! particle size (um)
REAL :: CPRTO2         ! cloud top pressure (mb)
REAL :: CPRBO2         ! cloud bottom pressure (mb)
REAL :: CNGWA2         ! cloud amount (g/m^2)
REAL :: ! emissivity
REAL :: ! reflectivity
REAL :: CSTMP2         ! cloud/surf temperature (K)
!      other cloud fractions





!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
INTEGER :: I      ! generic integer
INTEGER :: ICASE      ! case number for clouds

!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none


!***********************************************************************
!***********************************************************************
!      EXECUTABLE CODE begins below
!***********************************************************************
!***********************************************************************

!      Determine which of 5 possible cloud cases is in effect:
!         1: cfrac1=0 and cfrac2=0
!         2: cfrac1>0 and cfrac2=0
!         3: cfrac1=0 and cfrac2>0
!         4: cfrac1>0 and cfrac2>0 and cprtop1 <= cprtop2
!         5: cfrac1>0 and cfrac2>0 and cprtop1 > cprtop2

CFRAC1=PROF%cfrac
CFRAC2=PROF%cfrac2
CPRTO1=PROF%cprtop
CPRTO2=PROF%cprtop2

!      Initialize with default values
FCLEAR=1.0
CFRA12=0.0
CFRA1X=0.0
CFRA2X=0.0
LBLAC1=.TRUE.
LBLAC2=.TRUE.
ICASE=0

IF (CFRAC1 <= 0.0) THEN
  CFRAC1=0.0
  IF (CFRAC2 <= 0.0) THEN
    CFRAC2=0.0
    ICASE=1
  ELSE
    ICASE=3
  END IF
ELSE
  IF (CFRAC2 <= 0.0) THEN
    CFRAC2=0.0
    ICASE=2
  ELSE
    IF (CPRTO1 <= CPRTO2) THEN
      ICASE=4
    ELSE
      ICASE=5
    END IF
    IF (PROF%cfrac12 > 0.0) CFRA12=PROF%cfrac12
  END IF
END IF


!cc
!      print *, 'icase=',ICASE
!cc

!      Assign remaining cloud variables if icase > 1
IF (ICASE > 1) THEN
  
  IF (ICASE == 2 .OR. ICASE == 4) THEN
!            cloud1 RTP fields to cloud1 variables
    CTYPE1=PROF%ctype
    CPRTO1=PROF%cprtop
    CPRBO1=PROF%cprbot
    CNGWA1=PROF%cngwat
    CPSIZ1=PROF%cpsize
    IF (CTYPE1 < 100) THEN
!               WARNING! does not check if cemis is ok
      CSTMP1=PROF%cstemp
      DO I=1,PROF%nemis
        XCEMI1(I)=PROF%cemis(I)
        XCRHO1(I)=PROF%crho(I)
        IF (XCRHO1(I) < 0.0) THEN
          XCRHO1(I)=(1 - XCEMI1(I))/PI
        END IF
        ENDDO
        END IF
        
        IF (ICASE == 4) THEN
!               cloud2 RTP fields to cloud2 variables
          CTYPE2=PROF%ctype2
          CPRTO2=PROF%cprtop2
          CPRBO2=PROF%cprbot2
          CNGWA2=PROF%cngwat2
          CPSIZ2=PROF%cpsize2
          IF (CTYPE2 < 100) THEN
!                  WARNING! does not check if cemis2 is ok
            CSTMP2=PROF%cstemp2
            DO I=1,PROF%nemis
              XCEMI2(I)=PROF%cemis2(I)
              XCRHO2(I)=PROF%crho2(I)
              IF (XCRHO2(I) < 0.0) THEN
                XCRHO2(I)=(1 - XCEMI2(I))/PI
              END IF
              ENDDO
              END IF
            END IF
            
          ELSE ! icase=3 or 5
!            cloud2 RTP fields into cloud1 variables
            CFRAC1=CFRAC2
            CFRAC2=0.0
            CTYPE1=PROF%ctype2
            CPRTO1=PROF%cprtop2
            CPRBO1=PROF%cprbot2
            CNGWA1=PROF%cngwat2
            CPSIZ1=PROF%cpsize2
            IF (CTYPE1 < 100) THEN
!               WARNING! does not check if cemis2 is ok
              CSTMP1=PROF%cstemp2
              DO I=1,PROF%nemis
                XCEMI1(I)=PROF%cemis2(I)
                XCRHO1(I)=PROF%crho2(I)
                IF (XCRHO1(I) < 0.0) THEN
                  XCRHO1(I)=(1 - XCEMI1(I))/PI
                END IF
                ENDDO
                END IF
                
                IF (ICASE == 5) THEN
!               cloud1 RTP fields to cloud2 variables
                  CFRAC2=PROF%cfrac
                  CTYPE2=PROF%ctype
                  CPRTO2=PROF%cprtop
                  CPRBO2=PROF%cprbot
                  CNGWA2=PROF%cngwat
                  CPSIZ2=PROF%cpsize
                  IF (CTYPE2 < 100) THEN
!                  WARNING! does not check if cemis is ok
                    CSTMP2=PROF%cstemp
                    DO I=1,PROF%nemis
                      XCEMI2(I)=PROF%cemis(I)
                      XCRHO2(I)=PROF%crho(I)
                      IF (XCRHO2(I) < 0.0) THEN
                        XCRHO2(I)=(1 - XCEMI2(I))/PI
                      END IF
                      ENDDO
                      END IF
                    END IF
                  END IF
                  
                  
!         Check cloud1 varibles
                  IF (CFRAC1 > 1.0) THEN
                    WRITE(IOERR,1010) IPROF, 'CFRAC1', '0.0', '1.0'
                    1010        FORMAT('Error! iprof=',I5,', ',A6,  &
                        ' outside allowed ',A6,' to ',A6, ' range')
                    STOP
                  END IF
                  IF (CPRTO1 < PROF%plevs(1) .OR. CPRTO1 > PROF%spres) THEN
                    WRITE(IOERR,1010) IPROF, 'CPRTO1', 'PLEVS1', 'SPRES'
                    STOP
                  END IF
                  IF (CTYPE1 < 100) THEN
!            Black cloud
                    LBLAC1 = .TRUE.
                    CPRBO1=CPRTO1*1.001 ! safe dummy value
                  ELSE
!            Slab cloud
                    LBLAC1 = .FALSE.
!            Check cprbot, cpsize, & cngwat
                    IF (CPRBO1 < PROF%plevs(1) .OR. CPRBO1 > PROF%spres) THEN
                      WRITE(IOERR,1010) IPROF, 'CPRBO1', 'PLEVS1', 'SPRES'
                      STOP
                    END IF
                    IF (CPRTO1 > CPRBO1) THEN
                      WRITE(IOERR,1020) IPROF, 'CPRTO1', 'CPRBO1'
                      1020           FORMAT('Error! iprof=',I5,', ',A6,' > ',A6)
                      STOP
                    END IF
                    IF (CPSIZ1 < 0.0 .OR. CPSIZ1 > 1E+3) THEN
                      WRITE(IOERR,1010) IPROF, 'CPSIZ1', '0.0', '1E+3'
                      STOP
                    END IF
                    IF (CNGWA1 < 0.0 .OR. CNGWA1 > 1E+6) THEN
                      WRITE(IOERR,1010) IPROF, 'CNGWA1', '0.0', '1E+6'
                      STOP
                    END IF
                  END IF ! end check of cloud1 variables
                  
                  
!         Check cloud2 variables if needed
                  IF (CFRAC2 > 0.0) THEN
                    IF (CFRAC2 > 1.0) THEN
                      WRITE(IOERR,1010) IPROF, 'CFRAC2', '0.0', '1.0'
                      STOP
                    END IF
                    IF (CPRTO1 < PROF%plevs(1) .OR. CPRTO1 > PROF%spres) THEN
                      WRITE(IOERR,1010) IPROF, 'CPRTO2', 'PLEVS1', 'SPRES'
                      STOP
                    END IF
                    IF (CTYPE2 < 100) THEN
!               Black cloud
                      LBLAC2 = .TRUE.
                      CPRBO2=CPRTO2*1.001 ! safe dummy value
                    ELSE
!               Slab cloud
                      LBLAC2 = .FALSE.
!               Check cprbot, cpsize, & cngwat
                      IF (CPRBO2 < PROF%plevs(1) .OR.  &
                            CPRBO2 > PROF%spres) THEN
                        WRITE(IOERR,1010) IPROF, 'CPRBO2', 'PLEVS1', 'SPRES'
                        STOP
                      END IF
                      IF (CPRTO2 > CPRBO2) THEN
                        WRITE(IOERR,1020) IPROF, 'CPRTO2', 'CPRBO2'
                        STOP
                      END IF
                      IF (CPSIZ2 < 0.0 .OR. CPSIZ2 > 1E+3) THEN
                        WRITE(IOERR,1010) IPROF, 'CPSIZ2', '0.0', '1E+3'
                        STOP
                      END IF
                      IF (CNGWA2 < 0.0 .OR. CNGWA2 > 1E+6) THEN
                        WRITE(IOERR,1010) IPROF, 'CNGWA2', '0.0', '1E+6'
                        STOP
                      END IF
                    END IF
                  END IF ! end check of cloud2 variables
                  
                  
!         Compute exclusive cloud fractions
                  IF (CFRA12 > 0.0) THEN
!            First check two-cloud sanity
                    IF (CFRA12 > 1.0) THEN
                      WRITE(IOERR,1010) IPROF, 'CFRA12', '0.0', '1.0'
                      STOP
                    END IF
                    IF (LBLAC1) THEN
!               If top cloud is black then must have cfra12=0
                      WRITE(IOERR,1055) IPROF
                      1055           FORMAT('Error! iprof=',I5,  &
                          ' may not have CFRA12 > 0 with a black top cloud')
                      STOP
                    ELSE
                      IF (CPRBO1 > CPRTO2) THEN
!                  Bottom of top cloud is blow top of bottom cloud
                        WRITE(IOERR,1020) IPROF, 'CPRBO1', 'CPRTO2'
                      END IF
                    END IF
!            Now compute exclusive cloud fractions
                    IF ((CFRA12 <= CFRAC1) .AND. (CFRA12 <= CFRAC2)) THEN
                      CFRA1X=CFRAC1 - CFRA12
                      CFRA2X=CFRAC2 - CFRA12
                    ELSE
                      WRITE(IOERR,1065) IPROF
                      1065           FORMAT('Error! iprof=',I5,  &
                          ' may not have cfra12 > cfrac1 or cfrac2')
!       write(6,*) 'cfrac1,cfrac2,cfra12=',CFRAC1,CFRAC2,CFRA12
                    END IF
                  ELSE
                    CFRA1X=CFRAC1
                    CFRA2X=CFRAC2
                  END IF
                  
                  
!         Compute clear fraction
                  FCLEAR=1.0 - (CFRA1X + CFRA2X + CFRA12)
                  IF (FCLEAR < 0.0) THEN
                    IF (FCLEAR > -1E-4) THEN
!               close enough to interpret as zero
                      FCLEAR=0.0
                    ELSE
                      WRITE(IOERR,1070) IPROF,CFRA1X,CFRA2X,CFRA12,1.0-FCLEAR
                      1070           FORMAT('Error! iprof=',I5,', cfra1x=',F7.5,  &
                          ' + cfra2x=',F7.5,' + cfra12=',F7.5,' =',F7.5,' > 1')
                      STOP
                    END IF
                  END IF
                  
                END IF ! end icase > 1
                
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!      uncomment for cloud summary
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!       WRITE(6,2010) IPROF
! 2010  FORMAT('-------- Cloud Summary for profile',I5' --------')
!       WRITE(6,2011) FCLEAR
! 2011  FORMAT('fclear=',F6.3)
!C
!       IF (CFRAC1 .GT. 0.0) THEN
!        WRITE(6,2021) CTYPE1, CFRAC1, CFRA1X, CPRTO1
! 2021   FORMAT('Cloud1: ctype1=',I5,', cfrac1=',F6.3,', cfra1x=',F6.3,
!     $  ', cprto1=',F8.3)
!        IF (LBLAC1) THEN
!           WRITE(6,2022) CSTMP1
! 2022      FORMAT('Black cloud1: cstmp1=',F9.3)
!        ELSE
!           WRITE(6,2023) CPRBO1, CNGWA1, CPSIZ1
! 2023      FORMAT('Complex cloud1: cprbo1=',F8.3,', cngwa1=',F7.3,
!     $     ', cpsiz1=',F7.2)
!        ENDIF
!C
!        IF (CFRAC2 .GT. 0.0) THEN
!         WRITE(6,2034) CTYPE2, CFRAC2, CFRA2X, CPRTO2
! 2034    FORMAT('Cloud2: ctype2=',I5,', cfrac2=',F6.3,', cfra2x=',F6.3,
!     $   ', cprto2=',F8.3)
!         IF (LBLAC2) THEN
!            WRITE(6,2035) CSTMP2
! 2035       FORMAT('Black cloud2: cstmp2=',F9.3)
!         ELSE
!            WRITE(6,2036) CPRBO2, CNGWA2, CPSIZ2
! 2036       FORMAT('Complex cloud2: cprbo2=',F8.3,', cngwa2=',F7.3,
!     $      ', cpsiz2=',F7.2)
!         ENDIF
!         WRITE(6,2037) CFRA12
! 2037    FORMAT('cfra12=',F6.3)
!        ENDIF ! cfrac2 > 0
!       ENDIF ! cfrac1 > 0
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
                
                RETURN
              END SUBROUTINE GETCLD
