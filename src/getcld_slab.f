C=======================================================================
C=======================================================================
C
C              University of Maryland Baltimore County [UMBC]
C
C              AIRS
C
C              GETCLD
C
!F77====================================================================


!ROUTINE NAME: GETCLD


!ABSTRACT:
C    Get basic cloud info

!CALL PROTOCOL:
C    GETCLD( IPROF, HEAD, PROF,
C    LBLAC1, CTYPE1, CFRAC1, CPSIZ1, CPRTO1, CPRBO1, CNGWA1, ...
C    XCEMI1, XCRHO1, CSTMP1,
C    LBLAC2, CTYPE2, CFRAC2, CPSIZ2, CPRTO2, CPRBO2, CNGWA2,
C    XCEMI2, XCRHO2, CSTMP2, CFRA12, FCLEAR, CFRA1X, CFRA2X )


!INPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    INTEGER   IPROF   profile loop counter        none
C    STRUCT    HEAD    RTP header structure        various
C    STRUCT    PROF    RTP profile structure       various

!OUTPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    LOGICAL   LBLAC1  cloud1 is black?            none
C    INTEGER   CTYPE1  cloud type code number      none
C    REAL      CFRAC1  total cloud1 fraction       none (0.0 to 1.0)
C    REAL      CPSIZ1  particle size               um
C    REAL      CPRTO1  cloud top pressure          mb
C    REAL      CPRBO1  cloud bottom pressure       mb
C    REAL      CNGWA1  layer integrated profile    g/m^2
C    REAL arr  XCEMI1  emissivity cloud1           none (0.0 to 1.0)
C    REAL arr  XCRHO1  reflectivity cloud1         none
C    REAL      CSTMP1  cloud top temperature       Kelvin
C    LOGICAL   LBLAC2  cloud2 is black?            none
C    INTEGER   CTYPE2  cloud type code number      none
C    REAL      CFRAC2  total cloud2 fraction       none (0.0 to 1.0)
C    REAL      CPSIZ2  particle size               um
C    REAL      CPRTO2  cloud top pressure          mb
C    REAL      CPRBO2  cloud bottom pressure       mb
C    REAL      CNGWA2  layer integrated profile    g/m^2
C    REAL arr  XCEMI2  emissivity cloud2           none (0.0 to 1.0)
C    REAL arr  XCRHO2  reflectivity cloud2         none
C    REAL      CSTMP2  cloud top temperature       Kelvin
C    REAL      CFRA12  both clouds fraction        none (0.0 to 1.0)
C    REAL      FCLEAR  clear fraction              none (0.0 to 1.0)
C    REAL      CFRA1X  exclusive cloud1 fraction   none (0.0 to 1.0)
C    REAL      CFRA2X  exclusive cloud2 fraction   none (0.0 to 1.0)


!INPUT/OUTPUT PARAMETERS: none


!RETURN VALUES: none


!PARENT(S): SARTA


!ROUTINES CALLED: none


!FILES ACCESSED:
C    none


!COMMON BLOCKS: none


!DESCRIPTION:
C    Pulls out and checks cloud profile paramters.


!ALGORITHM REFERENCES: see DESCRIPTION


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date     Programmer        Comments
C------------ ----------------- ----------------------------------------
C 22 Feb 2007 Scott Hannon      created
C 10 Sep 2007 Scott Hannon      Mods for 100 layer particle size
C 14 Sep 2007 Scott Hannon      Add CPMIN check
C 15 Nov 2007 Scott Hannon      re-written for slab cloud
C 26 Nov 2008 Scott Hannon      Partial re-write for rtpV201 including
C                               spectral XCEMI1/2 & XCRHO1/2
C 01 Dec 2008 Scott Hannon      Add CSTMP1/2
C 07 May 2009 Scott Hannon      Bug fix: initialize CFRA1X=CFRA2X=0

!END====================================================================

C      =================================================================
       SUBROUTINE GETCLD( IPROF, HEAD, PROF,
     $    LBLAC1, CTYPE1, CFRAC1, CPSIZ1, CPRTO1, CPRBO1, CNGWA1,
     $    XCEMI1, XCRHO1, CSTMP1,
     $    LBLAC2, CTYPE2, CFRAC2, CPSIZ2, CPRTO2, CPRBO2, CNGWA2,
     $    XCEMI2, XCRHO2, CSTMP2, CFRA12, FCLEAR, CFRA1X, CFRA2X )
C      =================================================================


C-----------------------------------------------------------------------
C      IMPLICIT NONE
C-----------------------------------------------------------------------
       IMPLICIT NONE


C-----------------------------------------------------------------------
C      INCLUDE FILES
C-----------------------------------------------------------------------
      include 'incFTC.f'
      include 'rtpdefs.f'


C-----------------------------------------------------------------------
C      EXTERNAL FUNCTIONS
C-----------------------------------------------------------------------
C      none


C-----------------------------------------------------------------------
C      ARGUMENTS
C-----------------------------------------------------------------------
C      Input parameters:
       INTEGER  IPROF          ! profile loop counter
       RECORD /RTPHEAD/ HEAD   ! header data
       RECORD /RTPPROF/ PROF   ! profile data

C      Output parameters:
C      cloud1
       LOGICAL LBLAC1      ! black cloud?
       INTEGER CTYPE1      ! cloud type code number
       REAL CFRAC1         ! cloud fraction
       REAL CPSIZ1         ! particle size (um)
       REAL CPRTO1         ! cloud top pressure (mb)
       REAL CPRBO1         ! cloud bottom pressure (mb)
       REAL CNGWA1         ! cloud amount (g/m^2)
       REAL XCEMI1(MXEMIS) ! emissivity
       REAL XCRHO1(MXEMIS) ! reflectivity
       REAL CSTMP1         ! cloud/surf temperature (K)
C      cloud2
       LOGICAL LBLAC2      ! black cloud?
       INTEGER CTYPE2      ! cloud type code number
       REAL CFRAC2         ! cloud fraction
       REAL CPSIZ2         ! particle size (um)
       REAL CPRTO2         ! cloud top pressure (mb)
       REAL CPRBO2         ! cloud bottom pressure (mb)
       REAL CNGWA2         ! cloud amount (g/m^2)
       REAL XCEMI2(MXEMIS) ! emissivity
       REAL XCRHO2(MXEMIS) ! reflectivity
       REAL CSTMP2         ! cloud/surf temperature (K)
C      other cloud fractions
       REAL CFRA12         ! both clouds fraction
       REAL FCLEAR         ! clear fraction
       REAL CFRA1X         ! exclusive cloud1 fraction
       REAL CFRA2X         ! exclusive cloud2 fraction

C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER      I      ! generic integer
       INTEGER  ICASE      ! case number for clouds

C-----------------------------------------------------------------------
C      SAVE STATEMENTS
C-----------------------------------------------------------------------
C      none


C***********************************************************************
C***********************************************************************
C      EXECUTABLE CODE begins below
C***********************************************************************
C***********************************************************************

C      Determine which of 5 possible cloud cases is in effect:
C         1: cfrac1=0 and cfrac2=0
C         2: cfrac1>0 and cfrac2=0
C         3: cfrac1=0 and cfrac2>0
C         4: cfrac1>0 and cfrac2>0 and cprtop1 <= cprtop2
C         5: cfrac1>0 and cfrac2>0 and cprtop1 > cprtop2
C
       CFRAC1=PROF.cfrac
       CFRAC2=PROF.cfrac2
       CPRTO1=PROF.cprtop
       CPRTO2=PROF.cprtop2
C
C      Initialize with default values
       FCLEAR=1.0
       CFRA12=0.0
       CFRA1X=0.0
       CFRA2X=0.0
       LBLAC1=.TRUE.
       LBLAC2=.TRUE.
       ICASE=0
C
       IF (CFRAC1 .LE. 0.0) THEN
          CFRAC1=0.0
          IF (CFRAC2 .LE. 0.0) THEN
             CFRAC2=0.0
             ICASE=1
          ELSE
             ICASE=3
          ENDIF
       ELSE
          IF (CFRAC2 .LE. 0.0) THEN
             CFRAC2=0.0
             ICASE=2
          ELSE
             IF (CPRTO1 .LE. CPRTO2) THEN
                ICASE=4
             ELSE
                ICASE=5
             ENDIF
             IF (PROF.cfrac12 .GT. 0.0) CFRA12=PROF.cfrac12
          ENDIF
       ENDIF
C

ccc
c      print *, 'icase=',ICASE
ccc

C      Assign remaining cloud variables if icase > 1
       IF (ICASE .GT. 1) THEN
C
          IF (ICASE .EQ. 2 .OR. ICASE .EQ. 4) THEN
C            cloud1 RTP fields to cloud1 variables
             CTYPE1=PROF.ctype
             CPRTO1=PROF.cprtop
             CPRBO1=PROF.cprbot
             CNGWA1=PROF.cngwat
             CPSIZ1=PROF.cpsize
             IF (CTYPE1 .LT. 100) THEN
C               WARNING! does not check if cemis is ok
                CSTMP1=PROF.cstemp
                DO I=1,PROF.nemis
                   XCEMI1(I)=PROF.cemis(I)
                   XCRHO1(I)=PROF.crho(I)
                   IF (XCRHO1(I) .LT. 0.0) THEN
                      XCRHO1(I)=(1 - XCEMI1(I))/PI
                   ENDIF
                ENDDO
             ENDIF
C
             IF (ICASE .EQ. 4) THEN
C               cloud2 RTP fields to cloud2 variables
                CTYPE2=PROF.ctype2
                CPRTO2=PROF.cprtop2
                CPRBO2=PROF.cprbot2
                CNGWA2=PROF.cngwat2
                CPSIZ2=PROF.cpsize2
                IF (CTYPE2 .LT. 100) THEN
C                  WARNING! does not check if cemis2 is ok
                   CSTMP2=PROF.cstemp2
                   DO I=1,PROF.nemis
                      XCEMI2(I)=PROF.cemis2(I)
                      XCRHO2(I)=PROF.crho2(I)
                      IF (XCRHO2(I) .LT. 0.0) THEN
                         XCRHO2(I)=(1 - XCEMI2(I))/PI
                      ENDIF
                   ENDDO
                ENDIF
             ENDIF
C
          ELSE ! icase=3 or 5
C            cloud2 RTP fields into cloud1 variables
             CFRAC1=CFRAC2
             CFRAC2=0.0
             CTYPE1=PROF.ctype2
             CPRTO1=PROF.cprtop2
             CPRBO1=PROF.cprbot2
             CNGWA1=PROF.cngwat2
             CPSIZ1=PROF.cpsize2
             IF (CTYPE1 .LT. 100) THEN
C               WARNING! does not check if cemis2 is ok
                CSTMP1=PROF.cstemp2
                DO I=1,PROF.nemis
                   XCEMI1(I)=PROF.cemis2(I)
                   XCRHO1(I)=PROF.crho2(I)
                   IF (XCRHO1(I) .LT. 0.0) THEN
                      XCRHO1(I)=(1 - XCEMI1(I))/PI
                   ENDIF
                ENDDO
             ENDIF
C
             IF (ICASE .EQ. 5) THEN
C               cloud1 RTP fields to cloud2 variables
                CFRAC2=PROF.cfrac
                CTYPE2=PROF.ctype
                CPRTO2=PROF.cprtop
                CPRBO2=PROF.cprbot
                CNGWA2=PROF.cngwat
                CPSIZ2=PROF.cpsize
                IF (CTYPE2 .LT. 100) THEN
C                  WARNING! does not check if cemis is ok
                   CSTMP2=PROF.cstemp
                   DO I=1,PROF.nemis
                      XCEMI2(I)=PROF.cemis(I)
                      XCRHO2(I)=PROF.crho(I)
                      IF (XCRHO2(I) .LT. 0.0) THEN
                         XCRHO2(I)=(1 - XCEMI2(I))/PI
                      ENDIF
                   ENDDO
                ENDIF
             ENDIF
          ENDIF
C

C         Check cloud1 varibles
          IF (CFRAC1 .GT. 1.0) THEN
             WRITE(IOERR,1010) IPROF, 'CFRAC1', '0.0', '1.0'
 1010        FORMAT('Error! iprof=',I5,', ',A6,
     $       ' outside allowed ',A6,' to ',A6, ' range')
             STOP
          ENDIF
          IF (CPRTO1 .LT. PROF.plevs(1) .OR.
     $        CPRTO1 .GT. PROF.spres) THEN
             WRITE(IOERR,1010) IPROF, 'CPRTO1', 'PLEVS1', 'SPRES'
             STOP
          ENDIF
          IF (CTYPE1 .LT. 100) THEN
C            Black cloud
             LBLAC1 = .TRUE.
             CPRBO1=CPRTO1*1.001 ! safe dummy value
          ELSE
C            Slab cloud
             LBLAC1 = .FALSE.
C            Check cprbot, cpsize, & cngwat
             IF (CPRBO1 .LT. PROF.plevs(1) .OR.
     $           CPRBO1 .GT. PROF.spres) THEN
                WRITE(IOERR,1010) IPROF, 'CPRBO1', 'PLEVS1', 'SPRES'
                STOP
             ENDIF
             IF (CPRTO1 .GT. CPRBO1) THEN
                WRITE(IOERR,1020) IPROF, 'CPRTO1', 'CPRBO1'
 1020           FORMAT('Error! iprof=',I5,', ',A6,' > ',A6)
                STOP
             ENDIF
             IF (CPSIZ1 .LT. 0.0 .OR. CPSIZ1 .GT. 1E+3) THEN
                WRITE(IOERR,1010) IPROF, 'CPSIZ1', '0.0', '1E+3'
                STOP
             ENDIF
             IF (CNGWA1 .LT. 0.0 .OR. CNGWA1 .GT. 1E+6) THEN
                WRITE(IOERR,1010) IPROF, 'CNGWA1', '0.0', '1E+6'
                STOP
             ENDIF
          ENDIF ! end check of cloud1 variables
C

C         Check cloud2 variables if needed
          IF (CFRAC2 .GT. 0.0) THEN
             IF (CFRAC2 .GT. 1.0) THEN
                WRITE(IOERR,1010) IPROF, 'CFRAC2', '0.0', '1.0'
                STOP
             ENDIF
             IF (CPRTO1 .LT. PROF.plevs(1) .OR.
     $          CPRTO1 .GT. PROF.spres) THEN
                WRITE(IOERR,1010) IPROF, 'CPRTO2', 'PLEVS1', 'SPRES'
                STOP
             ENDIF
             IF (CTYPE2 .LT. 100) THEN
C               Black cloud
                LBLAC2 = .TRUE.
                CPRBO2=CPRTO2*1.001 ! safe dummy value
             ELSE
C               Slab cloud
                LBLAC2 = .FALSE.
C               Check cprbot, cpsize, & cngwat
                IF (CPRBO2 .LT. PROF.plevs(1) .OR.
     $             CPRBO2 .GT. PROF.spres) THEN
                   WRITE(IOERR,1010) IPROF, 'CPRBO2', 'PLEVS1', 'SPRES'
                   STOP
                ENDIF
                IF (CPRTO2 .GT. CPRBO2) THEN
                   WRITE(IOERR,1020) IPROF, 'CPRTO2', 'CPRBO2'
                   STOP
                ENDIF
                IF (CPSIZ2 .LT. 0.0 .OR. CPSIZ2 .GT. 1E+3) THEN
                   WRITE(IOERR,1010) IPROF, 'CPSIZ2', '0.0', '1E+3'
                   STOP
                ENDIF
                IF (CNGWA2 .LT. 0.0 .OR. CNGWA2 .GT. 1E+6) THEN
                   WRITE(IOERR,1010) IPROF, 'CNGWA2', '0.0', '1E+6'
                   STOP
                ENDIF
             ENDIF
          ENDIF ! end check of cloud2 variables
C

C         Compute exclusive cloud fractions
          IF (CFRA12 .GT. 0.0) THEN
C            First check two-cloud sanity
             IF (CFRA12 .GT. 1.0) THEN
                WRITE(IOERR,1010) IPROF, 'CFRA12', '0.0', '1.0'
                STOP
             ENDIF
             IF (LBLAC1) THEN
C               If top cloud is black then must have cfra12=0
                WRITE(IOERR,1055) IPROF
 1055           FORMAT('Error! iprof=',I5,
     $          ' may not have CFRA12 > 0 with a black top cloud')
                STOP
             ELSE
                IF (CPRBO1 .GT. CPRTO2) THEN
C                  Bottom of top cloud is blow top of bottom cloud
                   WRITE(IOERR,1020) IPROF, 'CPRBO1', 'CPRTO2'
                ENDIF
             ENDIF
C            Now compute exclusive cloud fractions
             IF ((CFRA12 .LE. CFRAC1) .AND. (CFRA12 .LE. CFRAC2)) THEN
                CFRA1X=CFRAC1 - CFRA12
                CFRA2X=CFRAC2 - CFRA12
             ELSE
                WRITE(IOERR,1065) IPROF
 1065           FORMAT('Error! iprof=',I5,
     $          ' may not have cfra12 > cfrac1 or cfrac2')
c       write(6,*) 'cfrac1,cfrac2,cfra12=',CFRAC1,CFRAC2,CFRA12
             ENDIF
          ELSE
             CFRA1X=CFRAC1
             CFRA2X=CFRAC2
          ENDIF
C

C         Compute clear fraction
          FCLEAR=1.0 - (CFRA1X + CFRA2X + CFRA12)
          IF (FCLEAR .LT. 0.0) THEN
             IF (FCLEAR .GT. -1E-4) THEN
C               close enough to interpret as zero
                FCLEAR=0.0
             ELSE
                WRITE(IOERR,1070) IPROF,CFRA1X,CFRA2X,CFRA12,1.0-FCLEAR
 1070           FORMAT('Error! iprof=',I5,', cfra1x=',F7.5,
     $          ' + cfra2x=',F7.5,' + cfra12=',F7.5,' =',F7.5,' > 1')
                STOP
             ENDIF
          ENDIF
C
       ENDIF ! end icase > 1

Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C      uncomment for cloud summary 
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       WRITE(6,2010) IPROF
c 2010  FORMAT('-------- Cloud Summary for profile',I5' --------')
c       WRITE(6,2011) FCLEAR
c 2011  FORMAT('fclear=',F6.3)
cC
c       IF (CFRAC1 .GT. 0.0) THEN
c        WRITE(6,2021) CTYPE1, CFRAC1, CFRA1X, CPRTO1
c 2021   FORMAT('Cloud1: ctype1=',I5,', cfrac1=',F6.3,', cfra1x=',F6.3,
c     $  ', cprto1=',F8.3)
c        IF (LBLAC1) THEN
c           WRITE(6,2022) CSTMP1
c 2022      FORMAT('Black cloud1: cstmp1=',F9.3)
c        ELSE
c           WRITE(6,2023) CPRBO1, CNGWA1, CPSIZ1
c 2023      FORMAT('Complex cloud1: cprbo1=',F8.3,', cngwa1=',F7.3,
c     $     ', cpsiz1=',F7.2)
c        ENDIF
cC
c        IF (CFRAC2 .GT. 0.0) THEN
c         WRITE(6,2034) CTYPE2, CFRAC2, CFRA2X, CPRTO2
c 2034    FORMAT('Cloud2: ctype2=',I5,', cfrac2=',F6.3,', cfra2x=',F6.3,
c     $   ', cprto2=',F8.3)
c         IF (LBLAC2) THEN
c            WRITE(6,2035) CSTMP2
c 2035       FORMAT('Black cloud2: cstmp2=',F9.3)
c         ELSE
c            WRITE(6,2036) CPRBO2, CNGWA2, CPSIZ2
c 2036       FORMAT('Complex cloud2: cprbo2=',F8.3,', cngwa2=',F7.3,
c     $      ', cpsiz2=',F7.2)
c         ENDIF
c         WRITE(6,2037) CFRA12
c 2037    FORMAT('cfra12=',F6.3)
c        ENDIF ! cfrac2 > 0
c       ENDIF ! cfrac1 > 0
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
       RETURN
       END
