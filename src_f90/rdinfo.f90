! rdinfo processes command line arguments
!
!       sarta  fin=input.rtp  fout=output.rtp  listp=1,2,3
!
!
! to compile
!   Absoft/Linux: f77 -N109 -o klayers $(SRC) -lU77
!   SGI Irix: no special compiler options are needed
!=======================================================================
!=======================================================================
!
!              University of Maryland Baltimore County [UMBC]
!
!              AIRS
!
!              RDINFO_sarta
!
!F77====================================================================


!ROUTINE NAME: RDINFO


!ABSTRACT:
!    Get info about the sarta run: the names of input & output
!    files, the channel list, and list of profile numbers.


!CALL PROTOCOL:
!    RDINFO(FIN, FOUT, LRHOT, NWANTP, LISTP)


!INPUT PARAMETERS:
!    none


!OUTPUT PARAMETERS:
!    type      name    purpose                     units
!    --------  ------  --------------------------  ---------------------
!    CHAR*80   FIN     input filename              none
!    CHAR*80   FOUT    output filename             none
!    LOGICAL   LRHOT   force RHO for refl thermal? none
!    INTEGER   NWANTP  Number of desired profiles  none
!    INT arr   LISTP   List of desired prof nums   none



!INPUT/OUTPUT PARAMETERS: none


!RETURN VALUES: none


!PARENT(S): SARTA_rtp


!ROUTINES CALLED:
!    none


!FILES ACCESSED:
!    none


!COMMON BLOCKS:
!      none


!DESCRIPTION:
!    Gets various info about sarta run.
!
!    Each command line argument is of the form <variable>=<value>
!    Each <variable>=<value> string must be 80 char or less.  It is
!    necessary to enclose values in quotes unless they contain
!    blanks.  The recognized command-line variables are:
!
!    fin : name of input file
!
!    fout : name of output file
!
!    lrhot : force reflected thermal rho?; logical. true/false of T/F
!       If true, the refl therm will use rho=(1-emis)/pi rather than
!       the rho (if any) from the input file.
!
!    listp : list of desired profile numbers (all other profiles will
!       be ignored).  If "listp" is not specified, SARTA will process
!       all profiles.
!
!       The listp profile numbers may be specified either as a
!       sequence of integers separated by a comma, or alternately as
!       a quoted string containing integers separated by a blank space.
!       Examples:
!          listp=1,2,3,4,5
!          listp='1 2 3 4 5'
!       Due to the 80 char limit, the maximum number of entries
!       in listp is limited.  (Eg 15 four digit numbers, or
!       25 two digit numbers.  MAXPRO is the hardcoded limit.)
!

!ALGORITHM REFERENCES: see DESCRIPTION


!KNOWN BUGS AND LIMITATIONS:
!    none


!ROUTINE HISTORY:
!    Date     Programmer        Comments
!------------ ----------------- ----------------------------------------
! 13 Feb 2001 H.Motteler/S.Hannon Re-write of KLAYERS version
! 28 Nov 2001 Scott Hannon      Remove command-line argument "nwantp"
!  5 Dec 2001 Scott Hannon      Remove unused local var LENNB
! 05 Aug 2003 Scott Hannon      Correct FIN & FOUT to CHAR*80 (not 70)
! 06 Feb 2004 Scott Hannon      Add LRHOT argument and associated code


!END====================================================================


!      =================================================================
       SUBROUTINE RDINFO(FIN, FOUT, LRHOT, NWANTP, LISTP)
!      =================================================================

!      use unix_library

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
!      From "util.f"
!      subroutine UPCASE = converts a string to upper case
!      function STR2BO = converts true/false string to boolean (LOGICAL)


!-----------------------------------------------------------------------
!      ARGUMENTS
!-----------------------------------------------------------------------
!      Input:
!      none
!
!      Output:
       CHARACTER*80 FIN
       CHARACTER*80 FOUT
       LOGICAL  LRHOT
       INTEGER NWANTP
       INTEGER  LISTP(MAXPRO)


!-----------------------------------------------------------------------
!      LOCAL VARIABLES
!-----------------------------------------------------------------------
       INTEGER I
       INTEGER IARGC
       INTEGER IP
       INTEGER IPJUNK(MXGAS+1)  ! junk gas id work array
       INTEGER J
       INTEGER K
       INTEGER NARGS   ! number of arguments
       INTEGER SORTED  ! flag for sorting

       CHARACTER*80 BUF
       CHARACTER*80 VAL
       CHARACTER*80 VAR

       LOGICAL LLISTP
       LOGICAL STR2BO

!-----------------------------------------------------------------------
!      SAVE STATEMENTS
!-----------------------------------------------------------------------
!      none


!***********************************************************************
!***********************************************************************
!      EXECUTABLE CODE begins below
!***********************************************************************
!***********************************************************************

!      ------------
!      Set defaults
!      ------------
       FIN='./data/sarta_in.rtp'                 ! input filename
       FOUT='./data/sarta_out.rtp'               ! output filename
       NWANTP=1   ! -1 do all profiles found in input file
       LRHOT=.FALSE. ! use input rho for reflected thermal
!
!      -----------------------------------------------------------------
!      Loop on program parameters
!      --------------------------
!      Determine the number of command-line arguments
       NARGS=IARGC()
!
!      Loop over the command-line arguments
       LLISTP=.FALSE.
       DO I = 1, NARGS
!
!         Pull out the ith argument
          CALL GETARG(I, BUF)
!
!         Find the "=" character in the command-line argument string
          J=INDEX(BUF, '=')
!
          IF (J .NE. 0) THEN
!
!            Name of variable
             VAR = BUF(1:J-1)
             CALL UPCASE(VAR)
!
!            Specified value
             VAL = BUF(J+1:LEN(BUF))
!
!            Big "IF" to set parameters
!            ----------------------------
             IF (VAR(1:3) .EQ. 'FIN') THEN
                FIN=VAL

             ELSEIF (VAR(1:4) .EQ. 'FOUT') THEN
                FOUT=VAL

             ELSEIF (VAR(1:5) .EQ. 'LRHOT') THEN
                LRHOT=STR2BO(VAL)

             ELSEIF (VAR(1:5) .EQ. 'LISTP') THEN
                LLISTP=.TRUE.
!
!               Read the indices of the desired profiles
                K=1
 10             IF (K .GT. MAXPRO) THEN
                   WRITE(6,1017)
 1017              FORMAT('ERROR! bad LISTP, ',&
                   'either an unrecognized value or too many entries')
                   STOP
                ENDIF
                READ(VAL,*,END=19) (IPJUNK(IP),IP=1,K)
                K=K + 1  ! increment count of profiles
                GOTO 10  ! loop to next entry
 19             CONTINUE
                K=K - 1  ! number of profiles
                DO IP=1,K
                   LISTP(IP)=IPJUNK(IP)
                ENDDO
                NWANTP=K
!

             ELSE
                WRITE(6,1020) VAR(1:6)
 1020           FORMAT('Unknown command-line argument: ',A6)
                STOP

             ENDIF

          ENDIF
       ENDDO  ! end of loop over command-line arguments
!      -----------------------------------------------------------------


!      -------------------------------------
!      Sort prof numbers & check for repeats
!      -------------------------------------
       IF (NWANTP .GT. 0) THEN
!
!         Sort in ascending order
          SORTED=1  ! initialize flag for first pass
 30       IF (SORTED .EQ. 1) THEN
             SORTED=0  ! initialize flag for this loop
             DO K=1,NWANTP-1
                IF (LISTP(K) .GT. LISTP(K+1)) THEN
                   IP=LISTP(K)
                   LISTP(K)=LISTP(K+1)
                   LISTP(K+1)=IP
                   SORTED=1  ! set flag to indicate ordering was altered
                ENDIF
             ENDDO
             GOTO 30
          ENDIF
!
!         Check for repeats
          DO K=1,NWANTP-1
             IF (LISTP(K) .EQ. LISTP(K+1)) THEN
                WRITE(6,1045) LISTP(K)
 1045           FORMAT('ERROR! profile ',I2,&
                ' appears more than once in LISTP')
                STOP
             ENDIF
          ENDDO

       ENDIF
!
       RETURN
       END
