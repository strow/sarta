c rdinfo processes command line arguments
c
c       sarta  fin=input.rtp  fout=output.rtp  listp=1,2,3
c
c
c to compile
c   Absoft/Linux: f77 -N109 -o klayers $(SRC) -lU77
c   SGI Irix: no special compiler options are needed
C=======================================================================
C=======================================================================
C
C              University of Maryland Baltimore County [UMBC]
C
C              AIRS
C
C              RDINFO_sarta
C
!F77====================================================================


!ROUTINE NAME: RDINFO


!ABSTRACT:
C    Get info about the sarta run: the names of input & output
C    files, the channel list, and list of profile numbers.


!CALL PROTOCOL:
C    RDINFO(FIN, FOUT, LRHOT, NWANTP, LISTP)


!INPUT PARAMETERS:
C    none


!OUTPUT PARAMETERS:
C    type      name    purpose                     units
C    --------  ------  --------------------------  ---------------------
C    CHAR*80   FIN     input filename              none
C    CHAR*80   FOUT    output filename             none
C    LOGICAL   LRHOT   force RHO for refl thermal? none
C    INTEGER   NWANTP  Number of desired profiles  none
C    INT arr   LISTP   List of desired prof nums   none



!INPUT/OUTPUT PARAMETERS: none


!RETURN VALUES: none


!PARENT(S): SARTA_rtp


!ROUTINES CALLED:
C    none


!FILES ACCESSED:
C    none


!COMMON BLOCKS:
C      none


!DESCRIPTION:
C    Gets various info about sarta run.
C
C    Each command line argument is of the form <variable>=<value>
C    Each <variable>=<value> string must be 80 char or less.  It is
C    not necessary to enclose values in quotes unless they contain
C    blanks.  The recognized command-line variables are:
C
C    fin : name of input file
C
C    fout : name of output file
C
C    lrhot : force reflected thermal rho?; logical. true/false of T/F
C       If true, the refl therm will use rho=(1-emis)/pi rather than
C       the rho (if any) from the input file.
C
C    listp : list of desired profile numbers (all other profiles will
C       be ignored).  If "listp" is not specified, SARTA will process
C       all profiles.
C
C       The listp profile numbers may be specified either as a
C       sequence of integers separated by a comma, or alternately as
C       a quoted string containing integers separated by a blank space.
C       Examples:
C          listp=1,2,3,4,5
C          listp='1 2 3 4 5'
C       Due to the 80 char limit, the maximum number of entries
C       in listp is limited.  (Eg 15 four digit numbers, or
C       25 two digit numbers.  MAXPRO is the hardcoded limit.)
C

!ALGORITHM REFERENCES: see DESCRIPTION


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date     Programmer        Comments
C------------ ----------------- ----------------------------------------
C 13 Feb 2001 H.Motteler/S.Hannon Re-write of KLAYERS version
C 28 Nov 2001 Scott Hannon      Remove command-line argument "nwantp"
C  5 Dec 2001 Scott Hannon      Remove unused local var LENNB
C 05 Aug 2003 Scott Hannon      Correct FIN & FOUT to CHAR*80 (not 70)
C 06 Feb 2004 Scott Hannon      Add LRHOT argument and associated code


!END====================================================================


C      =================================================================
       SUBROUTINE RDINFO(FIN, FOUT, LRHOT, NWANTP, LISTP)
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
C      From "util.f"
C      subroutine UPCASE = converts a string to upper case
C      function STR2BO = converts true/false string to boolean (LOGICAL)


C-----------------------------------------------------------------------
C      ARGUMENTS
C-----------------------------------------------------------------------
C      Input:
C      none
C
C      Output:
       CHARACTER*80 FIN
       CHARACTER*80 FOUT
       LOGICAL  LRHOT
       INTEGER NWANTP
       INTEGER  LISTP(MAXPRO)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
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

C-----------------------------------------------------------------------
C      SAVE STATEMENTS
C-----------------------------------------------------------------------
C      none


C***********************************************************************
C***********************************************************************
C      EXECUTABLE CODE begins below
C***********************************************************************
C***********************************************************************

C      ------------
C      Set defaults
C      ------------
       FIN='sarta_in.rtp'                 ! input filename
       FOUT='sarta_out.rtp'               ! output filename
       NWANTP=-1   ! do sarta for all profiles found in input file
       LRHOT=.FALSE. ! use input rho for reflected thermal
C
C      -----------------------------------------------------------------
C      Loop on program parameters
C      --------------------------
C      Determine the number of command-line arguments
       NARGS=IARGC()
C
C      Loop over the command-line arguments
       LLISTP=.FALSE.
       DO I = 1, NARGS
C
C         Pull out the ith argument
          CALL GETARG(I, BUF)
C
C         Find the "=" character in the command-line argument string
          J=INDEX(BUF, '=')
C
          IF (J .NE. 0) THEN
C
C            Name of variable
             VAR = BUF(1:J-1)
             CALL UPCASE(VAR)
C
C            Specified value
             VAL = BUF(J+1:LEN(BUF))
C
C            Big "IF" to set parameters
C            ----------------------------
             IF (VAR(1:3) .EQ. 'FIN') THEN
                FIN=VAL

             ELSEIF (VAR(1:4) .EQ. 'FOUT') THEN
                FOUT=VAL

             ELSEIF (VAR(1:5) .EQ. 'LRHOT') THEN
                LRHOT=STR2BO(VAL)

             ELSEIF (VAR(1:5) .EQ. 'LISTP') THEN
                LLISTP=.TRUE.
C
C               Read the indices of the desired profiles
                K=1
 10             IF (K .GT. MAXPRO) THEN
                   WRITE(6,1017)
 1017              FORMAT('ERROR! bad LISTP, ',
     $             'either an unrecognized value or too many entries')
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
C

             ELSE
                WRITE(6,1020) VAR(1:6)
 1020           FORMAT('Unknown command-line argument: ',A6)
                STOP

             ENDIF

          ENDIF
       ENDDO  ! end of loop over command-line arguments
C      -----------------------------------------------------------------


C      -------------------------------------
C      Sort prof numbers & check for repeats
C      -------------------------------------
       IF (NWANTP .GT. 0) THEN
C
C         Sort in ascending order
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
C
C         Check for repeats
          DO K=1,NWANTP-1
             IF (LISTP(K) .EQ. LISTP(K+1)) THEN
                WRITE(6,1045) LISTP(K)
 1045           FORMAT('ERROR! profile ',I2,
     $          ' appears more than once in LISTP')
                STOP
             ENDIF
          ENDDO

       ENDIF
C
       RETURN
       END
