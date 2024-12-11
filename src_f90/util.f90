!----------------------------------------------------------------------
! Utility functions & subroutines
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!
       LOGICAL FUNCTION STR2BO(BUF)
!
!      Translate "true"/"false" string to boolean
!
       use incFTC
!
       CHARACTER*(*) BUF
!
       INTEGER I
       INTEGER J
       CHARACTER*1 XCHAR
!      -------------------
!
       XCHAR=BUF(1:1)
       IF (XCHAR .EQ. 'T' .OR. XCHAR .EQ. 't') THEN
          STR2BO=.TRUE.
       ELSEIF (XCHAR .EQ. 'F' .OR. XCHAR .EQ. 'f') THEN
          STR2BO=.FALSE.
       ELSE
          I=LEN(BUF)
          J=MIN(I,5)
          WRITE(IOERR,1010) BUF(1:J)
 1010     FORMAT('Error, unexpected string boolean=',A5)
          STOP
       ENDIF
!
       RETURN
       END
!
!----------------------------------------------------------------------
!
       SUBROUTINE UPCASE(BUF)
!
!      Convert a string to upper case
!
       CHARACTER*(*) BUF
!
       INTEGER I
       INTEGER IC
       INTEGER J
!      -----------------
!
       DO I=1,LEN(BUF)
          IC=ICHAR( BUF(I:I) )
          IF (IC .GE. ICHAR('a') .AND. IC .LE. ICHAR('z')) THEN
             J=IC + (ICHAR('A') - ICHAR('a'))
             BUF(I:I)=CHAR(J)
          ENDIF
       ENDDO
!
       RETURN
       END
!
!----------------------------------------------------------------------
!
       INTEGER FUNCTION LENNB(BUF)
!
!      Find the index of the last non-blank char in a string
!
       CHARACTER*(*) BUF
!
       INTEGER I
!      -----------------
!
       I=LEN(BUF)
 10    IF (I .GT. 0) THEN
          IF (BUF(I:I) .EQ. ' ') THEN
             I=I - 1
             GOTO 10
          ENDIF
       ENDIF
       LENNB=I
!
       RETURN
       END
!
!----------------------------------------------------------------------
!
       SUBROUTINE N2BITS(NUMBER,LFLAGS)
!
!      Converts an integer number into a logical(32) array
!
!      Input:
       INTEGER*4 NUMBER
!
!      Ouput:
       LOGICAL LFLAGS(32)
!
!      Local:
       INTEGER N
       INTEGER I2NM1
       INTEGER IWORK
!      -----------------
!
       IWORK=NUMBER
!      Note: 1st bit which is for +- sign
       IF (IWORK .LT. 0) THEN
          IWORK=ABS(IWORK)
          LFLAGS(32)=.TRUE.
       ELSE
          LFLAGS(32)=.FALSE.
       ENDIF
       DO N=31,1,-1
          I2NM1=2**(N-1)
          IF (IWORK .GE. I2NM1) THEN
             LFLAGS(N)=.TRUE.
             IWORK=IWORK - I2NM1
          ELSE
             LFLAGS(N)=.FALSE.
          ENDIF
       ENDDO
!
       RETURN
       END
!
!-----------------------------------------------------------------------
!
       SUBROUTINE BITS2N(NUMBER,LFLAGS)
!
!      Converts a logical(32) array into an integer number
!
!      Input:
       LOGICAL LFLAGS(32)
!
!      Ouput:
       INTEGER*4 NUMBER
!
!      Local:
       INTEGER N
       INTEGER I2NM1
       INTEGER IWORK
!      ---------------
!
       IWORK=0
!      Note: ignore 1st bit which is for +- sign
       DO N=1,31
          I2NM1=2**(N-1)
          IF (LFLAGS(N)) THEN
             IWORK=IWORK + I2NM1
          ENDIF
       ENDDO
       IF (LFLAGS(32)) IWORK=-IWORK
!
       NUMBER=IWORK
!
       RETURN
       END
!
!-----------------------------------------------------------------------
