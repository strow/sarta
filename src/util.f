C----------------------------------------------------------------------
C Utility functions & subroutines
C----------------------------------------------------------------------
C
C----------------------------------------------------------------------
C
       LOGICAL FUNCTION STR2BO(BUF)
C
C      Translate "true"/"false" string to boolean
C
       include 'incFTC.f'
C
       CHARACTER*(*) BUF
C
       INTEGER I
       INTEGER J
       CHARACTER*1 XCHAR
C      -------------------
C
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
C
       RETURN
       END
C
C----------------------------------------------------------------------
C
       SUBROUTINE UPCASE(BUF)
C
C      Convert a string to upper case
C
       CHARACTER*(*) BUF
C
       INTEGER I
       INTEGER IC
       INTEGER J
C      -----------------
C
       DO I=1,LEN(BUF)
          IC=ICHAR( BUF(I:I) )
          IF (IC .GE. ICHAR('a') .AND. IC .LE. ICHAR('z')) THEN
             J=IC + (ICHAR('A') - ICHAR('a'))
             BUF(I:I)=CHAR(J)
          ENDIF
       ENDDO
C
       RETURN
       END
C
C----------------------------------------------------------------------
C
       INTEGER FUNCTION LENNB(BUF)
C
C      Find the index of the last non-blank char in a string
C
       CHARACTER*(*) BUF
C
       INTEGER I
C      -----------------
C
       I=LEN(BUF)
 10    IF (I .GT. 0) THEN
          IF (BUF(I:I) .EQ. ' ') THEN
             I=I - 1
             GOTO 10
          ENDIF
       ENDIF
       LENNB=I
C
       RETURN
       END
C
C----------------------------------------------------------------------
C
       SUBROUTINE N2BITS(NUMBER,LFLAGS)
C
C      Converts an integer number into a logical(32) array
C
C      Input:
       INTEGER*4 NUMBER
C
C      Ouput:
       LOGICAL LFLAGS(32)
C
C      Local:
       INTEGER N
       INTEGER I2NM1
       INTEGER IWORK
C      -----------------
C
       IWORK=NUMBER
C      Note: 1st bit which is for +- sign
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
C
       RETURN
       END
C
C-----------------------------------------------------------------------
C
       SUBROUTINE BITS2N(NUMBER,LFLAGS)
C
C      Converts a logical(32) array into an integer number
C
C      Input:
       LOGICAL LFLAGS(32)
C
C      Ouput:
       INTEGER*4 NUMBER
C
C      Local:
       INTEGER N
       INTEGER I2NM1
       INTEGER IWORK
C      ---------------
C
       IWORK=0
C      Note: ignore 1st bit which is for +- sign
       DO N=1,31
          I2NM1=2**(N-1)
          IF (LFLAGS(N)) THEN
             IWORK=IWORK + I2NM1
          ENDIF
       ENDDO
       IF (LFLAGS(32)) IWORK=-IWORK
C
       NUMBER=IWORK
C
       RETURN
       END
C
C-----------------------------------------------------------------------
