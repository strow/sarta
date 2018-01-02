c this computes planck for layers and for surface
c       INTEGER IWHICHLAY   !if      -1,    then this is default temp (so can set RAAPLNCK,RASURFE)
c                           !else if +9999, then just use what was set above
c                           !else if -9999, then just use what was set above for RAAPLNCK, adjust RASURFE
c                           !else if >0,    then just redo that layer RAAPLNCK, keep RASURFE same

      SUBROUTINE planckemis(NCHAN,NLAY,TEMP,FREQ,EMIS,TSURF,RAAPLNCK,RASURFE, IWHICHLAY)

      IMPLICIT NONE

      include 'incFTC.f'
      include 'rtpdefs.f'

c output
       REAL    RAAPLNCK(MAXLAY,MXCHAN) ! chan radiance at each lay
       REAL    RASURFE(MXCHAN) ! chan radiance at surf

c input
       INTEGER NCHAN,NLAY,IWHICHLAY
       REAL   FREQ(MXCHAN)    ! chan center frequency      
       REAL   TEMP(MAXLAY)  ! prof layer average temperature
       REAL   TSURF         ! surface temperature
       REAL   EMIS(MXCHAN)  ! chan surface emissivity
       
c local
       INTEGER I,L,LTOP,LBOT
       REAL C1V3,C2V

       IF ((IWHICHLAY .LT. 0) .AND. (IWHICHLAY .GT. -9999)) THEN
         LTOP = 1
         LBOT = NLAY
       ELSEIF (IWHICHLAY .LE. -9999) THEN
         LTOP = 1
         LBOT = -NLAY
       ELSE
         LTOP = IWHICHLAY
         LBOT = IWHICHLAY
	 IF ((NLAY-LBOT) .LE. 3) LBOT = NLAY
       END IF
       
       DO I=1,NCHAN

C         Radiation constants for current channel
          C1V3=C1*(FREQ(I)**3)
          C2V=C2*FREQ(I)

C         Calculate Planck & clear airs trans for full layers
          DO L=LTOP,LBOT
             RAAPLNCK(L,I)=C1V3/( EXP( C2V/TEMP(L) ) - 1.0 )
          ENDDO
	  
C         Note: TEMP(NLAY) already adjusted for bottom fractional layer
C         SO ABSORB THIS INTO ABOVE LOOP
c          RAAPLNCK(NLAY,I)=C1V3/( EXP( C2V/TEMP(NLAY) ) - 1.0 )
        END DO

        IF (IWHICHLAY .LT. 0) THEN
          DO I=1,NCHAN
            C1V3=C1*(FREQ(I)**3)
            C2V=C2*FREQ(I)		  
            RASURFE(I) = EMIS(I)*C1V3/( EXP( C2V/TSURF ) - 1.0 )
          END DO
	END IF
	
        RETURN
	END