c this computes planck for layers and for surface

      SUBROUTINE planckemis(NCHAN,LBOT,TEMP,FREQ,EMIS,TSURF,RAAPLNCK,RASURFE)

      IMPLICIT NONE

      include 'incFTC.f'
      include 'rtpdefs.f'

c output
       REAL    RAAPLNCK(MAXLAY,MXCHAN) ! chan radiance at each lay
       REAL    RASURFE(MXCHAN) ! chan radiance at surf

c input
       INTEGER NCHAN,LBOT
       REAL   FREQ(MXCHAN)    ! chan center frequency      
       REAL   TEMP(MAXLAY)  ! prof layer average temperature
       REAL   TSURF         ! surface temperature
       REAL   EMIS(MXCHAN)  ! chan surface emissivity
       
c local
       INTEGER I,L
       REAL C1V3,C2V

       DO I=1,NCHAN

C         Radiation constants for current channel
          C1V3=C1*(FREQ(I)**3)
          C2V=C2*FREQ(I)

C         Calculate Planck & clear airs trans for full layers
          DO L=1,LBOT-1
             RAAPLNCK(L,I)=C1V3/( EXP( C2V/TEMP(L) ) - 1.0 )
          ENDDO
C         Note: TEMP(LBOT) already adjusted for bottom fractional layer
          RAAPLNCK(LBOT,I)=C1V3/( EXP( C2V/TEMP(LBOT) ) - 1.0 )

          RASURFE(I) = EMIS(I)*C1V3/( EXP( C2V/TSURF ) - 1.0 )
        END DO

        RETURN
	END
