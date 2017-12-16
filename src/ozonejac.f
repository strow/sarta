ccc      note the alternative return to statement 77
ccc      https://docs.oracle.com/cd/E19957-01/805-4939/6j4m0vnb3/index.html

      SUBROUTINE OzoneJac(*,IO3ZLAYJAC,IDOO3ZJAC,IOUNJ,IPROF,LBOT,NCHAN,DST,DQ,
     $       TAU,TAUZ,TAUSN,TAUZSN,CO2TOP,
     $	     FIXMUL,CONPRD,FPRED1,FPRED2,FPRED3,FPRED4,FPRED5,FPRED6,FPRED7,
     $       WPRED1,WPRED2,WPRED3,WPRED4,WPRED5,WPRED6,WPRED7,
     $       OPRED1,OPRED2,OPRED4,OPRED5,OPRED6,OPRED7,
     $       MPRED3,CPRED4,TRCPRD,CO2MLT,SO2MLT,HNOMLT,N2OMLT,
     $       TAU0,TAUZ0,TAUSN0,TAUZSN0,CO2TOP0,
     $	     FIXMUL0,CONPRD0,FPRED10,FPRED20,FPRED30,FPRED40,FPRED50,FPRED60,FPRED70,
     $       WPRED10,WPRED20,WPRED30,WPRED40,WPRED50,WPRED60,WPRED70,
     $       OPRED10,OPRED20,OPRED40,OPRED50,OPRED60,OPRED70,
     $       MPRED30,CPRED40,TRCPRD0,CO2MLT0,SO2MLT0,HNOMLT0,N2OMLT0,	 
     $       TEMPX,WAMNTX,OAMNTX,TEMP,WAMNT,OAMNT,
     $       ISELECTLAY,RAD)

C-----------------------------------------------------------------------
C      IMPLICIT NONE
C-----------------------------------------------------------------------
       IMPLICIT NONE

C-----------------------------------------------------------------------
C      INCLUDE FILES
C-----------------------------------------------------------------------
       include 'incFTC.f'
       include 'rtpdefs.f'

       REAL CO2TOP                ! top layers CO2 mixing ratio
       REAL FIXMUL(MAXLAY)        ! "fixed" amount multiplier (~1)
       REAL CONPRD( N1CON,MAXLAY) ! water continuum predictors
       REAL FPRED1( N1FIX,MAXLAY) ! set1 "fixed" predictors
       REAL FPRED2( N2FIX,MAXLAY) ! set2 "fixed" predictors
       REAL FPRED3( N3FIX,MAXLAY) ! set3 "fixed" predictors
       REAL FPRED4( N4FIX,MAXLAY) ! set4 "fixed" predictors
       REAL FPRED5( N5FIX,MAXLAY) ! set5 "fixed" predictors
       REAL FPRED6( N6FIX,MAXLAY) ! set6 "fixed" predictors
       REAL FPRED7( N7FIX,MAXLAY) ! set7 "fixed" predictors
       REAL WPRED1( N1H2O,MAXLAY) ! set1 water predictors
       REAL WPRED2( N2H2O,MAXLAY) ! set2 water predictors
       REAL WPRED3( N3H2O,MAXLAY) ! set3 water predictors
       REAL WPRED4( N4H2O,MAXLAY) ! set4 water predictors
       REAL WPRED5( N5H2O,MAXLAY) ! set5 water predictors
       REAL WPRED6( N6H2O,MAXLAY) ! set6 water predictors
       REAL WPRED7( N7H2O,MAXLAY) ! set7 water predictors
       REAL OPRED1(  N1O3,MAXLAY) ! set1 ozone predictors
       REAL OPRED2(  N2O3,MAXLAY) ! set2 ozone predictors
       REAL OPRED4(  N4O3,MAXLAY) ! set4 ozone predictors
       REAL OPRED5(  N5O3,MAXLAY) ! set5 ozone predictors
       REAL OPRED6(  N6O3,MAXLAY) ! set6 ozone predictors
       REAL OPRED7(  N7O3,MAXLAY) ! set7 ozone predictors
       REAL MPRED3( N3CH4,MAXLAY) ! set3 methane predictors
       REAL CPRED4(  N4CO,MAXLAY) ! set4 carbon monoxide predictors
       REAL TRCPRD(NTRACE,MAXLAY) ! trace gas pert perdictors
       REAL CO2MLT(MAXLAY)        ! CO2 perturbation multiplier
       REAL SO2MLT(MAXLAY)        ! SO2 perturbation multiplier
       REAL HNOMLT(MAXLAY)        ! HNO3 perturbation multiplier
       REAL N2OMLT(MAXLAY)        ! N2O perturbation multiplier
c
c original values
       REAL CO2TOP0                ! top layers CO2 mixing ratio
       REAL FIXMUL0(MAXLAY)        ! "fixed" amount multiplier (~1)
       REAL CONPRD0( N1CON,MAXLAY) ! water continuum predictors
       REAL FPRED10( N1FIX,MAXLAY) ! set1 "fixed" predictors
       REAL FPRED20( N2FIX,MAXLAY) ! set2 "fixed" predictors
       REAL FPRED30( N3FIX,MAXLAY) ! set3 "fixed" predictors
       REAL FPRED40( N4FIX,MAXLAY) ! set4 "fixed" predictors
       REAL FPRED50( N5FIX,MAXLAY) ! set5 "fixed" predictors
       REAL FPRED60( N6FIX,MAXLAY) ! set6 "fixed" predictors
       REAL FPRED70( N7FIX,MAXLAY) ! set7 "fixed" predictors
       REAL WPRED10( N1H2O,MAXLAY) ! set1 water predictors
       REAL WPRED20( N2H2O,MAXLAY) ! set2 water predictors
       REAL WPRED30( N3H2O,MAXLAY) ! set3 water predictors
       REAL WPRED40( N4H2O,MAXLAY) ! set4 water predictors
       REAL WPRED50( N5H2O,MAXLAY) ! set5 water predictors
       REAL WPRED60( N6H2O,MAXLAY) ! set6 water predictors
       REAL WPRED70( N7H2O,MAXLAY) ! set7 water predictors
       REAL OPRED10(  N1O3,MAXLAY) ! set1 ozone predictors
       REAL OPRED20(  N2O3,MAXLAY) ! set2 ozone predictors
       REAL OPRED40(  N4O3,MAXLAY) ! set4 ozone predictors
       REAL OPRED50(  N5O3,MAXLAY) ! set5 ozone predictors
       REAL OPRED60(  N6O3,MAXLAY) ! set6 ozone predictors
       REAL OPRED70(  N7O3,MAXLAY) ! set7 ozone predictors
       REAL MPRED30( N3CH4,MAXLAY) ! set3 methane predictors
       REAL CPRED40(  N4CO,MAXLAY) ! set4 carbon monoxide predictors
       REAL TRCPRD0(NTRACE,MAXLAY) ! trace gas pert perdictors
       REAL CO2MLT0(MAXLAY)        ! CO2 perturbation multiplier
       REAL SO2MLT0(MAXLAY)        ! SO2 perturbation multiplier
       REAL HNOMLT0(MAXLAY)        ! HNO3 perturbation multiplier
       REAL N2OMLT0(MAXLAY)        ! N2O perturbation multiplier

C
C      for CALOWP
c       INTEGER LOPMIN
c       INTEGER LOPMAX
       REAL  WAANG(MAXLAY)
       REAL H2OPRD(  NH2O,MXOWLY)
       LOGICAL LOPUSE(MXOWLY)
       INTEGER LOPLOW(MAXLAY)
       REAL  DAOP(MAXLAY)

       REAL  WAANG0(MAXLAY)
       REAL H2OPRD0(  NH2O,MXOWLY)
       LOGICAL LOPUSE0(MXOWLY)
       INTEGER LOPLOW0(MAXLAY)
       REAL  DAOP0(MAXLAY)
C
C      for CALT
       REAL    TAU(MAXLAY,MXCHAN) ! chan layer effective optical depth
       REAL   TAUZ(MAXLAY,MXCHAN) ! chan surface-to-space trans
       REAL TAUSN(MAXLAY,MXCHAN)  ! sun OD       
       REAL TAUZSN(MAXLAY,MXCHAN) ! sun space-to-surface-to-space OD
       REAL   WAOP(MXOWLY)        ! OPTRAN abs coef scaling factor
       
       REAL   TAU0(MAXLAY,MXCHAN) ! chan layer effective optical depth, copy
       REAL  TAUZ0(MAXLAY,MXCHAN) ! chan surface-to-space trans, copy
       REAL TAUSN0(MAXLAY,MXCHAN)  ! sun OD              
       REAL TAUZSN0(MAXLAY,MXCHAN) ! sun space-to-surface-to-space OD       
       REAL   WAOP0(MXOWLY)       ! OPTRAN abs coef scaling factor       

       INTEGER IO3ZLAYJAC,IDOO3ZJAC,IOUNJ,IPROF,LBOT,NCHAN

       REAL RAD(MXCHAN)
       INTEGER ISELECTLAY
       
       REAL   TEMP(MAXLAY) ! prof layer average temperature
       REAL  WAMNT(MAXLAY) ! prof layer water (H2O) amount
       REAL  OAMNT(MAXLAY) ! prof layer ozone (O3) amount

       REAL   TEMPX(MAXLAY) ! prof layer average temperature
       REAL  WAMNTX(MAXLAY) ! prof layer water (H2O) amount
       REAL  OAMNTX(MAXLAY) ! prof layer ozone (O3) amount

       REAL DST,DQ
c local var
       INTEGER IJAC,J
       
c------------------------------------------------------------------------
         !! OZONE JAC
	 IF (IO3ZLAYJAC .EQ. -1) THEN
           write(IOUNJ) IPROF,+300
	   IO3ZLAYJAC = 0
	 END IF
	 
 771     CONTINUE
         IDOO3ZJAC = IDOO3ZJAC * -1
	 IF ((IDOO3ZJAC .GT. 0) .AND. (IO3ZLAYJAC .LT. LBOT)) THEN

	   CALL copypredictors(-1,NCHAN,
     $       TAU,TAUZ,TAUSN,TAUZSN,CO2TOP,
     $	     FIXMUL,CONPRD,FPRED1,FPRED2,FPRED3,FPRED4,FPRED5,FPRED6,FPRED7,
     $       WPRED1,WPRED2,WPRED3,WPRED4,WPRED5,WPRED6,WPRED7,
     $       OPRED1,OPRED2,OPRED4,OPRED5,OPRED6,OPRED7,
     $       MPRED3,CPRED4,TRCPRD,CO2MLT,SO2MLT,HNOMLT,N2OMLT,
     $       TAU0,TAUZ0,TAUSN0,TAUZSN0,CO2TOP0,
     $	     FIXMUL0,CONPRD0,FPRED10,FPRED20,FPRED30,FPRED40,FPRED50,FPRED60,FPRED70,
     $       WPRED10,WPRED20,WPRED30,WPRED40,WPRED50,WPRED60,WPRED70,
     $       OPRED10,OPRED20,OPRED40,OPRED50,OPRED60,OPRED70,
     $       MPRED30,CPRED40,TRCPRD0,CO2MLT0,SO2MLT0,HNOMLT0,N2OMLT0)

           DO IJAC = 1,MAXLAY
             TEMPX(IJAC)  = TEMP(IJAC)
             WAMNTX(IJAC) = WAMNT(IJAC)
             OAMNTX(IJAC) = OAMNT(IJAC)	 
           END DO

 	   IO3ZLAYJAC = IO3ZLAYJAC + 1
c	   print *,'IPROF,OZlayjac,LBOT = ',IPROF,IO3ZLAYJAC,LBOT
  	   OAMNTX(IO3ZLAYJAC) = OAMNTX(IO3ZLAYJAC)*(1.0 + DQ)
	   !ISELECTLAY = -1	   !!! testing, but slow since it makes sarta re-run and re-run
	   ISELECTLAY = IO3ZLAYJAC	   
  	   RETURN 1
	 ELSE
           write(IOUNJ) (1000.0*RAD(J),J=1,NCHAN)
	   IF (IO3ZLAYJAC .LT. LBOT) THEN
  	     GOTO 771
	   ELSE
	     IF (LBOT+1 .LE. 100) THEN
  	       DO IJAC = LBOT+1,100
                 write(IOUNJ) (000.0*RAD(J),J=1,NCHAN)
	       END DO
             END IF	     
	   END IF
	 END IF

      RETURN
      END
      
