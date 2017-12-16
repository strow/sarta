c this subroutine makes copies of the predictors

      SUBROUTINE copypredictors(IDIR,NCHAN,
     $     TAU,TAUZ,TAUSN,TAUZSN,CO2TOP,
     $	   FIXMUL,CONPRD,FPRED1,FPRED2,FPRED3,FPRED4,FPRED5,FPRED6,FPRED7,
     $     WPRED1,WPRED2,WPRED3,WPRED4,WPRED5,WPRED6,WPRED7,
     $     OPRED1,OPRED2,OPRED4,OPRED5,OPRED6,OPRED7,
     $     MPRED3,CPRED4,TRCPRD,CO2MLT,SO2MLT,HNOMLT,N2OMLT,
     $     TAU0,TAUZ0,TAUSN0,TAUZSN0,CO2TOP0,
     $	   FIXMUL0,CONPRD0,FPRED10,FPRED20,FPRED30,FPRED40,FPRED50,FPRED60,FPRED70,
     $     WPRED10,WPRED20,WPRED30,WPRED40,WPRED50,WPRED60,WPRED70,
     $     OPRED10,OPRED20,OPRED40,OPRED50,OPRED60,OPRED70,
     $     MPRED30,CPRED40,TRCPRD0,CO2MLT0,SO2MLT0,HNOMLT0,N2OMLT0)


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

       INTEGER IDIR,NCHAN
       
c local
       INTEGER J,K

       IF (IDIR .EQ. +1) THEN

         CO2TOP0 = CO2TOP
	 
         DO J = 1,MXOWLY
	   LOPUSE0(J) = LOPUSE(J)
	   WAOP0(J)   = WAOP(J)
	   DO K = 1,NH2O
	     H2OPRD0(K,J) = H2OPRD(K,J)
	   END DO
	 END DO
	 
         DO J = 1,NCHAN
           DO K = 1,MAXLAY       
  	     TAU0(K,J)    = TAU(K,J)
 	     TAUZ0(K,J)   = TAUZ(K,J)
 	     TAUSN0(K,J)  = TAUSN(K,J)	     	     
 	     TAUZSN0(K,J) = TAUZSN(K,J)	     
	   END DO
	 END DO

         DO J = 1,MAXLAY
	   FIXMUL0(J) = FIXMUL(J)
	   CO2MLT0(J) = CO2MLT(J)
	   SO2MLT0(J) = SO2MLT(J)
	   HNOMLT0(J) = HNOMLT(J)
	   N2OMLT0(J) = N2OMLT(J)	   	   	   
           WAANG0(J)  = WAANG(J)
           LOPLOW0(J) = LOPLOW(J)
	   DAOP0(J)   = DAOP(J)
	   
	   DO K = 1,NTRACE
	     TRCPRD0(K,J) = TRCPRD(K,J)
	   END DO

	   DO K = 1,N1CON
	     CONPRD0(K,J) = CONPRD(K,J)
	   END DO
	   DO K = 1,N3CH4
	     MPRED30(K,J) = MPRED3(K,J)
	   END DO
	   DO K = 1,N4CO
	     CPRED40(K,J) = CPRED4(K,J)
	   END DO
	   
	   DO K = 1,N1FIX
	     FPRED10(K,J) = FPRED1(K,J)
	   END DO
	   DO K = 1,N2FIX
	     FPRED20(K,J) = FPRED2(K,J)
	   END DO
	   DO K = 1,N3FIX
	     FPRED30(K,J) = FPRED3(K,J)
	   END DO
	   DO K = 1,N4FIX
	     FPRED40(K,J) = FPRED4(K,J)
	   END DO
   	   DO K = 1,N5FIX
	     FPRED50(K,J) = FPRED5(K,J)
	   END DO
	   DO K = 1,N6FIX
	     FPRED60(K,J) = FPRED6(K,J)
	   END DO
	   DO K = 1,N7FIX
	     FPRED70(K,J) = FPRED7(K,J)
	   END DO

	   DO K = 1,N1H2O
	     WPRED10(K,J) = WPRED1(K,J)
	   END DO
	   DO K = 1,N2H2O
	     WPRED20(K,J) = WPRED2(K,J)
	   END DO
	   DO K = 1,N3H2O
	     WPRED30(K,J) = WPRED3(K,J)
	   END DO
	   DO K = 1,N4H2O
	     WPRED40(K,J) = WPRED4(K,J)
	   END DO
	   DO K = 1,N5H2O
	     WPRED50(K,J) = WPRED5(K,J)
	   END DO
	   DO K = 1,N6H2O
	     WPRED60(K,J) = WPRED6(K,J)
	   END DO
	   DO K = 1,N7H2O
	     WPRED70(K,J) = WPRED7(K,J)
	   END DO

	   DO K = 1,N1O3
	     OPRED10(K,J) = OPRED1(K,J)
	   END DO
	   DO K = 1,N2O3
	     OPRED20(K,J) = OPRED2(K,J)
	   END DO
c	   DO K = 1,N3O3
c	     OPRED30(K,J) = OPRED3(K,J)
c	   END DO
	   DO K = 1,N4O3
	     OPRED40(K,J) = OPRED4(K,J)
	   END DO
	   DO K = 1,N5O3
	     OPRED50(K,J) = OPRED5(K,J)
	   END DO
	   DO K = 1,N6O3
	     OPRED60(K,J) = OPRED6(K,J)
	   END DO
	   DO K = 1,N7O3
	     OPRED70(K,J) = OPRED7(K,J)
	   END DO
         END DO
	 
       ELSE

         CO2TOP = CO2TOP0
	 
         DO J = 1,MXOWLY
	   LOPUSE(J) = LOPUSE0(J)
	   WAOP(J)   = WAOP0(J)
	   DO K = 1,NH2O
	     H2OPRD(K,J) = H2OPRD0(K,J)
	   END DO
	 END DO
	 
         DO J = 1,NCHAN
           DO K = 1,MAXLAY       
  	     TAU(K,J)    = TAU0(K,J)
 	     TAUZ(K,J)   = TAUZ0(K,J)
 	     TAUSN(K,J)  = TAUSN0(K,J)	     	     	     
 	     TAUZSN(K,J) = TAUZSN0(K,J)	     	     
	   END DO
	 END DO

         DO J = 1,MAXLAY
	   FIXMUL(J) = FIXMUL0(J)
	   CO2MLT(J) = CO2MLT0(J)
	   SO2MLT(J) = SO2MLT0(J)
	   HNOMLT(J) = HNOMLT0(J)
	   N2OMLT(J) = N2OMLT0(J)	   	   	   
           WAANG(J)  = WAANG0(J)
           LOPLOW(J) = LOPLOW0(J)
	   DAOP(J)   = DAOP0(J)
	   
 	   DO K = 1,NTRACE
	     TRCPRD(K,J) = TRCPRD0(K,J)
	   END DO

	   DO K = 1,N1CON
	     CONPRD(K,J) = CONPRD0(K,J)
	   END DO
	   DO K = 1,N3CH4
	     MPRED3(K,J) = MPRED30(K,J)
	   END DO
	   DO K = 1,N4CO
	     CPRED4(K,J) = CPRED40(K,J)
	   END DO
	   
	   DO K = 1,N1FIX
	     FPRED1(K,J) = FPRED10(K,J)
	   END DO
	   DO K = 1,N2FIX
	     FPRED2(K,J) = FPRED20(K,J)
	   END DO
	   DO K = 1,N3FIX
	     FPRED3(K,J) = FPRED30(K,J)
	   END DO
	   DO K = 1,N4FIX
	     FPRED4(K,J) = FPRED40(K,J)
	   END DO
   	   DO K = 1,N5FIX
	     FPRED5(K,J) = FPRED50(K,J)
	   END DO
	   DO K = 1,N6FIX
	     FPRED6(K,J) = FPRED60(K,J)
	   END DO
	   DO K = 1,N7FIX
	     FPRED7(K,J) = FPRED70(K,J)
	   END DO

	   DO K = 1,N1H2O
	     WPRED1(K,J) = WPRED10(K,J)
	   END DO
	   DO K = 1,N2H2O
	     WPRED2(K,J) = WPRED20(K,J)
	   END DO
	   DO K = 1,N3H2O
	     WPRED3(K,J) = WPRED30(K,J)
	   END DO
	   DO K = 1,N4H2O
	     WPRED4(K,J) = WPRED40(K,J)
	   END DO
	   DO K = 1,N5H2O
	     WPRED5(K,J) = WPRED50(K,J)
	   END DO
	   DO K = 1,N6H2O
	     WPRED6(K,J) = WPRED60(K,J)
	   END DO
	   DO K = 1,N7H2O
	     WPRED7(K,J) = WPRED70(K,J)
	   END DO

	   DO K = 1,N1O3
	     OPRED1(K,J) = OPRED10(K,J)
	   END DO
	   DO K = 1,N2O3
	     OPRED2(K,J) = OPRED20(K,J)
	   END DO
c	   DO K = 1,N3O3
c	     OPRED3(K,J) = OPRED30(K,J)
c	   END DO
	   DO K = 1,N4O3
	     OPRED4(K,J) = OPRED40(K,J)
	   END DO
	   DO K = 1,N5O3
	     OPRED5(K,J) = OPRED50(K,J)
	   END DO
	   DO K = 1,N6O3
	     OPRED6(K,J) = OPRED60(K,J)
	   END DO
	   DO K = 1,N7O3
	     OPRED7(K,J) = OPRED70(K,J)
	   END DO
         END DO

       END IF

       RETURN
       END
       
