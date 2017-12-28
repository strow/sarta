c this subroutine sets the cloud params
      subroutine SetCldParams(NCHAN, LBOT, DOSUN, SECANG, ODL,
     $          HSUN, SUNFAC, COSDAZ,
     $          LCBOT1, LCTOP1, CFRAC1,                               
     $          CLRB1, CLRT1, TCBOT1, TCTOP1, MASEC1, MASUN1,
     $          CFRCL1, G_ASY1, NEXTO1, NSCAO1,
     $          LCBOT2, LCTOP2, CFRAC2,                             
     $          CLRB2, CLRT2, TCBOT2, TCTOP2, MASEC2, MASUN2,
     $          CFRCL2, G_ASY2, NEXTO2, NSCAO2,
     $ CLD1EFFOD,CLD2EFFOD,CLD1SUN,CLD2SUN,OMEGA1LAY,OMEGA2LAY)

      IMPLICIT NONE

      include 'incFTC.f'
      include 'rtpdefs.f'

c input
       REAL SUNFAC         ! sun solid angle times cosine at surface
       REAL   HSUN(MXCHAN) ! irradiance from Sun at top of atmosphere
       LOGICAL DOSUN       ! is sun on
       REAL SECANG(MAXLAY) ! viewing angle secant
       REAL    ODL(MAXLAY,MXCHAN) ! clear air layer optical depth
       REAL COSDAZ         ! cosine of delta azimuth angles       
       
C      for CCPREP cloud1
       INTEGER LCBOT1         ! layer containing cloud bottom
       INTEGER LCTOP1         ! layer containing cloud top
       REAL  CLRB1            ! frac of layer at bottom of cloud clear
       REAL  CLRT1            ! frac of layer at top of cloud clear
       REAL TCBOT1            ! temperature at cloud bottom
       REAL TCTOP1            ! temperature at cloud top
       REAL MASEC1            ! mean cloud view angle secant
       REAL MASUN1            ! mean cloud sun-only angle secant
       REAL CFRCL1(MAXLAY)    ! fraction of cloud in layer
       REAL G_ASY1(MXCHAN)    ! "g" asymmetry
       REAL NEXTO1(MXCHAN)    ! nadir extinction optical depth
       REAL NSCAO1(MXCHAN)    ! nadir scattering optical depth
C
C      for CCPREP cloud2
       INTEGER LCBOT2         ! layer containing cloud bottom
       INTEGER LCTOP2         ! layer containing cloud top
       REAL  CLRB2            ! frac of layer at bottom of cloud clear
       REAL  CLRT2            ! frac of layer at top of cloud clear
       REAL TCBOT2            ! temperature at cloud bottom
       REAL TCTOP2            ! temperature at cloud top
       REAL MASEC2            ! mean cloud view angle secant
       REAL MASUN2            ! mean cloud sun-only angle secant
       REAL CFRCL2(MAXLAY)    ! fraction of cloud in layer
       REAL G_ASY2(MXCHAN)    ! "g" asymmetry
       REAL NEXTO2(MXCHAN)    ! nadir extinction optical depth
       REAL NSCAO2(MXCHAN)    ! nadir scattering optical depth

       REAL CFRAC1            ! cloud1(total) fraction of FOV
       REAL CFRAC2            ! cloud2(total) fraction of FOV

       INTEGER NCHAN          ! number of chans to process
       INTEGER   LBOT         ! bottom layer index number       
       
c output
       REAL    CLD1SUN(MAXLAY,MXCHAN)  ! chan solar scat due to cld1 at each lay
       REAL    CLD2SUN(MAXLAY,MXCHAN)  ! chan solar scat due to cld2 at each lay
       REAL    CLD1EFFOD(MXCHAN)       ! chan cld1 effOD
       REAL    CLD2EFFOD(MXCHAN)       ! chan cld2 effOD
       REAL    OMEGA1LAY(MAXLAY,MXCHAN) ! single scat at each lay
       REAL    OMEGA2LAY(MAXLAY,MXCHAN) ! single scat at each lay       

c local
       LOGICAL DOSUNL(MAXLAY) ! layer solar scattering true/false
       INTEGER I,L
       REAL PI4INV, HG3
       REAL KAIR
       REAL  SSECL(MAXLAY) ! solar angle secant
       REAL  SCOSL(MAXLAY) ! solar angle cosine
       REAL  VCOSL(MAXLAY) ! view angle cosine
       REAL     GL(MAXLAY) ! layer scattering asymmetry
       REAL  ODSUM         ! sum of optical depth
       REAL XFUDGE(MAXLAY) ! Sergio's fudged optical depth for RSUNSC
       
c code for CLD1 and CLD2
       PI4INV = 1.0/(4.0*PI)
       
       DO I = 1,NCHAN
         CLD1EFFOD(I) = NEXTO1(I) - NSCAO1(I)*(1.0+G_ASY1(I))/2.0
         CLD2EFFOD(I) = NEXTO2(I) - NSCAO2(I)*(1.0+G_ASY2(I))/2.0
       END DO

c code for CLD1
       DO I = 1,NCHAN
         DO L=1,LBOT
           DOSUNL(L)=.FALSE.
           KAIR=ODL(L,I)/SECANG(L)
c added 28 Mar 2006; layer-above-to-space
           OMEGA1LAY(L,I)=0.0
C
           IF (CFRCL1(L) .GT. 0.0) THEN
             SSECL(L)=MASUN1       ! note: if no sun, this is garbage
             SCOSL(L)=1.0/SSECL(L) ! note: if no sun, this is garbage
             VCOSL(L)=1.0/SECANG(L)
             GL(L)=G_ASY1(I)
             DOSUNL(L)=DOSUN
             ODSUM=ODSUM + KAIR + NEXTO1(I)*CFRCL1(L)
             XFUDGE(L)=KAIR + NEXTO1(I)*CFRCL1(L)
             OMEGA1LAY(L,I)=CFRCL1(L)*NSCAO1(I) / XFUDGE(L)
           ELSE
             XFUDGE(L)=KAIR
             ODSUM=ODSUM + KAIR
           ENDIF
         ENDDO ! downward loop over layers

C      -----------------------------------------------------------------
C      Loop upward over layers
C      -----------------------------------------------------------------
         DO L=LBOT,1,-1
           IF (DOSUNL(L)) THEN
C            Scattered solar
             CLD1SUN(L,I) = (SCOSL(L)/(VCOSL(L)+SCOSL(L)))*PI4INV*OMEGA1LAY(L,I)*
     $          HG3(-SCOSL(L),VCOSL(L),COSDAZ,GL(L))*SUNFAC*HSUN(I)
           ELSE
             CLD1SUN(L,I) = 0.0
           ENDIF
         ENDDO
       ENDDO

c code for CLD2
       DO I = 1,NCHAN
         DO L=1,LBOT
           DOSUNL(L)=.FALSE.
           KAIR=ODL(L,I)/SECANG(L)
c added 28 Mar 2006; layer-above-to-space
           OMEGA2LAY(L,I)=0.0
C
           IF (CFRCL2(L) .GT. 0.0) THEN
             SSECL(L)=MASUN2       ! note: if no sun, this is garbage
             SCOSL(L)=1.0/SSECL(L) ! note: if no sun, this is garbage
             VCOSL(L)=1.0/SECANG(L)
             GL(L)=G_ASY2(I)
             DOSUNL(L)=DOSUN
             ODSUM=ODSUM + KAIR + NEXTO2(I)*CFRCL2(L)
             XFUDGE(L)=KAIR + NEXTO2(I)*CFRCL2(L)
             OMEGA2LAY(L,I)=CFRCL2(L)*NSCAO2(I) / XFUDGE(L)
           ELSE
             XFUDGE(L)=KAIR
             ODSUM=ODSUM + KAIR
           ENDIF
         ENDDO ! downward loop over layers

C      -----------------------------------------------------------------
C      Loop upward over layers
C      -----------------------------------------------------------------
         DO L=LBOT,1,-1
           IF (DOSUNL(L)) THEN
C            Scattered solar
             CLD2SUN(L,I) = (SCOSL(L)/(VCOSL(L)+SCOSL(L)))*PI4INV*OMEGA2LAY(L,I)*
     $          HG3(-SCOSL(L),VCOSL(L),COSDAZ,GL(L))*SUNFAC*HSUN(I)
           ELSE
             CLD2SUN(L,I) = 0.0
           ENDIF
         ENDDO
       ENDDO

       RETURN
       END
       
