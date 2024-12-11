subroutine read_nlte_ann(IP_YMAX,IP_YMIN,IP_XMAX,IP_XMIN,B1,B2, IW, LW, OP_YMAX, OP_YMIN, &
    OP_XMAX, OP_XMIN,CLISTN,FCHANN)

! subroutine read_nlte_ann
!
! read the neural net model parameters for nonLTE
!

!CALL PROTOCOL
!



! included files
USE incFTC

implicit none

integer(8) :: ii, jj, ierr, ic,jc, XNCHNS
integer,dimension(NCHNTE) :: CLISTN                     ! channel ID list for nonLTE
real(4),dimension(NCHNTE) :: FCHANN                     ! frequencies of nonLTE channels.
real(4),dimension(NCHNTE) :: IP_YMAX, IP_YMIN, B2 
real(4),dimension(NCHNTE) :: OP_YMAX, OP_YMIN, OP_XMAX, OP_XMIN
real(4),dimension(NCHNTE,4) :: IP_XMAX, IP_XMIN
real(4),dimension(NCHNTE,NNNNTE) :: B1
real(4),dimension(nCHNTE,NNNNTE) :: LW
real(4),dimension(NCHNTE,NNNNTE,4) :: IW
character(len=12) C_YMAX, C_YMIN, C_XMAX, C_XMIN, C_IW, C_b1, C_LW, C_b2
character(len=12) C_YMAXO, C_YMINO, C_XMAXO, C_XMINO, C_NCHNS
character(len=132) nncomment
!common /coef/ ICHAN,FCHAN,IP_YMAX,IP_YMIN

  OPEN(UNIT=IOUN,FILE=FNCOFN,FORM='FORMATTED',STATUS='OLD',IOSTAT=IERR)
    IF (IERR .NE. 0) THEN
      WRITE(6,1020) IERR, FNCOFN
      STOP
    ENDIF
!
 1020     FORMAT('Error ',I5,' opening file:',/,A80)
! load nchans (not used for loading - see incFTC.f90: NCHNTE)
  read(IOUN,'(A)') nncomment
!
  read(IOUN,*) C_NCHNS
  read(IOUN,*) XNCHNS
    write(6,*) 'rd_nnte:', nncomment, XNCHNS
! load header information
  read(IOUN,*) C_YMAX
  read(IOUN,*) ii,jj

  read(IOUN,*) C_YMIN 
  read(IOUN,*) ii,jj

  read(IOUN,*) C_XMAX
  read(IOUN,*) ii,jj

  read(IOUN,*) C_XMIN
  read(IOUN,*) ii,jj

  read(IOUN,*) C_IW
  read(IOUN,*) ii,jj

  read(IOUN,*) C_b1
  read(IOUN,*) ii,jj

  read(IOUN,*) C_LW
  read(IOUN,*) ii,jj

  read(IOUN,*) C_b2
  read(IOUN,*) ii,jj
!
  read(IOUN,*) C_YMAXO
  read(IOUN,*) ii,jj

  read(IOUN,*) C_YMINO
  read(IOUN,*) ii,jj

  read(IOUN,*) C_XMAXO
  read(IOUN,*) ii,jj

  read(IOUN,*) C_XMINO
  read(IOUN,*) ii,jj
! Load recurring channel data
  do ic=1,NCHNTE
     read(IOUN,*) CLISTN(ic),FCHANN(ic)
     read(IOUN,*) IP_YMAX(ic)
     read(IOUN,*) IP_YMIN(ic)
     read(IOUN,*) IP_XMAX(ic,:)
     read(IOUN,*) IP_XMIN(ic,:)
     read(IOUN,*) IW(ic,:,:)
     read(IOUN,*) b1(ic,:)  
     read(IOUN,*) LW(ic,:)
     read(IOUN,*) b2(ic) 
     read(IOUN,*) OP_YMAX(ic)
     read(IOUN,*) OP_YMIN(ic) 
     read(IOUN,*) OP_XMAX(ic) 
     read(IOUN,*) OP_XMIN(ic) 
  enddo 

!  write(6,*) 'read_nlte: ip_ymax(9) ' IP_YMAX(9)

  close(UNIT=IOUN)

end subroutine read_nlte_ann




