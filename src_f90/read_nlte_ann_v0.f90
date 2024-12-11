subroutine read_nlte_ann(IP_YMAX,IP_YMIN,IP_XMAX,IP_XMIN,B1,B2, IW, LW, OP_YMAX, OP_YMIN, OP_XMAX, OP_XMIN)

! subroutine read_nlte_ann
!
! read the neural net model parameters for nonLTE
!

!CALL PROTOCOL
!



! included files
USE incFTC

implicit none

! character(len=30) :: FNCOFN='./data/nlte_ann_model.txt'
!character :: FNCOFN

integer(8) :: ii, jj, ierr, ic,jc, ICHAN    
real(4), INTENT (OUT) :: IP_YMAX, IP_YMIN, B2, OP_YMAX, OP_YMIN, OP_XMAX, OP_XMIN
real(4) :: FCHAN 
real(4),dimension(4), INTENT (OUT) :: IP_XMAX, IP_XMIN
real(4),dimension(10), INTENT (OUT) :: B1, LW
real(4),dimension(10,4), INTENT (OUT):: IW
character(len=10) C_YMAX, C_YMIN, C_XMAX, C_XMIN, C_IW, C_b1, C_LW, C_b2
character(len=10) C_YMAXO, C_YMINO, C_XMAXO, C_XMINO

  OPEN(UNIT=IOUN,FILE=FNCOFN,FORM='FORMATTED',STATUS='OLD',IOSTAT=IERR)
    IF (IERR .NE. 0) THEN
      WRITE(6,1020) IERR, FNCOFN
      STOP
    ENDIF
!
 1020     FORMAT('Error ',I5,' opening file:',/,A80)
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
  read(IOUN,*) ICHAN,FCHAN
  read(IOUN,*) IP_YMAX
  read(IOUN,*) IP_YMIN
  read(IOUN,*) IP_XMAX
  read(IOUN,*) IP_XMIN
  read(IOUN,*) IW
  read(IOUN,*) b1  
  read(IOUN,*) LW
  read(IOUN,*) b2 
  read(IOUN,*) OP_YMAX 
  read(IOUN,*) OP_YMIN 
  read(IOUN,*) OP_XMAX 
  read(IOUN,*) OP_XMIN 

  close(UNIT=IOUN)

end subroutine read_nlte_ann




