subroutine calxnte(INDCHN,CLISTN,FCHAN, IP_YMAX,IP_YMIN,IP_XMAX,IP_XMIN, &
        b1,b2,IW,LW,OP_YMAX,OP_YMIN,OP_XMAX,OP_XMIN, IPROF,PRDNTE, RAD)

! Adjust a LTE atmospheric radiance for a non-LTE upper atmosphere.


!CALL PROTOCOL:
!    CALNTE( INDCHN, TEMP, SUNCOS, SCOS1, VSEC1,
!           NCHNTE, CLISTN, COEFN, CO2TOP, RAD)

! included files
USE incFTC

implicit NONE

! Inputs
integer,dimension(MXCHAN) :: INDCHN         ! full passband channel index
integer,dimension(NCHNTE) :: CLISTN ! CHANID (INDNTE)     ! channel IDs for nonLTE
real(4),dimension(NCHNTE) :: IP_YMAX, IP_YMIN, B2, OP_YMAX, OP_YMIN, OP_XMAX, OP_XMIN, FCHAN
real(4),dimension(NCHNTE,4) :: IP_XMAX, IP_XMIN
real(4),dimension(NCHNTE,NNNNTE) :: B1, LW
real(4),dimension(NCHNTE,NNNNTE,4) :: IW
real(4),dimension(NNNNTE,4) :: IW1
real(4),dimension(NNNNTE) :: LW1
real(4),dimension(4) :: PRDNTE
integer :: IPROF 

! Input/Output
real(4), dimension(MXCHAN) :: RAD           ! i/p radiance

! Local
integer :: ich, J, ii
real(4), dimension(NCHNTE) :: DRAD
!real :: x1, x2, x3, x4
!real :: sangle, vangle, ptemp1, ptemp2
real, dimension(4) :: INPUT_SC
real, dimension(NNNNTE) :: y1, y1a 
real :: y2, res1, B21

! Test case [cos(sangle), vangle, ptemp1, ptemp2]
! real, dimension(4) :: input = [1.0000, 0.0,  216.9349,  263.5031];
! INPUT = PRDNTE

!!write(6,*) 'PREDNTE(:,1)= ', PREDNTE(:,1)
!!write( *, * ) 'Press Enter to continue'
!!      read( *, * ) 

! Assemble independant variables:
!x1 = cos(sangle*pi/180);
!x2 = cos(vangle*pi/180)
!x3 = ptemp1     ! mean(tprof(1:5,:),1);
!x4 = ptemp2     ! mean(tprof(6:9,:),1);
!x0 = [x1, x2, x3, x4]

! Pass through the ANN matrix multiplication - assumes tansig nn
! transfer function (net.layers{}.transferFcn).
do ich = 1,NCHNTE

   J = INDCHN(CLISTN(ich))            ! index for RAD

   ! pre-scale input:
   INPUT_SC = (IP_YMAX(ich)-IP_YMIN(ich))*(PRDNTE-IP_XMIN(ich,:))/ &
              (IP_XMAX(ich,:)-IP_XMIN(ich,:)) + IP_YMIN(ich);

!   IW1 = RESHAPE(IW(ich,:,:), (/10, 4/))
   IW1 = IW(ich,:,:)
   LW1 = LW(ich,:)     !   RESHAPE(LW(ich,:), (/1, 10/) )
   B21 = B2(ich)

!do i=1,10
!   y1a(i) = IW1(i,:) * input(:)
!enddo
   y1a = ( matmul(IW1,INPUT_SC) );    ! tanh(IW * input + b1)
!   y1a = ( matmul(IW1,PRDNTE) );    ! tanh(IW * input + b1)
!   print*, y1a
   y1 = tanh(y1a + B1(ich,:)) 

!   y2 = dot_product(LW1,y1) + B21     !   dot_product(LW1,y1) + B2;
   y2 = 0
   do ii = 1,NNNNTE
      y2 = y2 + LW1(ii) * y1(ii)
   enddo
   y2 = y2 + B21     !   dot_product(LW1,y1) + B2;
!   write(6,*), y2
!  (y2-OP_YMIN).* (OP_XMAX-OP_XMIN) /(OP_YMAX-OP_YMIN) + OP_XMIN;
   res1 = (y2 - OP_YMIN(ich)) * (OP_XMAX(ich) - OP_XMIN(ich))
   DRAD(ich) = res1/(OP_YMAX(ich)-OP_YMIN(ich)) + OP_XMIN(ich)
!
!   if (ich .eq. 641) then
!      print*,'calxnte ich: ', ich
!      print*,'calxnte input_sc: ', INPUT_SC
!      print*,'calxnte y1a: ', y1a
!      print*,'calxnte y1: ', y1
!      print*,'calxnte y2: ', y2
!      print*,'calxnte res1: ', res1
!       write(6,*) 'IPROF ', IPROF
!       write(6,*)  PRDNTE,DRAD(ich)
!   endif 

! Update RAD with nonLTE !! only if J was set to valid channel !!
!   RAD(CHANID(ich)) = RAD(CHANID(ich)) + 0.001* DRAD(ich)    ! convert mW.
   IF(J .GT. 0) THEN
     RAD(J) = RAD(J) - 0.001* DRAD(ich)    ! convert mW.
   ENDIF

enddo
!print*, 'calxnte DRAD(99): ', DRAD(99)
! DRAD(1) = res/1000.0          ! convert to Watts.cm-1.sr-1

!write( *, * ) 'Press Enter to continue'
!   read( *, * ) 
end subroutine calxnte
