subroutine read_r49_regdata(PREDNTE, DELTAR)

    !
    ! PREDNTE:      NLTE predictors (solzen,viewang,temp1, temp2)
    !
    !
    !
    USE incFTC

    character(len=80) :: FNREGR='./data/nlte_deps_v1.txt'
    character(len=8) :: str1,str2,str3,str4,str5,c_ich,c_fch,c_drad
    real(4),dimension(4,5358) :: PREDNTE
    real(4),dimension(NCHNTE,5358) :: DELTAR
    real(4),dimension(NCHNTE) :: vFCHAN
    integer,dimension(NCHNTE) :: vICHAN
    integer :: ich

    OPEN(UNIT=IOUN,FILE=FNREGR,FORM='FORMATTED',STATUS='OLD',IOSTAT=IERR)
    IF (IERR .NE. 0) THEN
     WRITE(6,1020) IERR, FNREGR
    STOP
    ENDIF
 1020     FORMAT('Error ',I5,' opening file:',/,A80)

    read(IOUN,*) str1,str2,str3,str4,str5
    read(IOUN,*) c_ich, c_fch
    read(IOUN,*) str1,c_drad
!
    read(IOUN,*) PREDNTE    
!
    do ich = 1,NCHNTE 
      read(IOUN,*) vICHAN(ich), vFCHAN(ich)
      read(IOUN,*) DELTAR(ich,:)
    enddo

    print*, size(DELTAR)
    close(IOUN)

end subroutine   