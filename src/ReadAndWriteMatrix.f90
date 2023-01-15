


subroutine ReadMatrixFile_Int(FileName_In, IntMat,  &
        NCols_In, NRows_In,XLLCorner_In,YLLCorner_In,  &
        CellSize_In,NoData_Value_In,bIsError,sFileFormat_In, &
        strDate_In)
    implicit none


    character(*):: FileName_In,sFileFormat_In,strDate_In
    character(len=200):: FileName
    character(14):: strDate,strTemp
    integer		:: NCols_In,NRows_In
    double precision :: XLLCorner_In,YLLCorner_In
    double precision :: CellSize_In,NoData_Value_In
    logical :: bIsError,fExist

    integer		:: NCols, NRows
    double precision :: XLLCorner,YLLCorner,CellSize,NoData_Value

    integer :: IntMat(0:NCols_In-1,0:NRows_In-1)
    integer,allocatable :: IntMatBig(:,:)
    character(len=10) :: sFileFormat

    sFileFormat=sFileFormat_In
    strDate=trim(strDate_In)

    call UPCASE(sFileFormat)

    bIsError=.false.

    select case (trim(sFileFormat))
    case ("ASC","TXT") !--------------------------------------------
        FileName=trim(FileName_In)//trim(strDate)// ".asc"
        inquire(file=trim(Filename), exist=fExist)
        if(fExist .eqv. .false.)then
            FileName=trim(FileName_In)//trim(strDate)// ".txt"
            inquire(file=trim(Filename), exist=fExist)
            if(fExist .eqv. .false.)then
                bIsError=.true.
                return
            end if
        end if

        call ReadASCFileHeader(trim(FileName),  &
                NCols, NRows,XLLCorner,YLLCorner,  &
                CellSize,NoData_Value,bIsError)
        if(bIsError .eqv. .true.)then
            return
        end if

        allocate(IntMatBig(0:NCols-1,0:NRows-1))

        call ReadASCFile_Int(trim(FileName),IntMatBig, &
                NCols, NRows,XLLCorner,YLLCorner, &
                CellSize,NoData_Value,bIsError)
        if(bIsError .eqv. .true.)then
            write(*,*) "Some errors in your file: " // trim(FileName)
            deallocate(IntMatBig)
            return
        end if
    case default
    end select

    if(NCols_In /=NCols .or. NRows_In /=NRows)then
        call GetBfromA_Int(NCols, NRows, XLLCorner, YLLCorner,  &
                CellSize, NoData_Value, IntMatBig,  &
                NCols_In, NRows_In,  &
                XLLCorner_In, YLLCorner_In,  &
                CellSize_In, NoData_Value_In, IntMat)
    else
        IntMat=IntMatBig
    end if

    if(NoData_Value_In/=NoData_Value)then
        where(IntMat==NoData_Value)
            IntMat=NoData_Value_In
        end where
    end if

    return
end subroutine ReadMatrixFile_Int

subroutine ReadMatrixFile(FileName_In, dblMat, &
        NCols_In, NRows_In,XLLCorner_In,YLLCorner_In, &
        CellSize_In,NoData_Value_In,bIsError,sFileFormat_In, &
        strDate_In)

    use CREST_Project
    implicit none

    character(*):: FileName_In,sFileFormat_In
    character(*) :: strDate_In
    character(len=200):: FileName
    character(14):: strDate,strTemp
    integer		:: NCols_In,NRows_In,intTemp
    double precision :: XLLCorner_In,YLLCorner_In
    double precision :: CellSize_In,NoData_Value_In
    logical :: bIsError,fExist

    integer		:: NCols, NRows
    double precision :: XLLCorner,YLLCorner,CellSize,NoData_Value

    double precision :: dblMat(0:NCols_In-1,0:NRows_In-1)
    double precision,allocatable :: dblMatBig(:,:)
    character(len=10) :: sFileFormat

    integer			 :: dtTemp(1:6),iNum
    character(20):: sTemp

    sFileFormat=trim(adjustl(sFileFormat_In))
    strDate=trim(strDate_In)

    call UPCASE(sFileFormat)

    bIsError=.false.

    !write(*,*)"Format: ",trim(sFileFormat_In), trim(sFileFormat)
    select case (trim(sFileFormat))

    case ("ASC","TXT") !--------------------------------------------

        FileName=trim(FileName_In)//trim(strDate)// ".asc"
        inquire(file=trim(Filename), exist=fExist)
        if(fExist .eqv. .false.)then
            FileName=trim(FileName_In)//trim(strDate)// ".txt"
            inquire(file=trim(Filename), exist=fExist)
            if(fExist .eqv. .false.)then
                bIsError=.true.
                return
            end if
        end if

        call ReadASCFileHeader(trim(FileName),  &
                NCols, NRows,XLLCorner,YLLCorner, &
                CellSize,NoData_Value,bIsError)
        !write(*,*)bIsError
        if(bIsError .eqv. .true.)then
            return
        end if

        allocate(dblMatBig(0:NCols-1,0:NRows-1))

        call ReadASCFile(trim(FileName), dblMatBig, &
                NCols, NRows,XLLCorner,YLLCorner, &
                CellSize,NoData_Value,bIsError)
        if(bIsError .eqv. .true.)then
            deallocate(dblMatBig)
            return
        end if

    case default
        write(*,*) "ERROR!!! This version does not use this Format!"
        bIsError=.true.
        !deallocate(dblMatBig)
        return
    end select



    if(NCols_In /=NCols .or. NRows_In /=NRows  &
            .or. CellSize_In /=CellSize)then

        call GetBfromA(NCols, NRows, XLLCorner, YLLCorner,   &
                    CellSize, NoData_Value, dblMatBig,  &
                    NCols_In, NRows_In,  &
                    XLLCorner_In, YLLCorner_In,  &
                    CellSize_In, NoData_Value_In, dblMat)

    else
        dblMat=dblMatBig
    end if

    if(NoData_Value_In/=NoData_Value)then
        where(dblMat==NoData_Value)
            dblMat=NoData_Value_In
        end where
    end if

    deallocate(dblMatBig)

    return
end subroutine ReadMatrixFile

subroutine WriteMatrixFile_Int(FileName_In,IntMat, &
        NCols, NRows,XLLCorner,YLLCorner, &
        CellSize,NoData_Value,bIsError,sFileFormat_In)
    implicit none

    character(*):: FileName_In,sFileFormat_In
    character(len=200):: FileName

    logical :: bIsError

    integer		:: NCols, NRows
    double precision :: XLLCorner,YLLCorner,CellSize,NoData_Value

    integer :: IntMat(0:NCols-1,0:NRows-1)
    character(len=10) :: sFileFormat

    sFileFormat=sFileFormat_In
    call UPCASE(sFileFormat)

    bIsError=.false.

    select case (trim(sFileFormat))
    case ("ASC")
        FileName=trim(FileName_In) // ".asc"
        call WriteASCFile_Int(trim(FileName), IntMat, &
                NCols, NRows,XLLCorner,YLLCorner, &
                CellSize,NoData_Value,bIsError)
    case ("TXT")
        FileName=trim(FileName_In) // ".txt"
        call WriteASCFile_Int(trim(FileName), IntMat, &
                NCols, NRows,XLLCorner,YLLCorner,  &
                CellSize,NoData_Value,bIsError)

    case default


    end select


    return
end subroutine WriteMatrixFile_Int

subroutine WriteMatrixFile(FileName_In,dblMat, &
        NCols, NRows,XLLCorner,YLLCorner, &
        CellSize,NoData_Value,bIsError,sFileFormat_In)
    implicit none

    character(*):: FileName_In,sFileFormat_In
    character(len=200):: FileName

    logical :: bIsError

    integer		:: NCols, NRows
    double precision :: XLLCorner,YLLCorner,CellSize,NoData_Value

    double precision :: dblMat(0:NCols-1,0:NRows-1)
    character(len=10) :: sFileFormat

    sFileFormat=trim(sFileFormat_In)
    call UPCASE(sFileFormat)

    bIsError=.false.

    select case (trim(sFileFormat))
    case ("ASC")
        FileName=trim(FileName_In) // ".asc"
        call WriteASCFile(trim(FileName), dblMat, &
                NCols, NRows,XLLCorner,YLLCorner, &
                CellSize,NoData_Value,bIsError)
    case ("TXT")
        FileName=trim(FileName_In) // ".txt"
        call WriteASCFile(trim(FileName), dblMat, &
                NCols, NRows,XLLCorner,YLLCorner, &
                CellSize,NoData_Value,bIsError)


    case default


    end select


    return
end subroutine WriteMatrixFile

subroutine GetBfromA(ANCols, ANRows, AXLLCorner, AYLLCorner,  &
        ACellSize, ANoData_Value, AdblMat,   &
        BNCols, BNRows, BXLLCorner, BYLLCorner,  &
        BCellSize, BNoData_Value, BdblMat)
    implicit none
    integer(kind=4):: ANCols, ANRows, BNCols, BNRows, i, j,souR, souC
    double precision :: AXLLCorner, AYLLCorner
    double precision :: ACellSize, ANoData_Value
    double precision :: AdblMat(0:AnCols-1, 0:AnRows-1)

    double precision :: BXLLCorner, BYLLCorner
    double precision :: BCellSize, BNoData_Value
    double precision :: BdblMat(0:BnCols-1, 0:BnRows-1)
    double precision :: SR, SC


    BdblMat=BNoData_Value
    do i=0, BNRows-1
        SR = BYLLCorner + (BNRows-i-0.5) * BCellSize
        souR = (AYLLCorner +ANRows * ACellSize - SR) / ACellSize
        if (souR>=0 .And. souR < ANRows) then
            do j=0, BNCols-1
                SC = BXLLCorner+(j + 0.5) * BCellSize
                souC = (SC - AXLLCorner) / ACellSize
                if (souC>=0 .And. souC < ANCols) then
                    if (AdblMat(souC, souR) /= ANoData_Value) then
                        BdblMat(j, i) = AdblMat(souC, souR)
                    end if
                end if
            enddo
        end if
    enddo
    return
end subroutine GetBfromA

subroutine GetBfromA_Int(ANCols, ANRows, AXLLCorner, AYLLCorner, &
        ACellSize, ANoData_Value, AdblMat,  &
        BNCols, BNRows, BXLLCorner, BYLLCorner, &
        BCellSize, BNoData_Value, BdblMat)
    implicit none
    integer(kind=4):: ANCols, ANRows, BNCols, BNRows, i, j,souR, souC
    double precision :: AXLLCorner, AYLLCorner
    double precision :: ACellSize, ANoData_Value
    integer :: AdblMat(0:AnCols-1, 0:AnRows-1)

    double precision :: BXLLCorner, BYLLCorner
    double precision :: BCellSize, BNoData_Value
    integer :: BdblMat(0:BnCols-1, 0:BnRows-1)
    double precision :: SR, SC


    BdblMat=BNoData_Value
    do i=0, BNRows-1
        SR = BYLLCorner + (BNRows-i-0.5) * BCellSize
        souR = (AYLLCorner +ANRows * ACellSize - SR) / ACellSize
        if (souR>=0 .And. souR < ANRows) then
            do j=0, BNCols-1
                SC = BXLLCorner+(j + 0.5) * BCellSize
                souC = (SC - AXLLCorner) / ACellSize
                if (souC>=0 .And. souC < ANCols) then
                    if (AdblMat(souC, souR) /= ANoData_Value) then
                        BdblMat(j, i) = AdblMat(souC, souR)
                    end if
                end if
            enddo
        end if
    enddo
    return
end subroutine GetBfromA_Int

subroutine ReadASCFileHeader(FileName, &
        NCols, NRows,XLLCorner,YLLCorner, &
        CellSize,NoData_Value,bIsError)

    implicit none
    character(*):: FileName
    logical :: bIsError
    integer		:: fileID

    integer		:: NCols,NRows
    double precision :: XLLCorner,YLLCorner
    double precision :: CellSize,NoData_Value

    character(20):: sTemp

    bIsError=.false.
    call XXWGetFreeFile(fileid)

    open(fileID,file=TRIM(FileName),form='formatted')
    read(fileID,*) sTemp, NCols
    !write(*,*) NCols

    read(fileID,*) sTemp, NRows
    !write(*,*) NRows

    read(fileID,*) sTemp, XLLCorner
    !write(*,*) XLLCorner

    read(fileID,*) sTemp, YLLCorner
    !write(*,*) YLLCorner

    read(fileID,*) sTemp, CellSize
    !write(*,*) CellSize

    read(fileID,*) sTemp, NoData_Value
    !write(*,*) NoData_Value

    close(fileID)

    return

end subroutine


subroutine ReadASCFile(FileName,dblMat, &
        NCols,NRows,XLLCorner,YLLCorner,  &
        CellSize,NoData_Value,bIsError)
    implicit none
    character(*):: FileName
    logical :: bIsError
    integer		:: fileID
    integer		:: i,j

    integer		:: NCols,NRows
    double precision :: XLLCorner,YLLCorner
    double precision :: CellSize,NoData_Value

    double precision :: dblMat(0:NCols-1,0:NRows-1)

    character(20):: sTemp

    bIsError=.false.

    call XXWGetFreeFile(fileid)

    open(fileID,file=TRIM(FileName),form='formatted')
    read(fileID,*) sTemp, NCols
    read(fileID,*) sTemp, NRows
    read(fileID,*) sTemp, XLLCorner
    read(fileID,*) sTemp, YLLCorner
    read(fileID,*) sTemp, CellSize
    read(fileID,*) sTemp, NoData_Value

    read(fileID,*)((dblMat(j,i),j=0,NCols-1),i=0,NRows-1)

    close(fileID)
    return
end subroutine

subroutine ReadASCFile_Int(FileName,IntMat,   &
        NCols,NRows,XLLCorner,YLLCorner,  &
        CellSize,NoData_Value,bIsError)

    implicit none
    character(*):: FileName
    logical :: bIsError
    integer		:: fileID
    integer		:: i,j

    integer		:: NCols,NRows
    double precision :: XLLCorner,YLLCorner
    double precision :: CellSize,NoData_Value

    integer :: IntMat(0:NCols-1,0:NRows-1)

    character(20):: sTemp

    bIsError=.false.

    call XXWGetFreeFile(fileid)

    open(fileID,file=TRIM(FileName),form='formatted')
    read(fileID,*) sTemp, NCols
    read(fileID,*) sTemp, NRows
    read(fileID,*) sTemp, XLLCorner
    read(fileID,*) sTemp, YLLCorner
    read(fileID,*) sTemp, CellSize
    read(fileID,*) sTemp, NoData_Value

    read(fileID,*)((IntMat(j,i),j=0,NCols-1),i=0,NRows-1)

    close(fileID)
    return
end subroutine


!WriteASCFile---------------------------------------------------
subroutine WriteASCFile(Filename, dblMat, nCols, nRows, XLLCorner, &
        YLLCorner, CellSize, NoData_Value,bIsError)
    implicit none
    character(*):: Filename
    integer :: lC, lR, nCols, nRows
    double precision :: dblMat(0:nCols-1, 0:nRows-1)
    double precision :: XLLCorner, YLLCorner
    double precision :: CellSize, NoData_Value
    integer :: FileID
    logical :: bIsError

    bIsError=.false.
    call XXWGetFreeFile(FileID)

    open(FileID,file=trim(Filename),form='formatted')
    write(FileID,"('nCols 		')",advance='no')
    write(FileID,"(i8)")nCols
    write(FileID,"('nRows 		')",advance='no')
    write(FileID,"(i8)")nRows
    write(FileID,"('XLLCorner 	')",advance='no')
    write(FileID,"(f16.6)")XLLCorner
    write(FileID,"('YLLCorner 	')",advance='no')
    write(FileID,"(f16.6)")YLLCorner
    write(FileID,"('cellSize 	 ')",advance='no')
    write(FileID,"(f11.6)")CellSize
    write(FileID,"('NODATA_value ')",advance='no')
    write(FileID,"(f11.0)")NoData_Value
    do lR=0, nRows-1
        do lC=0, nCols-1
            write(FileID,'(F10.2,A)',advance='no') &
                    dblMat(lC,lR),' ';
        end do
        write(FileID,*)
    end do
    close (FileID)
end subroutine

!WriteASCFile---------------------------------------------------
subroutine WriteASCFile_Int(Filename, IntMat, nCols, nRows,  &
        XLLCorner,YLLCorner, CellSize, NoData_Value,bIsError)
    implicit none
    character(*):: Filename
    integer :: lC, lR, nCols, nRows
    integer :: IntMat(0:nCols-1, 0:nRows-1)
    double precision :: XLLCorner, YLLCorner
    double precision :: CellSize, NoData_Value
    integer :: FileID
    logical :: bIsError

    bIsError=.false.
    call XXWGetFreeFile(FileID)

    open(FileID,file=trim(Filename),form='formatted')
    write(FileID,"('nCols 		')",advance='no')
    write(FileID,"(i8)")nCols
    write(FileID,"('nRows 		')",advance='no')
    write(FileID,"(i8)")nRows
    write(FileID,"('XLLCorner 	')",advance='no')
    write(FileID,"(f16.6)")XLLCorner
    write(FileID,"('YLLCorner 	')",advance='no')
    write(FileID,"(f16.6)")YLLCorner
    write(FileID,"('cellSize 	 ')",advance='no')
    write(FileID,"(f16.6)")CellSize
    write(FileID,"('NODATA_value ')",advance='no')
    write(FileID,"(f11.0)")NoData_Value
    do lR=0, nRows-1
        do lC=0, nCols-1
            write(FileID,'(I10,A)',advance='no')IntMat(lC,lR),' ';
        end do
        write(FileID,*)
    end do
    close (FileID)
end subroutine
!########################################################
subroutine ExportGridVar(strDate,Rain,PET,EPot,EAct, &
        W,Runoff,ExcS,ExcI,RS,RI, SM)
    use CREST_Project
    use CREST_Basic
    use CREST_Param
    use SoilDownscale_Basic
    use Landslide_Basic
    implicit none
    character*(*) strDate
    logical :: bIsError
    double precision :: Rain(0:g_NCols-1,0:g_NRows-1)
    double precision :: PET(0:g_NCols-1,0:g_NRows-1)
    double precision :: EPot(0:g_NCols-1,0:g_NRows-1)
    double precision :: W(0:g_NCols-1,0:g_NRows-1)
    double precision :: SM(0:g_NCols-1,0:g_NRows-1)
    double precision :: ExcS(0:g_NCols-1,0:g_NRows-1)
    double precision :: ExcI(0:g_NCols-1,0:g_NRows-1)
    double precision :: RI(0:g_NCols-1,0:g_NRows-1)
    double precision :: RS(0:g_NCols-1,0:g_NRows-1)
    double precision :: EAct(0:g_NCols-1,0:g_NRows-1)
    double precision :: Runoff(0:g_NCols-1,0:g_NRows-1)
    double precision :: dblTemp(0:g_NCols-1,0:g_NRows-1)
    double precision :: dblTemp_Land(0:g_NCols_Land-1,0:g_NRows_Land-1)


    integer :: iCountOfLakeMask,i
    double precision :: SumValueInLake

    !Output Grid Variables
    if(g_bGOVar(0))then
        where(g_Mask/=g_NoData_Value)
            dblTemp=Rain/g_TimeStep
        elsewhere
            dblTemp=g_NoData_Value
        end where

        call WriteMatrixFile(trim(g_ResultPath)//"GOVar_"  &
                //trim(g_sGOVarName(0)) // "_" //trim(strDate),  &
                dblTemp, g_NCols,g_NRows,    &
                g_XLLCorner,g_YLLCorner, g_CellSize, g_NoData_Value, &
                bIsError,g_ResultFormat)
    end if

    if(g_bGOVar(1))then
        where(g_Mask/=g_NoData_Value)
            dblTemp=PET/g_TimeStep
        elsewhere
            dblTemp=g_NoData_Value
        end where

        ! Added by Xianwu Xue 2011.4.21
        if(g_NLakes>0)then

            i=1
            do while(i<=g_NLakes)
                iCountOfLakeMask=count((g_LakeMask==i) .and.(dblTemp>=0))

                if(iCountOfLakeMask==0)then
                    i=i+1
                    cycle
                end if

                SumValueInLake &
                        =sum(dblTemp,(g_LakeMask==i) .and.(dblTemp>=0))
                SumValueInLake=SumValueInLake/dble(iCountOfLakeMask)

                where((g_LakeMask==i) .and.(dblTemp>=0))
                    dblTemp=SumValueInLake
                end where

                i=i+1
            end do
        end if

        call WriteMatrixFile(trim(g_ResultPath)//"GOVar_" &
                //trim(g_sGOVarName(1)) // "_" //trim(strDate),   &
                dblTemp, g_NCols,g_NRows,  &
                g_XLLCorner,g_YLLCorner, g_CellSize, g_NoData_Value,  &
                bIsError,g_ResultFormat)
    end if

    if(g_bGOVar(2))then
        where(g_Mask/=g_NoData_Value)
            dblTemp=EPot/g_TimeStep
        elsewhere
            dblTemp=g_NoData_Value
        end where

        ! Added by Xianwu Xue 2011.4.21
        if(g_NLakes>0)then

            i=1
            do while(i<=g_NLakes)
                iCountOfLakeMask=count((g_LakeMask==i) .and.(dblTemp>=0))

                if(iCountOfLakeMask==0)then
                    i=i+1
                    cycle
                end if

                SumValueInLake &
                        =sum(dblTemp,(g_LakeMask==i) .and.(dblTemp>=0))
                SumValueInLake=SumValueInLake/dble(iCountOfLakeMask)

                where((g_LakeMask==i) .and.(dblTemp>=0))
                    dblTemp=SumValueInLake
                end where

                i=i+1
            end do
        end if

        call WriteMatrixFile(trim(g_ResultPath)//"GOVar_" &
                //trim(g_sGOVarName(2)) // "_" //trim(strDate),  &
                dblTemp, g_NCols,g_NRows,   &
                g_XLLCorner,g_YLLCorner, g_CellSize, g_NoData_Value, &
                bIsError,g_ResultFormat)
    end if

    if(g_bGOVar(3))then
        where(g_Mask/=g_NoData_Value)
            dblTemp=EAct/g_TimeStep
        elsewhere
            dblTemp=g_NoData_Value
        end where

        ! Added by Xianwu Xue 2011.4.21
        if(g_NLakes>0)then

            i=1
            do while(i<=g_NLakes)
                iCountOfLakeMask=count((g_LakeMask==i) .and.(dblTemp>=0))

                if(iCountOfLakeMask==0)then
                    i=i+1
                    cycle
                end if

                SumValueInLake &
                        =sum(dblTemp,(g_LakeMask==i) .and.(dblTemp>=0))
                SumValueInLake=SumValueInLake/dble(iCountOfLakeMask)

                where((g_LakeMask==i) .and.(dblTemp>=0))
                    dblTemp=SumValueInLake
                end where

                i=i+1
            end do
        end if

        call WriteMatrixFile(trim(g_ResultPath)//"GOVar_" &
                //trim(g_sGOVarName(3)) // "_" //trim(strDate),  &
                dblTemp, g_NCols,g_NRows,   &
                g_XLLCorner,g_YLLCorner, g_CellSize, g_NoData_Value,  &
                bIsError,g_ResultFormat)
    end if

    if(g_bGOVar(4))then
        where(g_Mask/=g_NoData_Value)
            dblTemp=W
        elsewhere
            dblTemp=g_NoData_Value
        end where

        ! Added by Xianwu Xue 2011.4.21
        if(g_NLakes>0)then

            i=1
            do while(i<=g_NLakes)
                iCountOfLakeMask=count((g_LakeMask==i) .and.(dblTemp>=0))

                if(iCountOfLakeMask==0)then
                    i=i+1
                    cycle
                end if

                ! 	        SumValueInLake
                !      &          =sum(dblTemp,(g_LakeMask==i) .and.(dblTemp>=0))
                !               SumValueInLake=SumValueInLake/dble(iCountOfLakeMask)

                where((g_LakeMask==i) .and.(dblTemp>=0))
                    dblTemp=g_tParams%WM*(1+g_tParams%B)

                end where

                i=i+1
            end do
        end if

        call WriteMatrixFile(trim(g_ResultPath)//"GOVar_"  &
                //trim(g_sGOVarName(4)) // "_" //trim(strDate),  &
                dblTemp, g_NCols,g_NRows,    &
                g_XLLCorner,g_YLLCorner, g_CellSize, g_NoData_Value,  &
                bIsError,g_ResultFormat)
    end if

    if(g_bGOVar(5))then
        where(g_Mask/=g_NoData_Value)
            !dblTemp=W*100.0/(g_tParams%WM*(1+g_tParams%B))  !Modified by Xianwu Xue 2011.4.20
            dblTemp=SM  ! Modified by Guoding Chen 2021.4.09
        elsewhere
            dblTemp=g_NoData_Value
        end where

        ! Added by Xianwu Xue 2011.4.21
        if(g_NLakes>0)then

            i=1
            do while(i<=g_NLakes)
                iCountOfLakeMask=count((g_LakeMask==i) .and.(dblTemp>=0))

                if(iCountOfLakeMask==0)then
                    i=i+1
                    cycle
                end if

                ! 	        SumValueInLake
                !      &          =sum(dblTemp,(g_LakeMask==i) .and.(dblTemp>=0))
                !               SumValueInLake=SumValueInLake/dble(iCountOfLakeMask)
                SumValueInLake=100.0
                where((g_LakeMask==i) .and.(dblTemp>=0))
                    dblTemp=SumValueInLake
                end where

                i=i+1
            end do
        end if

        call WriteMatrixFile(trim(g_ResultPath)//"GOVar_"  &
                //trim(g_sGOVarName(5)) // "_" //trim(strDate),   &
                dblTemp, g_NCols,g_NRows,     &
                g_XLLCorner,g_YLLCorner, g_CellSize, g_NoData_Value, &
                bIsError,g_ResultFormat)
    end if

    if(g_bGOVar(6))then
        where(g_Mask/=g_NoData_Value)
            dblTemp=Runoff
        elsewhere
            dblTemp=g_NoData_Value
        end where

        ! Added by Xianwu Xue 2011.4.21
        if(g_NLakes>0)then

            i=1
            do while(i<=g_NLakes)
                iCountOfLakeMask=count((g_LakeMask==i) .and.(dblTemp>=0))

                if(iCountOfLakeMask==0)then
                    i=i+1
                    cycle
                end if

                SumValueInLake &
                        =sum(dblTemp,(g_LakeMask==i) .and.(dblTemp>=0))
                SumValueInLake=SumValueInLake/dble(iCountOfLakeMask)

                where((g_LakeMask==i) .and.(dblTemp>=0))
                    dblTemp=SumValueInLake
                end where

                i=i+1
            end do
        end if

        call WriteMatrixFile(trim(g_ResultPath)//"GOVar_"   &
                //trim(g_sGOVarName(6)) // "_" //trim(strDate),   &
                dblTemp, g_NCols,g_NRows,    &
                g_XLLCorner,g_YLLCorner, g_CellSize, g_NoData_Value,  &
                bIsError,g_ResultFormat)
    end if

    if(g_bGOVar(7))then
        where(g_Mask/=g_NoData_Value)
            dblTemp=ExcS/g_TimeStep
        elsewhere
            dblTemp=g_NoData_Value
        end where

        ! Added by Xianwu Xue 2011.4.21
        if(g_NLakes>0)then

            i=1
            do while(i<=g_NLakes)
                iCountOfLakeMask=count((g_LakeMask==i) .and.(dblTemp>=0))

                if(iCountOfLakeMask==0)then
                    i=i+1
                    cycle
                end if

                SumValueInLake &
                        =sum(dblTemp,(g_LakeMask==i) .and.(dblTemp>=0))
                SumValueInLake=SumValueInLake/dble(iCountOfLakeMask)

                where((g_LakeMask==i) .and.(dblTemp>=0))
                    dblTemp=SumValueInLake
                end where

                i=i+1
            end do
        end if

        call WriteMatrixFile(trim(g_ResultPath)//"GOVar_" &
                //trim(g_sGOVarName(7)) // "_" //trim(strDate),  &
                dblTemp, g_NCols,g_NRows,   &
                g_XLLCorner,g_YLLCorner, g_CellSize, g_NoData_Value,   &
                bIsError,g_ResultFormat)
    end if

    if(g_bGOVar(8))then
        where(g_Mask/=g_NoData_Value)
            dblTemp=ExcI/g_TimeStep
        elsewhere
            dblTemp=g_NoData_Value
        end where

        ! Added by Xianwu Xue 2011.4.21
        if(g_NLakes>0)then

            i=1
            do while(i<=g_NLakes)
                iCountOfLakeMask=count((g_LakeMask==i) .and.(dblTemp>=0))

                if(iCountOfLakeMask==0)then
                    i=i+1
                    cycle
                end if

                SumValueInLake  &
                        =sum(dblTemp,(g_LakeMask==i) .and.(dblTemp>=0))
                SumValueInLake=SumValueInLake/dble(iCountOfLakeMask)

                where((g_LakeMask==i) .and.(dblTemp>=0))
                    dblTemp=SumValueInLake
                end where


                i=i+1
            end do
        end if

        call WriteMatrixFile(trim(g_ResultPath)//"GOVar_"  &
                //trim(g_sGOVarName(8)) // "_" //trim(strDate),  &
                dblTemp, g_NCols,g_NRows,    &
                g_XLLCorner,g_YLLCorner, g_CellSize, g_NoData_Value, &
                bIsError,g_ResultFormat)
    end if

    if(g_bGOVar(9))then
        where(g_Mask/=g_NoData_Value)
            dblTemp=RS/g_TimeStep
        elsewhere
            dblTemp=g_NoData_Value
        end where

        ! Added by Xianwu Xue 2011.4.21
        if(g_NLakes>0)then

            i=1
            do while(i<=g_NLakes)
                iCountOfLakeMask=count((g_LakeMask==i) .and.(dblTemp>=0))

                if(iCountOfLakeMask==0)then
                    i=i+1
                    cycle
                end if

                SumValueInLake &
                        =sum(dblTemp,(g_LakeMask==i) .and.(dblTemp>=0))
                SumValueInLake=SumValueInLake/dble(iCountOfLakeMask)

                where((g_LakeMask==i) .and.(dblTemp>=0))
                    dblTemp=SumValueInLake
                end where

                i=i+1
            end do
        end if

        call WriteMatrixFile(trim(g_ResultPath)//"GOVar_" &
                //trim(g_sGOVarName(9)) // "_" //trim(strDate),  &
                dblTemp, g_NCols,g_NRows,  &
                g_XLLCorner,g_YLLCorner, g_CellSize, g_NoData_Value,  &
                bIsError,g_ResultFormat)
    end if

    if(g_bGOVar(10))then
        where(g_Mask/=g_NoData_Value)
            dblTemp=RI/g_TimeStep
        elsewhere
            dblTemp=g_NoData_Value
        end where

        ! Added by Xianwu Xue 2011.4.21
        if(g_NLakes>0)then

            i=1
            do while(i<=g_NLakes)
                iCountOfLakeMask=count((g_LakeMask==i) .and.(dblTemp>=0))

                if(iCountOfLakeMask==0)then
                    i=i+1
                    cycle
                end if

                SumValueInLake  &
                        =sum(dblTemp,(g_LakeMask==i) .and.(dblTemp>=0))
                SumValueInLake=SumValueInLake/dble(iCountOfLakeMask)

                where((g_LakeMask==i) .and.(dblTemp>=0))
                    dblTemp=SumValueInLake
                end where

                i=i+1
            end do
        end if

        call WriteMatrixFile(trim(g_ResultPath)//"GOVar_" &
                //trim(g_sGOVarName(10)) // "_" //trim(strDate),  &
                dblTemp, g_NCols,g_NRows,  &
                g_XLLCorner,g_YLLCorner, g_CellSize, g_NoData_Value, &
                bIsError,g_ResultFormat)
    end if

    if (g_ModelCore == 3) then
        ! -------------export landslide data----------------
        ! Safety factor
        if(g_bGOVar(11))then
            where(g_mask_fine/=g_NoData_Value)
                dblTemp_Land = g_FS_3D
            elsewhere
                dblTemp_Land = g_NoData_Value
            end where

            call WriteMatrixFile(trim(g_ResultPath)//"GOVar_"  &
                    //trim(g_sGOVarName(11)) // "_" //trim(strDate),  &
                    dblTemp_Land, g_NCols_Land,g_NRows_Land,    &
                    g_xllCorner_Land, g_yllCorner_Land, g_CellSize_Land, &
                    g_NoData_Value, bIsError,g_ResultFormat)
        end if

        ! failure probability
        if(g_bGOVar(12))then
            where(g_mask_fine/=g_NoData_Value)
                dblTemp_Land = g_probability
            elsewhere
                dblTemp_Land = g_NoData_Value
            end where

            call WriteMatrixFile(trim(g_ResultPath)//"GOVar_"  &
                    //trim(g_sGOVarName(12)) // "_" //trim(strDate),  &
                    dblTemp_Land, g_NCols_Land,g_NRows_Land,    &
                    g_xllCorner_Land, g_yllCorner_Land, g_CellSize_Land, &
                    g_NoData_Value, bIsError,g_ResultFormat)
        end if
        ! failure volume
        if(g_bGOVar(13))then
            where(g_mask_fine/=g_NoData_Value)
                dblTemp_Land = g_failure_volume
            elsewhere
                dblTemp_Land = g_NoData_Value
            end where

            call WriteMatrixFile(trim(g_ResultPath)//"GOVar_"  &
                    //trim(g_sGOVarName(13)) // "_" //trim(strDate),  &
                    dblTemp_Land, g_NCols_Land,g_NRows_Land,    &
                    g_xllCorner_Land, g_yllCorner_Land, g_CellSize_Land, &
                    g_NoData_Value, bIsError,g_ResultFormat)
        end if
        ! failure area
        if(g_bGOVar(14))then
            where(g_mask_fine/=g_NoData_Value)
                dblTemp_Land = g_failure_area
            elsewhere
                dblTemp_Land = g_NoData_Value
            end where

            call WriteMatrixFile(trim(g_ResultPath)//"GOVar_"  &
                    //trim(g_sGOVarName(14)) // "_" //trim(strDate),  &
                    dblTemp_Land, g_NCols_Land,g_NRows_Land,    &
                    g_xllCorner_Land, g_yllCorner_Land, g_CellSize_Land, &
                    g_NoData_Value, bIsError,g_ResultFormat)
        end if

    end if

    return
end subroutine ExportGridVar


subroutine Export_HDF5(strDate,Rain,PET,EPot,EAct, &
        W,Runoff,ExcS,ExcI,RS,RI, SM, HDF5_WriteCount)
    use CREST_Project
    use CREST_Basic
    use CREST_Param
    use SoilDownscale_Basic
    use Landslide_Basic
    use hdf5_utils
    use hdf5
    implicit none
    character*(*) strDate
    logical :: bIsError

    integer(HID_T) :: file_id
    character(len=3)::H5_STATUS
    integer :: HDF5_WriteCount
    double precision :: Rain(0:g_NCols-1,0:g_NRows-1)
    double precision :: PET(0:g_NCols-1,0:g_NRows-1)
    double precision :: EPot(0:g_NCols-1,0:g_NRows-1)
    double precision :: W(0:g_NCols-1,0:g_NRows-1)
    double precision :: SM(0:g_NCols-1,0:g_NRows-1)
    double precision :: ExcS(0:g_NCols-1,0:g_NRows-1)
    double precision :: ExcI(0:g_NCols-1,0:g_NRows-1)
    double precision :: RI(0:g_NCols-1,0:g_NRows-1)
    double precision :: RS(0:g_NCols-1,0:g_NRows-1)
    double precision :: EAct(0:g_NCols-1,0:g_NRows-1)
    double precision :: Runoff(0:g_NCols-1,0:g_NRows-1)
    integer :: dblTemp(0:g_NCols-1,0:g_NRows-1)
    integer :: dblTemp_Land(0:g_NCols_Land-1,0:g_NRows_Land-1)


    ! don't print the HDF5 write message
    call hdf_set_print_messages(.false.)

    if (HDF5_WriteCount == 1) then ! HDF5 file does not exist, then create it
        H5_STATUS = "NEW"
        call hdf_open_file(file_id, trim(g_ResultPath)//"Result_all.h5", &
                STATUS = H5_STATUS, ACTION='WRITE')
        ! creat the group for three types datasets
        call hdf_create_group(file_id, "Meteorology")
        call hdf_create_group(file_id, "Hydrology")
        call hdf_create_group(file_id, "Landslide")
        ! set the Attributes
        call hdf_write_attribute(file_id, "Meteorology", "NCols", g_NCols)
        call hdf_write_attribute(file_id, "Meteorology", "NRows", g_NRows)
        call hdf_write_attribute(file_id, "Meteorology", "XLLCorner", g_xllCorner)
        call hdf_write_attribute(file_id, "Meteorology", "YLLCorner", g_yllCorner)
        call hdf_write_attribute(file_id, "Meteorology", "CellSize", g_CellSize)

        call hdf_write_attribute(file_id, "Hydrology", "NCols", g_NCols)
        call hdf_write_attribute(file_id, "Hydrology", "NRows", g_NRows)
        call hdf_write_attribute(file_id, "Hydrology", "XLLCorner", g_xllCorner)
        call hdf_write_attribute(file_id, "Hydrology", "YLLCorner", g_yllCorner)
        call hdf_write_attribute(file_id, "Hydrology", "CellSize", g_CellSize)

        call hdf_write_attribute(file_id, "Landslide", "NCols", g_NCols_Land)
        call hdf_write_attribute(file_id, "Landslide", "NRows", g_NRows_Land)
        call hdf_write_attribute(file_id, "Landslide", "XLLCorner", g_xllCorner_Land)
        call hdf_write_attribute(file_id, "Landslide", "YLLCorner", g_yllCorner_Land)
        call hdf_write_attribute(file_id, "Landslide", "CellSize", g_CellSize_Land)
    else
        H5_STATUS = "OLD"
        call hdf_open_file(file_id, trim(g_ResultPath)//"Result_all.h5", &
                STATUS = H5_STATUS, ACTION='WRITE')
    end if


    ! write out the Meteorological data (rain and ET)
    ! rain
    if(g_bGOVar(0))then
        where(g_Mask/=g_NoData_Value)
            dblTemp=Rain/g_TimeStep * 100
        elsewhere
            dblTemp=g_NoData_Value
        end where

        ! write the datasets
        call hdf_write_dataset(file_id, "Meteorology/"//"GOVar_"  &
                //trim(g_sGOVarName(0)) // "_" //trim(strDate), &
                dblTemp, filter='gzip+shuffle')
    end if

    ! PET
    if(g_bGOVar(1))then
        where(g_Mask/=g_NoData_Value)
            dblTemp=PET/g_TimeStep * 100
        elsewhere
            dblTemp=g_NoData_Value
        end where

        ! write the datasets
        call hdf_write_dataset(file_id, "Meteorology/"//"GOVar_"  &
                //trim(g_sGOVarName(1)) // "_" //trim(strDate), &
                dblTemp, filter='gzip+shuffle')
    end if

    ! EPOT
    if(g_bGOVar(2))then
        where(g_Mask/=g_NoData_Value)
            dblTemp=EPot/g_TimeStep * 100
        elsewhere
            dblTemp=g_NoData_Value
        end where

        ! write the datasets
        call hdf_write_dataset(file_id, "Meteorology/"//"GOVar_"  &
                //trim(g_sGOVarName(2)) // "_" //trim(strDate), &
                dblTemp, filter='gzip+shuffle')
    end if

    ! EAct
    if(g_bGOVar(3))then
        where(g_Mask/=g_NoData_Value)
            dblTemp=EAct/g_TimeStep * 100
        elsewhere
            dblTemp=g_NoData_Value
        end where

        ! write the datasets
        call hdf_write_dataset(file_id, "Meteorology/"//"GOVar_"  &
                //trim(g_sGOVarName(3)) // "_" //trim(strDate), &
                dblTemp, filter='gzip+shuffle')
    end if

    ! W
    if(g_bGOVar(4))then
        where(g_Mask/=g_NoData_Value)
            dblTemp=W * 100
        elsewhere
            dblTemp=g_NoData_Value
        end where

        ! write the datasets
        call hdf_write_dataset(file_id, "Hydrology/"//"GOVar_"  &
                //trim(g_sGOVarName(4)) // "_" //trim(strDate), &
                dblTemp, filter='gzip+shuffle')
    end if

    ! SM
    if(g_bGOVar(5))then
        where(g_Mask/=g_NoData_Value)
            dblTemp=SM * 100
        elsewhere
            dblTemp=g_NoData_Value
        end where

        ! write the datasets
        call hdf_write_dataset(file_id, "Hydrology/"//"GOVar_"  &
                //trim(g_sGOVarName(5)) // "_" //trim(strDate), &
                dblTemp, filter='gzip+shuffle')
    end if

    ! Runoff
    if(g_bGOVar(6))then
        where(g_Mask/=g_NoData_Value)
            dblTemp=Runoff * 100
        elsewhere
            dblTemp=g_NoData_Value
        end where

        ! write the datasets
        call hdf_write_dataset(file_id, "Hydrology/"//"GOVar_"  &
                //trim(g_sGOVarName(6)) // "_" //trim(strDate), &
                dblTemp, filter='gzip+shuffle')
    end if



    ! ExcS
    if(g_bGOVar(7))then
        where(g_Mask/=g_NoData_Value)
            dblTemp=ExcS/g_TimeStep * 100
        elsewhere
            dblTemp=g_NoData_Value
        end where

        ! write the datasets
        call hdf_write_dataset(file_id, "Hydrology/"//"GOVar_"  &
                //trim(g_sGOVarName(7)) // "_" //trim(strDate), &
                dblTemp, filter='gzip+shuffle')
    end if

    ! ExcI
    if(g_bGOVar(8))then
        where(g_Mask/=g_NoData_Value)
            dblTemp=ExcI/g_TimeStep * 100
        elsewhere
            dblTemp=g_NoData_Value
        end where

        ! write the datasets
        call hdf_write_dataset(file_id, "Hydrology/"//"GOVar_"  &
                //trim(g_sGOVarName(8)) // "_" //trim(strDate), &
                dblTemp, filter='gzip+shuffle')
    end if

    ! RS
    if(g_bGOVar(9))then
        where(g_Mask/=g_NoData_Value)
            dblTemp=RS/g_TimeStep * 100
        elsewhere
            dblTemp=g_NoData_Value
        end where

        ! write the datasets
        call hdf_write_dataset(file_id, "Hydrology/"//"GOVar_"  &
                //trim(g_sGOVarName(9)) // "_" //trim(strDate), &
                dblTemp, filter='gzip+shuffle')
    end if

    ! RI
    if(g_bGOVar(10))then
        where(g_Mask/=g_NoData_Value)
            dblTemp=RI/g_TimeStep * 100
        elsewhere
            dblTemp=g_NoData_Value
        end where

        ! write the datasets
        call hdf_write_dataset(file_id, "Hydrology/"//"GOVar_"  &
                //trim(g_sGOVarName(10)) // "_" //trim(strDate), &
                dblTemp, filter='gzip+shuffle')
    end if

    ! only when using HYDROSLIDE3D will output following variables
    if (g_ModelCore == 3) then

        ! FS3D
        if(g_bGOVar(11))then
            where(g_mask_fine/=g_NoData_Value .and. g_FS_3D /=g_NoData_Value)
                dblTemp_Land = g_FS_3D * 100
            elsewhere
                dblTemp_Land = g_NoData_Value
            end where

            ! write the datasets
            call hdf_write_dataset(file_id, "Landslide/"//"GOVar_"  &
                    //trim(g_sGOVarName(11)) // "_" //trim(strDate), &
                    dblTemp_Land, filter='gzip+shuffle')
        end if

        ! failure probability
        if(g_bGOVar(12))then
            where(g_mask_fine/=g_NoData_Value .and. g_probability /=g_NoData_Value)
                dblTemp_Land = g_probability * 100
            elsewhere
                dblTemp_Land = g_NoData_Value
            end where

            ! write the datasets
            call hdf_write_dataset(file_id, "Landslide/"//"GOVar_"  &
                    //trim(g_sGOVarName(12)) // "_" //trim(strDate), &
                    dblTemp_Land, filter='gzip+shuffle')
        end if

        ! failure volume
        if(g_bGOVar(13))then
            where(g_mask_fine/=g_NoData_Value .and. g_failure_volume /=g_NoData_Value)
                dblTemp_Land = g_failure_volume * 100
            elsewhere
                dblTemp_Land = g_NoData_Value
            end where

            ! write the datasets
            call hdf_write_dataset(file_id, "Landslide/"//"GOVar_"  &
                    //trim(g_sGOVarName(13)) // "_" //trim(strDate), &
                    dblTemp_Land, filter='gzip+shuffle')
        end if

        ! failure area
        if(g_bGOVar(14))then
            where(g_mask_fine/=g_NoData_Value .and. g_failure_area /=g_NoData_Value)
                dblTemp_Land = g_failure_area * 100
            elsewhere
                dblTemp_Land = g_NoData_Value
            end where

            ! write the datasets
            call hdf_write_dataset(file_id, "Landslide/"//"GOVar_"  &
                    //trim(g_sGOVarName(14)) // "_" //trim(strDate), &
                    dblTemp_Land, filter='gzip+shuffle')
        end if

    end if

    ! close the h5 file
    call hdf_close_file(file_id)

end subroutine Export_HDF5


! # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
subroutine CalculateOutletData(k,Rain,PET,EPot,EAct,  &
        W,Runoff,ExcS,ExcI,RS,RI)
    use CREST_Project
    use CREST_Basic
    use CREST_Param
    implicit none
    integer :: N,k
    double precision :: Rain(0:g_NCols-1,0:g_NRows-1)
    double precision :: PET(0:g_NCols-1,0:g_NRows-1)
    double precision :: EPot(0:g_NCols-1,0:g_NRows-1)
    double precision :: W(0:g_NCols-1,0:g_NRows-1)
    double precision :: ExcS(0:g_NCols-1,0:g_NRows-1)
    double precision :: ExcI(0:g_NCols-1,0:g_NRows-1)
    double precision :: RI(0:g_NCols-1,0:g_NRows-1)
    double precision :: RS(0:g_NCols-1,0:g_NRows-1)
    double precision :: EAct(0:g_NCols-1,0:g_NRows-1)
    double precision :: Runoff(0:g_NCols-1,0:g_NRows-1)
    !-----------------------------------------------
    !Output the state of outlet
    N=count(g_tOutlet%Mask/= g_NoData_Value)
    if(g_tOutlet%bIsOut .eqv. .false.)then
        g_tOutlet%Rain(k)=sum(Rain/g_TimeStep,  &
                g_tOutlet%Mask/= g_NoData_Value)/N
        g_tOutlet%PET(k)=sum(PET/g_TimeStep,  &
                g_tOutlet%Mask/= g_NoData_Value)/N
        g_tOutlet%EPot(k)=sum(EPot/g_TimeStep,  &
                g_tOutlet%Mask/= g_NoData_Value)/N
        g_tOutlet%EAct(k)=sum(EAct/g_TimeStep, &
                g_tOutlet%Mask/= g_NoData_Value)/N
        g_tOutlet%W(k)=sum(W, &
                g_tOutlet%Mask/= g_NoData_Value)/N
        g_tOutlet%SM(k)=sum(W/g_tParams%WM,  &
                g_tOutlet%Mask/= g_NoData_Value)/N

        g_tOutlet%R(k)=Runoff(g_tOutlet%Col, &
                g_tOutlet%Row)

        g_tOutlet%ExcS(k)=sum(ExcS/g_TimeStep,  &
                g_tOutlet%Mask/= g_NoData_Value)/N
        g_tOutlet%ExcI(k)=sum(ExcI/g_TimeStep, &
                g_tOutlet%Mask/= g_NoData_Value)/N
        g_tOutlet%RS(k)=sum(RS/g_TimeStep,  &
                g_tOutlet%Mask/= g_NoData_Value)/N
        g_tOutlet%RI(k)=sum(RI/g_TimeStep, &
                g_tOutlet%Mask/= g_NoData_Value)/N
    ELSE
        g_tOutlet%Rain(k)=g_NoData_Value
        g_tOutlet%PET(k)=g_NoData_Value
        g_tOutlet%EPot(k)=g_NoData_Value
        g_tOutlet%EAct(k)=g_NoData_Value
        g_tOutlet%W(k)=g_NoData_Value
        g_tOutlet%SM(k)=g_NoData_Value
        g_tOutlet%R(k)=g_NoData_Value
        g_tOutlet%ExcS(k)=g_NoData_Value
        g_tOutlet%ExcI(k)=g_NoData_Value
        g_tOutlet%RS(k)=g_NoData_Value
        g_tOutlet%RI(k)=g_NoData_Value
    END IF
    return
end subroutine CalculateOutletData

subroutine ExportOutletData()
    use CREST_Project
    implicit none
    integer :: fileid
    integer :: i,k
    integer :: dtTemp(1:6)
    double precision :: NSCE, Bias, CC

    call XXWGetFreeFile(fileid)


    if (g_tOutlet%bIsOut .eqv. .true.) then !Outlet is not in Basin area
        return
    end if

    open(fileid,file=trim(g_ResultPath)//"Outlet_" &
            //trim(g_tOutlet%Name) // "_Results.csv", &
            form="formatted")
    if(g_tOutlet%HasOBS .eqv. .true.)then
        write(fileid,*) "DateTime,",  &
                "Rain,", &
                "PET,", &
                "EPot,", &
                "EAct,", &
                "W,", &
                "SM,", &
                "RS,", &
                "RI,", &
                "ExcS,", &
                "ExcI,", &
                "R,","RObs"
    else
        write(fileid,*) "DateTime,", &
                "Rain,",  &
                "PET,",  &
                "EPot,",  &
                "EAct,",  &
                "W,",  &
                "SM,", &
                "RS,",  &
                "RI,", &
                "ExcS,", &
                "ExcI,",  &
                "R"
    end if

    dtTemp=g_StartDate

    do k=0, ubound(g_tOutlet%Rain,1)
        write(fileid,"(I4,A1,I2,A1,I2,A1,I2,A1,I2,A1,I2,A1)", &
                advance='no') &
                dtTemp(1),"-",  &
                dtTemp(2),"-", &
                dtTemp(3)," ", &
                dtTemp(4),":",  &
                dtTemp(5),":",  &
                dtTemp(6),","

        write(fileid,"(10(F10.3,A1),F10.3)",advance='no')  &
                g_tOutlet%Rain(k),",",  &
                g_tOutlet%PET(k),",",  &
                g_tOutlet%EPot(k),",",  &
                g_tOutlet%EAct(k),",", &
                g_tOutlet%W(k),",",  &
                g_tOutlet%SM(k),",", &
                g_tOutlet%RS(k),",",  &
                g_tOutlet%RI(k),",", &
                g_tOutlet%ExcS(k),",", &
                g_tOutlet%ExcI(k),",",  &
                g_tOutlet%R(k)
        if(g_tOutlet%HasOBS .eqv. .true.)then
            write(fileid,"(A1,F10.3)")  &
                    ",",g_tOutlet%RObs(k)
        else
            write(fileid,*)
        end if
        call myDAdd(g_TimeMark,g_TimeStep,dtTemp)
    end do
    close(fileid)

    if(g_tOutlet%HasOBS .eqv. .true.)then
        call XXWGetFreeFile(fileid)
        open(fileid,file=trim(g_ResultPath)//"Outlet_" &
                //trim(g_tOutlet%Name) // "_Results_Statistics.csv", &
                form="formatted")

        call GetNSCE(g_tOutlet%RObs,  &
                g_tOutlet%R,g_NumWarmup,g_ITMax,  &
                NSCE)
        call GetBias(g_tOutlet%RObs, &
                g_tOutlet%R,g_NumWarmup,g_ITMax,  &
                Bias)
        call GetCC(g_tOutlet%RObs,  &
                g_tOutlet%R,g_NumWarmup,g_ITMax,  &
                CC)
        write(fileid,*)"NSCE,",NSCE
        write(fileid,*)"Bias(%),",Bias
        write(fileid,*)"CC,",CC

        close(fileid)
    end if
    return
end subroutine ExportOutletData
!########################################################

subroutine CalculateOutPixData(k,Rain,PET,EPot,EAct,   &
        W,Runoff,ExcS,ExcI,RS,RI)
    use CREST_Project
    use CREST_Basic
    use CREST_Param
    implicit none
    integer :: N,k,i
    double precision :: Rain(0:g_NCols-1,0:g_NRows-1)
    double precision :: PET(0:g_NCols-1,0:g_NRows-1)
    double precision :: EPot(0:g_NCols-1,0:g_NRows-1)
    double precision :: W(0:g_NCols-1,0:g_NRows-1)
    double precision :: ExcS(0:g_NCols-1,0:g_NRows-1)
    double precision :: ExcI(0:g_NCols-1,0:g_NRows-1)
    double precision :: RI(0:g_NCols-1,0:g_NRows-1)
    double precision :: RS(0:g_NCols-1,0:g_NRows-1)
    double precision :: EAct(0:g_NCols-1,0:g_NRows-1)
    double precision :: Runoff(0:g_NCols-1,0:g_NRows-1)
    !-----------------------------------------------
    !Output the state of the Pixes

    DO i = 0, g_NOutPixs-1
        N=count(g_tOutPix(i)%Mask/= g_NoData_Value)
        IF (g_tOutPix(i)%bIsOut .eqv. .false.) THEN
            g_tOutPix(i)%Rain(k)=sum(Rain/g_TimeStep,  &
                    g_tOutPix(i)%Mask/= g_NoData_Value)/N
            g_tOutPix(i)%PET(k)=sum(PET/g_TimeStep, &
                    g_tOutPix(i)%Mask/= g_NoData_Value)/N
            g_tOutPix(i)%EPot(k)=sum(EPot/g_TimeStep, &
                    g_tOutPix(i)%Mask/= g_NoData_Value)/N
            g_tOutPix(i)%EAct(k)=sum(EAct/g_TimeStep, &
                    g_tOutPix(i)%Mask/= g_NoData_Value)/N
            
            g_tOutPix(i)%W(k)=sum(W,  &
                    g_tOutPix(i)%Mask/= g_NoData_Value)/N
            g_tOutPix(i)%SM(k)=sum(W/g_tParams%WM,  &
                    g_tOutPix(i)%Mask/= g_NoData_Value)/N

            g_tOutPix(i)%R(k)=Runoff(g_tOutPix(i)%Col,  &
                    g_tOutPix(i)%Row)

            g_tOutPix(i)%ExcS(k)=sum(ExcS/g_TimeStep,  &
                    g_tOutPix(i)%Mask/= g_NoData_Value)/N
            g_tOutPix(i)%ExcI(k)=sum(ExcI/g_TimeStep,  &
                    g_tOutPix(i)%Mask/= g_NoData_Value)/N
            !g_tOutPix(i)%RS(k)=sum(RS/g_TimeStep,  &
            !g_tOutPix(i)%Mask/= g_NoData_Value)/N
            !g_tOutPix(i)%RI(k)=sum(RI/g_TimeStep, &
            !g_tOutPix(i)%Mask/= g_NoData_Value)/N
            ! modified by cgd: output the pixcel value of RS and RI
            g_tOutPix(i)%RS(k) = RS(g_tOutPix(i)%Col,  g_tOutPix(i)%Row)
            g_tOutPix(i)%RI(k) = RI(g_tOutPix(i)%Col,  g_tOutPix(i)%Row)

        ELSE
            g_tOutPix(i)%Rain(k)=g_NoData_Value
            g_tOutPix(i)%PET(k)=g_NoData_Value
            g_tOutPix(i)%EPot(k)=g_NoData_Value
            g_tOutPix(i)%EAct(k)=g_NoData_Value
            g_tOutPix(i)%W(k)=g_NoData_Value
            g_tOutPix(i)%SM(k)=g_NoData_Value
            g_tOutPix(i)%R(k)=g_NoData_Value
            g_tOutPix(i)%ExcS(k)=g_NoData_Value
            g_tOutPix(i)%ExcI(k)=g_NoData_Value
            g_tOutPix(i)%RS(k)=g_NoData_Value
            g_tOutPix(i)%RI(k)=g_NoData_Value
        END IF
    end do
    return
end subroutine CalculateOutPixData

!---------------------------------------------------------------------
subroutine ExportPixData()
    use CREST_Project
    implicit none
    integer :: fileid
    integer :: i,k
    integer	:: dtTemp(1:6)
    double precision :: NSCE, Bias, CC

    do i = 0, g_NOutPixs-1
        if (g_tOutPix(i)%bIsOut .eqv. .true.) then !OutPix is not in Basin area
            cycle
        end if

        call XXWGetFreeFile(fileid)

        open(fileid,file=trim(g_ResultPath)//"OutPix_"  &
                //trim(g_tOutPix(i)%Name) // "_Results.csv",  &
                form="formatted")
        if(g_tOutPix(i)%HasOBS .eqv. .true.)then
            write(fileid,*) "DateTime,",  &
                    "Rain,", &
                    "PET,",  &
                    "EPot,",  &
                    "EAct,",  &
                    "W,",  &
                    "SM,", &
                    "RS,",  &
                    "RI,", &
                    "ExcS,", &
                    "ExcI,",  &
                    "R,","RObs"
        else
            write(fileid,*) "DateTime,", &
                    "Rain,", &
                    "PET,", &
                    "EPot,", &
                    "EAct,",  &
                    "W,",  &
                    "SM,",   &
                    "RS,", &
                    "RI,",  &
                    "ExcS,",   &
                    "ExcI,",  &
                    "R"
        end if

        dtTemp=g_StartDate

        do k=0, ubound(g_tOutPix(i)%Rain,1)
            write(fileid,"(I4,A1,I2,A1,I2,A1,I2,A1,I2,A1,I2,A1)",  &
                    advance='no')  &
                    dtTemp(1),"-", &
                    dtTemp(2),"-",  &
                    dtTemp(3)," ",  &
                    dtTemp(4),":",  &
                    dtTemp(5),":",  &
                    dtTemp(6),","

            write(fileid,"(10(F10.3,A1),F10.3)",advance='no')  &
                    g_tOutPix(i)%Rain(k),",",  &
                    g_tOutPix(i)%PET(k),",",  &
                    g_tOutPix(i)%EPot(k),",",  &
                    g_tOutPix(i)%EAct(k),",", &
                    g_tOutPix(i)%W(k),",",  &
                    g_tOutPix(i)%SM(k),",",  &
                    g_tOutPix(i)%RS(k),",",   &
                    g_tOutPix(i)%RI(k),",",  &
                    g_tOutPix(i)%ExcS(k),",",  &
                    g_tOutPix(i)%ExcI(k),",",  &
                    g_tOutPix(i)%R(k)

            if(g_tOutPix(i)%HasOBS .eqv. .true.)then
                write(fileid,"(A1,F10.3)") &
                        ",",g_tOutPix(i)%RObs(k)
            else
                write(fileid,*)
            end if


            call myDAdd(g_TimeMark,g_TimeStep,dtTemp)
        end do

        close(fileid)

        if(g_tOutPix(i)%HasOBS .eqv. .true.)then
            call XXWGetFreeFile(fileid)
            open(fileid,file=trim(g_ResultPath)//"OutPix_"  &
                    //trim(g_tOutPix(i)%Name) // "_Results_Statistics.csv", &
                    form="formatted")

            call GetNSCE(g_tOutPix(i)%RObs,  &
                    g_tOutPix(i)%R,g_NumWarmup,g_ITMax,   &
                    NSCE)
            call GetBias(g_tOutPix(i)%RObs, &
                    g_tOutPix(i)%R,g_NumWarmup,g_ITMax, &
                    Bias)
            call GetCC(g_tOutPix(i)%RObs,  &
                    g_tOutPix(i)%R,g_NumWarmup,g_ITMax, &
                    CC)
            write(fileid,*)"NSCE,",NSCE
            write(fileid,*)"Bias(%),",Bias
            write(fileid,*)"CC,",CC

            close(fileid)
        end if
    end do

    return
end subroutine ExportPixData



