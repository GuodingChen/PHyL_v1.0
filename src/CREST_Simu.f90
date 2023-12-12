subroutine CREST_Simu()
    use CREST_Project
    use CREST_Basic
    use CREST_ICS
    use CREST_Param
    use CREST_Calib_SCEUA
    use OMP_LIB
    use SoilDownscale_Basic
    use Landslide_Basic
    use LandslideModel_parameters

    implicit none
    double precision :: NSCE, Bias, CC
    integer :: ITMax, P_monitor
    integer :: i,j,k,ii,jj, Pass_i, i_basin, HDF5_WriteCount
    integer :: RS_SumLoop_start, RS_SumLoop_end, RI_SumLoop_start, RI_SumLoop_end
    integer :: dtTemp(1:6)
    double precision :: HydroTime_start, HydroTime_end
    logical :: bIsError
    character(14):: strDate
    character(50):: sTemp
    character(len=3):: i_basin_str
    integer, allocatable::OneRowA(:,:),OneRowB(:,:)
    integer, allocatable::OneColA(:,:),OneColB(:,:)
    integer, allocatable::TwoRowA(:,:),TwoRowB(:,:)
    integer, allocatable::TwoColA(:,:),TwoColB(:,:)
    double precision, allocatable :: OnePerA(:,:)
    double precision, allocatable :: OnePerB(:,:)
    double precision, allocatable :: TwoPerA(:,:)
    double precision, allocatable :: TwoPerB(:,:)

    integer, allocatable :: RS_PassCell_StartIndex(:,:)
    integer, allocatable :: RS_PassCell_EndIndex(:,:)
    integer, allocatable :: RI_PassCell_StartIndex(:,:)
    integer, allocatable :: RI_PassCell_EndIndex(:,:)

    ! record the passing cell for runoff calculation (Guoding Chen in TUDelft)
    integer, allocatable :: RS_PassCell_Row(:)
    integer, allocatable :: RS_PassCell_Col(:)
    integer, allocatable :: RI_PassCell_Row(:)
    integer, allocatable :: RI_PassCell_Col(:)
    integer, allocatable :: RS_PassCell_Row_pre(:)
    integer, allocatable :: RS_PassCell_Col_pre(:)
    integer, allocatable :: RI_PassCell_Row_pre(:)
    integer, allocatable :: RI_PassCell_Col_pre(:)
    integer :: RS_RAM_pre, RI_RAM_pre, RS_RAM, RI_RAM
    ! basical variable for hydrological module
    double precision, allocatable :: Rain(:,:),PET(:,:),EPot(:,:)
    double precision, allocatable :: W0(:,:),SS0(:,:),SI0(:,:)
    double precision, allocatable :: W(:,:),ExcS(:,:),ExcI(:,:), g_SM(:,:)
    double precision, allocatable :: RI(:,:),RS(:,:)
    double precision, allocatable :: EAct(:,:),Runoff(:,:)
    double precision, allocatable :: NextTime(:,:)
    integer,allocatable :: g_SubMask(:,:)
    !-----------------------------------------------
    allocate(OneRowA(0:g_NCols-1,0:g_NRows-1))
    allocate(OneRowB(0:g_NCols-1,0:g_NRows-1))
    allocate(OneColA(0:g_NCols-1,0:g_NRows-1))
    allocate(OneColB(0:g_NCols-1,0:g_NRows-1))

    allocate(TwoRowA(0:g_NCols-1,0:g_NRows-1))
    allocate(TwoRowB(0:g_NCols-1,0:g_NRows-1))
    allocate(TwoColA(0:g_NCols-1,0:g_NRows-1))
    allocate(TwoColB(0:g_NCols-1,0:g_NRows-1))

    allocate(OnePerA(0:g_NCols-1,0:g_NRows-1))
    allocate(OnePerB(0:g_NCols-1,0:g_NRows-1))
    allocate(TwoPerA(0:g_NCols-1,0:g_NRows-1))
    allocate(TwoPerB(0:g_NCols-1,0:g_NRows-1))

    allocate(RS_PassCell_StartIndex(0:g_NCols-1,0:g_NRows-1))
    allocate(RS_PassCell_EndIndex(0:g_NCols-1,0:g_NRows-1))
    allocate(RI_PassCell_StartIndex(0:g_NCols-1,0:g_NRows-1))
    allocate(RI_PassCell_EndIndex(0:g_NCols-1,0:g_NRows-1))
    allocate(NextTime(0:g_NCols-1,0:g_NRows-1))

    !---------------------------------------------------
    allocate(Rain(0:g_NCols-1,0:g_NRows-1))
    allocate(PET(0:g_NCols-1,0:g_NRows-1))
    allocate(EPot(0:g_NCols-1,0:g_NRows-1))

    allocate(W0(0:g_NCols-1,0:g_NRows-1))
    allocate(SS0(0:g_NCols-1,0:g_NRows-1))
    allocate(SI0(0:g_NCols-1,0:g_NRows-1))

    allocate(W(0:g_NCols-1,0:g_NRows-1))
    allocate(g_SM(0:g_NCols-1,0:g_NRows-1))
    allocate(ExcS(0:g_NCols-1,0:g_NRows-1))
    allocate(ExcI(0:g_NCols-1,0:g_NRows-1))
    allocate(EAct(0:g_NCols-1,0:g_NRows-1))
    allocate(Runoff(0:g_NCols-1,0:g_NRows-1))

    allocate(RS(0:g_NCols-1,0:g_NRows-1))
    allocate(RI(0:g_NCols-1,0:g_NRows-1))
    allocate(g_SubMask(0:g_NCols-1,0:g_NRows-1))


    where(g_Mask/=g_NoData_Value)
        W0=g_WU0*g_tParams%WM/100.0
    elsewhere
        W0=g_NoData_Value
    end where

    SS0=g_SS0
    SI0=g_SI0

    Rain=g_NoData_Value
    PET=g_NoData_Value
    EPot=g_NoData_Value !Initial Potential Evapotranspiration
    EAct=g_NoData_Value
    W=g_NoData_Value
    g_SM = g_NoData_Value
    Runoff=g_NoData_Value
    ExcS=g_NoData_Value
    ExcI=g_NoData_Value
    RS=g_NoData_Value
    RI=g_NoData_Value


    ! initial the HDF5_write count

    HDF5_WriteCount = 0

    ! initial the time tic
    HydroRunTime = 0
    LandRunTime = 0

    g_ITMax=myDdIFf(g_TimeMark, g_StartDate, g_EndDate)  &
            / g_TimeStep + 1
    ITMax=g_ITMax

    if(g_RunStyle/=2)then
        if(g_tOutlet%HasOutlet .eqv. .true.)then
            allocate(g_tOutlet%Rain(0:ITMax-1))
            allocate(g_tOutlet%PET(0:ITMax-1))
            allocate(g_tOutlet%EPot(0:ITMax-1))
            allocate(g_tOutlet%EAct(0:ITMax-1))
            allocate(g_tOutlet%W(0:ITMax-1))
            allocate(g_tOutlet%SM(0:ITMax-1))
            allocate(g_tOutlet%R(0:ITMax-1))
            allocate(g_tOutlet%ExcS(0:ITMax-1))
            allocate(g_tOutlet%ExcI(0:ITMax-1))
            allocate(g_tOutlet%RS(0:ITMax-1))
            allocate(g_tOutlet%RI(0:ITMax-1))
        end if

        do k=0,g_NOutPixs-1
            allocate(g_tOutPix(k)%Rain(0:ITMax-1))
            allocate(g_tOutPix(k)%PET(0:ITMax-1))
            allocate(g_tOutPix(k)%EPot(0:ITMax-1))
            allocate(g_tOutPix(k)%EAct(0:ITMax-1))
            allocate(g_tOutPix(k)%W(0:ITMax-1))
            allocate(g_tOutPix(k)%SM(0:ITMax-1))
            allocate(g_tOutPix(k)%R(0:ITMax-1))
            allocate(g_tOutPix(k)%ExcS(0:ITMax-1))
            allocate(g_tOutPix(k)%ExcI(0:ITMax-1))
            allocate(g_tOutPix(k)%RS(0:ITMax-1))
            allocate(g_tOutPix(k)%RI(0:ITMax-1))
        end do
    end if

    call CalNextTime(NextTime)
    ! estimate the possible memeory might be occupied: assign a very huge one here
    RS_RAM_pre = g_TimeStep / minval(NextTime, mask =  NextTime > 0) * count(g_Mask == 1)
    ! allocate the very huge roouting scheme
    allocate(RS_PassCell_Row_pre(0:RS_RAM_pre))
    allocate(RS_PassCell_Col_pre(0:RS_RAM_pre))

    ! prepare for overland flow:
    call CREST_RouteTreat(NextTime,OneRowA,OneColA,OnePerA, &
            OneRowB,OneColB,OnePerB, RS_PassCell_Row_pre, RS_PassCell_Col_pre, &
            RS_PassCell_StartIndex, RS_PassCell_EndIndex, RS_RAM_pre)

    ! calculate the NextTime according to the parameter: coeS
    where(NextTime/=g_NoData_Value)
        NextTime=NextTime/g_tParams%coeS
    end where
    ! estimate the possible memory for soil interflow
    RI_RAM_pre = g_TimeStep / minval(NextTime, mask =  NextTime > 0) * count(g_Mask == 1)
    allocate(RI_PassCell_Row_pre(0:RI_RAM_pre))
    allocate(RI_PassCell_Col_pre(0:RI_RAM_pre))

    ! prepare for interflow:
    call CREST_RouteTreat(NextTime,TwoRowA,TwoColA,TwoPerA, &
            TwoRowB,TwoColB,TwoPerB, RI_PassCell_Row_pre, &
            RI_PassCell_Col_pre, RI_PassCell_StartIndex, &
            RI_PassCell_EndIndex, RI_RAM_pre)

    ! calculate the real memory for runoff passing array after executing the
    ! RouteTreat process
    RS_RAM = count(RS_PassCell_Row_pre > 0)
    RI_RAM = count(RI_PassCell_Row_pre > 0)


    allocate(RS_PassCell_Row(0: RS_RAM))
    allocate(RS_PassCell_Col(0: RS_RAM))
    allocate(RI_PassCell_Row(0: RI_RAM))
    allocate(RI_PassCell_Col(0: RI_RAM))
    ! using the new passing array to save the RAM
    RS_PassCell_Row(0: RS_RAM) = RS_PassCell_Row_pre(0: RS_RAM)
    RS_PassCell_Col(0: RS_RAM) = RS_PassCell_Col_pre(0: RS_RAM)

    RI_PassCell_Row(0: RI_RAM) = RI_PassCell_Row_pre(0: RI_RAM)
    RI_PassCell_Col(0: RI_RAM) = RI_PassCell_Col_pre(0: RI_RAM)

    deallocate(NextTime)
    deallocate(RS_PassCell_Row_pre)
    deallocate(RS_PassCell_Col_pre)
    deallocate(RI_PassCell_Row_pre)
    deallocate(RI_PassCell_Col_pre)


    ! Load the State Data
    if(g_LoadState .eqv. .true.)then !.and. g_RunStyle/=2

        dtTemp=g_StartDate   ! Modified by Xianwu Xue 2011.3.24
        call myDadd(g_TimeMark,-g_TimeStep,dtTemp)! Modified by Xianwu Xue 2011.3.24

        strDate=myDtoStr(dtTemp, g_TimeMark)! Modified by Xianwu Xue 2011.3.24

        write(*,*) "Loading States at date: " // strDate
        write(g_CREST_LogFileID,*)  &
                "Loading States at date: " // strDate

        call LoadStates(strDate,W0,SS0,SI0,bIsError)
        if(bIsError .eqv. .true.)then ! File does not exist!
            write(*,"(1X,A)")  &
                    "*** Something wrong in your States file!"
            write(g_CREST_LogFileID,"(1X,A)")  &
                    "*** Something wrong in your States file!"

            return
        end if
    end if

    dtTemp=g_StartDate




    do k=0, ITMax-1
        strDate=myDtoStr(dtTemp, g_TimeMark)

        P_monitor = (k+1) * 100 / ITMax * 100 / 100

        if(g_RunStyle/=2)then !#y(year);m(month);d(day);h(hour);u(minute);s(second)
            if(trim(g_TimeMark)=="y")then
                sTemp=strDate(1:4)
            elseif(trim(g_TimeMark)=="m")then
                sTemp=strDate(1:4)//"-"//strDate(5:6)
            elseif(trim(g_TimeMark)=="d")then
                sTemp=strDate(1:4)//"-"//strDate(5:6)//"-"//strDate(7:8)
            elseif(trim(g_TimeMark)=="h")then
                sTemp=strDate(1:4)//"-"//strDate(5:6)//"-"//strDate(7:8)&
                        //" "//strDate(9:10)//":"//"00"//":"//"00"
            elseif(trim(g_TimeMark)=="u")then
                sTemp=strDate(1:4)//"-"//strDate(5:6)//"-"//strDate(7:8) &
                        //" "//strDate(9:10)//":"//strDate(11:12)//":"//"00"
            elseif(trim(g_TimeMark)=="s")then
                sTemp=strDate(1:4)//"-"//strDate(5:6)//"-"//strDate(7:8) &
                        //" "//strDate(9:10)//":"//strDate(11:12)//":"//strDate(13:14)
            endif

            write(*,"(g0,1X,A, A, g0, 1X, A)") k+1, trim(sTemp), " (Finished: ", P_monitor, "%)"
            write(g_CREST_LogFileID,*) k+1," ", trim(sTemp)
        end if

        !write(*,*)"Reading Rain File!"
        
        
        call ReadMatrixFile(TRIM(g_RainPath),  &
                Rain,  &
                g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
                g_CellSize,g_NoData_Value,bIsError,g_RainFormat, &
                TRIM(strDate))

        

        if(bIsError .eqv. .true.)then
            if(g_RunStyle==2)then !Added by Dr. Xianwu Xue 2011.4.16
            else
                write(*,"(15X,A)")"---Rain Data File Missed!"
                where(Rain>0) !Added by Dr. Xianwu Xue 2011.5.9
                    Rain=Rain/g_TimeStep
                end where
            end if
        end if

        !write(*,*)"Reading PET File!"
        call ReadMatrixFile(TRIM(g_PETPath),  &
                PET, &
                g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
                g_CellSize,g_NoData_Value,bIsError,g_PETFormat, &
                TRIM(strDate))
        if(bIsError .eqv. .true.)then
            if(g_RunStyle==2)then !Added by Dr. Xianwu Xue 2011.4.16
            else
                write(*,"(15X,A)")"---PET Data File Missed!"
                where(PET>0)  !Added by Dr. Xianwu Xue 2011.5.9
                    PET=PET/g_TimeStep
                end where
            end if
        end if
        
        ! calculate the hydrological state of channel pixels
        do i = 1, Npixel_channel
            ii = ChannelIndex_Rows(i)
            jj = ChannelIndex_Cols(i)

            !Convert unite of Rain and PET from mm/dt to mm
            if(Rain(jj,ii)>0.0)then
                Rain(jj,ii)=Rain(jj,ii) * g_TimeStep
            else
                Rain(jj,ii)=0.0
            end if
            if(PET(jj,ii)>0.0)then
                PET(jj,ii)=PET(jj,ii)*g_TimeStep
            else
                PET(jj,ii)=0.0
            end if

            call CREST_PrecipInt(Rain(jj,ii),  &
                    g_tParams%RainFact(jj,ii),Rain(jj,ii))

            call CREST_EPotential(PET(jj,ii),  &
                    g_tParams%KE(jj,ii),EPot(jj,ii))

            call CREST_RunoffGen(W0(jj,ii), &
                    Rain(jj,ii),EPot(jj,ii),  &
                    g_tParams%WM(jj,ii),g_tParams%IM(jj,ii),  &
                    g_tParams%B(jj,ii),g_tParams%Ksat(jj,ii)*g_TimeStep,  &
                    W(jj,ii),ExcS(jj,ii),ExcI(jj,ii))

            call CREST_EAct(W0(jj,ii), &
                    Rain(jj,ii),EPot(jj,ii),W(jj,ii),EAct(jj,ii))

            SS0(jj,ii)=SS0(jj,ii)+ExcS(jj,ii)
            RS(jj,ii)=SS0(jj,ii)*g_tParams%KS(jj,ii)
            SS0(jj,ii)=SS0(jj,ii)*(1.0-g_tParams%KS(jj,ii))

            SI0(jj,ii)=SI0(jj,ii)+ExcI(jj,ii)
            RI(jj,ii)=SI0(jj,ii)*g_tParams%KI(jj,ii)
            SI0(jj,ii)=SI0(jj,ii)*(1.0-g_tParams%KI(jj,ii))
            ! this "/3.6" only consider the relationship between
            ! the timescale of second and hour (1000/3600)
            ! please change the code if your TimeMark is day or others
            Runoff(jj,ii)=((RS(jj,ii)+RI(jj,ii)) / g_TimeStep) * g_GridArea(jj,ii)/3.6  !m3/s


            W0(jj,ii)=W(jj,ii)



        end do



        ! activate the hydrological threads
        call omp_set_num_threads(Nthread_hydro)
        !call omp_set_num_threads(2)
        !write (*,*) OMP_GET_MAX_THREADS()

        HydroTime_start = OMP_get_wtime()

        !-------------hydrological parallel region start-----------------
        !$OMP PARALLEL PRIVATE(g_SubMask, i_basin_str, i, j, ii, jj, &
        !$OMP& Pass_i, RS_SumLoop_start, RS_SumLoop_end, RI_SumLoop_start, &
        !$OMP& RI_SumLoop_end) DEFAULT(SHARED)
        !$OMP DO
        do i_basin = 1, N_Subbasin
            write(i_basin_str , '(i3)') i_basin
            ! read the mask of all sub-basins for each computing thread
            g_SubMask = Subbasin_assemble(i_basin, :, :)
            ! print '("Thread: ", i0)', omp_get_thread_num()
            ! calculate the hillslope process based on sub-basins
            ! DO NOT redo the calculation for channel pixels
            do i=0, g_NRows-1
                do j=0, g_NCols-1
                    if(g_SubMask(j,i)==g_NoData_Value .or. &
                            g_Stream(j,i) == 1)then
                        cycle
                    end if

                    !Convert unite of Rain and PET from mm/dt to mm
                    if(Rain(j,i)>0.0)then
                        Rain(j,i)=Rain(j,i) * g_TimeStep
                    else
                        Rain(j,i)=0.0
                    end if
                    if(PET(j,i)>0.0)then
                        PET(j,i)=PET(j,i)*g_TimeStep
                    else
                        PET(j,i)=0.0
                    end if

                    call CREST_PrecipInt(Rain(j,i),  &
                            g_tParams%RainFact(j,i),Rain(j,i))

                    call CREST_EPotential(PET(j,i),  &
                            g_tParams%KE(j,i),EPot(j,i))

                    call CREST_RunoffGen(W0(j,i), &
                            Rain(j,i),EPot(j,i),  &
                            g_tParams%WM(j,i),g_tParams%IM(j,i),  &
                            g_tParams%B(j,i),g_tParams%Ksat(j,i)*g_TimeStep,  &
                            W(j,i),ExcS(j,i),ExcI(j,i))

                    call CREST_EAct(W0(j,i), &
                            Rain(j,i),EPot(j,i),W(j,i),EAct(j,i))

                    SS0(j,i)=SS0(j,i)+ExcS(j,i)
                    RS(j,i)=SS0(j,i)*g_tParams%KS(j,i)
                    SS0(j,i)=SS0(j,i)*(1.0-g_tParams%KS(j,i))

                    SI0(j,i)=SI0(j,i)+ExcI(j,i)
                    RI(j,i)=SI0(j,i)*g_tParams%KI(j,i)
                    SI0(j,i)=SI0(j,i)*(1.0-g_tParams%KI(j,i))

                    Runoff(j,i)=((RS(j,i)+RI(j,i)) / g_TimeStep) * g_GridArea(j,i)/3.6  !m3/s

                    W0(j,i)=W(j,i)

                end do
            end do

            ! sum the passing runoff cell (both RS and RI)
            do i=0, g_NRows-1
                do j=0, g_NCols-1

                    if(g_SubMask(j,i)==g_NoData_Value)then
                        cycle
                    end if


                    RS_SumLoop_start =  RS_PassCell_StartIndex(j,i)
                    RS_SumLoop_end =  RS_PassCell_EndIndex(j,i)
                    RI_SumLoop_start = RI_PassCell_StartIndex(j,i)
                    RI_SumLoop_end = RI_PassCell_EndIndex(j,i)

                    ! sum the passing runoff for overland flow, from (j,i) to (jj,ii)
                    do Pass_i = RS_SumLoop_start, RS_SumLoop_end
                        ii = RS_PassCell_Row(Pass_i)
                        jj = RS_PassCell_Col(Pass_i)
                        if(InBasin(jj,ii) .eqv. .true.)then
                            Runoff(jj,ii) = Runoff(jj,ii) + (RS(j,i) / g_TimeStep) * g_GridArea(j,i)/3.6  !m3/s
                        end if

                    end do


                    ! sum the passing runoff for inter flow, from (j,i) to (jj,ii)
                    do Pass_i = RI_SumLoop_start, RI_SumLoop_end
                        ii = RI_PassCell_Row(Pass_i)
                        jj = RI_PassCell_Col(Pass_i)
                        if(InBasin(jj,ii) .eqv. .true.)then
                            Runoff(jj,ii) = Runoff(jj,ii) + (RI(j,i) / g_TimeStep) * g_GridArea(j,i)/3.6  !m3/s
                        end if

                    end do


                end do
            end do

            !DownStream Routing and hydrological state update
            do i=0, g_NRows-1
                do j=0, g_NCols-1
                    if(g_SubMask(j,i)==g_NoData_Value)then
                        cycle
                    end if

                    ii=OneRowA(j,i)
                    jj=OneColA(j,i)
                    if(InBasin(jj,ii) .eqv. .true.)then
                        SS0(jj,ii)=SS0(jj,ii)+RS(j,i)*OnePerA(j,i)  &
                                *g_GridArea(j,i)/g_GridArea(jj,ii)
                    end if

                    ii=OneRowB(j,i)
                    jj=OneColB(j,i)
                    if(InBasin(jj,ii) .eqv. .true.)then
                        SS0(jj,ii)=SS0(jj,ii)+RS(j,i)*OnePerB(j,i)  &
                                *g_GridArea(j,i)/g_GridArea(jj,ii)
                    end if

                    ii=TwoRowA(j,i)
                    jj=TwoColA(j,i)
                    if(InBasin(jj,ii) .eqv. .true.)then
                        SI0(jj,ii)=SI0(jj,ii)+RI(j,i)*TwoPerA(j,i)  &
                                *g_GridArea(j,i)/g_GridArea(jj,ii)
                    end if

                    ii=TwoRowB(j,i)
                    jj=TwoColB(j,i)
                    if(InBasin(jj,ii) .eqv. .true.)then
                        SI0(jj,ii)=SI0(jj,ii)+RI(j,i)*TwoPerB(j,i) &
                                *g_GridArea(j,i)/g_GridArea(jj,ii)
                    end if

                end do
            end do


        end do
        !$OMP END DO
        !$OMP END PARALLEL
        HydroTime_end = OMP_get_wtime()
        HydroRunTime = HydroRunTime + (HydroTime_end - HydroTime_start)
        ! write(*,*) TimeRecord_end - TimeRecord_start



        !-------------hydrological parallel region end-----------------
        
        ! calculate the soil moisture (SM) of hydrological model, unit: %
        where(g_Mask/=g_NoData_Value)
            g_SM = W*100.0/(g_tParams%WM)
        elsewhere
            g_SM = g_NoData_Value
        end where

        ! do the soil downscaling process
        if (g_ModelCore == 3) then

            ! activate the landslide module threads
            call omp_set_num_threads(Nthread_Land)

            g_SM_fine = g_NoData_Value
            g_FS_3D = g_NoData_Value
            g_failure_volume = g_NoData_Value
            g_failure_area = g_NoData_Value
            g_probability = g_NoData_Value
            g_cal_count = 0
            g_unstable_count = 0
            if (g_NOutDTs == 0) then
                call SoilDownscale_pre(g_SM)
                call Landslide_module()

            else
                ! only activate the
                Do i=lbound(g_OutDTIndex,1), ubound(g_OutDTIndex,1)
                    if(g_OutDTIndex(i)==k)then
                        call SoilDownscale_pre(g_SM)
                        call Landslide_module()
                    end if
                end do

            end if
        end if

!        call WriteMatrixFile(trim(g_ResultPath)//"g_probability",  &
!                g_probability, g_NCols_Land,g_NRows_Land,    &
!                    g_xllCorner_Land,g_yllCorner_Land, &
!                    g_CellSize_Land, g_NoData_Value, &
!                    bIsError,g_ResultFormat)



        if(g_RunStyle/=2)then

            !Output the state of outlet
            if(g_tOutlet%HasOutlet)then
                call CalculateOutletData(k,Rain,PET,EPot,EAct, &
                        W,Runoff,ExcS,ExcI,RS,RI)
            end if

            !Output the state of the Pixes
            call CalculateOutPixData(k,Rain,PET,EPot,EAct,  &
                    W,Runoff,ExcS,ExcI,RS,RI)

            !output the continuous time series
            if (g_NOutDTs == 0) then
                select case (trim(g_ResultFormat))
                    case("HDF5")
                        HDF5_WriteCount = HDF5_WriteCount + 1
                        call Export_HDF5(strDate,Rain,PET,EPot,EAct,  &
                                W,Runoff,ExcS,ExcI,RS,RI, g_SM, HDF5_WriteCount)
                    case("ASC")
                        call ExportGridVar(strDate,Rain,PET,EPot,EAct,  &
                                W,Runoff,ExcS,ExcI,RS,RI, g_SM)
                end select
            end if

            !Output specific Date
            Do i=lbound(g_OutDTIndex,1), ubound(g_OutDTIndex,1)
                if(g_OutDTIndex(i)==k)then

                    select case (trim(g_ResultFormat))
                        case("HDF5")
                            HDF5_WriteCount = HDF5_WriteCount + 1
                            call Export_HDF5(strDate,Rain,PET,EPot,EAct,  &
                                    W,Runoff,ExcS,ExcI,RS,RI, g_SM, HDF5_WriteCount)
                        case("ASC")
                            call ExportGridVar(strDate,Rain,PET,EPot,EAct,  &
                                    W,Runoff,ExcS,ExcI,RS,RI, g_SM)
                    end select


                end if
            end do

        end if

        if(g_RunStyle==2)then
            ii=g_tCalibSta(g_RegNum)%Row
            jj=g_tCalibSta(g_RegNum)%Col
            g_tCalibSta(g_RegNum)%RSim(k)=Runoff(jj,ii)
        end if

        call myDAdd(g_TimeMark,g_TimeStep,dtTemp)


!        stop


    end do !End loop



    if(g_RunStyle/=2)then

        call ExportPixData()

        ! Save the State Data
        call SaveStates(strDate,W0,SS0,SI0)

        if(g_tOutlet%HasOutlet)then
            call ExportOutletData()

            if(g_tOutlet%HasOBS .eqv. .true.)then
                call GetNSCE(g_tOutlet%RObs, &
                        g_tOutlet%R,g_NumWarmup,g_ITMax, &
                        NSCE)
                call GetBias(g_tOutlet%RObs, &
                        g_tOutlet%R,g_NumWarmup,g_ITMax, &
                        Bias)
                call GetCC(g_tOutlet%RObs, &
                        g_tOutlet%R,g_NumWarmup,g_ITMax, &
                        CC)

                write(*,*)
                write(*,*)"  The results of the Outlet is: "
                write(*,"(5X,A9,F15.8)")"NSCE: ",NSCE
                write(*,"(5X,A9,F15.8)")"Bias(%): ",Bias
                write(*,"(5X,A9,F15.8)")"CC: ",CC

                write(g_CREST_LogFileID,*)
                write(g_CREST_LogFileID,*)"  The results of the Outlet is: "
                write(g_CREST_LogFileID,*)"     NSCE: ",NSCE
                write(g_CREST_LogFileID,*)"     Bias(%): ",Bias
                write(g_CREST_LogFileID,*)"     CC: ",CC
            end if
        end if

    end if

    write(g_CREST_LogFileID,*)"Hydrological runtime (s): ", HydroRunTime
    write(g_CREST_LogFileID,*)"Landslide runtime (s): ", LandRunTime
    write(*,*)"Hydrological runtime (s): ", HydroRunTime
    write(*,*)"Landslide runtime (s): ", LandRunTime

    ! deallocate
    deallocate(OneRowA)
    deallocate(OneRowB)
    deallocate(OneColA)
    deallocate(OneColB)

    deallocate(TwoRowA)
    deallocate(TwoRowB)
    deallocate(TwoColA)
    deallocate(TwoColB)

    deallocate(OnePerA)
    deallocate(OnePerB)
    deallocate(TwoPerA)
    deallocate(TwoPerB)

    deallocate(RS_PassCell_Row)
    deallocate(RS_PassCell_Col)
    deallocate(RI_PassCell_Row)
    deallocate(RI_PassCell_Col)
    deallocate(RS_PassCell_StartIndex)
    deallocate(RS_PassCell_EndIndex)
    deallocate(RI_PassCell_StartIndex)
    deallocate(RI_PassCell_EndIndex)

    ! record the passing cell for runoff calculation (Guoding Chen in TUDelft)


    return
end subroutine


