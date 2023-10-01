!
!******************************************************************************
!       The Coupled Routing and Excelss STorage (CREST) Version 2.0(02-02-2011)
!                Developed by Dr. Xianwu Xue and Dr. Yang Hong
!
!  						  Previous versions(e.g. v1.6c) were developed
!										by Dr. Jiahu Wang and Dr. Yang Hong
!                 Copyright (c) 2014 University of OKlahoma
!                   All rights reserved
!       THE UNIVERSITY OF OKLAHOMA MAKE NO REPRESENTATION OR WARRANTIES
!       WITH RESPECT TO THE CONTENTS HEROF AND SPECIFICALLY DISCLAIM ANY
!       IMPLIED WARRANTIES OR MERCHANTABILITY OR FITNESS FOR ANY
!       PARTICULAR PURPOSE.
!
!       Further, we reserve the right to revise this software and/or
!       documentation and to make changes from time to time in the
!       content without obligation to notify any person of such revision
!       or change.
!
!       Please cite the following reference if you use CREST model:
!       Wang, J., H. Yang, L. Li, et al. (2011), The coupled routing
!          and excess storage(CREST) distributed hydrological model,
!          Hydrological Sciences Journal, 56(1), 84 - 98.
!       Xue, X., Y. Hong, A. S. Limaye, et al. (2013), Statistical and hydrological evaluation
!          of TRMM-based Multi-satellite Precipitation Analysis over the Wangchu Basin of Bhutan:
!          Are the latest satellite precipitation products 3B42V7 ready for use in ungauged basins?,
!          Journal of Hydrology, 499, 91-99.
!
!       If you have any problem with this model, please e-mail:
!       Dr. Xianwu Xue (xuexianwu@ou.edu) or Dr. Yang Hong (yanghong@ou.edu)
!
!******************************************************************************
!
!	Main Program of CREST
program CREST_Main

    use CREST_Project
    use CREST_Basic
    use CREST_Param
    use CREST_ICS
    use Landslide_Basic
    use LandslideModel_parameters
    use OMP_LIB

    implicit none

    INTEGER :: IBDT(8),IEDT(8)
    integer :: I
    logical :: bIsError, ControlFile_Exist

    ! read the control file that named as "Control.Project"
    g_PrjNP = trim("Control.Project")

    inquire(file = trim(g_PrjNP), exist = ControlFile_Exist)

    if (ControlFile_Exist .eqv. .false.) then
        write(*,*) "Can't find your Contorl file!"
        stop
    end if

    write(*,*)
    write(*,"(25X,A)") "PHyL_v1.0"
    write(*,"(1X,A)") "A massively parallel coupled hydrological-geotechnical framework"
    write(*,"(25X,'Version ',A)") g_CREST_Version

    call UtiGetAndDisplayBeginTime(IBDT)
    write(g_strD,"(I4,'.',I2.2,'.',I2.2,'-',I2.2,'.',I2.2,'.',I2.2)") &
            (IBDT(I),I=1,3),(IBDT(I),I=5,7)

    !Write to the log file

    call XXWGetFreeFile(g_CREST_LogFileID)

    open(g_CREST_LogFileID,file &
            = "logs//"//"PHyL_v1.0-"//trim(g_strD)//".log",form='formatted')

    write(g_CREST_LogFileID,"(25X,A)") "PHyL_v1.0"
    write(g_CREST_LogFileID,"(1X,A)") "A massively parallel coupled &
            hydrological-geotechnical framework"
    write(g_CREST_LogFileID,"(25X,'Version ',A)") g_CREST_Version

    !Get the begin time and then display on Screen
    call UtiGetAndDisplayBeginTimeToFile(IBDT)

    call CREST_Main_Pre(bIsError)


    if(bIsError .eqv. .true.)then
        stop
    end if


    write(*,"(2X,A)") "--------All necessary data for the model has been read---------"
    write(*,*)
    write(*,"(2X,A)") "------parallel setup----------"
    if (g_ModelCore == 1) then ! only for hydrological modeling
        write(*,*) "Subbasin number: ", N_Subbasin
        write(*,*) "Thread for hydrology: ", Nthread_hydro

    elseif (g_ModelCore == 3) then ! for both hydrology and landslide
        write(*,"(2X, A, g0)") "Subbasin number: ", N_Subbasin
        write(*,"(2X, A, g0)") "Thread for hydrology: ", Nthread_hydro
        write(*,"(2X, A, g0)") "Total tile number for landslide: ", total_tile_number
        write(*,"(2X, A, g0)") "Thread for landslide: ", Nthread_Land
        write(*,"(2X, A, g0)") "ellipse density: ", ellipse_density

    end if
    ! save to the log file
    write(g_CREST_LogFileID,"(2X,A)") "--------All necessary data for the model has been read---------"
    write(g_CREST_LogFileID,*)
    write(g_CREST_LogFileID,"(2X,A)") "------parallel setup----------"
    if (g_ModelCore == 1) then ! only for hydrological modeling
        write(g_CREST_LogFileID,*) "Subbasin number: ", N_Subbasin
        write(g_CREST_LogFileID,*) "Thread for hydrology: ", Nthread_hydro

    elseif (g_ModelCore == 3) then ! for both hydrology and landslide
        write(g_CREST_LogFileID,"(2X, A, g0)") "Subbasin number: ", N_Subbasin
        write(g_CREST_LogFileID,"(2X, A, g0)") "Thread for hydrology: ", Nthread_hydro
        write(g_CREST_LogFileID,"(2X, A, g0)") "Total tile numbber for landslide: ", total_tile_number
        write(g_CREST_LogFileID,"(2X, A, g0)") "Thread for landslide: ", Nthread_Land
        write(g_CREST_LogFileID,"(2X, A, g0)") "ellipse density: ", ellipse_density

    end if

    ! the parallel preprocessing is prepared based on g_ModelCore
    select case(g_ModelCore)
        case(1) ! only simulate the hydrological processes
            call Parallel_hydro_pre()
        case(3) ! coupled hydrological-stability simulation
            call Parallel_hydro_pre()
            call Parallel_land_pre()
        case default ! default only for hydrology model
            call Parallel_hydro_pre()
    end select

    if(bIsError .eqv. .true.)then
        stop
    end if

    write(*,*)
    write(g_CREST_LogFileID,*)

    select case (trim(g_sRunStyle))
    case ("SIMU")
        write(*,"(2X,A)") &
                "Running Style is Simulation!"
        write(g_CREST_LogFileID,"(2X,A)") &
                "Running Style is Simulation!"

        write(*,*)"  Model Start Time: "&
                //myDtoStr(g_StartDate, g_TimeMark)
        write(*,*)"  Model End Time: " &
                //myDtoStr(g_EndDate, g_TimeMark)
        write(g_CREST_LogFileID,*)"  Model Start Time: " &
                //myDtoStr(g_StartDate, g_TimeMark)
        write(g_CREST_LogFileID,*)"  Model End Time: " &
                //myDtoStr(g_EndDate, g_TimeMark)
        write(*,*)
        write(g_CREST_LogFileID,*)

        call CREST_Simu()

    case ("CALI_SCEUA")
        write(*,"(2X,A)") &
                "Running Style is Calibration using SCE-UA!"
        write(g_CREST_LogFileID,"(2X,A)") &
                "Running Style is Calibration using SCE-UA!"

        call CREST_SCEUA()

    case ("REALTIME")
        write(*,"(2X,A)") &
                "Running Style is Real Time!"
        write(g_CREST_LogFileID,"(2X,A)") &
                "Running Style is Real Time!"

        call CREST_RealTime()

    case ("REPE")
        write(*,"(2X,A)") &
                "Running Style is Return Period!"
        write(g_CREST_LogFileID,"(2X,A)") &
                "Running Style is Return Period!"

        call CREST_ReturnPeriod()


    case ("FORECAST_KMDQPF")
        write(*,"(2X,A)") &
                "Running Style is Forecast using KMDQPF!"
        write(g_CREST_LogFileID,"(2X,A)") &
                "Running Style is Forecast!"

        call CREST_Forecast_KMDQPF()

    end select

    write(*,*)
    write(g_CREST_LogFileID,*)

    !Get the end time and then display on Screen
    call UtiGetAndDisplayEndTime(0,IBDT,0)

    write(*,*)"Project: "// trim(g_PrjNP) //" is finished!"

    ! 		write(*,*)"Press any key to continue!"
    ! 		read(*,*)

    close(g_CREST_LogFileID)
    stop
end


!########################################################
subroutine CREST_EAct(W0,P,EPot,W,EAct)

    implicit none

    double precision,intent(in) :: W0,P,EPot,W
    double precision,intent(out) :: EAct


    if(P>EPot)then
        EAct=EPot
    else !P<=EPot
        EAct=W0-W
    end if

    return
end subroutine



!########################################################
subroutine CREST_EPotential(E,EFact,EPot)

    implicit none

    double precision :: E,EFact
    double precision :: EPot

    EPot=E*EFact

    return
end subroutine



!########################################################
subroutine CREST_Forecast_KMDQPF()
    use CREST_Project
    use CREST_Basic
    use CREST_ICS
    use CREST_Param
    use CREST_Calib_SCEUA

    implicit none

    logical ::	bIsError,fExist
    character(14):: strDate
    character(len=200):: sFileNamePath
    integer :: dtTemp(1:6),dtNow(1:6),dtStart(1:6),dtEnd(1:6)
    integer :: k,i

    !Get the QPF Start Date and Time
    !Search today's file
    bIsError=.true.
    call myNow(g_QPFBaseDate)

    g_QPFBaseDate(4)=06  !18  !Modified 2013.2.19 based on Ashutosh Request
    g_QPFBaseDate(5)=0
    g_QPFBaseDate(6)=0
    strDate=myDtoStr(g_QPFBaseDate, g_TimeMark)
    sFileNamePath=trim(g_ForecastPath) &
            //"Rain"//trim(strDate(3:10))//"00" &
            // "_arw_d01.grb1f010000.txt"
    inquire(file=trim(sFileNamePath), exist=fExist)

    if(fExist .eqv. .false.)then
        !           !Search Yesterday's file
        !           call myDadd(g_TimeMark,-24,g_QPFBaseDate)
        !
        ! 	    strDate=myDtoStr(g_QPFBaseDate, g_TimeMark)
        ! 	    sFileNamePath=trim(g_ForecastPath)
        !      &      //"Rain"//trim(strDate(3:10))//"00"
        !      &      // "_arw_d01.grb1f010000.txt"
        ! 	    inquire(file=trim(sFileNamePath), exist=fExist)
        ! 		  if(fExist .eqv. .false.)then
        !             write(*,*)"There is no available QPF Data!"
        ! 	      write(g_CREST_LogFileID,*)
        !      &          "There is no available QPF Data!"
        ! 	      return
        ! 	    end if

        do i=1,15
            call myDadd(g_TimeMark,-24,g_QPFBaseDate)

            strDate=myDtoStr(g_QPFBaseDate, g_TimeMark)
            sFileNamePath=trim(g_ForecastPath) &
                    //"Rain"//trim(strDate(3:10))//"00"  &
                    // "_arw_d01.grb1f010000.txt"
            inquire(file=trim(sFileNamePath), exist=fExist)
            if(fExist .eqv. .false.)then
                !               write(*,*)"There is no available QPF Data!"
                ! 	        write(g_CREST_LogFileID,*)
                !      &          "There is no available QPF Data!"
            else
                exit ! exit do
            end if

        end do
    end if



    g_StartDate=g_QPFBaseDate
    g_EndDate=g_QPFBaseDate
    call myDadd(g_TimeMark,72,g_EndDate)

    g_sQPFBaseDate=myDtoStr(g_QPFBaseDate, g_TimeMark)


    !Get date of last status
    dtStart=g_StartDate
    g_StartDate=g_EndDate
    call myDadd(g_TimeMark,g_TimeStep,g_StartDate)
    bIsError=.true.
    do while(bIsError)
        strDate=myDtoStr(g_StartDate, g_TimeMark)
        call InquireMatrixFile(trim(g_StatePath) &
                // "State_"//trim(strDate)// "_W0",  &
                bIsError,g_StateFormat,"")

        if(myDEqual(g_StartDate,dtStart))then
            bIsError=.false. !Indicate the file exist!
        else
            if(bIsError)then
                call myDadd(g_TimeMark,-g_TimeStep,g_StartDate)
            end if
        end if
    end do

    call myDadd(g_TimeMark,g_TimeStep,g_StartDate) ! Modified by Xianwu Xue 2011.3.24

    if(myDCompare(g_StartDate,g_EndDate,g_TimeMark)==1)then !When g_StartDate<g_EndDate

        write(*,*)"  Model Start Time: " &
                //myDtoStr(g_StartDate, g_TimeMark)
        write(*,*)"  Model End Time: " &
                //myDtoStr(g_EndDate, g_TimeMark)
        write(g_CREST_LogFileID,*)"  Model Start Time: " &
                //myDtoStr(g_StartDate, g_TimeMark)
        write(g_CREST_LogFileID,*)"  Model End Time: " &
                //myDtoStr(g_EndDate, g_TimeMark)
        write(*,*)
        write(g_CREST_LogFileID,*)

        g_LoadState=.true.
        g_SaveState=.false.

        g_WarmupDate=g_StartDate
        g_NumWarmup=0
        g_ITMax=myDdIFf(g_TimeMark, g_StartDate, g_EndDate)  &
                / g_TimeStep + 1
        if(g_tOutlet%HasOBS .eqv. .true.)then
            deallocate(g_tOutlet%RObs)
            allocate(g_tOutlet%RObs(0:g_ITMax-1))
            call XXWReadRunoffObs(trim(g_OBSPath) &
                    //trim(g_tOutlet%Name)//"_Obs.csv", &
                    g_tOutlet%RObs,bIsError)
        end if

        Do i=lbound(g_tOutPix,1), ubound(g_tOutPix,1)
            deallocate(g_tOutPix(i)%RObs)
            allocate(g_tOutPix(i)%RObs(0:g_ITMax-1))
            call XXWReadRunoffObs(trim(g_OBSPath) &
                    //trim(g_tOutPix(i)%Name)//"_Obs.csv", &
                    g_tOutPix(i)%RObs,bIsError)
            if(bIsError .eqv. .true.)then
                g_tOutPix(i)%HasOBS=.false.
                bIsError=.false.
            else
                g_tOutPix(i)%HasOBS=.true.
                bIsError=.false.
            end if
        end do

        call CREST_Simu()

    end if

    return
end subroutine



!########################################################
subroutine CREST_PrecipInt(P,PFact,PInt)

    implicit none

    double precision :: P,PFact
    double precision :: PInt

    PInt=P*PFact

    return
end subroutine



!########################################################
subroutine CREST_RealTime()
    use CREST_Project
    use CREST_Basic
    use CREST_ICS
    use CREST_Param
    use CREST_Calib_SCEUA

    implicit none

    logical ::	bIsError
    character(14):: strDate

    integer :: dtTemp(1:6),dtNow(1:6),dtStart(1:6),dtEnd(1:6)
    integer :: k,i

    call myNow(dtNow)
    dtTemp=dtNow

    select case (g_TimeMark)
    case ("y","Y")
        i=1
    case ("m","M")
        i=2
    case ("d","D")
        i=3
    case ("h","H")
        i=4
    case ("u","U")
        i=5
    case ("s","S")
        i=6
    end select
    do k=i,ubound(dtTemp,1)
        dtTemp(k)=0
    end do
    do while(dtTemp(i)<=dtNow(i))
        dtTemp(i)=dtTemp(i)+g_TimeStep
    end do
    dtTemp(i)=dtTemp(i)-g_TimeStep

    dtEnd=g_EndDate
    if(myDCompare(dtTemp,g_EndDate,g_TimeMark)==-1)then !indicate dtTemp>g_EndDate
        g_EndDate=dtTemp
    end if

    !Get the End date of Rain file
    bIsError=.true.
    do while(bIsError)

        strDate=myDtoStr(g_EndDate, g_TimeMark)
        call InquireMatrixFile(TRIM(g_RainPath),bIsError, &
                g_RainFormat,strDate)
        if(myDEqual(g_StartDate,g_EndDate))then
            bIsError=.false. !Indicate the file exist!
        else
            if(bIsError)then
                call myDadd(g_TimeMark,-g_TimeStep,g_EndDate)
            end if
        end if
    end do

    !Get date of last status
    dtStart=g_StartDate
    g_StartDate=g_EndDate
    call myDadd(g_TimeMark,g_TimeStep,g_StartDate)
    bIsError=.true.
    do while(bIsError)
        strDate=myDtoStr(g_StartDate, g_TimeMark)
        call InquireMatrixFile(trim(g_StatePath)  &
                // "State_"//trim(strDate)// "_W0",  &
                bIsError,g_StateFormat,"")

        if(myDEqual(g_StartDate,dtStart))then
            bIsError=.false. !Indicate the file exist!
        else
            if(bIsError)then
                call myDadd(g_TimeMark,-g_TimeStep,g_StartDate)
            end if
        end if
    end do

    call myDadd(g_TimeMark,g_TimeStep,g_StartDate) ! Modified by Xianwu Xue 2011.3.24

    if(myDCompare(g_StartDate,g_EndDate,g_TimeMark)==1)then !When g_StartDate<g_EndDate

        write(*,*)"  Model Start Time: " &
                //myDtoStr(g_StartDate, g_TimeMark)
        write(*,*)"  Model End Time: "  &
                //myDtoStr(g_EndDate, g_TimeMark)
        write(g_CREST_LogFileID,*)"  Model Start Time: "  &
                //myDtoStr(g_StartDate, g_TimeMark)
        write(g_CREST_LogFileID,*)"  Model End Time: " &
                //myDtoStr(g_EndDate, g_TimeMark)
        write(*,*)
        write(g_CREST_LogFileID,*)

        g_LoadState=.true.
        g_SaveState=.true.

        g_WarmupDate=g_StartDate
        g_NumWarmup=0
        g_ITMax=myDdIFf(g_TimeMark, g_StartDate, g_EndDate)  &
                / g_TimeStep + 1
        if(g_tOutlet%HasOBS .eqv. .true.)then
            deallocate(g_tOutlet%RObs)
            allocate(g_tOutlet%RObs(0:g_ITMax-1))
            call XXWReadRunoffObs(trim(g_OBSPath)  &
                    //trim(g_tOutlet%Name)//"_Obs.csv",  &
                    g_tOutlet%RObs,bIsError)
        end if
        ! 			if(bIsError .eqv. .true.)then
        ! 				g_tOutlet%HasOBS=.false.
        ! 			else
        ! 				g_tOutlet%HasOBS=.true.
        ! 			end if

        Do i=lbound(g_tOutPix,1), ubound(g_tOutPix,1)
            deallocate(g_tOutPix(i)%RObs)
            allocate(g_tOutPix(i)%RObs(0:g_ITMax-1))
            call XXWReadRunoffObs(trim(g_OBSPath)  &
                    //trim(g_tOutPix(i)%Name)//"_Obs.csv",  &
                    g_tOutPix(i)%RObs,bIsError)
            if(bIsError .eqv. .true.)then
                g_tOutPix(i)%HasOBS=.false.
                bIsError=.false.
            else
                g_tOutPix(i)%HasOBS=.true.
                bIsError=.false.
            end if
        end do

        call CREST_Simu()

    end if

    return
end subroutine

!########################################################
subroutine CREST_ReturnPeriod()
    use CREST_Project
    use CREST_Basic

    implicit none

    integer :: i,j,k,l
    integer :: lYmin, lYmax
    Integer ::  D(1:6),DEnd(1:6)
    double precision,allocatable:: Q(:,:,:), V(:,:)
    Character(80):: Filename, strDate
    Logical:: bIsError
    double precision :: temV

    If (g_StartDate(2)<=4) Then
        lYmin=g_StartDate(1)
    Else
        lYmin=g_StartDate(1)+1
    End if

    If (g_EndDate(2)>=9) Then
        lYmax=g_EndDate(1)
    Else
        lYmax=g_EndDate(1)-1
        if(g_EndDate(1)-1<g_StartDate(1))then
            lYmax=lYmin
        end if
    End if

    allocate(Q(lYmin:lYmax, 0:g_NCols-1, 0:g_NRows-1))
    allocate(V(0:g_NCols-1, 0:g_NRows-1))
    Q=g_NoData_Value

    DO k=lYmin, lYmax
        D(1)=k
        D(2)=1
        D(3)=1
        D(4)=0
        D(5)=0
        D(6)=0
        DEnd=D
        DEnd(1)=k+1

        DO While (myDcompare2(D, DEnd,g_TimeMark)==1)
            strDate=myDtoStr(D, g_TimeMark)

            Write(*,*) "  Loading Runoff File: " // trim(strDate)
            Write(g_CREST_LogFileID,*) "  Loading Runoff File: "  &
                    // trim(strDate)

            Filename=trim(g_ResultPath)//"GOVar_"  &
                    //trim("R") // "_" //trim(strDate)

            call ReadMatrixFile(Filename,V,   &
                    g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
                    g_CellSize,g_NoData_Value,bIsError, &
                    g_ResultFormat,"")
            if(bIsError .eqv. .true.)then
                Call myDadd(g_TimeMark, g_TimeStep, D)
                cycle
            end if

            do i=0, g_NRows-1
                do j=0, g_NCols-1
                    if (Q(k,j,i) < V(j,i))then
                        Q(k,j,i) = V(j,i)
                    end if
                end do
            end do

            Call myDadd(g_TimeMark, g_TimeStep, D)
        End DO
    End DO

    Write(*,*) " Sorting for Return Period!"
    Write(g_CREST_LogFileID,*) " Sorting for Return Period!"

    do i=0, g_NRows-1
        do j=0, g_NCols-1
            do k=lYmin, lYmax-1
                do l=k+1, lYmax
                    if (Q(k, j, i)<Q(l, j, i)) then
                        temV=Q(k, j, i)
                        Q(k, j, i)=Q(l, j, i)
                        Q(l, j, i)=temV
                    end if
                end do
            end do
        end do
    end do

    DO k=lYmin, lYmax
        l=lYmax-k+1
        Write (*,*) "Exporting Num", l
        Write (g_CREST_LogFileID,*) "Exporting Num", l

        do i=0, g_NRows-1
            do j=0, g_NCols-1
                V(j,i)=Q(k, j, i)
            END DO
        END DO
        Filename=trim(g_ResultPath) // "Level."   &
                // trim(PosIntMsg(l))

        call WriteMatrixFile(Filename,   &
                V, g_NCols,g_NRows,    &
                g_XLLCorner,g_YLLCorner, g_CellSize,  &
                g_NoData_Value,  &
                bIsError,g_ResultFormat)

    end do

    return
end subroutine


!########################################################






!########################################################
subroutine CREST_RunoffGen(W0,P,EPot,WM,IM,B,Ksat,W,ExcS,ExcI)

    implicit none

    double precision,intent(in) :: W0,P,EPot,WM,IM,B,Ksat
    double precision,intent(out) :: W,ExcS,ExcI

    double precision :: PSoil,R,temX,WMM,A

    if(P>EPot)then
        ! Calculate part of precip that goes into soil
        PSoil=(P-EPot)*(1.0-IM)

        if(W0<WM)then
            WMM=WM*(1.0+B)
            A=WMM*(1.0-(1.0-W0/WM)**(1.0/(1.0+B)))
            if(PSoil+A>=WMM)then
                R=PSoil-(WM-W0)
                W=WM
            else
                R=PSoil-WM*((1.0-A/WMM)**(1.0+B) &
                        -(1.0-(A+PSoil)/WMM)**(1.0+B))
                if(R<0.0)then
                    R=0.0
                end if
                W=W0+PSoil-R
            end if
        else ! W0>WM   the soil is full
            R=PSoil
            W=W0
        end if
        ! Calculate how much water can infiltrate
        temX=((W0+W)/2.0)*KSat/WM
        if(R<=temX)then
            ExcI=R
        else
            ExcI=temX
        end if
        ExcS=R-ExcI+(P-EPot)*IM
        if(ExcS<0)then !Added by Dr.Xianwu Xue 2011.5.13
            ExcS=0.0
        end if
    else !P<=EPot
        ExcS=0.0
        ExcI=0.0
        temX=(EPot-P)*W0/WM
        if(temX<W0)then
            W=W0-temX
        else
            W=0.0
        end if
    end if

    return
end subroutine



!########################################################
subroutine CREST_SCEUA()
    use CREST_Project
    use CREST_Basic
    use CREST_ICS
    use CREST_Param
    use CREST_Calib_SCEUA
    implicit none
    integer :: i,k,j,iOutputID
    double precision,external :: XXWCRESTFunctn
    double precision :: dblValue
    logical :: bIsError
    double precision, allocatable :: a(:)
    double precision, allocatable :: bl(:)
    double precision, allocatable :: bu(:)
    character(len=80) :: strNum

    dblValue=1.0

    call InitParamsType(g_tParams_Cali)
    call InitParamsType(g_tParamsAdj_Cali)
    call AssignValueToParamsType(g_tParamsAdj_Cali,dblValue)
    call EqualParamsType(g_tParams,g_tParams_Cali)

    call XXWGetFreeFile(iOutputID)
    open(unit=iOutputID,file=trim(g_ResultPath) &
            //trim("SCEUAOut_")//trim(g_strD)// '.dat',status='unknown')

    do i=0,g_NCalibStas-1

        g_RegNum=i

        write(strNum,*)g_tCalibSta(i)%Value

        call CREST_Simu_For_Calib_ReadDoneValue(  &
                trim(g_CalibPath)//trim(adjustl(strNum))//"\\",bIsError)
        if(bIsError .eqv. .false.)then
            write(*,"(4X,A14,I3)")"Skiped Region ", &
                    g_tCalibSta(i)%Value
            write(g_CREST_LogFileID,"(4X,A14,I3)")"Skiped Region ", &
                    g_tCalibSta(i)%Value

            cycle
        else ! For Linux by Dr. Xianwu Xue 2011.3.14
            call CREST_Simu_For_Calib_ReadDoneValue( &
                    trim(g_CalibPath)//trim(adjustl(strNum))//"/",bIsError)
            if(bIsError .eqv. .false.)then
                write(*,"(4X,A14,I3)")"Skiped Region ", &
                        g_tCalibSta(i)%Value
                write(g_CREST_LogFileID,"(4X,A14,I3)")"Skiped Region ", &
                        g_tCalibSta(i)%Value
                cycle
            end if
        end if

        if(g_tCalibSta(i)%bIsOut .eqv. .true.)then
            write(*,"(4X,A14,I3,A34)")"Skiped Region ",  &
                    g_tCalibSta(i)%Value, &
                    "Because Station out of this region"
            write(g_CREST_LogFileID,"(4X,A14,I3,A34)")"Skiped Region ", &
                    g_tCalibSta(i)%Value, &
                    "Because Station out of this region"
            ! 				g_RegNum=i
            cycle
        end if
        ! 			g_RegNum=i

        g_nopt=count(g_tCalibSta(i)%bu-g_tCalibSta(i)%bl>0)
        allocate(a(1:g_nopt))
        allocate(bl(1:g_nopt))
        allocate(bu(1:g_nopt))

        k=0
        do j=1,ubound(g_sParamName,1)

            if(g_tCalibSta(i)%bu(j)-g_tCalibSta(i)%bl(j)>0)then
                k=k+1
                a(k)=g_tCalibSta(i)%a(j)
                bl(k)=g_tCalibSta(i)%bl(j)
                bu(k)=g_tCalibSta(i)%bu(j)
            end if
        end do

        call XXWGeneralSCEUA2(a,bl,bu, &
                g_nopt,g_maxn, &
                g_kstop,g_pcento,g_iseed,g_ngs,55, &
                XXWCRESTFunctn,trim(g_ResultPath),iOutputID)

        deallocate(a)
        deallocate(bl)
        deallocate(bu)
    end do
    close(iOutputID)

    g_RunStyle=1
    call CREST_Simu()
    !=========================================

    call CREST_OutDISParamsToFile(trim(g_ResultPath),"")

    return
end subroutine

!-------------------------------------------------------------------
double precision function XXWCRESTFunctn(nopt,x)
    use CREST_Project
    use CREST_Calib_SCEUA
    implicit none
    integer nopt
    double precision :: x(1:nopt)
    character(len=20):: sPrefix

    call CREST_Simu_For_Calib(x)

    call GetNSCE(g_tCalibSta(g_RegNum)%RObs,  &
            g_tCalibSta(g_RegNum)%RSim,g_NumWarmup,g_ITMax,  &
            XXWCRESTFunctn)

    XXWCRESTFunctn=1.0-XXWCRESTFunctn ! need to be modified by cgd

    write(*,"(4X,F20.15,3X,A15,I3)") 1.0-XXWCRESTFunctn,  &
            "Region Number: ",g_tCalibSta(g_RegNum)%Value

    return
end function

!--------------------------------------------------------------
subroutine CREST_OutDISParamsToFile(sWS,sPrefix)
    use CREST_Project
    use CREST_Basic
    use CREST_Param
    implicit none
    logical :: bIsError
    character(*) sWS,sPrefix

    call WriteMatrixFile(trim(sWS)//trim(sPrefix)//"RainFact",  &
            g_tParams%RainFact, g_NCols,g_NRows,  &
            g_XLLCorner,g_YLLCorner,  &
            g_CellSize, g_NoData_Value,bIsError,g_ParamFormat)

    call WriteMatrixFile(trim(sWS)//trim(sPrefix)//"Ksat", &
            g_tParams%Ksat, g_NCols,g_NRows,  &
            g_XLLCorner,g_YLLCorner, &
            g_CellSize, g_NoData_Value,bIsError,g_ParamFormat)

    call WriteMatrixFile(trim(sWS)//trim(sPrefix)//"WM",   &
            g_tParams%WM, g_NCols,g_NRows,    &
            g_XLLCorner,g_YLLCorner,   &
            g_CellSize, g_NoData_Value,bIsError,g_ParamFormat)

    call WriteMatrixFile(trim(sWS)//trim(sPrefix)//"B",   &
            g_tParams%B, g_NCols,g_NRows,   &
            g_XLLCorner,g_YLLCorner,   &
            g_CellSize, g_NoData_Value,bIsError,g_ParamFormat)

    call WriteMatrixFile(trim(sWS)//trim(sPrefix)//"IM",   &
            g_tParams%IM, g_NCols,g_NRows,    &
            g_XLLCorner,g_YLLCorner,   &
            g_CellSize, g_NoData_Value,bIsError,g_ParamFormat)

    call WriteMatrixFile(trim(sWS)//trim(sPrefix)//"KE",  &
            g_tParams%KE, g_NCols,g_NRows,   &
            g_XLLCorner,g_YLLCorner,   &
            g_CellSize, g_NoData_Value,bIsError,g_ParamFormat)

    call WriteMatrixFile(trim(sWS)//trim(sPrefix)//"coeM",  &
            g_tParams%coeM, g_NCols,g_NRows,  &
            g_XLLCorner,g_YLLCorner,   &
            g_CellSize, g_NoData_Value,bIsError,g_ParamFormat)

    call WriteMatrixFile(trim(sWS)//trim(sPrefix)//"expM",   &
            g_tParams%expM, g_NCols,g_NRows,  &
            g_XLLCorner,g_YLLCorner,   &
            g_CellSize, g_NoData_Value,bIsError,g_ParamFormat)

    call WriteMatrixFile(trim(sWS)//trim(sPrefix)//"coeR",  &
            g_tParams%coeR, g_NCols,g_NRows,   &
            g_XLLCorner,g_YLLCorner,  &
            g_CellSize, g_NoData_Value,bIsError,g_ParamFormat)

    call WriteMatrixFile(trim(sWS)//trim(sPrefix)//"coeS",  &
            g_tParams%coeS, g_NCols,g_NRows,   &
            g_XLLCorner,g_YLLCorner,   &
            g_CellSize, g_NoData_Value,bIsError,g_ParamFormat)

    call WriteMatrixFile(trim(sWS)//trim(sPrefix)//"KS",  &
            g_tParams%KS, g_NCols,g_NRows, &
            g_XLLCorner,g_YLLCorner,   &
            g_CellSize, g_NoData_Value,bIsError,g_ParamFormat)

    call WriteMatrixFile(trim(sWS)//trim(sPrefix)//"KI",  &
            g_tParams%KI, g_NCols,g_NRows,  &
            g_XLLCorner,g_YLLCorner,   &
            g_CellSize, g_NoData_Value,bIsError,g_ParamFormat)

end subroutine CREST_OutDISParamsToFile

!---------------------------------------------------------
subroutine CREST_ReadDISParamsFile(sWS,sPrefix,tParams,bIsError)
    use CREST_Project
    use CREST_Basic
    use CREST_Param
    implicit none
    logical :: bIsError
    character(*) sWS,sPrefix
    type(CREST_Params):: tParams

    call ReadMatrixFile(trim(sWS)//trim(sPrefix)//"RainFact",   &
            tParams%RainFact, g_NCols,g_NRows,    &
            g_XLLCorner,g_YLLCorner,   &
            g_CellSize, g_NoData_Value,bIsError,g_ParamFormat,"")
    if(bIsError .eqv. .true.)then
        return
    end if

    call ReadMatrixFile(trim(sWS)//trim(sPrefix)//"Ksat",  &
            tParams%Ksat, g_NCols,g_NRows,  &
            g_XLLCorner,g_YLLCorner,  &
            g_CellSize, g_NoData_Value,bIsError,g_ParamFormat,"")
    if(bIsError .eqv. .true.)then
        return
    end if

    call ReadMatrixFile(trim(sWS)//trim(sPrefix)//"WM",  &
            tParams%WM, g_NCols,g_NRows,   &
            g_XLLCorner,g_YLLCorner,   &
            g_CellSize, g_NoData_Value,bIsError,g_ParamFormat,"")
    if(bIsError .eqv. .true.)then
        return
    end if

    call ReadMatrixFile(trim(sWS)//trim(sPrefix)//"B",   &
            tParams%B, g_NCols,g_NRows,   &
            g_XLLCorner,g_YLLCorner,   &
            g_CellSize, g_NoData_Value,bIsError,g_ParamFormat,"")
    if(bIsError .eqv. .true.)then
        return
    end if

    call ReadMatrixFile(trim(sWS)//trim(sPrefix)//"IM",  &
            tParams%IM, g_NCols,g_NRows,   &
            g_XLLCorner,g_YLLCorner,  &
            g_CellSize, g_NoData_Value,bIsError,g_ParamFormat,"")
    if(bIsError .eqv. .true.)then
        return
    end if

    call ReadMatrixFile(trim(sWS)//trim(sPrefix)//"KE",  &
            tParams%KE, g_NCols,g_NRows,   &
            g_XLLCorner,g_YLLCorner,  &
            g_CellSize, g_NoData_Value,bIsError,g_ParamFormat,"")
    if(bIsError .eqv. .true.)then
        return
    end if

    call ReadMatrixFile(trim(sWS)//trim(sPrefix)//"coeM",  &
            tParams%coeM, g_NCols,g_NRows,    &
            g_XLLCorner,g_YLLCorner,  &
            g_CellSize, g_NoData_Value,bIsError,g_ParamFormat,"")
    if(bIsError .eqv. .true.)then
        return
    end if

    call ReadMatrixFile(trim(sWS)//trim(sPrefix)//"expM",  &
            tParams%expM, g_NCols,g_NRows,   &
            g_XLLCorner,g_YLLCorner,   &
            g_CellSize, g_NoData_Value,bIsError,g_ParamFormat,"")
    if(bIsError .eqv. .true.)then
        return
    end if

    call ReadMatrixFile(trim(sWS)//trim(sPrefix)//"coeR",  &
            tParams%coeR, g_NCols,g_NRows,   &
            g_XLLCorner,g_YLLCorner,  &
            g_CellSize, g_NoData_Value,bIsError,g_ParamFormat,"")
    if(bIsError .eqv. .true.)then
        return
    end if

    call ReadMatrixFile(trim(sWS)//trim(sPrefix)//"coeS",  &
            tParams%coeS, g_NCols,g_NRows,   &
            g_XLLCorner,g_YLLCorner,    &
            g_CellSize, g_NoData_Value,bIsError,g_ParamFormat,"")
    if(bIsError .eqv. .true.)then
        return
    end if

    call ReadMatrixFile(trim(sWS)//trim(sPrefix)//"KS",   &
            tParams%KS, g_NCols,g_NRows, &
            g_XLLCorner,g_YLLCorner,   &
            g_CellSize, g_NoData_Value,bIsError,g_ParamFormat,"")
    if(bIsError .eqv. .true.)then
        return
    end if

    call ReadMatrixFile(trim(sWS)//trim(sPrefix)//"KI",  &
            tParams%KI, g_NCols,g_NRows,  &
            g_XLLCorner,g_YLLCorner,  &
            g_CellSize, g_NoData_Value,bIsError,g_ParamFormat,"")
    if(bIsError .eqv. .true.)then
        return
    end if


end subroutine CREST_ReadDISParamsFile


!------------------------------------------------------
subroutine CREST_Simu_For_Calib_ReadDoneValue(sWS,bIsError)
    use CREST_Project
    use CREST_Basic
    use CREST_ICS
    use CREST_Param
    use CREST_Calib_SCEUA
    implicit none

    integer :: j,k
    type(CREST_Params):: tParams
    logical :: bIsError
    character (*) :: sWS

    bIsError=.false.

    call InitParamsType(tParams)

    call CREST_ReadDISParamsFile(trim(sWS),"", &
            tParams,bIsError)
    if(bIsError .eqv. .true.)then
        return
    end if

    where(g_RegMask==g_tCalibSta(g_RegNum)%Value)

        g_tParams_Cali%RainFact	 &
                =	tParams%RainFact
        g_tParams_Cali%Ksat		 &
                =	tParams%Ksat
        g_tParams_Cali%WM	  &
                =	tParams%WM
        g_tParams_Cali%B	 &
                =	tParams%B
        g_tParams_Cali%IM	 &
                =	tParams%IM
        g_tParams_Cali%KE		 &
                =	tParams%KE
        g_tParams_Cali%coeM	 &
                =	tParams%coeM

        g_tParams_Cali%expM	 &
                =	tParams%expM
        g_tParams_Cali%coeR	 &
                =	tParams%coeR
        g_tParams_Cali%coeS	 &
                =	tParams%coeS
        g_tParams_Cali%KS	 &
                =	tParams%KS
        g_tParams_Cali%KI	 &
                =	tParams%KI



        !-----
        g_tParams%RainFact  &
                =	g_tParams_Cali%RainFact * g_tParamsAdj_Cali%RainFact
        g_tParams%Ksat		  &
                =	g_tParams_Cali%Ksat * g_tParamsAdj_Cali%Ksat
        g_tParams%WM	  &
                =	g_tParams_Cali%WM * g_tParamsAdj_Cali%WM
        g_tParams%B	 &
                =	g_tParams_Cali%B * g_tParamsAdj_Cali%B
        g_tParams%IM	 &
                =	g_tParams_Cali%IM * g_tParamsAdj_Cali%IM
        g_tParams%KE		  &
                =	g_tParams_Cali%KE * g_tParamsAdj_Cali%KE
        g_tParams%coeM		  &
                =	g_tParams_Cali%coeM * g_tParamsAdj_Cali%coeM

        g_tParams%expM		  &
                =	g_tParams_Cali%expM * g_tParamsAdj_Cali%expM
        g_tParams%coeR	 &
                =	g_tParams_Cali%coeR * g_tParamsAdj_Cali%coeR
        g_tParams%coeS	 &
                =	g_tParams_Cali%coeS * g_tParamsAdj_Cali%coeS
        g_tParams%KS	 &
                =	g_tParams_Cali%KS * g_tParamsAdj_Cali%KS
        g_tParams%KI	 &
                =	g_tParams_Cali%KI * g_tParamsAdj_Cali%KI
    end where

    bIsError=.false.

    return
end subroutine

!------------------------------------------------------
subroutine CREST_Simu_For_Calib(x)
    use CREST_Project
    use CREST_Basic
    use CREST_ICS
    use CREST_Param
    use CREST_Calib_SCEUA
    implicit none
    double precision :: x(1:g_nopt)
    integer :: j,k

    k=0
    do j=1,ubound(g_sParamName,1)

        if(g_tCalibSta(g_RegNum)%bu(j) &
                -g_tCalibSta(g_RegNum)%bl(j)>0)then
            k=k+1
            g_tCalibSta(g_RegNum)%x(j)=x(k)
        end if
    end do


    where(g_RegMask==g_tCalibSta(g_RegNum)%Value)

        g_tParamsAdj_Cali%RainFact	=	g_tCalibSta(g_RegNum)%x(1)
        g_tParamsAdj_Cali%Ksat			=	g_tCalibSta(g_RegNum)%x(2)
        g_tParamsAdj_Cali%WM				=	g_tCalibSta(g_RegNum)%x(3)
        g_tParamsAdj_Cali%B					=	g_tCalibSta(g_RegNum)%x(4)
        g_tParamsAdj_Cali%IM				=	g_tCalibSta(g_RegNum)%x(5)
        g_tParamsAdj_Cali%KE				=	g_tCalibSta(g_RegNum)%x(6)
        g_tParamsAdj_Cali%coeM			=	g_tCalibSta(g_RegNum)%x(7)

        g_tParamsAdj_Cali%expM			=	g_tCalibSta(g_RegNum)%x(8)
        g_tParamsAdj_Cali%coeR			=	g_tCalibSta(g_RegNum)%x(9)
        g_tParamsAdj_Cali%coeS			=	g_tCalibSta(g_RegNum)%x(10)
        g_tParamsAdj_Cali%KS				=	g_tCalibSta(g_RegNum)%x(11)
        g_tParamsAdj_Cali%KI				=	g_tCalibSta(g_RegNum)%x(12)

    end where

    where(g_Mask/=g_NoData_Value)
        g_tParams%RainFact  &
                =	g_tParams_Cali%RainFact * g_tParamsAdj_Cali%RainFact
        g_tParams%Ksat	 &
                =	g_tParams_Cali%Ksat * g_tParamsAdj_Cali%Ksat
        g_tParams%WM	  &
                =	g_tParams_Cali%WM * g_tParamsAdj_Cali%WM
        g_tParams%B	  &
                =	g_tParams_Cali%B * g_tParamsAdj_Cali%B
        g_tParams%IM	 &
                =	g_tParams_Cali%IM * g_tParamsAdj_Cali%IM
        g_tParams%KE	  &
                =	g_tParams_Cali%KE * g_tParamsAdj_Cali%KE
        g_tParams%coeM &
                =	g_tParams_Cali%coeM * g_tParamsAdj_Cali%coeM

        g_tParams%expM		  &
                =	g_tParams_Cali%expM * g_tParamsAdj_Cali%expM
        g_tParams%coeR		  &
                =	g_tParams_Cali%coeR * g_tParamsAdj_Cali%coeR
        g_tParams%coeS	  &
                =	g_tParams_Cali%coeS * g_tParamsAdj_Cali%coeS
        g_tParams%KS	 &
                =	g_tParams_Cali%KS * g_tParamsAdj_Cali%KS
        g_tParams%KI	 &
                =	g_tParams_Cali%KI * g_tParamsAdj_Cali%KI

    end where

    call CREST_Simu()

    return
end subroutine
!########################################################


subroutine InquireMatrixFile(FileName_In,bIsError,sFileFormat_In, &
        strDate_In)
    implicit none

    character(*):: FileName_In,sFileFormat_In
    character(*) :: strDate_In
    character(len=200):: FileName
    character(14):: strDate,strTemp
    integer :: intTemp
    logical :: bIsError,fExist
    character(len=10) :: sFileFormat

    sFileFormat=trim(adjustl(sFileFormat_In))
    strDate=trim(strDate_In)

    call UPCASE(sFileFormat)

    bIsError=.false.

    select case (trim(sFileFormat))
        !##########################################################################
    case ("ASBIMO") !For PET--------------------------------------------
        FileName=trim(FileName_In)//"PET025."  &
                //trim(strDate(5:6))// ".txt"
        inquire(file=trim(Filename), exist=fExist)
        if(fExist .eqv. .false.)then
            bIsError=.true.
            return
        end if

        !##########################################################################
    case ("BIBIMO") !For PET--------------------------------------------
        FileName=trim(FileName_In)//"PET025." &
                //trim(strDate(5:6))// ".bif"
        inquire(file=trim(Filename), exist=fExist)
        if(fExist .eqv. .false.)then
            bIsError=.true.
            return
        end if

        !##########################################################################
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

        !##########################################################################
    case ("DBIF") !--------------------------------------------
        FileName=trim(FileName_In)//trim(strDate)// ".dbif"
        inquire(file=trim(Filename), exist=fExist)
        if(fExist .eqv. .false.)then
            bIsError=.true.
            return
        end if

        !##########################################################################
    case ("BIFFIT") !--------------------------------------------
        FileName=trim(FileName_In)//trim(strDate)//".bif"
        inquire(file=trim(Filename), exist=fExist)
        if(fExist .eqv. .false.)then
            bIsError=.true.
            return
        end if

        !##########################################################################
    case ("BBIF") !--------------------------------------------
        FileName=trim(FileName_In)//trim(strDate)//".bbif"
        inquire(file=trim(Filename), exist=fExist)
        if(fExist .eqv. .false.)then
            bIsError=.true.
            return
        end if

        !##########################################################################
    case ("TRMMRT") !--------------------------------------------
        FileName=trim(FileName_In) // "3B42RT."  &
                //trim(strDate(1:10))//".7R2.bin"
        inquire(file=trim(Filename), exist=fExist)
        if(fExist .eqv. .false.)then
            FileName=trim(FileName_In) // "3B42RT."  &
                    //trim(strDate(1:10))//".7.bin"
            inquire(file=trim(Filename), exist=fExist)
            if(fExist .eqv. .false.)then
                FileName=trim(FileName_In) // "3B42RT." &
                        //trim(strDate(1:10))// ".6A.bin"
                inquire(file=trim(Filename), exist=fExist)
                if(fExist .eqv. .false.)then
                    FileName=trim(FileName_In) // "3B42RT."  &
                            //trim(strDate(1:10))// ".6.bin"
                    inquire(file=trim(Filename), exist=fExist)
                    if(fExist .eqv. .false.)then
                        FileName=trim(FileName_In) // "3B42RT."  &
                                //trim(strDate(1:10))// ".bin"
                        inquire(file=trim(Filename), exist=fExist)
                        if(fExist .eqv. .false.)then
                            bIsError=.true.
                            return
                        end if
                    end if
                end if
            end if
        end if

        !##########################################################################
    case ("TRMMV6") !--------------------------------------------
        strTemp=trim(strDate(9:10))
        read(strTemp,*) intTemp
        if(intTemp<10)then
            write(strTemp,"(I1)")intTemp
        else
            write(strTemp,"(I2)")intTemp
        end if

        FileName=trim(FileName_In) // "3B42." &
                //trim(strDate(3:8))//"."//trim(strTemp)//".6.HDF"
        inquire(file=trim(Filename), exist=fExist)
        if(fExist .eqv. .false.)then
            FileName=trim(FileName_In) // "3B42."  &
                    //trim(strDate(3:8))//"."//trim(strTemp)// ".6A.HDF"
            inquire(file=trim(Filename), exist=fExist)
            if(fExist .eqv. .false.)then
                bIsError=.true.
                return
            end if
        end if


        !##########################################################################
    case ("NMQBIN") !------XXW: Not Validate--------------------------------------
        strTemp=trim(strDate(9:10))
        FileName=trim(FileName_In) // "1HRAD.HSR."   &
                //trim(strDate(1:8))//"."//trim(strTemp)//"00"
        inquire(file=trim(Filename), exist=fExist)
        if(fExist .eqv. .false.)then
            bIsError=.true.
            return
        end if

        !##########################################################################
    case ("DIAMOND4") !--------------------------------------------
        FileName=trim(FileName_In)//trim(strDate)//".000.TMP"
        inquire(file=trim(Filename), exist=fExist)
        if(fExist .eqv. .false.)then
            bIsError=.true.
            return
        end if

    case default
        write(*,*) "ERROR!!! This version do not use this Format!"
        bIsError=.true.
        return
    end select

    return
end subroutine InquireMatrixFile



!########################################################
subroutine SaveStates(strDate,W0,SS0,SI0)
    use CREST_Project
    use CREST_Basic

    implicit none
    character*(*) :: strDate
    logical :: bIsError
    double precision :: W0(0:g_NCols-1,0:g_NRows-1)
    double precision :: SS0(0:g_NCols-1,0:g_NRows-1)
    double precision :: SI0(0:g_NCols-1,0:g_NRows-1)
    double precision :: dblTemp(0:g_NCols-1,0:g_NRows-1)

    ! Save the State Data
    if(g_SaveState .eqv. .true.)then
        where(g_Mask/=g_NoData_Value)
            dblTemp=W0
        elsewhere
            dblTemp=g_NoData_Value
        end where
        call WriteMatrixFile(trim(g_StatePath) &
                // "State_"//trim(strDate)// "_W0",  &
                dblTemp,g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
                g_CellSize,g_NoData_Value,bIsError,g_StateFormat)

        where(g_Mask/=g_NoData_Value)
            dblTemp=SS0
        elsewhere
            dblTemp=g_NoData_Value
        end where
        call WriteMatrixFile(trim(g_StatePath)  &
                // "State_"//trim(strDate)// "_SS0", &
                dblTemp,g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
                g_CellSize,g_NoData_Value,bIsError,g_StateFormat)

        where(g_Mask/=g_NoData_Value)
            dblTemp=SI0
        elsewhere
            dblTemp=g_NoData_Value
        end where
        call WriteMatrixFile(trim(g_StatePath) &
                // "State_"//trim(strDate)// "_SI0",  &
                dblTemp,g_NCols, g_NRows,g_XLLCorner,g_YLLCorner,  &
                g_CellSize,g_NoData_Value,bIsError,g_StateFormat)
    end if

    return
end subroutine SaveStates

subroutine LoadStates(strDate,W0,SS0,SI0,bIsError)
    use CREST_Project
    use CREST_Basic

    implicit none
    character*(*) :: strDate
    logical :: bIsError
    double precision :: W0(0:g_NCols-1,0:g_NRows-1)
    double precision :: SS0(0:g_NCols-1,0:g_NRows-1)
    double precision :: SI0(0:g_NCols-1,0:g_NRows-1)

    call ReadMatrixFile(trim(g_StatePath) &
            // "State_"//trim(strDate)// "_W0", &
            W0,g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
            g_CellSize,g_NoData_Value,bIsError,g_StateFormat,"")
    if(bIsError .eqv. .true.)then ! File does not exist!
        return
    end if

    call ReadMatrixFile(trim(g_StatePath) &
            // "State_"//trim(strDate)// "_SS0", &
            SS0,g_NCols, g_NRows,g_XLLCorner,g_YLLCorner,  &
            g_CellSize,g_NoData_Value,bIsError,g_StateFormat,"")

    if(bIsError .eqv. .true.)then ! File does not exist!
        return
    end if


    call ReadMatrixFile(trim(g_StatePath) &
            // "State_"//trim(strDate)// "_SI0", &
            SI0,g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
            g_CellSize,g_NoData_Value,bIsError,g_StateFormat,"")
    if(bIsError .eqv. .true.)then ! File does not exist!
        return
    end if

    bIsError=.false.
    return
end subroutine LoadStates
!########################################################


subroutine XXWReadRunoffObs(sFileNP,dblArray,bIsError)
    use CREST_Project
    implicit none
    character(*)::sFileNP
    logical :: bIsError
    logical :: fExist
    integer :: fileid
    integer :: error
    character(len=200)::sTemp,sTemp2
    double precision :: dblTemp
    integer	:: dtTemp(1:6)
    character(len=14)::strDate
    double precision :: dblArray(0:g_ITMax-1)
    integer :: k

    dblArray=-9999

    bIsError=.false.
    inquire(file=trim(sFileNP), exist=fExist)
    if(fExist .eqv. .false.)then
        bIsError=.true.
        return
    end if
    call XXWGetFreeFile(fileid)

    open(fileid,file=TRIM(sFileNP), form="formatted")

    error=0

    read(fileid,*,iostat=error,end=999)sTemp,sTemp2 !omit the header

    strDate=myDtoStr(g_StartDate, g_TimeMark)

    do while (error==0)
        read(fileid,*,iostat=error,end=999)sTemp,dblTemp
        if(dblTemp<0)dblTemp=-9999
        call myStrtoD(sTemp,dtTemp,g_TimeMark)
        k=myDdIFf(g_TimeMark, g_StartDate, dtTemp)/ g_TimeStep
        if(k>=0 .and. k<g_ITMax)then
            dblArray(k)=dblTemp
        end if

    end do
    999 continue
    close(fileid)

    return
end subroutine XXWReadRunoffObs

!########################################################
! Change resolution and span, including INTerpolation FUNCTION based on resampling technique



!########################################################




!########################################################

!########################################################

!########################################################
!ReadASCFile---------------------------------------------------


SUBROUTINE SwapINT(Vsou)
    IMPLICIT NONE
    INTEGER*2:: Vsou, temI
    CHARACTER*1:: cha2(1:2), cha
    Equivalence (temI, cha2)
    temI=Vsou
    cha=cha2(1)
    cha2(1)=cha2(2)
    cha2(2)=cha
    Vsou=temI
END

!########################################################
!ReadTRMMV6FileHeader------------------------------

subroutine ReadTRMMV6FileHeader(FileName, &
        NCols, NRows,XLLCorner,YLLCorner, &
        CellSize,NoData_Value,bIsError)
    implicit none

    character(*):: Filename
    integer		:: fileID
    integer		:: NCols, NRows,blocklength,i,j
    double precision :: XLLCorner,YLLCorner,CellSize,NoData_Value
    CHARACTER(20):: sTemp

    logical :: bIsError

    bIsError=.false.
    NRows=400
    NCols=1440
    NoData_Value=-9999.
    XLLCorner=-180.
    YLLCorner=-50.
    CellSize=0.25

    return
end subroutine



!ReadTRMMV6File---------------------------------------------------
subroutine ReadTRMMV6File(FileName,dblMat, &
        NCols, NRows,XLLCorner,YLLCorner,  &
        CellSize,NoData_Value,bIsError)
    implicit none

    character(*):: Filename
    integer		:: fileID
    integer		:: NCols, NRows,BlockLength,i,j
    double precision :: XLLCorner,YLLCorner,CellSize,NoData_Value
    CHARACTER(20):: sTemp
    double precision :: dblMat(0:NCols-1,0:NRows-1)
    CHARACTER*298:: EmptySpace
    logical :: bIsError
    Real(4), allocatable:: temS(:,:),temSTemp(:)

    bIsError=.false.
    call XXWGetFreeFile(fileid)

    allocate(temS(0:NRows-1, 0:NCols-1))
    BlockLength=298+NCols*NRows*4
    open(fileid, file=TRIM(FileName), access='direct',  &
            form='unformatted', recl=BlockLength)

    ! Developed by Tiger
    ! 		READ(fileid, rec=1)EmptySpace,
    !      &			((temS(i,j), i=0, NRows-1), j=0, NCols-1) !Tiger
    ! Developed by Tiger

    ! Developed by Xianwu xue 2010.12.26
    allocate(temSTemp(0:NCols-1))
    read(fileid, rec=1)EmptySpace,  &
            ((temS(i,j), i=1, NRows-1),temSTemp(j), j=0, NCols-1)

    do j=0,NCols-1
        temS(0,j)=temSTemp(j)
    end do
    ! Developed by Xianwu xue 2010.12.26

    close(fileid)

    DO i=0,NRows-1
        DO j=0,NCols-1
            CALL SwapSng(temS(i,j))
            if (temS(i,j)<0) temS(i,j)=NoData_Value
        ENDDO
    ENDDO

    DO i=0,NRows-1
        DO j=0,NCols-1
            dblMat(j,NRows-1-i)=temS(i,j)
        ENDDO
    ENDDO

    return
end subroutine

SUBROUTINE SwapSng(Vsou)
    IMPLICIT NONE
    Real(4):: Vsou, temS
    CHARACTER*1:: cha2(1:4), cha
    Equivalence (temS, cha2)
    temS=Vsou
    cha=cha2(1)
    cha2(1)=cha2(4)
    cha2(4)=cha
    cha=cha2(2)
    cha2(2)=cha2(3)
    cha2(3)=cha
    Vsou=temS
END

SUBROUTINE SwapINT4(Vsou)
    IMPLICIT NONE
    integer*4:: Vsou, temS
    CHARACTER*1:: cha2(1:4), cha
    Equivalence (temS, cha2)
    temS=Vsou
    cha=cha2(1)
    cha2(1)=cha2(4)
    cha2(4)=cha
    cha=cha2(2)
    cha2(2)=cha2(3)
    cha2(3)=cha
    Vsou=temS
END
!########################################################


subroutine XXWReadLineDbl3(sFileNP,sKey,error,bl,a,bu)
    implicit none
    character(len=200)::sKey2
    character*(*)::sKey,sFileNP
    integer :: fileid
    integer, INTent(out) :: error
    character(len=200)::sTemp,sTemp2
    integer :: ICOL=1,ISTART,ISTOP,NCODE=1,N,IOUT,IN
    integer :: ISTART_in,ISTOP_in
    double precision :: bl,a,bu

    IOUT=-1
    IN=-1
    sKey2=sKey
    ICOL=1
    call URWORD(sKey2,ICOL,ISTART_in,ISTOP_in,NCODE,N,IOUT,IN)

    call XXWGetFreeFile(fileid)

    open(fileid,file=TRIM(sFileNP), form="formatted")

    error=0
    do while (error==0)
        read(fileid,*,iostat=error)sTemp
        !  			write(*,*)sTemp(1:1)
        if (sTemp(1:1)=="#")then
            cycle
        end if
        ICOL=1
        call URWORD(sTemp,ICOL,ISTART,ISTOP,NCODE,N,IOUT,IN)
        if(sTemp(ISTART:ISTOP)==sKey2(ISTART_in:ISTOP_in)) then
            BACKSPACE(fileid)
            read(fileid,*,iostat=error)sTemp, &
                    sTemp2,bl,a,bu
            close(fileid)
            return
        end if
    end do

    close(fileid)
    !If it can't find the sKey's value
    bl=1.0
    a=1.0
    bu=1.0

    return
end



logical function XXWReadLineBln(sFileNP,sKey,error,sKeyValue)
    implicit none
    character(len=200)::sKey2,sKeyValue2
    character*(*)::sKey,sKeyValue,sFileNP
    integer :: fileid
    integer, INTent(out) :: error
    character(len=200)::sTemp,sTemp2,sTemp3
    integer :: ICOL=1,ISTART,ISTOP,NCODE=1,N,IOUT,IN
    integer :: ISTART_in,ISTOP_in

    sKeyValue2=sKeyValue
    call UPCASE(sKeyValue2)

    IOUT=-1
    IN=-1
    sKey2=sKey
    ICOL=1
    call URWORD(sKey2,ICOL,ISTART_in,ISTOP_in,NCODE,N,IOUT,IN)

    call XXWGetFreeFile(fileid)

    open(fileid,file=TRIM(sFileNP), form="formatted")

    error=0
    ! 		rewind(fileid,iostat=error)
    do while (error==0)
        read(fileid,*,iostat=error)sTemp
        !  			write(*,*)sTemp(1:1)
        if (sTemp(1:1)=="#")then
            cycle
        end if
        ICOL=1
        call URWORD(sTemp,ICOL,ISTART,ISTOP,NCODE,N,IOUT,IN)
        if(sTemp(ISTART:ISTOP)==sKey2(ISTART_in:ISTOP_in)) then
            BACKSPACE(fileid)
            read(fileid,*,iostat=error)sTemp,sTemp2,sTemp3
            call UPCASE(sTemp3)

            if(trim(sTemp3)==trim(sKeyValue2))then
                XXWReadLineBln=.true.
            else
                XXWReadLineBln=.false.
            end if
            close(fileid)
            return
        end if
    end do

    close(fileid)

    return
end

!########################################################

!########################################################
subroutine CalSlope_Tiger(bIsError)
    use CREST_Project
    use CREST_Basic
    use CREST_Param

    implicit none
    integer :: i,j,ii,jj
    double precision :: GM(0:g_NCols-1,0:g_NRows-1)
    double precision :: GMValue,GMValueIn

    logical :: fExist,bIsError
    integer :: fileid
    character(len=200):: fileName
    double precision :: dblTemp
    fileName=trim(g_BasicPath) // "Slope.def"

    bIsError=.false.
    inquire(file=trim(Filename), exist=fExist)

    if(fExist .eqv. .false.)then
        bIsError=.true.
        return
    end if
    call XXWGetFreeFile(fileid)
    open(fileid,file=TRIM(Filename), form="formatted")
    read(fileid,*)GMValue
    close(fileid)

    ! 		GMValue=1.185728
    GM=GMValue

    do i=0, g_NRows-1
        do j=0, g_NCols-1
            ii=g_NextR(j,i)
            jj=g_NextC(j,i)

            if(g_Mask(j,i)/=g_NoData_Value)then
                if(InBasin(jj,ii) .eqv. .false.)then
                    g_Slope(j,i)=GM(j,i)/g_NextLen(j,i)
                else
                    if(g_DEM(j,i)>g_DEM(jj,ii))then
                        g_Slope(j,i)=(g_DEM(j,i)-g_DEM(jj,ii))  &
                                /g_NextLen(j,i)
                    else
                        g_Slope(j,i) &
                                =GM(j,i)/g_NextLen(j,i)
                    end if
                end if
            else
                g_Slope(j,i)=g_NoData_Value
            end if
        end do
    end do

    return
end subroutine

!------------------------------------------------------------
subroutine CalSlope_Tiger_XXW()
    use CREST_Project
    use CREST_Basic
    use CREST_Param

    implicit none
    integer :: i,j,ii,jj
    double precision,external :: XXWCell_i_j_Slope_All

    do i=0, g_NRows-1
        do j=0, g_NCols-1

            if(g_Mask(j,i)/=g_NoData_Value)then
                ii=g_NextR(j,i)
                jj=g_NextC(j,i)
                if(InBasin(jj,ii) .eqv. .false.)then
                    g_Slope(j,i)=XXWCell_i_j_Slope_All(i,j)

                else
                    if(g_DEM(j,i)>g_DEM(jj,ii))then
                        g_Slope(j,i)=(g_DEM(j,i)-g_DEM(jj,ii)) &
                                /g_NextLen(j,i)
                    else ! If the direction map is correct, this condition will not happen
                        g_Slope(j,i)=XXWCell_i_j_Slope_All(i,j)
                    end if
                end if
                if(g_Slope(j,i)<0)then
                    g_Slope(j,i)=-g_Slope(j,i)
                end if
                if(abs(g_Slope(j,i)-0.0)<0.000001)then
                    g_Slope(j,i)=0.000001
                end if
            else
                g_Slope(j,i)=g_NoData_Value
            end if

        end do
    end do

    return
end subroutine
!------------------------------------------------------------
subroutine CalSlope_XXW()
    use CREST_Project
    use CREST_Basic
    use CREST_Param

    implicit none
    ! 		integer :: i,j,ii,jj
    logical :: bGCS ,bIsNotExist ! Examine whether the GridArea.asc exist
    integer :: j,i
    double precision :: LenSN, LenEW

    double precision, external :: XXWCell_i_j_Slope

    !Identify whether it is a Geographic Coordinate System (GCS)
    !   or Projected Coordinate System (PCS)
    if(g_XLLCorner>=-180.0 .and. g_XLLCorner<=180 &
            .and. g_YLLcorner>=-90 .and. g_YLLCorner<=90)then
        bGCS=.true.
    else
        bGCS=.false.
    end if
    if (trim(g_CS) == "GCS") then
        bGCS=.true.
    else
        bGCS = .false.
    end if


    if(bGCS .eqv. .true.)then
        LenSN=g_CellSize*110574.0
    else
        LenSN=g_CellSize
    end if

    do i=0, g_NRows-1
        do j=0, g_NCols-1
            if(g_DEM(j,i)/=g_NoData_Value)then
                if(bGCS .eqv. .true.)then
                    LenEW=g_YLLCorner+(g_NRows-i-0.5)*g_CellSize
                    LenEW=LenSN*cos(LenEW*4.0*atan(1.0)/180.0)
                else
                    LenEW=g_CellSize
                end if
                g_Slope(j,i)=XXWCell_i_j_Slope(g_NCols,g_NRows,i,j, &
                        g_CellSize, g_NoData_Value,g_DEM,LenSN, LenEW)
                if(g_Slope(j,i)<0)then
                    g_Slope(j,i)=-g_Slope(j,i)
                end if
                if(g_Slope(j,i)==0)then
                    g_Slope(j,i)=0.00000001
                end if
            end if
        end do
    end do

    return
end subroutine

double precision Function XXWCell_i_j_Slope_All(i,j)
    use CREST_Project
    use CREST_Basic
    implicit none

    integer :: i,j
    double precision :: LenSN, LenEW
    double precision, external :: XXWCell_i_j_Slope
    logical :: bGCS

    if(g_XLLCorner>=-180.0 .and. g_XLLCorner<=180 &
            .and. g_YLLcorner>=-90 .and. g_YLLCorner<=90)then
        bGCS=.true.
    else
        bGCS=.false.
    end if
    if (trim(g_CS) == "GCS") then
        bGCS=.true.
    else
        bGCS = .false.
    end if



    if(bGCS .eqv. .true.)then
        LenSN=g_CellSize*110574.0
    else
        LenSN=g_CellSize
    end if

    if(bGCS .eqv. .true.)then
        LenEW=g_YLLCorner+(g_NRows-i-0.5)*g_CellSize
        LenEW=LenSN*cos(LenEW*4.0*atan(1.0)/180.0)
    else
        LenEW=g_CellSize
    end if

    XXWCell_i_j_Slope_All=XXWCell_i_j_Slope(g_NCols,g_NRows,i,j, &
            g_CellSize, g_NoData_Value,g_DEM,LenSN, LenEW)
end function XXWCell_i_j_Slope_All


double precision Function XXWCell_i_j_Slope(NCols,NRows,i,j, &
        CellSize, NoData_Value,dblDEMMat,LenSN, LenEW)
    implicit none

    integer :: NCols,NRows,i,j
    double precision :: CellSize, NoData_Value
    double precision :: dblDEMMat(0:NCols-1,0:NRows-1)
    double precision :: aa,bb,cc,dd,ee,ff,gg,hh,ii
    double precision :: LenSN, LenEW,dzdx,dzdy

    if(dblDEMMat(j,i)==NoData_Value)then
        XXWCell_i_j_Slope=NoData_Value
        return
    end if

    ee=dblDEMMat(j,i)

    if(i==0)then
        if(j==0)then
            aa=ee
            bb=ee
            cc=ee
            dd=ee

            ff=dblDEMMat(j+1,i)
            gg=ee
            hh=dblDEMMat(j,i+1)
            ii=dblDEMMat(j+1,i+1)
        elseif(j==NCols-1)then
            aa=ee
            bb=ee
            cc=ee
            dd=dblDEMMat(j-1,i)

            ff=ee
            gg=dblDEMMat(j-1,i+1)
            hh=dblDEMMat(j,i+1)
            ii=ee
        else
            aa=ee
            bb=ee
            cc=ee
            dd=dblDEMMat(j-1,i)

            ff=dblDEMMat(j+1,i)
            gg=dblDEMMat(j-1,i+1)
            hh=dblDEMMat(j,i+1)
            ii=dblDEMMat(j+1,i+1)
        end if

    elseif(i==NRows-1)then
        if(j==0)then
            aa=ee
            bb=dblDEMMat(j,i-1)
            cc=dblDEMMat(j+1,i-1)
            dd=ee

            ff=dblDEMMat(j+1,i)
            gg=ee
            hh=ee
            ii=ee
        elseif(j==NCols-1)then
            aa=dblDEMMat(j-1,i-1)
            bb=dblDEMMat(j,i-1)
            cc=ee
            dd=dblDEMMat(j-1,i)

            ff=ee
            gg=ee
            hh=ee
            ii=ee
        else
            aa=dblDEMMat(j-1,i-1)
            bb=dblDEMMat(j,i-1)
            cc=dblDEMMat(j+1,i-1)
            dd=dblDEMMat(j-1,i)

            ff=dblDEMMat(j+1,i)
            gg=ee
            hh=ee
            ii=ee
        end if
    else
        if(j==0)then
            aa=ee
            bb=dblDEMMat(j,i-1)
            cc=dblDEMMat(j+1,i-1)
            dd=ee

            ff=dblDEMMat(j+1,i)
            gg=ee
            hh=dblDEMMat(j,i+1)
            ii=dblDEMMat(j+1,i+1)
        elseif(j==NCols-1)then
            aa=dblDEMMat(j-1,i-1)
            bb=dblDEMMat(j,i-1)
            cc=ee
            dd=dblDEMMat(j-1,i)

            ff=ee
            gg=dblDEMMat(j-1,i+1)
            hh=dblDEMMat(j,i+1)
            ii=ee
        else
            aa=dblDEMMat(j-1,i-1)
            bb=dblDEMMat(j,i-1)
            cc=dblDEMMat(j+1,i-1)
            dd=dblDEMMat(j-1,i)

            ff=dblDEMMat(j+1,i)
            gg=dblDEMMat(j-1,i+1)
            hh=dblDEMMat(j,i+1)
            ii=dblDEMMat(j+1,i+1)
        end if
    end if

    if(aa==NoData_Value)then
        aa=ee
    end if

    if(bb==NoData_Value)then
        bb=ee
    end if

    if(cc==NoData_Value)then
        cc=ee
    end if

    if(dd==NoData_Value)then
        dd=ee
    end if

    if(ee==NoData_Value)then
        ee=ee
    end if

    if(ff==NoData_Value)then
        ff=ee
    end if

    if(gg==NoData_Value)then
        gg=ee
    end if

    if(hh==NoData_Value)then
        hh=ee
    end if

    if(ii==NoData_Value)then
        ii=ee
    end if

    dzdx=(cc+2.0*ff+ii)-(aa+2.0*dd+gg)
    dzdx=dzdx/(8.0*LenEW)

    dzdy=(gg+2.0*hh+ii)-(aa+2.0*bb+cc)
    dzdy=dzdy/(8.0*LenSN)

    XXWCell_i_j_Slope=sqrt(dzdx**2+dzdy**2)
    ! 		XXWCell_i_j_Slope=XXWCell_i_j_Slope*4.0*atan(1.0)/180.0
end function XXWCell_i_j_Slope



!########################################################

subroutine ConvDDMToFDR()
    use CREST_Project
    use CREST_Basic
    implicit none

    integer :: i,j,ii,jj
    integer :: numNoData,iRowNoData,iColNoData

    do i=0,g_NRows-1
        do j=0,g_NCols-1
            if(g_FDR(j,i)==g_NoData_Value)then
                cycle
            end if
            select case(g_FDR(j,i))
            case(1)
                g_FDR(j,i)=64
            case(2)
                g_FDR(j,i)=128
            case(8)
                g_FDR(j,i)=32
            case(3)
                g_FDR(j,i)=1
            case(4)
                g_FDR(j,i)=2
            case(5)
                g_FDR(j,i)=4
            case(6)
                g_FDR(j,i)=8
            case(7)
                g_FDR(j,i)=16
            case(0) !Outlet location
                numNoData=0
                iRowNoData=g_NoData_Value
                iColNoData=g_NoData_Value
                do ii=-1,1
                    do jj=-1,1
                        if((ii==0).and.(jj==0))then
                            cycle
                        end if
                        if(InBasin(j+jj,i+ii))then
                            if(g_FDR(j+jj,i+ii)/=g_NoData_Value)then
                            else
                                numNoData=numNoData+1
                                iRowNoData=ii
                                iColNoData=jj
                            end if
                        else
                            numNoData=numNoData+1
                            iRowNoData=ii
                            iColNoData=jj
                        end if
                    end do
                end do
                if(numNoData==0)then
                    g_FDR(j,i)=256 ! This cell is a sink
                elseif(numNoData==8)then
                    g_FDR(j,i)=1 ! This cell is an island
                else
                    if(iRowNoData==-1)then
                        if(iColNoData==-1)then
                            g_FDR(j,i)=32
                        elseif(iColNoData==0)then
                            g_FDR(j,i)=64
                        else
                            g_FDR(j,i)=128
                        end if
                    elseif(iRowNoData==0)then
                        if(iColNoData==-1)then
                            g_FDR(j,i)=16
                        elseif(iColNoData==0)then
                            write(*,*)"Wrong!!!!!!!!!!"
                        else
                            g_FDR(j,i)=1
                        end if
                    else
                        if(iColNoData==-1)then
                            g_FDR(j,i)=8
                        elseif(iColNoData==0)then
                            g_FDR(j,i)=4
                        else
                            g_FDR(j,i)=2
                        end if
                    end if
                end if
            end select
        end do
    end do
    return
end subroutine
!########################################################

!########################################################
subroutine EqualParamsType(tParamsIn,tParamsOut)
    use CREST_Param
    implicit none
    type(CREST_Params) tParamsIn,tParamsOut

    tParamsOut%RainFact	=	tParamsIn%RainFact
    tParamsOut%Ksat	=	tParamsIn%Ksat
    tParamsOut%WM	=	tParamsIn%WM
    tParamsOut%B	=	tParamsIn%B
    tParamsOut%IM	=	tParamsIn%IM
    tParamsOut%KE	=	tParamsIn%KE
    tParamsOut%coeM	=	tParamsIn%coeM

    tParamsOut%expM	=	tParamsIn%expM
    tParamsOut%coeR	=	tParamsIn%coeR
    tParamsOut%coeS	=	tParamsIn%coeS
    tParamsOut%KS	=	tParamsIn%KS
    tParamsOut%KI	=	tParamsIn%KI

    return
end subroutine

subroutine InitParamsType(tParams)
    use CREST_Basic
    use CREST_Param
    implicit none
    type(CREST_Params) tParams

    allocate(tParams%RainFact(0:g_NCols-1,0:g_NRows-1))
    allocate(tParams%Ksat(0:g_NCols-1,0:g_NRows-1))
    allocate(tParams%WM(0:g_NCols-1,0:g_NRows-1))
    allocate(tParams%B(0:g_NCols-1,0:g_NRows-1))
    allocate(tParams%IM(0:g_NCols-1,0:g_NRows-1))
    allocate(tParams%KE(0:g_NCols-1,0:g_NRows-1))
    allocate(tParams%coeM(0:g_NCols-1,0:g_NRows-1))

    allocate(tParams%expM(0:g_NCols-1,0:g_NRows-1))
    allocate(tParams%coeR(0:g_NCols-1,0:g_NRows-1))
    allocate(tParams%coeS(0:g_NCols-1,0:g_NRows-1))
    allocate(tParams%KS(0:g_NCols-1,0:g_NRows-1))
    allocate(tParams%KI(0:g_NCols-1,0:g_NRows-1))

    tParams%RainFact=g_NoData_Value
    tParams%Ksat=g_NoData_Value
    tParams%WM=g_NoData_Value
    tParams%B=g_NoData_Value
    tParams%IM=g_NoData_Value
    tParams%KE=g_NoData_Value
    tParams%coeM=g_NoData_Value

    tParams%expM=g_NoData_Value
    tParams%coeR=g_NoData_Value
    tParams%coeS=g_NoData_Value
    tParams%KS=g_NoData_Value
    tParams%KI=g_NoData_Value

    return
end subroutine

subroutine AssignValueToParamsType(tParams,dblValue)
    use CREST_Basic
    use CREST_Param
    implicit none
    type(CREST_Params) tParams
    double precision:: dblValue

    where(g_Mask==1)
        tParams%RainFact=dblValue
        tParams%Ksat=dblValue
        tParams%WM=dblValue
        tParams%B=dblValue
        tParams%IM=dblValue
        tParams%KE=dblValue
        tParams%coeM=dblValue

        tParams%expM=dblValue
        tParams%coeR=dblValue
        tParams%coeS=dblValue
        tParams%KS=dblValue
        tParams%KI=dblValue

    elsewhere
        tParams%RainFact=g_NoData_Value
        tParams%Ksat=g_NoData_Value
        tParams%WM=g_NoData_Value
        tParams%B=g_NoData_Value
        tParams%IM=g_NoData_Value
        tParams%KE=g_NoData_Value
        tParams%coeM=g_NoData_Value

        tParams%expM=g_NoData_Value
        tParams%coeR=g_NoData_Value
        tParams%coeS=g_NoData_Value
        tParams%KS=g_NoData_Value
        tParams%KI=g_NoData_Value

    end where
    return
end subroutine
!########################################################
subroutine XXWGetFreeFile(fileID)
    implicit none
    integer::i,fileID
    logical::bOpen
    bOpen = .TRUE.

    do i = 15, 100000
        inquire(UNIT=i, OPENED=bOpen)
        if (.NOT. bOpen) then
            fileID = i
            return
        end if
    end do
    fileID = -1
end subroutine XXWGetFreeFile
!########################################################
SUBROUTINE UtiGetAndDisplayBeginTime(IBDT)

    INTEGER, INTent(out):: IBDT(8)
    integer :: i


    CALL DATE_AND_TIME(VALUES=IBDT)

    WRITE(*,*)
    WRITE(*,2) (IBDT(I),I=1,3),(IBDT(I),I=5,7)
    2 FORMAT(1X,'Run start date and time (yyyy/mm/dd hh:mm:ss): ',  &
            I4,'/',I2.2,'/',I2.2,1X,I2,':',I2.2,':',I2.2)
    WRITE(*,*)

End

SUBROUTINE UtiGetAndDisplayBeginTimeToFile(IBDT)
    use CREST_Project

    INTEGER, INTent(in):: IBDT(8)
    integer :: i


    2 FORMAT(1X,'Run start date and time (yyyy/mm/dd hh:mm:ss): ',  &
            I4,'/',I2.2,'/',I2.2,1X,I2,':',I2.2,':',I2.2)

    WRITE(g_CREST_LogFileID,*)
    WRITE(g_CREST_LogFileID,2) (IBDT(I),I=1,3),(IBDT(I),I=5,7)
    WRITE(g_CREST_LogFileID,*)

End

SUBROUTINE UtiGetAndDisplayEndTime(IOUT,IBDT,IPRTIM)
    use CREST_Project

    INTEGER IBDT(8), IEDT(8), IDPM(12)
    DATA IDPM/31,28,31,30,31,30,31,31,30,31,30,31/
    DATA NSPD/86400/

    CALL DATE_AND_TIME(VALUES=IEDT)

    WRITE(*,*)
    WRITE(*,1000) (IEDT(I),I=1,3),(IEDT(I),I=5,7)

    WRITE(g_CREST_LogFileID,*)
    WRITE(g_CREST_LogFileID,1000) (IEDT(I),I=1,3),(IEDT(I),I=5,7)

    1000 FORMAT(1X,'Run end date and time (yyyy/mm/dd hh:mm:ss): ', &
            I4,'/',I2.2,'/',I2.2,1X,I2,':',I2.2,':',I2.2)

    IF(IPRTIM.GT.0) THEN
        WRITE(IOUT,'(1X)')
        WRITE(IOUT,1000) (IEDT(I),I=1,3),(IEDT(I),I=5,7)
    END IF
    !
    !     Calculate elapsed time in days and seconds
    NDAYS=0
    LEAP=0
    IF (MOD(IEDT(1),4).EQ.0) LEAP = 1
    IBD = IBDT(3)            ! BEGIN DAY
    IED = IEDT(3)            ! END DAY
    !     FIND DAYS
    IF (IBDT(2).NE.IEDT(2)) THEN
        !       MONTHS DIFFER
        MB = IBDT(2)             ! BEGIN MONTH
        ME = IEDT(2)             ! END MONTH
        NM = ME-MB+1             ! NUMBER OF MONTHS TO LOOK AT
        IF (MB.GT.ME) NM = NM+12
        MC=MB-1
        DO 10 M=1,NM
            MC=MC+1                ! MC IS CURRENT MONTH
            IF (MC.EQ.13) MC = 1
            IF (MC.EQ.MB) THEN
                NDAYS = NDAYS+IDPM(MC)-IBD
                IF (MC.EQ.2) NDAYS = NDAYS + LEAP
            ELSEIF (MC.EQ.ME) THEN
                NDAYS = NDAYS+IED
            ELSE
                NDAYS = NDAYS+IDPM(MC)
                IF (MC.EQ.2) NDAYS = NDAYS + LEAP
            ENDIF
        10  CONTINUE
    ELSEIF (IBD.LT.IED) THEN
        !       START AND END IN SAME MONTH, ONLY ACCOUNT FOR DAYS
        NDAYS = IED-IBD
    ENDIF
    ELSEC=NDAYS*NSPD
    !
    !     ADD OR SUBTRACT SECONDS
    ELSEC = ELSEC+(IEDT(5)-IBDT(5))*3600.0
    ELSEC = ELSEC+(IEDT(6)-IBDT(6))*60.0
    ELSEC = ELSEC+(IEDT(7)-IBDT(7))
    ELSEC = ELSEC+(IEDT(8)-IBDT(8))*0.001
    !
    !     CONVERT SECONDS TO DAYS, HOURS, MINUTES, AND SECONDS
    NDAYS = ELSEC/NSPD
    RSECS = MOD(ELSEC,86400.0)
    NHOURS = RSECS/3600.0
    RSECS = MOD(RSECS,3600.0)
    NMINS = RSECS/60.0
    RSECS = MOD(RSECS,60.0)
    NSECS = RSECS
    RSECS = MOD(RSECS,1.0)
    MSECS = NINT(RSECS*1000.0)
    NRSECS = NSECS
    IF (RSECS.GE.0.5) NRSECS=NRSECS+1
    !
    !     Write elapsed time to screen
    IF (NDAYS.GT.0) THEN
        WRITE(*,1010) NDAYS,NHOURS,NMINS,NRSECS

        WRITE(g_CREST_LogFileID,1010) NDAYS,NHOURS,NMINS,NRSECS

        1010 FORMAT(1X,'Elapsed run time: ',I3,' Days, ',I2,' Hours, ',I2,  &
                ' Minutes, ',I2,' Seconds',/)
    ELSEIF (NHOURS.GT.0) THEN
        WRITE(*,1020) NHOURS,NMINS,NRSECS

        WRITE(g_CREST_LogFileID,1020) NHOURS,NMINS,NRSECS

        1020 FORMAT(1X,'Elapsed run time: ',I2,' Hours, ',I2, &
                ' Minutes, ',I2,' Seconds',/)
    ELSEIF (NMINS.GT.0) THEN
        WRITE(*,1030) NMINS,NSECS,MSECS

        WRITE(g_CREST_LogFileID,1030) NMINS,NSECS,MSECS

        1030 FORMAT(1X,'Elapsed run time: ',I2,' Minutes, ',  &
                I2,'.',I3.3,' Seconds',/)
    ELSE
        WRITE(*,1040) NSECS,MSECS

        WRITE(g_CREST_LogFileID,1040) NSECS,MSECS

        1040 FORMAT(1X,'Elapsed run time: ',I2,'.',I3.3,' Seconds',/)
    ENDIF
    !
    RETURN
END


SUBROUTINE URWORD(LINE,ICOL,ISTART,ISTOP,NCODE,N,IOUT,IN)

    CHARACTER*(*) LINE
    CHARACTER*20 STRING
    CHARACTER*30 RW
    CHARACTER*1 TAB
    integer :: N,IOUT,IN
    !     ------------------------------------------------------------------
    TAB=CHAR(9)
    !

    LINLEN=LEN(LINE)
    LINE(LINLEN:LINLEN)=' '
    ISTART=LINLEN
    ISTOP=LINLEN
    LINLEN=LINLEN-1
    IF(ICOL.LT.1 .OR. ICOL.GT.LINLEN) GO TO 100
    !
    !2------Find start of word, which is indicated by first character that
    !2------is not a blank, a comma, or a tab.
    DO 10 I=ICOL,LINLEN
        IF(LINE(I:I).NE.' ' .AND. LINE(I:I).NE.',' &
                .AND. LINE(I:I).NE.TAB) GO TO 20
    10 CONTINUE
    ICOL=LINLEN+1
    GO TO 100
    !
    !3------Found start of word.  Look for end.
    !3A-----When word is quoted, only a quote can terminate it.
    20 IF(LINE(I:I).EQ.'''') THEN
        I=I+1
        IF(I.LE.LINLEN) THEN
            DO 25 J=I,LINLEN
                IF(LINE(J:J).EQ.'''') GO TO 40
            25    CONTINUE
        END IF
        !
        !3B-----When word is not quoted, space, comma, or tab will terminate.
    ELSE
        DO 30 J=I,LINLEN
            IF(LINE(J:J).EQ.' ' .OR. LINE(J:J).EQ.',' &
                    .OR. LINE(J:J).EQ.TAB) GO TO 40
        30  CONTINUE
    END IF
    !
    !3C-----End of line without finding end of word; set end of word to
    !3C-----end of line.
    J=LINLEN+1
    !
    !4------Found end of word; set J to point to last character in WORD and
    !-------set ICOL to point to location for scanning for another word.
    40 ICOL=J+1
    J=J-1
    IF(J.LT.I) GO TO 100
    ISTART=I
    ISTOP=J
    !
    !5------Convert word to upper case and RETURN if NCODE is 1.
    IF(NCODE.EQ.1) THEN
        IDIFF=ICHAR('a')-ICHAR('A')
        DO 50 K=ISTART,ISTOP
            IF(LINE(K:K).GE.'a' .AND. LINE(K:K).LE.'z') &
                    LINE(K:K)=CHAR(ICHAR(LINE(K:K))-IDIFF)
        50  CONTINUE
        RETURN
    END IF
    !
    !6------Convert word to a number if requested.
    100 IF(NCODE.EQ.2 .OR. NCODE.EQ.3) THEN
        RW=' '
        L=30-ISTOP+ISTART
        IF(L.LT.1) GO TO 200
        RW(L:30)=LINE(ISTART:ISTOP)
        IF(NCODE.EQ.2) READ(RW,'(I30)',ERR=200) N
        IF(NCODE.EQ.3) READ(RW,'(F30.0)',ERR=200) R
    END IF
    RETURN
    !
    !7------Number conversion error.
    200 IF(NCODE.EQ.3) THEN
        STRING= 'A REAL NUMBER'
        L=13
    ELSE
        STRING= 'AN INTEGER'
        L=10
    END IF
    !
    !7A-----If output unit is negative, set last character of string to 'E'.
    IF(IOUT.LT.0) THEN
        N=0
        R=0.
        LINE(LINLEN+1:LINLEN+1)='E'
        RETURN
        !
        !7B-----If output unit is positive; write a message to output unit.
    ELSE IF(IOUT.GT.0) THEN
        IF(IN.GT.0) THEN
            WRITE(IOUT,201) IN,LINE(ISTART:ISTOP),STRING(1:L),LINE
        ELSE
            WRITE(IOUT,202) LINE(ISTART:ISTOP),STRING(1:L),LINE
        END IF
        201 FORMAT(1X,/1X,'FILE UNIT ',I4,' : ERROR CONVERTING "',A, &
                '" TO ',A,' IN LINE:',/1X,A)
        202 FORMAT(1X,/1X,'KEYBOARD INPUT : ERROR CONVERTING "',A, &
                '" TO ',A,' IN LINE:',/1X,A)
        !
        !7C-----If output unit is 0; write a message to default output.
    ELSE
        IF(IN.GT.0) THEN
            WRITE(*,201) IN,LINE(ISTART:ISTOP),STRING(1:L),LINE
        ELSE
            WRITE(*,202) LINE(ISTART:ISTOP),STRING(1:L),LINE
        END IF
    END IF
    !
END
SUBROUTINE UPCASE(WORD)
    !     ******************************************************************
    !     CONVERT A CHARACTER STRING TO ALL UPPER CASE
    !     ******************************************************************
    !       SPECIFICATIONS:
    !     ------------------------------------------------------------------
    CHARACTER WORD*(*)
    !
    !1------Compute the difference between lowercase and uppercase.
    L = LEN(WORD)
    IDIFF=ICHAR('a')-ICHAR('A')
    !
    !2------Loop through the string and convert any lowercase characters.
    DO 10 K=1,L
        IF(WORD(K:K).GE.'a' .AND. WORD(K:K).LE.'z') &
                WORD(K:K)=CHAR(ICHAR(WORD(K:K))-IDIFF)
    10 CONTINUE
    !
    !3------return.
    RETURN
END
!########################################################


subroutine GetMaskByNoData(NCols,NRows,NoData_Value, &
        DEM,MaskOut)
    implicit none
    integer :: NCols,NRows
    double precision :: NoData_Value
    double precision :: DEM(0:NCols-1,0:NRows-1)
    integer, intent(out) :: MaskOut(0:NCols-1,0:NRows-1)
    integer :: i,j

    do i=0, NRows-1
        do j=0, NCols-1
            if(DEM(j,i)   ==  NoData_Value)then
                MaskOut(j,i)    =   NoData_Value
            else
                MaskOut(j,i)=1
            end if
        end do
    end do
    return
end subroutine
!########################################################


!########################################################
logical function InBasin(jCol,iRow)
    use CREST_Basic
    implicit none
    integer :: iRow,jCol
    if(jCol<0 .or. jCol>=g_NCols .or. iRow<0 .or. iRow>=g_NRows)then
        InBasin=.false.
    else
        ! 			if(g_Mask(jCol,iRow)==g_NoData_Value)then
        ! 				InBasin=.false.
        ! 			else
        InBasin=.true.
        ! 			end if
    end if
    return
end function

logical function InBasin2(jCol,iRow)
    use CREST_Basic
    implicit none
    integer :: iRow,jCol
    if(jCol<0  .or. jCol>=g_NCols .or. iRow<0 .or. iRow>=g_NRows)then
        InBasin2=.false.
    else
        ! 			if(g_Mask(jCol,iRow)==g_NoData_Value)then
        ! 				InBasin=.false.
        ! 			else
        InBasin2=.true.
        ! 			end if
    end if
    return
end function
!########################################################
subroutine GetNSCE(RObs,RSim,indexFrom,ITMax,NSCE)
    implicit none
    double precision, intent(out):: NSCE
    integer :: i,indexFrom,indexL,ITMax,iCount
    double precision::RObs(0:ITMax-1),RSim(0:ITMax-1)
    double precision::RObs_Ave,RObs_Sum,RSim_Sum

    indexL=lbound(Robs,1)

    if(indexFrom<indexL)then
        indexFrom=indexL
    end if

    RObs_Ave=0.0
    iCount=0
    do i=indexFrom,ITMax-1
        if(RObs(i)<0 .or. RSim(i)<0)then
            cycle
        end if
        RObs_Ave=RObs_Ave+RObs(i)
        iCount=iCount+1
    end do
    RObs_Ave=RObs_Ave/iCount

    RObs_Sum=0.0
    RSim_Sum=0.0
    do i=indexFrom,ITMax-1
        if(RObs(i)<0 .or. RSim(i)<0)then
            cycle
        end if
        RObs_Sum=RObs_Sum+(RObs(i)-RObs_Ave)**2.0
        RSim_Sum=RSim_Sum+(RObs(i)-RSim(i))**2.0
    end do

    NSCE=1.0-RSim_Sum/RObs_Sum

    return

end subroutine
!--------------------------------------------------------------------
subroutine GetBias(RObs,RSim,indexFrom,ITMax,Bias)
    implicit none
    double precision, intent(out):: Bias
    integer :: i,indexFrom,indexL,ITMax,iCount
    double precision::RObs(0:ITMax-1),RSim(0:ITMax-1)
    double precision::RObs_Sum,RSim_Sum

    indexL=lbound(Robs,1)

    if(indexFrom<indexL)then
        indexFrom=indexL
    end if

    RObs_Sum=0.0
    RSim_Sum=0.0
    iCount=0
    do i=indexFrom,ITMax-1
        if(RObs(i)<0 .or. RSim(i)<0)then
            cycle
        end if
        RObs_Sum=RObs_Sum+RObs(i)
        RSim_Sum=RSim_Sum+RSim(i)
    end do

    Bias=(RSim_Sum/RObs_Sum-1.0)*100

    return

end subroutine
!----------------------------------------------------------------
subroutine GetCC(RObs,RSim,indexFrom,ITMax,CC)
    !Correlation coefficient
    implicit none
    double precision, intent(out):: CC
    integer :: i,indexFrom,indexL,ITMax,iCount
    double precision::RObs(0:ITMax-1),RSim(0:ITMax-1)
    double precision::RObs_Ave,RSim_Ave
    double precision::RAbove_Sum,RBelow_Sum
    double precision::RObs_Sum,RSim_Sum
    indexL=lbound(Robs,1)

    if(indexFrom<indexL)then
        indexFrom=indexL
    end if

    RObs_Ave=0.0
    RSim_Ave=0.0
    iCount=0
    do i=indexFrom,ITMax-1
        if(RObs(i)<0 .or. RSim(i)<0)then
            cycle
        end if
        RObs_Ave=RObs_Ave+RObs(i)
        RSim_Ave=RSim_Ave+RSim(i)
        iCount=iCount+1
    end do
    RObs_Ave=RObs_Ave/iCount
    RSim_Ave=RSim_Ave/iCount

    RAbove_Sum=0.0
    RBelow_Sum=0.0
    RObs_Sum=0.0
    RSim_Sum=0.0
    do i=indexFrom,ITMax-1
        if(RObs(i)<0 .or. RSim(i)<0)then
            cycle
        end if
        RAbove_Sum=RAbove_Sum  &
                +(RObs(i)-RObs_Ave)*(RSim(i)-RSim_Ave)
        RObs_Sum=RObs_Sum   &
                +(RObs(i)-RObs_Ave)**2.0
        RSim_Sum=RSim_Sum &
                +(RSim(i)-RSim_Ave)**2.0
    end do
    RBelow_Sum=sqrt(RObs_Sum*RSim_Sum)
    CC=RAbove_Sum/RBelow_Sum
    return

end subroutine

!########################################################
Function PosIntMsg(I)
    IMPLICIT NONE
    Integer ::I
    Character(len=6):: PosIntMsg
    IF (I<10) Then
        Write(PosIntMsg, "(I1)") I
    ELSEIF (I<100) THEN
        Write(PosIntMsg, "(I2)") I
    ELSEIF (I<1000) THEN
        Write(PosIntMsg, "(I3)") I
    ELSEIF (I<10000) THEN
        Write(PosIntMsg, "(I4)") I
    ELSEIF (I<100000) THEN
        Write(PosIntMsg, "(I5)") I
    ELSE
        Write(PosIntMsg, "(I6)") I
    END IF
End Function
!########################################################
SUBROUTINE myStrtoD(Str, myD, Mark)
    INTEGER(4):: myD(1:6), I
    CHARACTER(14):: Str, msg
    CHARACTER(1)::Mark

    myD=0

    SELECT CASE (Mark)
    CASE ("y","Y")
        msg=Str(1:4)
        READ(msg,*) myD(1)
        myD(2)=1
        myD(3)=1
        Str(5:14)="0101000000"
    CASE ("m","M")
        msg=Str(1:4)
        READ(msg,*) myD(1)
        msg=Str(5:6)
        READ(msg,*) myD(2)
        myD(3)=1
        Str(7:14)="01000000"
    CASE ("d","D")
        msg=Str(1:4)
        READ(msg,*) myD(1)
        msg=Str(5:6)
        READ(msg,*) myD(2)
        msg=Str(7:8)
        READ(msg,*) myD(3)
        Str(9:14)="000000"
    CASE ("h","H")
        msg=Str(1:4)
        READ(msg,*) myD(1)
        msg=Str(5:6)
        READ(msg,*) myD(2)
        msg=Str(7:8)
        READ(msg,*) myD(3)
        msg=Str(9:10)
        READ(msg,*) myD(4)
        Str(11:14)="0000"
    CASE ("u","U")
        msg=Str(1:4)
        READ(msg,*) myD(1)
        msg=Str(5:6)
        READ(msg,*) myD(2)
        msg=Str(7:8)
        READ(msg,*) myD(3)
        msg=Str(9:10)
        READ(msg,*) myD(4)
        msg=Str(11:12)
        READ(msg,*) myD(5)
        Str(13:14)="00"
    CASE ("s","S")
        msg=Str(1:4)
        READ(msg,*) myD(1)
        msg=Str(5:6)
        READ(msg,*) myD(2)
        msg=Str(7:8)
        READ(msg,*) myD(3)
        msg=Str(9:10)
        READ(msg,*) myD(4)
        msg=Str(11:12)
        READ(msg,*) myD(5)
        msg=Str(13:14)
        READ(msg,*) myD(6)
    END SELECT

END SUBROUTINE

integer FUNCTION myDdIFf(Mark, myDAout, myDBout)
    IMPLICIT NONE
    CHARACTER(1):: Mark
    INTEGER(4)::  myDAout(1:6), myDBout(1:6)
    INTEGER(4):: myDA(1:6), myDB(1:6), DX(1:6), Num, Sign
    INTEGER(4), EXTERNAL:: myDcompare

    myDA=myDAout
    myDB=myDBout
    Sign=myDcompare(myDA, myDB,Mark)
    IF (Sign==-1) THEN
        myDA=myDBout
        myDB=myDAout
    END IF

    SELECT CASE (Mark)
    CASE ("h","H", "u", "U", "s", "S")
        DX=myDA
        Num=0
        DO
            IF (DX(1)==myDB(1) .and. DX(2)==myDB(2)  &
                    .And. DX(3)==myDB(3)) EXIT
            CALL myDadd("d", 1, DX)
            Num=Num+1
        END DO

        SELECT CASE (Mark)
        CASE ("h","H")
            myDdIFf=Num*24+myDB(4)-myDA(4)
        CASE ("u","U")
            myDdIFf=(Num*24+myDB(4)-myDA(4))*60+myDB(5)-myDA(5)
        CASE ("s","S")
            myDdIFf=((Num*24+myDB(4)-myDA(4))*60 &
                    +myDB(5)-myDA(5))*60+myDB(6)-myDA(6)
        END SELECT
    CASE Default
        DX=myDA
        Num=0
        DO
            SELECT CASE (Mark)
            CASE ("y", "Y")
                IF (DX(1)==myDB(1)) EXIT
            CASE ("m","M")
                IF (DX(1)==myDB(1) .and. DX(2)==myDB(2)) EXIT
            CASE ("d", "D")
                IF (DX(1)==myDB(1) .and. DX(2)==myDB(2)  &
                        .And. DX(3)==myDB(3)) EXIT
            END SELECT
            CALL myDadd(Mark, 1, DX)
            Num=Num+1
        END DO
        myDdIFf=Num
    END SELECT
    myDdIFf=myDdIFf*Sign
END FUNCTION

SUBROUTINE myDAdd(Mark, Num, myD)
    IMPLICIT NONE
    CHARACTER(1):: Mark
    INTEGER(4):: myD(1:6)
    INTEGER(4):: Num, I, Days

    SELECT CASE (Mark)
    CASE ("y","Y");	I=1
    CASE ("m","M");	I=2
    CASE ("d","D");	I=3
    CASE ("h","H");	I=4
    CASE ("u","U");	I=5
    CASE ("s","S");	I=6
    END SELECT
    myD(I)=myD(I)+Num

    IF (I==6) THEN ! second
        IF (myD(6)<0) THEN
            DO WHILE (myD(6)<0)
                myD(6)=myD(6)+60
                myD(5)=myD(5)-1
            END DO
        ELSE
            DO WHILE (myD(6)>=60)
                myD(6)=myD(6)-60
                myD(5)=myD(5)+1
            END DO
        END IF
    END IF
    IF (I>=5) THEN ! minute
        IF (myD(5)<0) THEN
            DO WHILE (myD(5)<0)
                myD(5)=myD(5)+60
                myD(4)=myD(4)-1
            END DO
        ELSE
            DO WHILE (myD(5)>=60)
                myD(5)=myD(5)-60
                myD(4)=myD(4)+1
            END DO
        END IF
    END IF
    IF (I>=4) THEN ! hour
        IF (myD(4)<0) THEN
            DO WHILE (myD(4)<0)
                myD(4)=myD(4)+24
                myD(3)=myD(3)-1
            END DO
        ELSE
            DO WHILE (myD(4)>=24)
                myD(4)=myD(4)-24
                myD(3)=myD(3)+1
            END DO
        END IF
    END IF
    IF (I>=3) THEN ! day
        IF (myD(3)<0) THEN
            DO WHILE (myD(3)<0)
                SELECT CASE (myD(2))
                CASE (1,3,5,7,8,10,12)
                    Days=31
                CASE (4,6,9,11)
                    Days=30
                CASE (2)
                    IF (Mod(myD(1), 4)==0) THEN
                        Days=29
                    ELSE
                        Days=28
                    END IF
                END SELECT
                myD(3)=myD(3)+days
                myD(2)=myD(2)-1
            END DO
        ELSE if(myD(3)==0) THEN
            myD(2)=myD(2)-1
            IF (myD(2)==0) THEN
                myD(2)=12
                myD(1)=myD(1)-1
            end if
            SELECT CASE (myD(2))
            CASE (1,3,5,7,8,10,12)
                Days=31
            CASE (4,6,9,11)
                Days=30
            CASE (2)
                IF (Mod(myD(1), 4)==0) THEN
                    Days=29
                ELSE
                    Days=28
                END IF
            END SELECT
            myD(3)=Days
        ELSE
            DO
                SELECT CASE (myD(2))
                CASE (1,3,5,7,8,10,12)
                    Days=31
                CASE (4,6,9,11)
                    Days=30
                CASE (2)
                    IF (Mod(myD(1), 4)==0) THEN
                        Days=29
                    ELSE
                        Days=28
                    END IF
                END SELECT
                IF (myD(3)<=Days) THEN
                    EXIT
                ELSE
                    myD(3)=myD(3)-Days
                    myD(2)=myD(2)+1
                END IF
            END DO
        END IF
    END IF
    IF (I>=2) THEN ! Month
        IF (myD(2)<=0) THEN
            DO WHILE (myD(2)<0)
                myD(2)=myD(2)+12
                myD(1)=myD(1)-1
            END DO
        ELSEIF (myD(2)==0) THEN
            myD(2)=12
            myD(1)=myD(1)-1
        ELSE
            DO WHILE (myD(2)>12)
                myD(1)=myD(1)+1
                myD(2)=myD(2)-12
            END DO
        END IF
    END IF
END SUBROUTINE


FUNCTION myDcompare(DA, DB,Mark)
    IMPLICIT NONE
    INTEGER(4):: DA(1:6), DB(1:6), myDcompare,I
    CHARACTER(1):: Mark

    SELECT CASE (Mark)
    CASE ("y","Y");	I=1
    CASE ("m","M");	I=2
    CASE ("d","D");	I=3
    CASE ("h","H");	I=4
    CASE ("u","U");	I=5
    CASE ("s","S");	I=6
    END SELECT

    myDcompare=1
    IF (DA(1)>DB(1) .and. I>=1) THEN
        myDcompare=-1
    ElseIF (DA(1)==DB(1) .and. DA(2)>DB(2) .and. I>=2) THEN
        myDcompare=-1
    ELSEIF (DA(1)==DB(1)  &
            .and. DA(2)==DB(2)  &
            .and. DA(3)>DB(3) .and. I>=3) THEN
        myDcompare=-1
    ELSEIF (DA(1)==DB(1)  &
            .and. DA(2)==DB(2)   &
            .and. DA(3)==DB(3)  &
            .and. DA(4)>DB(4) .and. I>=4) THEN
        myDcompare=-1
    ELSEIF (DA(1)==DB(1)  &
            .and. DA(2)==DB(2) &
            .and. DA(3)==DB(3)   &
            .and. DA(4)==DB(4) &
            .and. DA(5)>DB(5) .and. I>=5) THEN
        myDcompare=-1
    ELSEIF (DA(1)==DB(1)  &
            .and. DA(2)==DB(2)  &
            .and. DA(3)==DB(3)  &
            .and. DA(4)==DB(4) &
            .and. DA(5)==DB(5) &
            .and. DA(6)>DB(6) .and. I>=6) THEN
        myDcompare=-1
    END IF
END FUNCTION

FUNCTION myDcompare2(DA, DB,Mark)
    IMPLICIT NONE
    INTEGER(4):: DA(1:6), DB(1:6), myDcompare2,I
    CHARACTER(1):: Mark

    SELECT CASE (Mark)
    CASE ("y","Y");	I=1
    CASE ("m","M");	I=2
    CASE ("d","D");	I=3
    CASE ("h","H");	I=4
    CASE ("u","U");	I=5
    CASE ("s","S");	I=6
    END SELECT

    myDcompare2=1
    IF (DA(1)>=DB(1) .and. I>=1) THEN
        myDcompare2=-1
    ElseIF (DA(1)==DB(1) .and. DA(2)>=DB(2) .and. I>=2) THEN
        myDcompare2=-1
    ELSEIF (DA(1)==DB(1)  &
            .and. DA(2)==DB(2)  &
            .and. DA(3)>=DB(3) .and. I>=3) THEN
        myDcompare2=-1
    ELSEIF (DA(1)==DB(1)  &
            .and. DA(2)==DB(2)  &
            .and. DA(3)==DB(3)  &
            .and. DA(4)>=DB(4) .and. I>=4) THEN
        myDcompare2=-1
    ELSEIF (DA(1)==DB(1)  &
            .and. DA(2)==DB(2) &
            .and. DA(3)==DB(3)  &
            .and. DA(4)==DB(4) &
            .and. DA(5)>=DB(5) .and. I>=5) THEN
        myDcompare2=-1
    ELSEIF (DA(1)==DB(1)  &
            .and. DA(2)==DB(2)   &
            .and. DA(3)==DB(3)  &
            .and. DA(4)==DB(4)  &
            .and. DA(5)==DB(5)  &
            .and. DA(6)>=DB(6) .and. I>=6) THEN
        myDcompare2=-1
    END IF
END FUNCTION

FUNCTION myDtoStr(myD, Mark)
    IMPLICIT NONE
    INTEGER(4):: myD(1:6)
    CHARACTER(1):: Mark
    CHARACTER(14):: myDtoStr, msg

    myDtoStr=""
    WRITE(msg,"(I4)") myD(1)
    myDtoStr(1:4)=msg(1:4)

    SELECT CASE (Mark)
    CASE ("m","M")
        WRITE(msg,"(I2)") myD(2)
        myDtoStr(5:6)=msg(1:2)
        IF (myD(2)<10) myDtoStr(5:5)="0"
    CASE ("d","D")
        WRITE(msg,"(I2)") myD(2)
        myDtoStr(5:6)=msg(1:2)
        IF (myD(2)<10) myDtoStr(5:5)="0"
        WRITE(msg,"(I2)") myD(3)
        myDtoStr(7:8)=msg(1:2)
        IF (myD(3)<10) myDtoStr(7:7)="0"
    CASE ("h","H")
        WRITE(msg,"(I2)") myD(2)
        myDtoStr(5:6)=msg(1:2)
        IF (myD(2)<10) myDtoStr(5:5)="0"
        WRITE(msg,"(I2)") myD(3)
        myDtoStr(7:8)=msg(1:2)
        IF (myD(3)<10) myDtoStr(7:7)="0"
        WRITE(msg,"(I2)") myD(4);   myDtoStr(9:10)=msg(1:2)
        IF (myD(4)<10) myDtoStr(9:9)="0"
    CASE ("u","U")
        WRITE(msg,"(I2)") myD(2)
        myDtoStr(5:6)=msg(1:2)
        IF (myD(2)<10) myDtoStr(5:5)="0"
        WRITE(msg,"(I2)") myD(3)
        myDtoStr(7:8)=msg(1:2)
        IF (myD(3)<10) myDtoStr(7:7)="0"
        WRITE(msg,"(I2)") myD(4);   myDtoStr(9:10)=msg(1:2)
        IF (myD(4)<10) myDtoStr(9:9)="0"
        WRITE(msg,"(I2)") myD(5);   myDtoStr(11:12)=msg(1:2)
        IF (myD(5)<10) myDtoStr(11:11)="0"
    CASE ("s","S")
        WRITE(msg,"(I2)") myD(2)
        myDtoStr(5:6)=msg(1:2)
        IF (myD(2)<10) myDtoStr(5:5)="0"
        WRITE(msg,"(I2)") myD(3)
        myDtoStr(7:8)=msg(1:2)
        IF (myD(3)<10) myDtoStr(7:7)="0"
        WRITE(msg,"(I2)") myD(4);   myDtoStr(9:10)=msg(1:2)
        IF (myD(4)<10) myDtoStr(9:9)="0"
        WRITE(msg,"(I2)") myD(5);   myDtoStr(11:12)=msg(1:2)
        IF (myD(5)<10) myDtoStr(11:11)="0"
        WRITE(msg,"(I2)") myD(6);   myDtoStr(13:14)=msg(1:2)
        IF (myD(6)<10) myDtoStr(13:13)="0"
    END SELECT
END FUNCTION

SUBROUTINE myNow(DX)
    IMPLICIT NONE
    INTEGER(4):: DX(1:6)
    CHARACTER(20):: A, B
    CHARACTER(14):: strD

    CALL Date_and_Time(A, B)
    strD(1:8)=A(1:8)
    strD(9:14)=B(1:6)
    CALL myStrtoD(strD, DX, "s")
END SUBROUTINE

FUNCTION myDequal(DA, DB)
    INTEGER(4):: DA(1:6), DB(1:6)
    LOGICAL:: myDequal
    myDequal=(DA(1)==DB(1)  &
            .and. DA(2)==DB(2)  &
            .and. DA(3)==DB(3)   &
            .and. DA(4)==DB(4)  &
            .and. DA(5)==DB(5)  &
            .and. DA(6)==DB(6))
END FUNCTION
!########################################################
!  THIS IS THE MAIN PROGRAM WHICH RUNS SCEUA PROGRAMS ON
!  THE SIXPAR MODEL - BY QINGYUN DUAN, APRIL 1992

!  Xianwu Xue modified it to a subroutine named as "SCEUSMain"
!	in Oct. 12 2010
!	Email: xuexianwu@ou.edu or xuexianwuqd@gmail.com

!----------------------------------------------------------------
subroutine XXWGeneralSCEUA2(a_In,bl_In,bu_In,nopt_In,maxn_In, &
        kstop_In,pcento_In,iseed_In,ngs_In,idfunc_In, &
        XXWFunctn,sPath,iOutputID)
    use CREST_Project

    implicit real*8 (a-h,o-z)
    parameter(ijk=6000,ldt=24)
    common /block4/ npar,loc(50)
    common /block5/ pt(50),idfunc
    common /iopar/ ipr
    real*8 aa
    common /ablu/  bl(50), bu(50)
    dimension a(50),jseed(10)
    dimension a_In(50),bl_In(50), bu_In(50)
    data jseed/2,3,5,7,11,13,17,19,23,29/
    character*(*) sPath

    double precision, external :: XXWFunctn
    !
    write (*,"(/,28X,A)") ' ENTER THE MAIN PROGRAM SCE-UA '

    WRITE(g_CREST_LogFileID,*)
    WRITE(g_CREST_LogFileID,"(/,28X,A)")  &
            ' ENTER THE MAIN PROGRAM SCE-UA '
    WRITE(g_CREST_LogFileID,*)

    ipr=iOutputID
    !XXW
    idfunc=idfunc_In
    a(1:nopt_In)=a_In
    bl(1:nopt_In)=bl_In
    bu(1:nopt_In)=bu_In
    nopt=nopt_IN
    maxn=maxn_In
    kstop=kstop_In
    pcento=pcento_In
    iseed=iseed_In
    ngs=ngs_In
    ! 	call XXWGetFreeFile(ipr)
    ! 	open(unit=ipr,file=sPath // 'SCEUAOut.dat',status='unknown')
    !
    call XXWSCEIn(a,bl,bu,nopt,maxn,kstop,pcento,iseed, &
            ngs,npg,nps,nspl,mings,iniflg,iprint,sPath)
    iniflg=1
    select case (idfunc)
    case (8)
        call sxpinp(nopt,a,sPath)

    case (1:7)
        call otherf(a,50,nopt,idfunc,loc,bl,bu)
    case (55)

    case default
        write(*,*)	"Something wrong in your parameters--idfunc"
    end select

    if (iseed .gt. 0) then
        nrun = min(iseed,10)
    else
        nrun = 1
    end if

    do i=1, nrun
        if (nrun /= 1) iseed = jseed(i)
        write (*,*) '@ SCE-UA Run Number',i, &
                '    Random Seed Value',iseed

        WRITE(g_CREST_LogFileID,*)
        WRITE(g_CREST_LogFileID,*)   &
                '@ SCE-UA Run Number',i,  &
                '    Random Seed Value',iseed
        WRITE(g_CREST_LogFileID,*)

        call XXWSCEUA(a,bl,bu,nopt,maxn,kstop,pcento,iseed, &
                ngs,npg,nps,nspl,mings,iniflg,iprint,  &
                XXWFunctn)

    end do

    !
    !  END OF PROGRAM

    return
end
!########################################################
!====================================================================
subroutine XXWCCE(nopt,nps,s,sf,bl,bu,xnstd,icall,maxn,iseed, &
        XXWFunctn)
    use CREST_Project
    use CREST_Calib_SCEUA
    !  ALGORITHM GENERATE A NEW POINT(S) FROM A SUB-COMPLEX
    !
    !  SUB-COMPLEX VARIABLES
    implicit real*8 (a-h,o-z)
    parameter (c1=0.8,c2=0.4)
    dimension s(50,16),sf(50),bu(16),bl(16),xnstd(16)
    double precision,external :: XXWFunctn !XXW

    !
    dimension sw(16),sb(16),ce(16),snew(16)
    !
    !  EQUIVALENCE OF VARIABLES FOR READABILTY OF CODE
    n = nps
    m = nopt
    alpha = 1.0
    beta = 0.5
    !
    !  IDENTIFY THE WORST POINT wo OF THE SUB-COMPLEX s
    !  COMPUTE THE CENTROID ce OF THE REMAINING POINTS
    !  COMPUTE step, THE VECTOR BETWEEN wo AND ce
    !  IDENTIFY THE WORST FUNCTION VALUE fw
    do j = 1, m
        sb(j) = s(1,j)
        sw(j) = s(n,j)
        ce(j) = 0.0
        do i = 1, n-1
            ce(j) = ce(j) + s(i,j)
        end do
        ce(j) = ce(j)/dble(n-1)
    end do
    fw = sf(n)
    !
    !c  COMPUTE THE NEW POINT snew
    !c
    !c  FIRST TRY A REFLECTION STEP
    do j = 1, m
        snew(j) = ce(j) + alpha * (ce(j) - sw(j))
    end do
    !c
    !c  CHECK IF snew SATISFIES ALL CONSTRAINTS
    call chkcst(nopt,snew,bl,bu,ibound)
    !c
    !c
    !c  snew IS OUTSIDE THE BOUND,
    !c  CHOOSE A POINT AT RANDOM WITHIN FEASIBLE REGION ACCORDING TO
    !c  A NORMAL DISTRIBUTION WITH BEST POINT OF THE SUB-COMPLEX
    !c  AS MEAN AND STANDARD DEVIATION OF THE POPULATION AS STD
    if (ibound .ge. 1) call getpnt(nopt,2,iseed,snew,bl,bu,xnstd,sb)
    !c
    !c
    !c  COMPUTE THE FUNCTION VALUE AT snew
    fnew = XXWFunctn(nopt,snew)

    icall = icall + 1

    write(g_CREST_LogFileID,"(4X,A8,I5,F20.15,3X,A15,I3)")   &
            "Trials: ",iCall,fnew,"Region Number: ",  &
            g_tCalibSta(g_RegNum)%Value
    !c
    !c  COMPARE fnew WITH THE WORST FUNCTION VALUE fw
    !c
    !c  fnew IS LESS THAN fw, ACCEPT THE NEW POINT snew AND RETURN
    if (fnew .le. fw) go to 2000
    if (icall .ge. maxn) go to 3000
    !c
    !c
    !c  fnew IS GREATER THAN fw, SO TRY A CONTRACTION STEP
    do j = 1, m
        snew(j) = ce(j) - beta * (ce(j) - sw(j))
    end do
    !c
    !c  COMPUTE THE FUNCTION VALUE OF THE CONTRACTED POINT
    !       fnew = functn(nopt,snew,idfunc) !XXW
    fnew = XXWFunctn(nopt,snew)
    icall = icall + 1

    write(g_CREST_LogFileID,"(4X,A8,I5,F20.15,3X,A15,I3)")  &
            "Trials: ",iCall,fnew,"Region Number: ", &
            g_tCalibSta(g_RegNum)%Value
    !c
    !c  COMPARE fnew TO THE WORST VALUE fw
    !c  IF fnew IS LESS THAN OR EQUAL TO fw, THEN ACCEPT THE POINT AND RETURN
    if (fnew <= fw) then
        go to 2000
    end if
    if (icall .ge. maxn) go to 3000
    !c
    !c
    !c  IF BOTH REFLECTION AND CONTRACTION FAIL, CHOOSE ANOTHER POINT
    !c  ACCORDING TO A NORMAL DISTRIBUTION WITH BEST POINT OF THE SUB-COMPLEX
    !c  AS MEAN AND STANDARD DEVIATION OF THE POPULATION AS STD
    1000 call getpnt(nopt,2,iseed,snew,bl,bu,xnstd,sb)
    !c
    !c  COMPUTE THE FUNCTION VALUE AT THE RANDOM POINT
    !       fnew = functn(nopt,snew,idfunc)
    fnew = XXWFunctn(nopt,snew)
    icall = icall + 1

    write(g_CREST_LogFileID,"(4X,A8,I5,F20.15,3X,A15,I3)")  &
            "Trials: ",iCall,fnew,"Region Number: ", &
            g_tCalibSta(g_RegNum)%Value
    !c
    !c
    !c  REPLACE THE WORST POINT BY THE NEW POINT
    2000 continue
    do j = 1, m
        s(n,j) = snew(j)
    end do
    sf(n) = fnew
    3000 continue
    !c
    !c  END OF SUBROUTINE CCE
    return
end
!c
!########################################################
!c==================================================================
subroutine chkcst(nopt,x,bl,bu,ibound)
    !c
    !c     This subroutine check if the trial point satisfies all
    !c     constraints.
    !c
    !c     ibound - violation indicator
    !c            = -1 initial value
    !c            = 0  no violation
    !c            = 1  violation
    !c     nopt = number of optimizing variables
    !c     ii = the ii'th variable of the arrays x, bl, and bu
    !c
    implicit real*8 (a-h,o-z)
    dimension x(nopt),bl(nopt),bu(nopt)
    !c
    ibound = -1
    !c
    !c     Check if explicit constraints are violated
    !c
    do ii=1, nopt
        if (x(ii) .lt. bl(ii) .or. x(ii) .gt. bu(ii)) go to 10
    end do
    !c      if (nopt .eq. 1) go to 9
    !c
    !c     Check if implicit constraints are violated
    !c
    !c      c1 = 5.0*x(1) + 2.0*x(3) - 100.0
    !c      c2 = x(3) + 50.*x(4) - 50.0
    !c      c3 = 10.*x(5) - x(6) - 2.0
    !c      c4 = -10.*x(5) + x(6) - 2.0
    !c      if (c1 .gt. 0.) go to 10
    !c      if (c2 .gt. 0.) go to 10
    !c      if (c3 .gt. 0.) go to 10
    !c      if (c4 .gt. 0.) go to 10
    !c
    !c     No constraints are violated
    !c
    9 ibound = 0
    return
    10 ibound = 1
    return
end
!c
!c
!c
!########################################################
!c====================================================================
subroutine comp(n,npt,ngs1,ngs2,npg,a,af,b,bf)
    !c
    !c
    !c  THIS SUBROUTINE REDUCE INPUT MATRIX a(n,ngs2*npg) TO MATRIX
    !c  b(n,ngs1*npg) AND VECTOR af(ngs2*npg) TO VECTOR bf(ngs1*npg)
    implicit real*8 (a-h,o-z)
    parameter(ijk=6000,np=50)
    dimension a(ijk,np),af(ijk),b(ijk,np),bf(ijk)
    do igs=1, ngs1
        do ipg=1, npg
            k1=(ipg-1)*ngs2 + igs
            k2=(ipg-1)*ngs1 + igs
            do i=1, n
                b(k2,i) = a(k1,i)
            end do
            bf(k2) = af(k1)
        end do
    end do
    !c
    do j=1, npt
        do i=1, n
            a(j,i) = b(j,i)
        end do
        af(j) = bf(j)
    end do
    !c
    !c  END OF SUBROUTINE COMP
    return
end
!c
!c
!c
!########################################################
function functn(nopt,x,idfunc)
    implicit none
    double precision functn,functOthers,functnsc
    integer nopt
    double precision x
    dimension x(nopt)

    integer idfunc

    if ((idfunc .ge. 1).and.(idfunc .le. 7)) then
        functn = functOthers(nopt,x)
    end if
    !c  This is the SAC model
    if (idfunc .eq. 8) then
        functn = functnsc(nopt,x)
    end if

    !c  END OF FUNCTION FUNCTN
    return
end
!########################################################
!c===============================================================
real*8 function gasdev(idum)
    !c
    implicit real*8 (a-h,o-z)
    common /gasblk/ iset
    save
    data iset / 0 /
    if (iset .eq. 0) then
        1   v1 = (2. * ran1(idum)) - 1.
        v2 = (2. * ran1(idum)) - 1.
        r = (v1 ** 2) + (v2 ** 2)
        if (r .ge. 1.) goto 1
        fac = sqrt(- ((2. * log(r)) / r))
        gset = v1 * fac
        gasdev = v2 * fac
        iset = 1
    else
        gasdev = gset
        iset = 0
    end if
    !c
    !c  END OF SUBROUTINE GASDEV
    return
end

!c
!c
!c
!########################################################
!c===================================================================
subroutine getpnt(nopt,idist,iseed,x,bl,bu,std,xi)
    !c
    !c
    implicit real*8 (a-h,o-z)
    dimension x(16),bl(16),bu(16),std(16),xi(16)
    !c
    1 do j=1, nopt

        2 if(bu(j)-bl(j)/=0.0)then
            if (idist .eq. 1) rand = ran1(iseed)
            if (idist .eq. 2) rand = gasdev(iseed)

            x(j) = xi(j) + std(j) * rand * (bu(j) - bl(j))
        else
            x(j)=bl(j)
        end if
        !c	if(rand.lt.0.0000001)write(*,*)rand
        !c
        !c     Check explicit constraints
        !c
        call chkcst(1,x(j),bl(j),bu(j),ibound)
        if (ibound .ge. 1) go to 2
    end do
    !c
    !c     Check implicit constraints
    !c
    call chkcst(nopt,x,bl,bu,ibound)
    if (ibound .ge. 1) go to 1
    !c
    return
end
!c
!c
!c
!########################################################
!c=======================================================
subroutine indexx(n, arrin, indx)

    !c  THIS SUBROUTINE IS FROM "NUMERICAL RECIPES" BY PRESS ET AL.
    implicit real*8 (a-h,o-z)
    integer n
    double precision arrin
    integer index
    dimension arrin(n), indx(n)

    integer i,j,l,ir
    !c
    do 11 j = 1, n
        indx(j) = j
    11 continue
    l = (n / 2) + 1
    ir = n
    10 continue
    if (l .gt. 1) then
        l = l - 1
        indxt = indx(l)
        q = arrin(indxt)
    else
        indxt = indx(ir)
        q = arrin(indxt)
        indx(ir) = indx(1)
        ir = ir - 1
        if (ir .eq. 1) then
            indx(1) = indxt
            return
        end if
    end if
    i = l
    j = l + l
    20 if (j .le. ir) then
        if (j .lt. ir) then
            if (arrin(indx(j)) .lt. arrin(indx(j + 1))) j = j + 1
        end if
        if (q .lt. arrin(indx(j))) then
            indx(i) = indx(j)
            i = j
            j = j + j
        else
            j = ir + 1
        end if
        goto 20
    end if
    indx(i) = indxt
    goto 10
    !c
    !c  END OF SUBROUTINE INDEXX
end
!c
!c
!c
!########################################################
!c===================================================================
subroutine parstt(npt,nopt,x,xnstd,bound,gnrng,ipcnvg)
    !c
    !c  SUBROUTINE CHECKING FOR PARAMETER CONVERGENCE
    implicit real*8 (a-h,o-z)
    parameter(ijk=6000,np=50)
    dimension x(ijk,np),xmax(np),xmin(np)
    dimension xmean(np),xnstd(np),bound(np)
    parameter (delta = 1.0d-20,peps=1.0d-3)
    !c
    !c  COMPUTE MAXIMUM, MINIMUM AND STANDARD DEVIATION OF PARAMETER VALUES
    gsum = 0.d0
    do k = 1, nopt
        xmax(k) = -1.0d+20
        xmin(k) = 1.0d+20
        xsum1 = 0.d0
        xsum2 = 0.d0
        do i = 1, npt
            xmax(k) = dmax1(x(i,k), xmax(k))
            xmin(k) = dmin1(x(i,k), xmin(k))
            xsum1 = xsum1 + x(i,k)
            xsum2 = xsum2 + x(i,k)*x(i,k)
        end do
        xmean(k) = xsum1 / dble(npt)
        xnstd(k) = (xsum2 / dble(npt) - xmean(k)*xmean(k))
        if (xnstd(k) <= delta) then
            xnstd(k) = delta
        end if
        xnstd(k) = dsqrt(xnstd(k))
        xnstd(k) = xnstd(k) / bound(k)
        gsum = gsum + dlog( delta + (xmax(k)-xmin(k))/bound(k) )
    end do
    gnrng = dexp(gsum/dble(nopt))
    !c
    !c  CHECK IF NORMALIZED STANDARD DEVIATION OF PARAMETER IS <= eps
    ipcnvg = 0
    if (gnrng .le. peps) then
        ipcnvg = 1
    end if
    !c
    !c  END OF SUBROUTINE PARSTT
    return
end
!c
!c
!c
!########################################################
!c==============================================================
real*8 function ran1(idum)
    !c
    implicit real*8 (a-h,o-z)
    dimension r(97)
    parameter (m1 = 259200, ia1 = 7141, ic1 = 54773,  &
            rm1 =3.8580247e-6)
    parameter (m2 = 134456, ia2 = 8121, ic2 = 28411,  &
            rm2 =7.4373773e-6)
    parameter (m3 = 243000, ia3 = 4561, ic3 = 51349)
    save
    data iff / 0 /
    if ((idum .lt. 0) .or. (iff .eq. 0)) then
        iff = 1
        ix1 = mod(ic1 - idum,m1)
        ix1 = mod((ia1 * ix1) + ic1,m1)
        ix2 = mod(ix1,m2)
        ix1 = mod((ia1 * ix1) + ic1,m1)
        ix3 = mod(ix1,m3)
        do 11 j = 1, 97
            ix1 = mod((ia1 * ix1) + ic1,m1)
            ix2 = mod((ia2 * ix2) + ic2,m2)
            r(j) = (dble(ix1) + (dble(ix2) * rm2)) * rm1
        11  continue
        idum = 1
    end if

    ix1 = mod((ia1 * ix1) + ic1,m1)
    ix2 = mod((ia2 * ix2) + ic2,m2)
    ix3 = mod((ia3 * ix3) + ic3,m3)
    j = 1 + ((97 * ix3) / m3)
    !if ((j .gt. 97) .or. (j .lt. 1)) pause
    ran1 = r(j)
    r(j) = (dble(ix1) + (dble(ix2) * rm2)) * rm1
    !c
    !c  END OF SUBROUTINE RAN1
    return
end
!c
!c
!c
!########################################################
!c  ::::::::::::::
!c  scein.for
!c  ::::::::::::::
subroutine XXWSCEIn(a,bl,bu,  &
        nopt,maxn,kstop,pcento,iseed,  &
        ngs,npg,nps,nspl,mings,iniflg,iprint, &
        sPath)
    !c
    !c   THIS SUBROUTINE READS AND PRINTS THE INPUT VARIABLES FOR
    !c   SHUFFLED COMPLEX EVOLUTION METHOD FOR GLOBAL OPTIMIZATION
    !c     -- Version 2.2
    !c
    !c   WRITTEN BY QINGYUN DUAN - UNIVERSITY OF ARIZONA, APRIL 1992
    !c
    !c
    implicit real*8 (a-h,o-z)
    !#############Parameters

    double precision a,bl,bu
    dimension a(50),bl(50),bu(50)
    character*(*)sPath
    integer nopt,maxn,kstop,iseed
    integer ngs,npg,nps,nspl,mings,iniflg,iprint
    double precision pcento
    !#############Global Variables
    integer ipr !Block iopar

    character*10 pcntrl,deflt,usrsp
    character*4 reduc,initl,ysflg,noflg,xname(50)
    parameter(ijk=6000,ldt=24)

    !#############Local Variables

    integer in !Block iopar

    !       common /block1/ ndata,ns,iobj,nday,nord,ndt
    !       common /block2/px(ijk,ldt),pet(ijk),qobs(ijk),uhg(50),state(20)
    !       common /block3/ smq(ijk),sqin(ijk*ldt),tci(ijk*ldt)
    common /block4/ npar,loc(50)
    common /block5/ pt(50),idfunc
    ! 	common /routing/ckm,cxm,nm
    common /iopar/ ipr
    ! 	common /idid/iduh,area

    data deflt/' DEFAULT  '/
    data usrsp/'USER SPEC.'/
    data ysflg/'YES '/
    data noflg/'NO  '/
    data xname /'  X1','  X2','  X3','  X4','  X5','  X6','  X7', &
            '  X8','  X9',' X10',' X11',' X12',' X13',' X14',' X15',' X16', &
            '  X8','  X9',' X10',' X11',' X12',' X13',' X14',' X15',' X25', &
            '  X8','  X9',' X10',' X11',' X12',' X13',' X14',' X15',' X34', &
            '  X8','  X9',' X10',' X11',' X12',' X13',' X14',' X15',' X43', &
            '  X8','  X9',' X10',' X11',' X12',' X13',' X50'/
    !c
    !!       write (*,*) ' ENTER THE SCEIN SUBROUTINE --- '
    !c
    !c
    !c  INITIALIZE I/O VARIABLES
    in = 12
    !XXW     ipr = 15
    if(idfunc/=55)then
        open(unit=in,file=sPath // 'scein.dat',status='old')
        !XXW      open(unit=ipr,file=sPath // 'sceout.dat',status='unknown')
    end if

    ierror = 0
    iwarn = 0
    write(ipr,700)
    700 format(10x,'SHUFFLED COMPLEX EVOLUTION GLOBAL OPTIMIZATION',  &
    /,10x,46(1h=))
    !c
    !c
    !c  READ THE SCE CONTROL PARAMETERS
    ideflt = 0
    if(idfunc/=55)then
        read(in,*)idfunc
        read(in,*) nopt,maxn,kstop,pcento,ngs,iseed,ideflt
    end if
    800 format(2i5,f6.2,3i5)
    if (iseed .eq. 0) iseed = 1969
    !c
    !c
    !c  IF ideflt IS EQUAL TO 1, READ THE SCE CONTROL PARAMETERS
    if (ideflt .eq. 1) Then
        if(idfunc/=55)then
            read(in,*) npg,nps,nspl,mings,iniflg,iprint
        end if
        810 format(6i5)
        pcntrl = usrsp
    else
        !c        read(in,*)
        pcntrl = deflt
    end if

    if(idfunc.ne.8) go to 555

    do 654 i=1,nopt
        if(idfunc/=55)then
            read(in,*) a(i),bl(i),bu(i)
        end if
        loc(i)=i
    654 continue

    555 continue
    !c
    !c  IF ideflt IS EQUAL TO 0, SET THE SCE CONTROL PARAMETERS TO
    !c  THE DEFAULT VALUES
    if (ideflt .eq. 0) then
        npg = 2*nopt + 1
        nps = nopt + 1
        nspl = npg
        mings = ngs
        iniflg = 0
        iprint = 0
    end if
    !c
    !c
    !c  CHECK IF THE SCE CONTROL PARAMETERS ARE VALID
    if (ngs .lt. 1 .or. ngs .ge. 1320) then
        write(ipr,900) ngs
        900 format(//,1x,'**ERROR** NUMBER OF COMPLEXES IN INITIAL ', &
                ' POPULATION ',i5,' IS NOT A VALID CHOICE')
        ierror = ierror + 1
    end if
    !c
    if (kstop .lt. 0 .or. kstop .ge. 20) then
        write(ipr,901) kstop
        901 format(//,1x,'**WARNING** THE NUMBER OF SHUFFLING LOOPS IN', &
                ' WHICH THE CRITERION VALUE MUST CHANGE ',/,13x,'SHOULD BE', &
                ' GREATER THAN 0 AND LESS THAN 10.  ','kstop = ',i2, &
                ' WAS SPECIFIED.'/,13x,'BUT kstop = 5 WILL BE USED INSTEAD.')
        iwarn = iwarn + 1
        kstop=5
    end if
    !c
    if (mings .lt. 1 .or. mings .gt. ngs) then
        write(ipr,902) mings
        902 format(//,1x,'**WARNING** THE MINIMUM NUMBER OF COMPLEXES ', &
                i2,' IS NOT A VALID CHOICE. SET IT TO DEFAULT')
        iwarn = iwarn + 1
        mings = ngs
    end if
    !c
    if (npg .lt. 2 .or. npg .gt. 1320/max(ngs,1)) then
        write(ipr,903) npg
        903 format(//,1x,'**WARNING** THE NUMBER OF POINTS IN A COMPLEX ', &
                I4,' IS NOT A VALID CHOICE, SET IT TO DEFAULT')
        iwarn = iwarn + 1
        npg = 2*nopt+1
    end if
    !c
    if (nps.lt.2 .or. nps.gt.npg .or. nps.gt.50) then
        write(ipr,904) nps
        904 format(//,1x,'**WARNING** THE NUMBER OF POINTS IN A SUB-', &
                'COMPLEX ',i4,' IS NOT A VALID CHOICE, SET IT TO DEFAULT')
        iwarn = iwarn + 1
        nps = nopt + 1
    end if
    !c
    if (nspl .lt. 1) then
        write(ipr,905) nspl
        905 format(//,1x,'**WARNING** THE NUMBER OF EVOLUTION STEPS ', &
                'TAKEN IN EACH COMPLEX BEFORE SHUFFLING ',I4,/,13x, &
                'IS NOT A VALID CHOICE, SET IT TO DEFAULT')
        iwarn = iwarn + 1
        nspl = npg
    end if
    !c
    !c  COMPUTE THE TOTAL NUMBER OF POINTS IN INITIAL POPULATION
    npt = ngs * npg
    !c
    if (npt .gt. 1320) then
        write(ipr,906) npt
        906 format(//,1x,'**WARNING** THE NUMBER OF POINTS IN INITIAL ', &
                'POPULATION ',i5,' EXCEED THE POPULATION LIMIT,',/,13x, &
                'SET NGS TO 2, AND NPG, NPS AND NSPL TO DEFAULTS')
        iwarn = iwarn + 1
        ngs = 2
        npg = 2*nopt + 1
        nps = nopt + 1
        nspl = npg
    end if
    !c
    !c  PRINT OUT THE TOTAL NUMBER OF ERROR AND WARNING MESSAGES
    if (ierror .ge. 1) write(ipr,907) ierror
    907 format(//,1x,'*** TOTAL NUMBER OF ERROR MESSAGES IS ',i2)
    !c
    if (iwarn .ge. 1) write(ipr,908) iwarn
    908 format(//,1x,'*** TOTAL NUMBER OF WARNING MESSAGES IS ',i2)
    !c
    if (mings .lt. ngs) then
        reduc = ysflg
    else
        reduc = noflg
    end if
    !c
    if (iniflg .ne. 0) then
        initl = ysflg
    else
        initl = noflg
    end if
    !c
    !c
    !c  PRINT SHUFFLED COMPLEX EVOLUTION OPTIMIZATION OPTIONS
    104 write(ipr,910)
    910 format(//,2x,'SCE CONTROL',5x,'MAX TRIALS',5x, &
    'REQUIRED IMPROVEMENT',5x,'RANDOM',/,3x,'PARAMETER',8x, &
    'ALLOWED',6x,'PERCENT',4x,'NO. LOOPS',6x,'SEED',/, &
    2 x,11(1h-),5x,10(1H-),5x,7(1h-),4x,9(1h-),5x,6(1h-))
    !c
    pcenta=pcento*100.
    write(ipr,912) pcntrl,maxn,pcenta,kstop,iseed
    912 format(3x,a10,7x,i5,10x,f5.1,9x,i2,9x,i5)
    write(ipr,914) ngs,npg,npt,nps,nspl
    914 format(//,18x,'SCE ALGORITHM CONTROL PARAMETERS',/,18x,32(1H=), &
            //,2x,'NUMBER OF',5x,'POINTS PER',5x,'POINTS IN',6x,'POINTS PER', &
            4 x,'EVOL. STEPS',/,2x,'COMPLEXES',6X,'COMPLEX',6x,'INI. POPUL.', &
            5 x,'SUB-COMPLX',4x,'PER COMPLEX',/,2x,9(1h-),5x,10(1h-),4x, &
            11 (1h-),5x,10(1h-),4x,11(1h-),5x,/,2x,5(i5,10x))
    write(ipr,915) reduc,mings,initl
    915 format(//,15x,'COMPLX NO.',5x,'MIN COMPLEX',5x,'INI. POINT',/, &
    15 x,'REDUCTION',6x,'NO. ALLOWED',6x,'INCLUDED',/, &
    15 x,10(1h-),5x,11(1h-),5x,10(1h-),/,18x,a4,6x,i8,13x,a4)
    write(ipr,916)
    916 format(//,8x,'INITIAL PARAMETER VALUES AND PARAMETER BOUNDS',/, &
    8 x,45(1h=),//,2x,'PARAMETER',5x,'INITIAL VALUE',5x &
            'LOWER BOUND',5x,'UPPER BOUND',/,2x,9(1h-),5x,13(1h-),5x, &
            11 (1h-),5x,11(1h-))
    do 920 i = 1, nopt
        write(ipr,918) xname(i),a(i),bl(i),bu(i)
        918 format(4x,a4,4x,3(6x,f10.3))
    920 continue
    if (ierror .ge. 1) then
        write(ipr,922)
        922 format(//,'*** THE OPTIMIZATION SEARCH IS NOT CONDUCTED BECAUSE', &
                ' OF INPUT DATA ERROR ***')
        stop
    end if
    !c
    !C  END OF SUBROUTINE SCEIN
    return
end




!------------------------------------------------------

subroutine scein(a,bl,bu,  &
        nopt,maxn,kstop,pcento,iseed, &
        ngs,npg,nps,nspl,mings,iniflg,iprint, &
        sPath)
    !c
    !c   THIS SUBROUTINE READS AND PRINTS THE INPUT VARIABLES FOR
    !c   SHUFFLED COMPLEX EVOLUTION METHOD FOR GLOBAL OPTIMIZATION
    !c     -- Version 2.2
    !c
    !c   WRITTEN BY QINGYUN DUAN - UNIVERSITY OF ARIZONA, APRIL 1992
    !c
    !c
    implicit real*8 (a-h,o-z)
    !#############Parameters

    double precision a,bl,bu
    dimension a(50),bl(50),bu(50)
    character*(*)sPath
    integer nopt,maxn,kstop,iseed
    integer ngs,npg,nps,nspl,mings,iniflg,iprint
    double precision pcento
    !#############Global Variables
    integer ipr !Block iopar

    character*10 pcntrl,deflt,usrsp
    character*4 reduc,initl,ysflg,noflg,xname(50)
    parameter(ijk=6000,ldt=24)

    !#############Local Variables

    integer in !Block iopar

    !       common /block1/ ndata,ns,iobj,nday,nord,ndt
    !       common /block2/px(ijk,ldt),pet(ijk),qobs(ijk),uhg(50),state(20)
    !       common /block3/ smq(ijk),sqin(ijk*ldt),tci(ijk*ldt)
    common /block4/ npar,loc(50)
    common /block5/ pt(50),idfunc
    ! 	common /routing/ckm,cxm,nm
    common /iopar/ ipr
    ! 	common /idid/iduh,area

    data deflt/' DEFAULT  '/
    data usrsp/'USER SPEC.'/
    data ysflg/'YES '/
    data noflg/'NO  '/
    data xname /'  X1','  X2','  X3','  X4','  X5','  X6','  X7', &
            '  X8','  X9',' X10',' X11',' X12',' X13',' X14',' X15',' X16', &
            '  X8','  X9',' X10',' X11',' X12',' X13',' X14',' X15',' X25', &
            '  X8','  X9',' X10',' X11',' X12',' X13',' X14',' X15',' X34', &
            '  X8','  X9',' X10',' X11',' X12',' X13',' X14',' X15',' X43', &
            '  X8','  X9',' X10',' X11',' X12',' X13',' X50'/
    !c
    !!       write (*,*) ' ENTER THE SCEIN SUBROUTINE --- '
    !c
    !c
    !c  INITIALIZE I/O VARIABLES
    in = 12
    !XXW     ipr = 15
    open(unit=in,file=sPath // 'scein.dat',status='old')
    !XXW      open(unit=ipr,file=sPath // 'sceout.dat',status='unknown')
    !
    ierror = 0
    iwarn = 0
    write(ipr,700)
    700 format(10x,'SHUFFLED COMPLEX EVOLUTION GLOBAL OPTIMIZATION', &
    /,10x,46(1h=))
    !c
    !c
    !c  READ THE SCE CONTROL PARAMETERS
    ideflt = 0
    read(in,*)idfunc
    read(in,*) nopt,maxn,kstop,pcento,ngs,iseed,ideflt
    800 format(2i5,f6.2,3i5)
    if (iseed .eq. 0) iseed = 1969
    !c
    !c
    !c  IF ideflt IS EQUAL TO 1, READ THE SCE CONTROL PARAMETERS
    if (ideflt .eq. 1) Then
        read(in,*) npg,nps,nspl,mings,iniflg,iprint
        810 format(6i5)
        pcntrl = usrsp
    else
        !c        read(in,*)
        pcntrl = deflt
    end if

    if(idfunc.ne.8) go to 555

    do 654 i=1,nopt
        read(in,*) a(i),bl(i),bu(i)
        loc(i)=i
    654 continue

    555 continue
    !c
    !c  IF ideflt IS EQUAL TO 0, SET THE SCE CONTROL PARAMETERS TO
    !c  THE DEFAULT VALUES

    if (ideflt .eq. 0) then
        npg = 2*nopt + 1
        nps = nopt + 1
        nspl = npg
        mings = ngs
        iniflg = 0
        iprint = 0
    end if
    !c
    !c
    !c  CHECK IF THE SCE CONTROL PARAMETERS ARE VALID
    if (ngs .lt. 1 .or. ngs .ge. 1320) then
        write(ipr,900) ngs
        900 format(//,1x,'**ERROR** NUMBER OF COMPLEXES IN INITIAL ', &
                ' POPULATION ',i5,' IS NOT A VALID CHOICE')
        ierror = ierror + 1
    end if
    !c
    if (kstop .lt. 0 .or. kstop .ge. 20) then
        write(ipr,901) kstop
        901 format(//,1x,'**WARNING** THE NUMBER OF SHUFFLING LOOPS IN', &
                ' WHICH THE CRITERION VALUE MUST CHANGE ',/,13x,'SHOULD BE', &
                ' GREATER THAN 0 AND LESS THAN 10.  ','kstop = ',i2, &
                ' WAS SPECIFIED.'/,13x,'BUT kstop = 5 WILL BE USED INSTEAD.')
        iwarn = iwarn + 1
        kstop=5
    end if
    !c
    if (mings .lt. 1 .or. mings .gt. ngs) then
        write(ipr,902) mings
        902 format(//,1x,'**WARNING** THE MINIMUM NUMBER OF COMPLEXES ', &
                i2,' IS NOT A VALID CHOICE. SET IT TO DEFAULT')
        iwarn = iwarn + 1
        mings = ngs
    end if
    !c
    if (npg .lt. 2 .or. npg .gt. 1320/max(ngs,1)) then
        write(ipr,903) npg
        903 format(//,1x,'**WARNING** THE NUMBER OF POINTS IN A COMPLEX ', &
                I4,' IS NOT A VALID CHOICE, SET IT TO DEFAULT')
        iwarn = iwarn + 1
        npg = 2*nopt+1
    end if
    !c
    if (nps.lt.2 .or. nps.gt.npg .or. nps.gt.50) then
        write(ipr,904) nps
        904 format(//,1x,'**WARNING** THE NUMBER OF POINTS IN A SUB-', &
                'COMPLEX ',i4,' IS NOT A VALID CHOICE, SET IT TO DEFAULT')
        iwarn = iwarn + 1
        nps = nopt + 1
    end if
    !c
    if (nspl .lt. 1) then
        write(ipr,905) nspl
        905 format(//,1x,'**WARNING** THE NUMBER OF EVOLUTION STEPS ', &
                'TAKEN IN EACH COMPLEX BEFORE SHUFFLING ',I4,/,13x, &
                'IS NOT A VALID CHOICE, SET IT TO DEFAULT')
        iwarn = iwarn + 1
        nspl = npg
    end if
    !c
    !c  COMPUTE THE TOTAL NUMBER OF POINTS IN INITIAL POPULATION
    npt = ngs * npg
    !c
    if (npt .gt. 1320) then
        write(ipr,906) npt
        906 format(//,1x,'**WARNING** THE NUMBER OF POINTS IN INITIAL ', &
                'POPULATION ',i5,' EXCEED THE POPULATION LIMIT,',/,13x, &
                'SET NGS TO 2, AND NPG, NPS AND NSPL TO DEFAULTS')
        iwarn = iwarn + 1
        ngs = 2
        npg = 2*nopt + 1
        nps = nopt + 1
        nspl = npg
    end if
    !c
    !c  PRINT OUT THE TOTAL NUMBER OF ERROR AND WARNING MESSAGES
    if (ierror .ge. 1) write(ipr,907) ierror
    907 format(//,1x,'*** TOTAL NUMBER OF ERROR MESSAGES IS ',i2)
    !c
    if (iwarn .ge. 1) write(ipr,908) iwarn
    908 format(//,1x,'*** TOTAL NUMBER OF WARNING MESSAGES IS ',i2)
    !c
    if (mings .lt. ngs) then
        reduc = ysflg
    else
        reduc = noflg
    end if

    if (iniflg .ne. 0) then
        initl = ysflg
    else
        initl = noflg
    end if
    !c
    !c
    !c  PRINT SHUFFLED COMPLEX EVOLUTION OPTIMIZATION OPTIONS
    104 write(ipr,910)
    910 format(//,2x,'SCE CONTROL',5x,'MAX TRIALS',5x, &
    'REQUIRED IMPROVEMENT',5x,'RANDOM',/,3x,'PARAMETER',8x, &
    'ALLOWED',6x,'PERCENT',4x,'NO. LOOPS',6x,'SEED',/, &
    2 x,11(1h-),5x,10(1H-),5x,7(1h-),4x,9(1h-),5x,6(1h-))
    !c
    pcenta=pcento*100.
    write(ipr,912) pcntrl,maxn,pcenta,kstop,iseed
    912 format(3x,a10,7x,i5,10x,f5.1,9x,i2,9x,i5)
    write(ipr,914) ngs,npg,npt,nps,nspl
    914 format(//,18x,'SCE ALGORITHM CONTROL PARAMETERS',/,18x,32(1H=), &
            //,2x,'NUMBER OF',5x,'POINTS PER',5x,'POINTS IN',6x,'POINTS PER', &
            4 x,'EVOL. STEPS',/,2x,'COMPLEXES',6X,'COMPLEX',6x,'INI. POPUL.', &
            5 x,'SUB-COMPLX',4x,'PER COMPLEX',/,2x,9(1h-),5x,10(1h-),4x, &
            11 (1h-),5x,10(1h-),4x,11(1h-),5x,/,2x,5(i5,10x))
    write(ipr,915) reduc,mings,initl
    915 format(//,15x,'COMPLX NO.',5x,'MIN COMPLEX',5x,'INI. POINT',/, &
    15 x,'REDUCTION',6x,'NO. ALLOWED',6x,'INCLUDED',/, &
    15 x,10(1h-),5x,11(1h-),5x,10(1h-),/,18x,a4,6x,i8,13x,a4)
    write(ipr,916)
    916 format(//,8x,'INITIAL PARAMETER VALUES AND PARAMETER BOUNDS',/, &
    8 x,45(1h=),//,2x,'PARAMETER',5x,'INITIAL VALUE',5x, &
            'LOWER BOUND',5x,'UPPER BOUND',/,2x,9(1h-),5x,13(1h-),5x, &
            11 (1h-),5x,11(1h-))
    do 920 i = 1, nopt
        write(ipr,918) xname(i),a(i),bl(i),bu(i)
        918 format(4x,a4,4x,3(6x,f10.3))
    920 continue
    if (ierror .ge. 1) then
        write(ipr,922)
        922 format(//,'*** THE OPTIMIZATION SEARCH IS NOT CONDUCTED BECAUSE', &
                ' OF INPUT DATA ERROR ***')
        stop
    end if
    !c
    !C  END OF SUBROUTINE SCEIN
    return
end
!########################################################
!c ::::::::::::::
!c sceua.for
!c ::::::::::::::
subroutine XXWSCEUA(a,bl,bu,nopt,maxn,kstop,pcento,iseed, &
        ngs,npg,nps,nspl,mings,iniflg,iprint, &
        XXWFunctn)
    use CREST_Project
    use CREST_Calib_SCEUA

    !c
    implicit real*8 (a-h,o-z)
    !c
    !c  ARRAYS FROM THE INPUT DATA
    dimension a(16),bl(16),bu(16)
    !c
    !c  LOCAL ARRAYS
    parameter(ijk=6000,np=50)
    dimension x(ijk,np),xx(np),bestx(np),worstx(np),xf(ijk)
    dimension s(50,np),sf(50),lcs(50),cx(ijk,np),cf(ijk)
    dimension xnstd(np),bound(np),criter(20),unit(np)

    double precision,external :: XXWFunctn  !XXW
    double precision :: XXWBestValue  !XXW
    logical :: fExist
    !c
    common /iopar/ ipr
    !cis finished!
    character*4 xname(16)
    data xname /'  X1','  X2','  X3','  X4','  X5','  X6','  X7', &
            '  X8','  X9',' X10',' X11',' X12',' X13',' X14',' X15',' X16'/
    !c
    write (*,"(30X,A,/)") ' ENTER THE SCEUA SUBROUTINE '
    WRITE(g_CREST_LogFileID,*)
    WRITE(g_CREST_LogFileID,"(30X,A,/)")  &
            ' ENTER THE SCEUA SUBROUTINE '
    WRITE(g_CREST_LogFileID,*)
    !c
    !c  INITIALIZE VARIABLES
    nloop = 0
    loop = 0
    igs = 0
    nopt1 = 8
    if (nopt.lt.8) nopt1 = nopt
    nopt2 = 12
    if (nopt.lt.12) nopt2 = nopt
    !c
    !c  INITIALIZE RANDOM SEED TO A NEGATIVE INTEGER
    iseed1 = -abs(iseed)
    !c
    !c  COMPUTE THE TOTAL NUMBER OF POINTS IN INITIAL POPUALTION
    npt = ngs * npg
    ngs1 = ngs
    npt1 = npt
    !c
    write(ipr,400)
    write (*,*) ' ***  Evolution Loop Number ',nloop

    WRITE(g_CREST_LogFileID,*)
    WRITE(g_CREST_LogFileID,*)   &
            ' ***  Evolution Loop Number ',nloop
    
    !c
    !c  COMPUTE THE BOUND FOR PARAMETERS BEING OPTIMIZED
    do j = 1, nopt
        bound(j) = bu(j) - bl(j)
        unit(j) = 1.
    end do
    !c
    !c  COMPUTE THE FUNCTION VALUE OF THE INITIAL POINT
    !       fa = functn(nopt,a,idfunc) !XXW
    fa = XXWFunctn(nopt,a)
    write(g_CREST_LogFileID,"(4X,A8,I5,F20.15,3X,A15,I3)")  &
            "Trials: ",iCall,fa,"Region Number: ", &
            g_tCalibSta(g_RegNum)%Value

    !To stop for anytime when "XXW.CREST.STOP" exists
    inquire(file=trim(g_ProjectWS)//trim("XXW.CREST.STOP"), &
            exist=fExist)
    if(fExist .eqv. .true.)then
        go to 9500
    end if
    !c
    !c  PRINT THE INITIAL POINT AND ITS CRITERION VALUE
    write(ipr,500)
    write(ipr,510) (xname(j),j=1,nopt2)
    write(ipr,520) fa,(a(j),j=1,nopt2)
    if (nopt.lt.13) go to 101
    write(ipr,530) (xname(j),j=13,nopt)
    write(ipr,540) (a(j),j=13,nopt)
    101 continue
    !c
    !c  GENERATE AN INITIAL SET OF npt1 POINTS IN THE PARAMETER SPACE
    !c  IF iniflg IS EQUAL TO 1, SET x(1,.) TO INITIAL POINT a(.)
    if (iniflg .eq. 1) then
        do j = 1, nopt
            x(1,j) = a(j)
        end do
        xf(1) = fa
        !c
        !c  ELSE, GENERATE A POINT RANDOMLY AND SET IT EQUAL TO x(1,.)
    else
        call getpnt(nopt,1,iseed1,xx,bl,bu,unit,bl)
        do j=1, nopt
            x(1,j) = xx(j)
        end do
        !         xf(1) = functn(nopt,xx,idfunc) !XXW
        xf(1) = XXWFunctn(nopt,xx)

        icall = 1
        write(g_CREST_LogFileID,"(4X,A8,I5,F20.15,3X,A15,I3)")  &
                "Trials: ",iCall,xf(1),"Region Number: ", &
                g_tCalibSta(g_RegNum)%Value

    end if
    icall = 1
    if (icall .ge. maxn) go to 9000
    !c
    !c  GENERATE npt1-1 RANDOM POINTS DISTRIBUTED UNIFORMLY IN THE PARAMETER
    !c  SPACE, AND COMPUTE THE CORRESPONDING FUNCTION VALUES
    do i = 2, npt1
        call getpnt(nopt,1,iseed1,xx,bl,bu,unit,bl)
        do j = 1, nopt
            x(i,j) = xx(j)
        end do
        !         xf(i) = functn(nopt,xx,idfunc)
        xf(i) = XXWFunctn(nopt,xx)

        icall = icall + 1

        write(g_CREST_LogFileID,"(4X,A8,I5,F20.15,3X,A15,I3)")  &
                "Trials: ",iCall,xf(i),"Region Number: ", &
                g_tCalibSta(g_RegNum)%Value

        if (icall .ge. maxn) then
            nopt1 = i
            go to 45
        end if
    end do
    !c
    !c  ARRANGE THE POINTS IN ORDER OF INCREASING FUNCTION VALUE
    45 call sort(npt1,nopt,x,xf)
    !c
    !c  RECORD THE BEST AND WORST POINTS
    do j = 1, nopt
        bestx(j) = x(1,j)
        worstx(j) = x(npt1,j)
    end do
    bestf = xf(1)
    worstf = xf(npt1)
    !c
    !c  COMPUTE THE PARAMETER RANGE FOR THE INITIAL POPULATION
    call parstt(npt1,nopt,x,xnstd,bound,gnrng,ipcnvg)
    !c
    !c  PRINT THE RESULTS FOR THE INITIAL POPULATION
    write(ipr,600)
    write(ipr,610) (xname(j),j=1,nopt1)
    if (nopt .lt. 9) go to 201
    write(ipr,620) (xname(j),j=9,nopt)
    201 continue
    write(ipr,630) nloop,icall,ngs1,bestf,worstf,gnrng, &
            (bestx(j),j=1,nopt1)
    if (nopt .lt. 9) go to 301
    write(ipr,640) (bestx(j),j=9,nopt)
    301 continue
    !	iprint=1
    if (iprint .eq. 1) then
        write(ipr,650) nloop
        do i = 1, npt1
            write(ipr,660) xf(i),(x(i,j),j=1,nopt1)
            if (nopt .lt. 9) go to 401
            write(ipr,640) (x(i,j),j=9,nopt)
        401 end do
    end if
    !c
    if (icall .ge. maxn) go to 9000
    if (ipcnvg .eq. 1) go to 9200
    !c
    !c  BEGIN THE MAIN LOOP ----------------
    1000 continue


    inquire(file=trim(g_ProjectWS)//trim("XXW.CREST.STOP"),  &
            exist=fExist)
    if(fExist .eqv. .true.)then
        go to 9500
    end if
    nloop = nloop + 1
    !c
    write (*,*) ' ***  Evolution Loop Number ',nloop
    write(g_CREST_LogFileID,*)  &
            ' ***  Evolution Loop Number ',nloop
    !c
    !c  BEGIN LOOP ON COMPLEXES
    do 3000 igs = 1, ngs1
        !c
        !c  ASSIGN POINTS INTO COMPLEXES
        do k1 = 1, npg
            k2 = (k1-1) * ngs1 + igs
            do j = 1, nopt
                cx(k1,j) = x(k2,j)
            end do
            cf(k1) = xf(k2)
        end do
        !c
        !c  BEGIN INNER LOOP - RANDOM SELECTION OF SUB-COMPLEXES ---------------
        do 2000 loop = 1, nspl
            !c
            !c  CHOOSE A SUB-COMPLEX (nps points) ACCORDING TO A LINEAR
            !c  PROBABILITY DISTRIBUTION
            if (nps .eq. npg) then
                do k = 1, nps
                    lcs(k) = k				!lcs(.) = indices locating position of s(.,.)
                end do
                go to 85
            end if
            !c
            rand = ran1(iseed1)


            lcs(1) = 1 + dint(npg + 0.5 - dsqrt( (npg+.5)**2 - &
                    npg * (npg+1) * rand ))
            do k = 2, nps
                60      rand = ran1(iseed1)
                lpos = 1 + dint(npg + 0.5 - dsqrt((npg+.5)**2 -  &
                        npg * (npg+1) * rand ))
                do k1 = 1, k-1
                    if (lpos .eq. lcs(k1)) go to 60
                end do
                lcs(k) = lpos
            end do
            !c
            !c  ARRANGE THE SUB-COMPLEX IN ORDER OF INCEASING FUNCTION VALUE
            call sort1(nps,lcs)
            !c
            !c  CREATE THE SUB-COMPLEX ARRAYS
            85    do k = 1, nps
                do j = 1, nopt
                    s(k,j) = cx(lcs(k),j)
                end do
                sf(k) = cf(lcs(k))
            end do
            !c
            !c  USE THE SUB-COMPLEX TO GENERATE NEW POINT(S)
            call XXWCCE(nopt,nps,s,sf,bl,bu,xnstd,icall,maxn,iseed1,XXWFunctn)
            !c
            !c  IF THE SUB-COMPLEX IS ACCEPTED, REPLACE THE NEW SUB-COMPLEX
            !c  INTO THE COMPLEX
            do k = 1, nps
                do j = 1, nopt
                    cx(lcs(k),j) = s(k,j)
                end do
                cf(lcs(k)) = sf(k)
            end do
            !c
            !c  SORT THE POINTS
            call sort(npg,nopt,cx,cf)
            !c
            !c  IF MAXIMUM NUMBER OF RUNS EXCEEDED, BREAK OUT OF THE LOOP
            if (icall .ge. maxn) go to 2222
            !c
            !c  END OF INNER LOOP ------------
        2000 continue
        2222 continue
        !c
        !c  REPLACE THE NEW COMPLEX INTO ORIGINAL ARRAY x(.,.)
        do k1 = 1, npg
            k2 = (k1-1) * ngs1 + igs
            do j = 1, nopt
                x(k2,j) = cx(k1,j)
            end do
            xf(k2) = cf(k1)
        end do
        if (icall .ge. maxn) go to 3333
        !c
        !c  END LOOP ON COMPLEXES
    3000 continue
    !c
    !c  RE-SORT THE POINTS
    3333 call sort(npt1,nopt,x,xf)
    !c
    !c  RECORD THE BEST AND WORST POINTS
    do j = 1, nopt
        bestx(j) = x(1,j)
        worstx(j) = x(npt1,j)
    end do
    bestf = xf(1)
    worstf = xf(npt1)
    !c
    !c  TEST THE POPULATION FOR PARAMETER CONVERGENCE
    call parstt(npt1,nopt,x,xnstd,bound,gnrng,ipcnvg)
    !       write(*,*)"   Nloop:",nloop
    !c
    !c  PRINT THE RESULTS FOR CURRENT POPULATION
    if (mod(nloop,5) .ne. 0) go to 501
    write(ipr,610) (xname(j),j=1,nopt1)
    if (nopt .lt. 9) go to 501
    write(ipr,620) (xname(j),j=9,nopt)
    501 continue
    write(ipr,630) nloop,icall,ngs1,bestf,worstf,gnrng,  &
            (bestx(j),j=1,nopt1)
    if (nopt.lt.9) go to 601
    write(ipr,640) (bestx(j),j=9,nopt)
    601 continue
    if (iprint .eq. 1) then
        write(ipr,650) nloop
        do i = 1, npt1
            write(ipr,660) xf(i),(x(i,j),j=1,nopt1)
            if (nopt .lt. 9) go to 701
            write(ipr,640) (x(i,j),j=9,nopt)
        701 end do
    end if
    !c
    !c  TEST IF MAXIMUM NUMBER OF FUNCTION EVALUATIONS EXCEEDED
    if (icall .ge. maxn) go to 9000
    !c
    !c  COMPUTE THE COUNT ON SUCCESSIVE LOOPS W/O FUNCTION IMPROVEMENT
    criter(20) = bestf
    if (nloop .lt. (kstop+1)) go to 132
    denomi = dabs(criter(20-kstop) + criter(20)) / 2.
    timeou = dabs(criter(20-kstop) - criter(20)) / denomi
    if (timeou .lt. pcento) go to 9100
    132 continue
    do l = 1, 19
        criter(l) = criter(l+1)
    end do
    !c
    !c  IF POPULATION IS CONVERGED INTO A SUFFICIENTLY SMALL SPACE
    if (ipcnvg .eq. 1) go to 9200
    !c
    !c  NONE OF THE STOPPING CRITERIA IS SATISFIED, CONTINUE SEARCH
    !c
    !c  CHECK FOR COMPLEX NUMBER REDUCTION
    if (ngs1 .gt. mings) then
        ngs2 = ngs1
        ngs1 = ngs1 - 1
        npt1 = ngs1 * npg
        call comp(nopt,npt1,ngs1,ngs2,npg,x,xf,cx,cf)
    end if
    !c
    !c  END OF MAIN LOOP -----------
    go to 1000
    !c
    !c  SEARCH TERMINATED
    9000 continue
    write(ipr,800) maxn,loop,igs,nloop
    write(g_CREST_LogFileID,*)
    write(g_CREST_LogFileID,800) maxn,loop,igs,nloop
    go to 9999
    9100 continue
    write(ipr,810) pcento*100.,kstop
    write(g_CREST_LogFileID,*)
    write(g_CREST_LogFileID,810) pcento*100.,kstop
    go to 9999
    9200 continue
    write(g_CREST_LogFileID,*)
    write(ipr,820) gnrng*100.
    write(g_CREST_LogFileID,820) gnrng*100.
    go to 9999
    9500 continue
    write(g_CREST_LogFileID,*)
    write(ipr,850) pcento*100.,kstop !XXW
    write(g_CREST_LogFileID,850) pcento*100.,kstop !XXW
    9999 continue
    !c
    !c  PRINT THE FINAL PARAMETER ESTIMATE AND ITS FUNCTION VALUE
    write(ipr,830)
    write(ipr,510) (xname(j),j=1,nopt2)
    write(ipr,520) bestf,(bestx(j),j=1,nopt2)
    if (nopt.lt.13) go to 801
    write(ipr,530) (xname(j),j=13,nopt)
    write(ipr,540) (bestx(j),j=13,nopt)

    801 continue

    XXWBestValue = XXWFunctn(nopt,bestx)

    write(g_CREST_LogFileID,"(4X,A12,F20.15,3X,A15,I3)")   &
            "Best Value: ",XXWBestValue,"Region Number: ", &
            g_tCalibSta(g_RegNum)%Value
    !c
    !c  END OF SUBROUTINE SCEUA
    return
    400 format(//,2x,50(1h=),/,2x,'ENTER THE SHUFFLED COMPLEX EVOLUTION', &
            ' GLOBAL SEARCH',/,2x,50(1h=))
    500 format(//,'*** PRINT THE INITIAL POINT AND ITS CRITERION ',   &
            'VALUE ***')
    510 format(/,' CRITERION',12(6x,a4),/1x,60(1h-))
    520 format(g10.3,12f10.3)
    530 format(10x,12(6x,a4))
    540 format(10x,12f10.3)
    600 format(//,1x,'*** PRINT THE RESULTS OF THE SCE SEARCH ***')
    610 format(/,1x,'LOOP',1x,'TRIALS',1x,'COMPLXS',2x,'BEST F',3x,  &
            'WORST F',3x,'PAR RNG',1x,8(6x,a4))
    620 format(49x,8(6x,a4))
    630 format(i5,1x,i5,3x,i5,3g10.3,8(f10.3))
    640 format(49x,8(f10.3))
    650 format(/,1x,'POPULATION AT LOOP ',i3,/,1x,22(1h-))
    660 format(15x,g10.3,20x,8(f10.3))
    800 format(//,1x,'*** OPTIMIZATION SEARCH TERMINATED BECAUSE THE',  &
            ' LIMIT ON THE MAXIMUM',/,5x,'NUMBER OF TRIALS ',i5,  &
            ' EXCEEDED.  SEARCH WAS STOPPED AT',/,5x,'SUB-COMPLEX ', &
            i3,' OF COMPLEX ',i3,' IN SHUFFLING LOOP ',i3,' ***')
    810 format(//,1x,'*** OPTIMIZATION TERMINATED BECAUSE THE CRITERION',  &
            ' VALUE HAS NOT CHANGED ',/,5x,f5.2,' PERCENT IN',i3,  &
            ' SHUFFLING LOOPS ***')
    820 format(//,1x,'*** OPTIMIZATION TERMINATED BECAUSE THE POPULATION',  &
            ' HAS CONVERGED INTO ',/,4x,f5.2,' PERCENT OF THE', &
            ' FEASIBLE SPACE ***')
    830 format(//,'*** PRINT THE FINAL PARAMETER ESTIMATE AND ITS',  &
            ' CRITERION VALUE ***')

    850 format(//,1x,'*** OPTIMIZATION TERMINATED BECAUSE Dr. Xianwu Xue',  &
            ' Want to stop it! ',/,5x,f5.2,' PERCENT IN',i3,   &
            ' SHUFFLING LOOPS ***')

end subroutine XXWSCEUA
!c
!########################################################
!c===================================================================
subroutine sort(n,m,rb,ra)

    !c
    implicit real*8 (a-h,o-z)
    parameter(ijk=6000)
    dimension ra(ijk),rb(ijk,16),wk(ijk,16),iwk(ijk)
    !c
    call indexx(n, ra, iwk)
    do 11 i = 1, n
        wk(i,1) = ra(i)
    11 continue
    do 12 i = 1, n
        ra(i) = wk(iwk(i),1)
    12 continue
    do 14 j = 1, m
        do 13 i = 1, n
            wk(i,j) = rb(i,j)
        13  continue
    14 continue
    do 16 j = 1, m
        do 15 i = 1, n
            rb(i,j) = wk(iwk(i),j)
        15  continue
    16 continue
    !c
    !c  END OF SUBROUTINE SORT
    return
end
!c
!c
!########################################################
!c===========================================================
subroutine sort1(n,ra)
    !c
    implicit real*8 (a-h,o-z)
    dimension ra(n)
    !c
    integer ra, rra
    !c
    l = (n / 2) + 1
    ir = n
    10 continue
    if (l .gt. 1) then
        l = l - 1
        rra = ra(l)
    else
        rra = ra(ir)
        ra(ir) = ra(1)
        ir = ir - 1
        if (ir .eq. 1) then
            ra(1) = rra
            return
        end if
    end if
    i = l
    j = l + l
    20 if (j .le. ir) then
        if (j .lt. ir) then
            if (ra(j) .lt. ra(j + 1)) j = j + 1
        end if
        if (rra .lt. ra(j)) then
            ra(i) = ra(j)
            i = j
            j = j + j
        else
            j = ir + 1
        end if
        goto 20
    end if
    ra(i) = rra
    goto 10
    !c
    !c  END OF SUBROUTINE SORT1
end
!c
!c
!c
!########################################################
function functOthers(nopt,x)
    implicit real*8 (a-h,o-z)

    parameter(ijk=6000,ldt=24,bigm = 1.0d10)

    common /block4/ npar,loc(50)
    common /block5/ pt(50),idfunc
    !       common /iopar/ ipr
    dimension x(nopt), par(50)
    common /ablu/  bl(50), bu(50)


    dimension a1(10,4), c1(10), a2(4,6), c2(4), p2(4,6), a3(4,3), c3(4), p3(4,3)
    !c  Data for Skekel function coefficients (n=4, m=10)
    data a1 /4.,1.,8.,6.,3.,2.,5.,8.,6.,7., 4.,1.,8.,6.,7.,9.,5., &
            1.,2.,3.6, 4.,1.,8.,6.,3.,2.,3.,8.,6.,7.,  &
            4.,1.,8.,6.,7.,9.,3.,1.,2.,3.6/
    data c1 /.1,.2,.2,.4,.4,.6,.3,.7,.5,.5/
    !c  Data for Hartman function coefficients (6-D)
    data a2 /10.,0.05,3.,17.,3.,10.,3.5,8.,17.,17.,1.7,.05, 3.5, &
            0.1,10.,10.,1.7,8.,17.,.1,8.,14.,8.,14./
    data c2 /1.,1.2,3.,3.2/
    data p2 /.1312,.2329,.2348,.4047,.1696,.4135,.1451,.8828, .5569,  &
            .8307,.3522,.8732,.0124,.3736,.2883,.5743,   &
            .8283,.1004,.3047,.1091,.5886,.9991,.6650,.0381/
    !c  Data for Hartman function coefficient (3-D)
    data a3 /3.,.1,3.,.1,10.,10.,10.,10.,30.,35.,30.,35./
    data c3 /1.,1.2,3.,3.2/
    data p3 /.3689,.4699,.1091,.03815, .1170,.4387,.8732,.5743,  &
            .2673,.7470,.5547,.8828/

    !c  IF PARAMETER IS OUTSIDE BOUND, ASSIGN bigm to FUNCTION VALUE
    do 1 j = 1, nopt
        if (x(j) .lt. bl(loc(j)) .or. x(j) .gt. bu(loc(j))) then
            functOthers = bigm
            return
        end if
    1 continue
    !c  CHECK nopt AND PASS x(1) and x(2) TO x1 AND x2 FOR ifunc = 1, 4
    if (idfunc .ge. 1 .and. idfunc .le. 4) then
        if (nopt .ne. 2) then
            write(*,*) ' Error in nopt'
            stop
        end if
        x1 = x(1)
        x2 = x(2)
    end if

    !c  This is the Goldstein-Price Function G:3,(0,-1)
    !c  (True G:3.0,(0.0,-1.0)
    if (idfunc .eq. 1) then
        u1 = (x1 + x2 + 1.0)**2
        u2 = 19. - 14.*x1 + 3.*x1**2 - 14.*x2 + 6.*x1*x2 +3.*x2**2
        u3 = (2.*x1 - 3.*x2)**2
        u4 = 18. - 32.*x1 + 12.*x1**2 + 48.*x2 -36.*x1*x2 + 27.*x2**2
        u5 = u1 * u2
        u6 = u3 * u4
        functOthers = (1. + u5) * (30. + u6)
        functOthers = functOthers - 3.0
    end if
    !c  This is the Rosenbrock Function with Parameter (a) G:0,(1,1)
    if(idfunc .eq. 2) then
        a = 100.0
        functOthers = a * (x2 - x1**2)**2 + (1 - x1)**2
    end if
    !c  This is the Six-hump Camelback function.
    !c  (True optima = (-0.08983,0.7126), (0.08983,-0.7126)
    if (idfunc .eq. 3) then
        functOthers = (4. - 2.1*x1**2 + x1**4/3.)*x1**2 + x1*x2 +  &
                (-4. + 4.*x2**2)*x2**2
        functOthers = functOthers + 1.031628453489877
    end if
    !c  This is the Rastrigin function G:-2,(0,0)
    if (idfunc .eq. 4) then
        functOthers = x1**2 + x2**2 - cos(18.0*x1) - cos(18.0*x2)
        functOthers = 2.0 + functOthers
    end if
    !c  This is the Griewank function G:0,(0,0), 2-D or 10-D
    if (idfunc .eq. 5) then
        if (nopt .eq. 2) then
            d = 200.0
        else
            d = 4000.0
        end if
        u1 = 0.0
        u2 = 1.0
        do 10 j = 1, nopt
            u1 = u1 + x(j)**2 / d
            u2 = u2 * cos(x(j)/sqrt(dble(j)))
        10  continue
        functOthers = u1 -u2 + 1
    end if
    !c  This is the Shekel function G:-10.5364098252,(4,4), 4-D
    if (idfunc .eq. 6) then
        functOthers = 11.0309996713 - 0.494589854607
        do 30 i = 1, 10
            u = 0.0
            do 20 j = 1, nopt
                u = u + (x(j) - a1(i,j))**2
            20    continue
            u = 1.0 / (u + c1(i))
            functOthers = functOthers - u
        30  continue
    end if
    !c   This is the Hartman function
    !c   G:-3.322368011415515,(0.201,0.150,0.477,0.275,0.311,0.657)
    if (idfunc .eq. 7) then
        functOthers = 3.322368011415515
        do 50 i = 1, 4
            u = 0.0
            do 40 j = 1, nopt
                if (nopt == 3) then
                    a2(i,j) = a3(i,j)
                    p2(i,j) = p3(i,j)
                end if
                u = u + a2(i,j) * (x(j) - p2(i,j))**2
            40    continue
            if (nopt .eq. 3) c2(i) = c3(i)
            functOthers = functOthers - c2(i) * exp(-u)
        50  continue
    end if

    !c  END OF FUNCTION functOthers
    return
end
!########################################################
!cccc  ------- other function conditions

subroutine otherf(a,n,nopt,idfunc,loc,bl,bu)
    ! 	      implicit real*8 (a-h,o-z)
    implicit none
    integer n,nopt,idfunc
    double precision a,bl,bu
    dimension a(n),bl(50), bu(50)

    integer loc
    dimension loc(50)

    integer i

    do i=1,50
        loc(i)=i
    end do
    if(idfunc==1) then
        nopt=2
        a(1)=0
        a(2)=0
        bl(1)=-2
        bu(1)=2
        bl(2)=-2
        bu(2)=2
    else if(idfunc.eq.2) then
        nopt=2
        a(1)=0
        a(2)=0
        bl(1)=-5
        bu(1)=5
        bl(2)=-2
        bu(2)=8
    else if(idfunc==3) then
        nopt=2
        a(1)=0
        a(2)=0
        bl(1)=-5
        bu(1)=5
        bl(2)=-5
        bu(2)=5
    else if(idfunc.eq.4) then
        nopt=2
        a(1)=0
        a(2)=0
        bl(1)=-1
        bu(1)=1
        bl(2)=-1
        bu(2)=1
    else if(idfunc==5) then
        nopt=10
        do 10 i=1,nopt
            a(i)=0
            bl(i)=-600
            bu(i)=600
        10  continue
    else if(idfunc.eq.6) then
        nopt=4
        do 11 i=1,nopt
            a(i)=5
            bl(i)=0
            bu(i)=10
        11  continue
    else if(idfunc.eq.7) then
        nopt=6
        do 12 i=1,nopt
            a(i)=0.5
            bl(i)=0
            bu(i)=1
        12  continue
    end if

end
!########################################################
!c
!c  ========  SUBROUTINE EX1  ========
!c
subroutine ex1(par,n)
    !c******************************************************************
    !c  This is the execution subroutine for the Sacramento
    !c  soil moisture accounting operation as modified for research
    !c  on the University of Arizona campus.  No derivative computations
    !c  are included in this subroutine.  Last revised 4/25/88 wtw.
    !c******************************************************************
    !c
    implicit real*8 (a-h,o-z)
    real*8 lztwc,lzfsc,lzfpc,lztwm,lzfpm,lzfsm,lzpk,lzsk


    common /fsums1/ srot,simpvt,srodt,srost,sintft,sgwfp,sgwfs,  &
            srecht,sett,se1,se3,se4,se5

    parameter(ijk=6000,ldt=24)
    common /block1/ ndata,ns,iobj,nday,nord,ndt
    common /block2/px(ijk,ldt),pet(ijk),qobs(ijk),uhg(50),state(20)
    common /block3/ smq(ijk),sqin(ijk*ldt),tci(ijk*ldt)
    common /block4/ npar,loc(50)
    !       common /block5/ pt(50),idfunc
    !       common /idid/iduh,area
    common /incntnt/ uztwc,uzfwc,lztwc,lzfsc,lzfpc,adimc
    dimension par(n)
    common /pval/ uztwm,uzfwm,uzk,pctim,adimp,riva,zperc,rexp,   &
            lztwm,lzfsm,lzfpm,lzsk,lzpk,pfree,rserv,side

    !c  Evapotranspiration distribution (EPDIST) is uniform each day.

    epdist = 1.0/ndt

    uztwm=par(1)
    uzfwm=par(2)
    lztwm=par(3)
    lzfpm=par(4)
    lzfsm=par(5)
    adimp=par(6)
    uzk=par(7)
    lzpk=par(8)
    lzsk=par(9)
    pctim=par(10)
    zperc=par(11)
    rexp=par(12)
    pfree=par(13)
    riva=par(14)
    rserv=par(15)
    side=par(16)


    !c
    !c  Initialize sums and carryover.
    uztwc = state(1)
    uzfwc = state(2)
    lztwc = state(3)
    lzfsc = state(4)
    lzfpc = state(5)
    adimc = state(6)
    if (uztwc .gt. uztwm) uztwc = uztwm
    if (uzfwc .gt. uzfwm) uzfwc = uzfwm
    if (lztwc .gt. lztwm) lztwc = lztwm
    if (lzfsc .gt. lzfsm) lzfsc = lzfsm
    if (lzfpc .gt. lzfpm) lzfpc = lzfpm
    !c      if (adimc .gt. adimp) adimc = adimp
    !c
    srot = 0.
    simpvt = 0.
    srodt = 0.
    srost = 0.
    sintft = 0.
    sgwfp = 0.
    sgwfs = 0.
    srecht = 0.
    sett = 0.
    sprt = 0.
    spet = 0.
    se1 = 0.
    se3 = 0.
    se4 = 0.
    se5 = 0.
    sro = 0.
    srech = 0.
    set = 0.
    spr = 0.
    !c
    !c  DT is the length of each time interval in days.
    dt = 1.0/ndt
    !c
    !c  Begin day and hour loops for ET and PXV calculations.
    do 105 i = 1, nday
        ep = pet(i)
        !c        spet = spet + ep
        do 100 j = 1, ndt
            pxv = px(i,j)
            !c  Perform soil moisture accounting operations.
            !c          sprt = sprt + pxv

            call fland1(dt, pxv, ep, epdist, tlci)

            tci((i - 1) * ndt + j) = tlci
        100 continue
    105 continue
    return
end
!########################################################
!c
!c  ========  SUBROUTINE EX2  ========
!c
subroutine ex2 !(area)
    !c***************************************************************
    !c  This is a revised subroutine ex2.  It converts total channel
    !c  inflow to simulated instantaneous streamflow in cms, then
    !c  converts simulated instantaneous streamflow to simulated
    !c  mean daily streamflow in cms.  This subroutine combines
    !c  the functions of subroutines ex2 and ex6 of the NWSRFS
    !c  model as modified for research on the University of
    !c  Arizona campus.  Last revised 4/25/88 wtw.
    !c***************************************************************
    !c
    implicit real*8 (a-h,o-z)
    ! 	double precision area
    parameter(ijk=6000,ldt=24)
    common /block1/ ndata,ns,iobj,nday,nord,ndt
    common /block2/px(ijk,ldt),pet(ijk),qobs(ijk),uhg(50),state(20)
    common /block3/ smq(ijk),sqin(ijk*ldt),tci(ijk*ldt)
    common /block4/ npar,loc(50)
    !       common /block5/ pt(50),idfunc
    common /routing/ckm,cxm,nm
    !common /idid/iduh,area

    !c
    !c  Convert total channel inflow (TCI, in mm), to simulated
    !c  instantaneous streamflow (SQIN, in cms).
    !c
    if(iduh.eq.1) then
        do 205 i = 1, nday * ndt
            sqin(i) = 0.
            nno = nord
            if (i .lt. nord) nno = i
            do 200 j = 1, nno
                sqin(i) = sqin(i) + (tci(i + 1 - j) * uhg(j))
            200   continue
        205 continue
        !c	else if(iduh.eq.2)then
        !c	    call msk(k,x,n,dt)
        !c
        !c	else if(iduh.eq.3) then
        !c	    call kinm()
    else
        do i=1,nday*ndt
            sqin(i)=tci(i)*area/(3.6*24/ndt)
        end do
    end if

    !c
    !c  Convert simulated instantaneous streamflow to simulated mean
    !c  daily streamflow (SMQ, in cms).
    !c
    do 215 i = 1, nday
        if (i .eq. 1) then
            smq(i)=0.0
            do 216 j=1,ndt-1
                smq(i) = smq(i)+sqin(j)
            216   continue
            smq(i)=smq(i)/ndt+sqin(ndt)/(2.0*ndt)
        end if

        if (i .ne. 1) then
            j = ndt * (i - 1)
            smq(i) = (sqin(j) + sqin(j + ndt)) /(2.0*ndt)
            do 210 k = 1, ndt-1
                smq(i) = smq(i) + (sqin(j + k) / ndt)
            210   continue
        end if
    215 continue
    return
end
!########################################################
!c
!c  ========  SUBROUTINE FLAND1  ========
!c
subroutine fland1(dt, pxv, ep, epdist, tci)
    !c.......................................
    !c  THIS SUBROUTINE EXECUTES THE 'SAC-SMA ' OPERATION FOR ONE TIME
    !c      PERIOD.
    !c.......................................
    !c  SUBROUTINE INITIALLY WRITTEN BY. . .
    !c      ERIC ANDERSON - HRL     APRIL 1979     VERSION 1
    !c  MODIFIED FEB 1988 FOR RESEARCH ON THE U OF A CAMPUS
    !c  Last revised 4/25/88 wtw.
    !c............................................................
    implicit real*8 (a-h,o-z)

    real*8 lztwc,lzfsc,lzfpc,lztwm,lzfpm,lzfsm,lzpk,lzsk

    dimension rsum(7)
    common /incntnt/ uztwc,uzfwc,lztwc,lzfsc,lzfpc,adimc
    common /fsums1/ srot,simpvt,srodt,srost,sintft,sgwfp,sgwfs,  &
            srecht,sett,se1,se3,se4,se5
    common /pval/ uztwm,uzfwm,uzk,pctim,adimp,riva,zperc,rexp,  &
            lztwm,lzfsm,lzfpm,lzsk,lzpk,pfree,rserv,side

    !c.......................................
    !c
    !c  REPARAMETERIZATION OPTION
    !c     izflag=2, reparameterized percolation,
    !c               otherwise, original percolation
    !c     izflag = 1
    zp = zperc
    !c     if (izflag .eq. 2) aperc = zperc
    !c
    !c  COMPUTE EVAPOTRANSPIRATION LOSS FOR THE TIME INTERVAL.
    !c      EDMND IS THE ET-DEMAND FOR THE TIME INTERVAL
    !c
    !c  COMPUTE ET FROM UPPER ZONE.
    edmnd = ep * epdist
    !c  RED IS RESIDUAL EVAP DEMAND
    e1 = edmnd * uztwc / uztwm
    red = edmnd - e1
    uztwc = uztwc - e1
    if (abs(uztwc) .lt. 0.00001) uztwc = 0.0
    e2 = 0.0
    if (uztwc .ge. 0.) goto 220
    !c  E1 CAN NOT EXCEED UZTWC
    e1 = e1 + uztwc
    uztwc = 0.0
    red = edmnd - e1
    if (uzfwc .ge. red) goto 221
    !c  E2 IS EVAP FROM UZFWC.
    e2 = uzfwc
    uzfwc = 0.0
    red = red - e2
    goto 225
    221 e2 = red
    uzfwc = uzfwc - e2
    red = 0.0
    220 if ((uztwc / uztwm) .ge. (uzfwc / uzfwm)) goto 225
    !c  UPPER ZONE FREE WATER RATIO EXCEEDS UPPER ZONE
    !c  TENSION WATER RATIO, THUS TRANSFER FREE WATER TO TENSION
    uzrat = (uztwc + uzfwc) / (uztwm + uzfwm)
    uztwc = uztwm * uzrat
    uzfwc = uzfwm * uzrat
    !c
    !c  COMPUTE ET FROM THE LOWER ZONE.
    !c  COMPUTE ET FROM LZTWC (E3)
    225 e3 = red * lztwc / (uztwm + lztwm)
    lztwc = lztwc - e3
    if (abs(lztwc) .lt. 0.00001) lztwc = 0.0
    if (lztwc .ge. 0.0) goto 226
    !c  E3 CAN NOT EXCEED LZTWC
    e3 = e3 + lztwc
    lztwc = 0.0
    226 ratlzt = lztwc / lztwm
    saved = rserv * (lzfpm + lzfsm)

    ratlz = (lztwc+lzfpc+lzfsc-saved) / (lztwm+lzfpm+lzfsm-saved)
    if (ratlzt .ge. ratlz) goto 230
    !c  RESUPPLY LOWER ZONE TENSION WATER FROM LOWER
    !c  ZONE FREE WATER IF MORE WATER AVAILABLE THERE.
    del = (ratlz - ratlzt) * lztwm
    !c  TRANSFER FROM LZFSC TO LZTWC.
    lztwc = lztwc + del
    lzfsc = lzfsc - del
    if (lzfsc .ge. 0.0) goto 230
    !c  IF TRANSFER EXCEEDS LZFSC THEN REMAINDER COMES FROM LZFPC
    lzfpc = lzfpc + lzfsc
    lzfsc = 0.0
    !c
    !c  COMPUTE ET FROM ADIMP AREA.-E5
    230 e5 = e1 + (red+e2) * (adimc-e1-uztwc) / (uztwm+lztwm)
    !c  ADJUST ADIMC,ADDITIONAL IMPERVIOUS AREA STORAGE, FOR EVAPORATION.
    adimc = adimc - e5
    if (abs(adimc) .lt. 0.00001) adimc = 0.0
    if (adimc .ge. 0.0) goto 231
    !c  E5 CAN NOT EXCEED ADIMC.
    e5 = e5 + adimc
    adimc = 0.0
    231 e5 = e5 * adimp
    !c  E5 IS ET FROM THE AREA ADIMP.
    !c.......................................
    !c  COMPUTE PERCOLATION AND RUNOFF AMOUNTS.
    twx = pxv + uztwc - uztwm
    !c  TWX IS THE TIME INTERVAL AVAILABLE MOISTURE IN EXCESS
    !c  OF UZTW REQUIREMENTS.
    if (twx .ge. 0.0) goto 232
    !c  ALL MOISTURE HELD IN UZTW--NO EXCESS.
    uztwc = uztwc + pxv
    twx = 0.0
    goto 233
    !c  MOISTURE AVAILABLE IN EXCESS OF UZTW STORAGE.
    232 uztwc = uztwm
    233 adimc = adimc + pxv - twx
    !c
    !c  COMPUTE IMPERVIOUS AREA RUNOFF.
    roimp = pxv * pctim
    !c  ROIMP IS RUNOFF FROM THE MINIMUM IMPERVIOUS AREA.
    simpvt = simpvt + roimp
    !c
    !c  INITIALIZE TIME INTERVAL SUMS.
    sbf = 0.0
    ssur = 0.0
    sif = 0.0
    sperc = 0.0
    sdro = 0.0
    spbf = 0.0
    !c
    !c  DETERMINE COMPUTATIONAL TIME INCREMENTS FOR THE BASIC TIME
    !c  INTERVAL
    !c     ninc = 1.0 + 0.2 * uzfwc + twx
    ninc = 1.0 + 0.2 * (uzfwc + twx)
    !c  NINC=NUMBER OF TIME INCREMENTS THAT THE TIME INTERVAL
    !c  IS DIVIDED INTO FOR FURTHER
    !c  SOIL-MOISTURE ACCOUNTING.  NO ONE INCREMENT
    !c  WILL EXCEED 5.0 MILLIMETERS OF UZFWC+PAV

    dinc = 1.0 / ninc * dt
    !c  DINC=LENGTH OF EACH INCREMENT IN DAYS.
    pinc = twx / ninc
    !c  PINC=AMOUNT OF AVAILABLE MOISTURE FOR EACH INCREMENT.
    !c  COMPUTE FREE WATER DEPLETION FRACTIONS FOR
    !c  THE TIME INCREMENT BEING USED-BASIC DEPLETIONS
    !c  ARE FOR ONE DAY
    if (uzk .gt. 1.d0) write(*,*)'uzk = ', uzk
    if (lzpk .gt. 1.d0) write(*,*)'lzpk = ', lzpk
    if (lzsk .gt. 1.d0) write(*,*)'lzsk = ', lzsk
    duz = 1.0 - (1.0 - uzk) ** dinc
    dlzp = 1.0 - (1.0 - lzpk) ** dinc
    dlzs = 1.0 - (1.0 - lzsk) ** dinc
    parea = 1.0 - adimp - pctim
    !c.......................................
    !c  START INCREMENTAL DO LOOP FOR THE TIME INTERVAL.
    !c.......................................
    do 240 i = 1, ninc
        adsur = 0.0
        !c  COMPUTE DIRECT RUNOFF (FROM ADIMP AREA).
        ratio = (adimc - uztwc) / lztwm
        addro = pinc * ratio ** 2
        sdro = sdro + addro * adimp
        !c  ADDRO IS THE AMOUNT OF DIRECT RUNOFF FROM
        !c  THE AREA ADIMP-SDRO IS THE SIX HOUR SUMMATION
        !c
        !c  COMPUTE BASEFLOW AND KEEP TRACK OF TIME INTERVAL SUM.
        bf = lzfpc * dlzp
        lzfpc = lzfpc - bf
        if (lzfpc .gt. 0.0001) goto 234
        bf = bf + lzfpc
        lzfpc = 0.0
        234 sbf = sbf + bf
        spbf = spbf + bf
        bf = lzfsc * dlzs
        lzfsc = lzfsc - bf
        if (lzfsc .gt. 0.0001) goto 235
        bf = bf + lzfsc
        lzfsc = 0.0
        235 sbf = sbf + bf
        !c
        !c  COMPUTE PERCOLATION-IF NO WATER AVAILABLE THEN SKIP
        if ((pinc + uzfwc) .gt. 0.01) goto 251
        uzfwc = uzfwc + pinc
        goto 249
        251 percm = lzfpm * dlzp + lzfsm * dlzs
        !c     if (izflag .eq. 2) zp = (uzfwm - percm) / (percm * aperc**rexp)
        if (zp .lt. 0.0) zp = 0.0
        perc = percm * uzfwc / uzfwm

        defr = 1.0 - (lztwc+lzfpc+lzfsc) / (lztwm+lzfpm+lzfsm)

        if (defr .lt. 0.d0) then
            write(*,*)'defr = ', defr
            write(*,*) lztwc, lzfpc, lzfsc
            write(*,*) lztwm, lzfpm, lzfsm
            stop
        end if
        !c  DEFR IS THE LOWER ZONE MOISTURE DEFICIENCY RATIO
        uzdefr = 1.0 - (uztwc + uzfwc) / (uztwm + uzfwm)
        239 perc = perc * (1.0 + zp * defr ** rexp)
        !c  NOTE...PERCOLATION OCCURS FROM UZFWC BEFORE PAV IS ADDED.
        if (perc .lt. uzfwc) goto 241
        !c  PERCOLATION RATE EXCEEDS UZFWC.
        perc = uzfwc
        !c  PERCOLATION RATE IS LESS THAN UZFWC.
        241 uzfwc = uzfwc - perc
        !c  CHECK TO SEE IF PERCOLATION EXCEEDS LOWER ZONE DEFICIENCY.
        check = lztwc+lzfpc+lzfsc+perc-lztwm-lzfpm-lzfsm
        if (check .le. 0.0) goto 242
        perc = perc - check
        uzfwc = uzfwc + check
        242 sperc = sperc + perc
        !c  SPERC IS THE TIME INTERVAL SUMMATION OF PERC
        !c
        !c  COMPUTE INTERFLOW AND KEEP TRACK OF TIME INTERVAL SUM.
        !c  NOTE...PINC HAS NOT YET BEEN ADDED
        del = uzfwc * duz
        sif = sif + del
        uzfwc = uzfwc - del
        !c  DISTRIBE PERCOLATED WATER INTO THE LOWER ZONES
        !c  TENSION WATER MUST BE FILLED FIRST EXCEPT FOR THE PFREE AREA.
        !c  PERCT IS PERCOLATION TO TENSION WATER AND PERCF IS PERCOLATION
        !c      GOING TO FREE WATER.
        perct = perc * (1.0 - pfree)
        if ((perct + lztwc) .gt. lztwm) goto 243
        lztwc = lztwc + perct
        percf = 0.0
        goto 244
        243 percf = perct + lztwc - lztwm
        lztwc = lztwm
        !c
        !c  DISTRIBUTE PERCOLATION IN EXCESS OF TENSION
        !c  REQUIREMENTS AMONG THE FREE WATER STORAGES.
        244 percf = percf + (perc * pfree)
        if (percf .eq. 0.0) goto 245
        hpl = lzfpm / (lzfpm + lzfsm)
        !c  HPL IS THE RELATIVE SIZE OF THE PRIMARY STORAGE
        !c  AS COMPARED WITH TOTAL LOWER ZONE FREE WATER STORAGE.
        ratlp = lzfpc / lzfpm
        ratls = lzfsc / lzfsm
        !c  RATLP AND RATLS ARE CONTENT TO CAPACITY RATIOS, OR
        !c  IN OTHER WORDS, THE RELATIVE FULLNESS OF EACH STORAGE

        fracp = hpl * 2.0 * (1.0-ratlp) / (1.0-ratlp+1.0-ratls)
        !c  FRACP IS THE FRACTION GOING TO PRIMARY.
        if (fracp .gt. 1.0) fracp = 1.0
        percp = percf * fracp
        percs = percf - percp
        !c  PERCP AND PERCS ARE THE AMOUNT OF THE EXCESS
        !c  PERCOLATION GOING TO PRIMARY AND SUPPLEMENTAL
        !c  STORGES,RESPECTIVELY.
        lzfsc = lzfsc + percs
        if (lzfsc .le. lzfsm) goto 246
        percs = percs - lzfsc + lzfsm
        lzfsc = lzfsm
        246 lzfpc = lzfpc + percf - percs
        !c  CHECK TO MAKE SURE LZFPC DOES NOT EXCEED LZFPM.
        if (lzfpc .le. lzfpm) goto 245
        excess = lzfpc - lzfpm
        lztwc = lztwc + excess
        lzfpc = lzfpm
        !c
        !c  DISTRIBUTE PINC BETWEEN UZFWC AND SURFACE RUNOFF.
        245 if (pinc .eq. 0.0) goto 249
        !c  CHECK IF PINC EXCEEDS UZFWM
        if ((pinc + uzfwc) .gt. uzfwm) goto 248
        !c  NO SURFACE RUNOFF
        uzfwc = uzfwc + pinc
        goto 249
        !c
        !c  COMPUTE SURFACE RUNOFF (SUR) AND KEEP TRACK OF TIME INTERVAL SUM.
        248 sur = pinc + uzfwc - uzfwm
        uzfwc = uzfwm
        ssur = ssur + (sur * parea)

        adsur = sur * (1.0 - addro / pinc)
        !c  ADSUR IS THE AMOUNT OF SURFACE RUNOFF WHICH COMES
        !c  FROM THAT PORTION OF ADIMP WHICH IS NOT
        !c  CURRENTLY GENERATING DIRECT RUNOFF.  ADDRO/PINC
        !c  IS THE FRACTION OF ADIMP CURRENTLY GENERATING
        !c  DIRECT RUNOFF.
        ssur = ssur + adsur * adimp
        249 adimc = adimc + pinc - addro - adsur
        if (adimc .le. (uztwm + lztwm)) goto 240
        addro = (addro + adimc) - (uztwm + lztwm)
        adimc = uztwm + lztwm
    240 continue
    !c.......................................
    !c  END OF INCREMENTAL DO LOOP.
    !c.......................................
    !c  COMPUTE SUMS AND ADJUST RUNOFF AMOUNTS BY THE AREA OVER
    !c  WHICH THEY ARE GENERATED.
    eused = e1 + e2 + e3
    !c  EUSED IS THE ET FROM PAREA WHICH IS 1.0-ADIMP-PCTIM
    sif = sif * parea
    !c
    !c  SEPARATE CHANNEL COMPONENT OF BASEFLOW
    !c  FROM THE NON-CHANNEL COMPONENT
    tbf = sbf * parea
    !c  TBF IS TOTAL BASEFLOW

    bfcc = tbf * (1.0 / (1.0 + side))
    !c  BFCC IS BASEFLOW, CHANNEL COMPONENT
    bfp = (spbf * parea) / (1.0 + side)
    bfs = bfcc - bfp
    if (bfs .lt. 0.0) bfs = 0.0
    bfncc = tbf - bfcc
    !c  BFNCC IS BASEFLOW,NON-CHANNEL COMPONENT
    !c
    !c  ADD TO MONTHLY SUMS.
    sintft = sintft + sif
    sgwfp = sgwfp + bfp
    sgwfs = sgwfs + bfs
    srecht = srecht + bfncc
    srost = srost + ssur
    srodt = srodt + sdro
    !c
    !c  COMPUTE TOTAL CHANNEL INFLOW FOR THE TIME INTERVAL.
    tci = roimp + sdro + ssur + sif + bfcc
    !c
    !c  COMPUTE E4-ET FROM RIPARIAN VEGETATION.
    e4 = (edmnd - eused) * riva
    !c
    !c  SUBTRACT E4 FROM CHANNEL INFLOW
    tci = tci - e4
    if (tci .ge. 0.0) goto 250
    e4 = e4 + tci
    tci = 0.0
    250 srot = srot + tci
    !c
    !c  COMPUTE TOTAL EVAPOTRANSPIRATION-TET
    eused = eused * parea
    tet = eused + e5 + e4
    sett = sett + tet
    se1 = se1 + e1 * parea
    se3 = se3 + e3 * parea
    se4 = se4 + e4
    se5 = se5 + e5
    !c  CHECK THAT ADIMC IS GT UZTWC
    !c
    if (adimc .lt. uztwc) adimc = uztwc
    !c
    !c  ADD TO SUMS OF RUNOFF COMPONENTS.

    !c.......................................
    return
end
!ccccc
!########################################################
!c==================================================================
real*8 function functnsc(nopt,x)
    !c
    !c
    !c  THIS IS THE SIXPAR MODEL
    !c  Bound: PAR(1)=UM=[0,50], PAR(2)=UK=[0,1], PAR(3)=BM=[0,50],
    !c         PAR(4)=BK=[0,1],  PAR(5)=A=[0,1],  PAR(6)=X=[0,10]
    !c  Global Optimum: 0, [10, 0.5, 20, 0.2, 0.31, 3]
    !c
    implicit real*8 (a-h,o-z)
    parameter(ijk=6000,ldt=24,bigm = 1.0d10)
    common /block1/ ndata,ns,iobj,nday,nord,ndt
    common /block2/px(ijk,ldt),pet(ijk),qobs(ijk),uhg(50),state(20)
    common /block3/ smq(ijk),sqin(ijk*ldt),tci(ijk*ldt)
    common /block4/ npar,loc(50)
    common /block5/ pt(50),idfunc
    common /iopar/ ipr
    common /ablu/  bl(50), bu(50)
    dimension x(nopt), par(50)

    !c  IF PARAMETER IS OUTSIDE BOUND, ASSIGN bigm to FUNCTION VALUE
    do 1 j = 1, nopt
        if (x(j) .lt. bl(loc(j)) .or. x(j) .gt. bu(loc(j))) then
            functnsc = bigm
            return
        end if
    1 continue
    !c
    if (nopt .gt. 16) then
        write (*,*) ' ???? CHECK THE INPUT FILE ???? '
        stop
    end if
    !c
    !c  ASSIGN x TO APPROPRIATE PARAMETERS IN SIXPAR MODEL
    do 10 j = 1, npar
        par(j) = pt(j)
    10 continue
    do 20 k = 1, nopt
        par(loc(k)) = x(k)
    20 continue


    !c
    !c  COMPUTE THE STREAMFLOW FOR PARAMETER SET par
    call model(par,npar)
    !c
    !c  COMPUTE THE SIMPLE LEAST SQUARE OBJECTIVE FUNCTION VALUE
    if (iobj .eq. 1) then
        functnsc = 0.d0
        do 40 i = 1, ndata
            functnsc = functnsc + (qobs(i) - smq(i))**2
        40  continue
    end if
    !c
    !c  COMPUTE THE HMLE VALUE
    if (iobj .eq. 2) functnsc = hmle()
    !c
    !c  END OF FUNCTION functnsc
    return
end
!c
!c
!c
!########################################################
!c=======================================================================
real*8 function hmle()
    implicit real*8 (a-h,o-z)

    common /fnblk/ rlamda, ad
    parameter(ijk=6000,ldt=24)
    common /block1/ ndata,ns,iobj,nday,nord,ndt
    common /block2/px(ijk,ldt),pet(ijk),qobs(ijk),uhg(50),state(20)
    common /block3/ smq(ijk),sqin(ijk*ldt),tci(ijk*ldt)
    common /block4/ npar,loc(50)
    common /block5/ pt(50),idfunc
    common /iopar/ ipr

    dimension ra(2)
    parameter (eps=5.d-02, del=5.d-02)
    data iflag /0/
    !c
    !c  COMPUTE THE MEAN OF LOGARITHM OF OBSERVED FLOWS
    if (iflag .eq. 0) then
        ad = 0.d0
        do 10 i = 1, ndata
            ad = ad + dlog(qobs(i))
        10  continue
        ad = ad / dble(ndata)
        rlamda = 1.d0
        iflag = 1
    end if
    !c
    !c  ESTIMATE THE LAMDA VALUE
    lcount = 0
    ict = 1
    ra(1) = 0.d0
    ra(2) = 0.d0
    25 continue
    lcount = lcount + 1
    if(lcount .gt. 40) then
        write(*,*) 'LAMDA ITERATION GO OVER 40', rlamda, ra(1), ra(2)
        go to 50
    end if
    rd = 0.d0
    rn = 0.d0
    do 30 i = 1, ndata
        a = dlog(qobs(i)) / ad
        w = qobs(i)**(2*(rlamda-1.d0))
        rd = rd + w*(qobs(i) - smq(i))**2
        rn = rn + w*(qobs(i) - smq(i))**2 * a
    30 continue
    ra(ict) = rn / rd - 1.d0
    if (dabs(ra(ict)) .le. eps) go to 50
    isign = -1
    if (ra(ict) .lt. 0.d0) isign = 1
    rlamda = rlamda + isign * del
    if (ict .eq. 2) go to 35
    ict = 2
    go to 25
    !c
    35 continue
    if (ra(1)*ra(2) .lt. 0.d0) go to 40
    ra(1) = ra(2)
    go to 25
    !c
    40 continue
    rlamda = rlamda - isign * del / 2.d0
    !c
    !c  COMPUTE HMLE
    50 continue
    hmle = 0.d0
    ex = 2. * (rlamda - 1.)
    do 60 i = 1, ndata
        hmle = hmle + qobs(i)**ex * (qobs(i) -smq(i))**2
    60 continue
    hmle = hmle / dble(ndata)
    hmle = hmle / dexp(ex * ad)
    !c
    return
end
!c
!c
!c
!c=======================================================================
!########################################################
!c  ========  SUBROUTINE MODEL  ========
!c
subroutine model(par,n)
    !c**********************************************************************
    !c  This is the execution controller for the SAC-SMA model.  It controls
    !c  operation of the streamflow simulation model for each set of
    !c  parameter values.  Parameter values are set in the NWS main program
    !c  if no parameter estimation is performed. Selected parameter values
    !c  are varied in the OARSCH subroutine if parameter estimation is
    !c  performed.  Last revised 2/1/89 wtw.
    !c**********************************************************************
    !c
    implicit real*8 (a-h,o-z)

    parameter(ijk=6000,ldt=24)
    common /block1/ ndata,ns,iobj,nday,nord,ndt
    common /block2/px(ijk,ldt),pet(ijk),qobs(ijk),uhg(50),state(20)
    common /block3/ smq(ijk),sqin(ijk*ldt),tci(ijk*ldt)
    common /block4/ npar,loc(50)
    common /block5/ pt(50),idfunc
    !  common /idid/iduh,area
    dimension par(n)

    !c  Calculate total channel inflow (TCI) in mm.

    call ex1(par,n)

    continue
    !c
    !c  Convert channel inflow in mm to simulated instantaneous streamflow
    !c  (SQIN)  and simulated mean daily streamflow (SMQ) in cms
    !c      call ex2(tci, uhg, sqin, smq)

    call ex2 !(area)
    continue
    return
end
!########################################################
subroutine sxpinp(nopt,a,sPath)
    !c
    !c
    !c  THIS SUBROUTINE READS THE INPUT DATA FOR SIXPAR MODEL
    !c
    !c  SIXPAR MODEL VARIABLES AND PARAMETERS
    !c       ndata = number of data points
    !c       idata = flag on what data set to use for calibration
    !c             = 0, use existing data
    !c             = 1, generate data within the program
    !c       iobj = flag on which objective function to use
    !c             = 1, simple least square (SLS)
    !c             = 2, heteroscedastic maximum likelihood estimator (HMLE)
    !c       pt(i) = true value for ith parameter, i=1,2,...,6
    !c       loc(i) = index on the order of parameters to be optimized
    !c       ns = number of state variables
    !c       p(.) = precipitation data
    !c       obsq(.) = observed streamflow data
    !c       s0(.) = initial states
    !c
    implicit real*8 (a-h,o-z)

    parameter(ijk=6000,ldt=24)
    common /block1/ ndata,ns,iobj,nday,nord,ndt
    common /block2/px(ijk,ldt),pet(ijk),qobs(ijk),uhg(50),state(20)
    common /block3/ smq(ijk),sqin(ijk*ldt),tci(ijk*ldt)
    common /block4/ npar,loc(50)
    common /block5/ pt(50),idfunc
    !common /idid/iduh,area
    common /iopar/ ipr
    integer in
    character*4 name(50) !character*3 name(50)
    character*4 objty,obj1,obj2
    character*9 dataty,data1,data2
    data obj1/'SLS '/
    data obj2/'HMLE'/
    data data1/'OBSERVED '/
    data data2/'SYNTHETIC'/
    data name /'  X1','  X2','  X3','  X4','  X5','  X6','  X7',&
            '  X8','  X9',' X10',' X11',' X12',' X13',' X14',' X15',' X16',&
            '  X8','  X9',' X10',' X11',' X12',' X13',' X14',' X15',' X25', &
            '  X8','  X9',' X10',' X11',' X12',' X13',' X14',' X15',' X34', &
            '  X8','  X9',' X10',' X11',' X12',' X13',' X14',' X15',' X43', &
            '  X8','  X9',' X10',' X11',' X12',' X13',' X50'/
    dimension a(50)
    character*(*)sPath

    write (*,*) ' ENTER SUBROUTINE SXPINP --- '

    write(ipr,800)
    800 format(//,10x,'READ AND WRITE THE INPUT INFORMATION FOR SIXPAR', &
    ' MODEL',/,10x,53(1h=))

    in=7

    open(in, file=sPath // 'sxpinp.dat')

    read(in,*) npar,ns,iobj,idata,ndt

    do i=1,nopt
        pt(i)=a(i)
    end do

    read(in,*) (pt(j),j=nopt+1,npar)

    read(in,*)iduh,area

    read(in,*) (state(j),j=1,ns)

    !   routing by UH
    if (iduh.eq.1) then
        read(in,*)nord
        read(in,*)(uhg(i),i=1,nord)
        !c   By MushK---
    elseif (iduh.eq.2) then
        read(in,*)ckm,cxm,nm
        !c   by Kinematic method----
    elseif(iduh.eq.3) then
        read(in,*)c
    end if

    !c     READ THE PRE-SELECTED DATA SET

    i=0
    777 i=i+1
    read(in,*,end=778) iid,iim,iiy,qobs(i),pet(i),(px(i,j),j=1,ndt)

    !c        qobs(i)=qobs(i)*24*3.6/area  iid,

    go to 777
    778 ndata=i-1


    !c   50   format(i5,2f10.4)
    100 continue

    nday=ndata

    !c  IF idata IS EQUAL TO 1, GENERATE ERROR-FREE SYNTHETIC DATA
    !c  BY CALLING THE SIXPAR MODEL USING PARAMETER SET pt
    if (idata .eq. 1) then
        ft = functn(nopt,pt,idfunc)
        do 200 i = 1, ndata
            qobs(i) = smq(i)
        200 continue
        dataty = data2
    else
        dataty = data1
    end if

    if (iobj .eq. 1) objty = obj1
    if (iobj .eq. 2) objty = obj2

    !c  PRINT THE INPUT INFORMATION FOR THE SIXPAR MODEL
    write(ipr,810) npar, ndata, objty, dataty
    810 format(//,2x,'PARAMETERS',4x,'DATA POINTS',4x,'CRITERION',4x,  &
    'DATA TYPE',/,2x,10(1h-),4x,11(1h-),4x,9(1h-),4x,9(1h-),  &
            /,6x,i2,10x,i4,12x,a4,6x,a9)
    write(ipr,820) (pt(j), j = 1, npar)
    820 format(//,2x,'TRUE PARAMETER VALUES:',//,5x, &
            'UM',8x,'UK',8x,'BM',8x,'BK',8x,'A ',8x,'X ',/,6(f8.2,2x))
    write(ipr,830) (name(loc(j)),j = 1, nopt)
    830 format(//,2x,'THE PARAMETERS TO BE OPTIMIZED:',//,1x,6(4x,a2,4x))
    !c
    !c  CLOSE THE SIXPAR MODEL INPUT FILE
    if(idfunc/=55)then
        close(in)
    end if
    !c
    !c  END OF SUBROUTINE SXPINP
    return
end
!########################################################
