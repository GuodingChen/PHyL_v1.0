

subroutine CREST_Main_Pre(bIsError)

    use CREST_Project
    use CREST_Basic
    use CREST_Param
    use CREST_ICS
    use SoilDownscale_Basic
    use Landslide_Basic

    implicit none

    integer :: IBDT(8), IEDT(8)
    integer :: k, i, j, ii,jj
    logical :: bIsError

    bIsError = .false.
    write(*,"(2X,A)")"Reading Project Data!"
    call ReadProjectFile(bIsError)



    if(bIsError .eqv. .true.)then
        write(*,"(1X,A)") &
                "*** Something wrong in your project file!"
        write(g_CREST_LogFileID,"(1X,A)") &
                "*** Something wrong in your project file!"
        return
    end if

    write(*,"(2X,A)")"Reading Basic Data!"
    call ReadBasicFile(bIsError)
    if(bIsError .eqv. .true.)then
        write(*,"(1X,A)") &
                "*** Something wrong in your basics file!"
        write(g_CREST_LogFileID,"(1X,A)")  &
                "*** Something wrong in your basics file!"
        return
    end if

    if(g_tOutlet%HasOutlet)then
        g_tOutlet%Mask=g_NoData_Value
        ii=g_tOutlet%Row
        jj=g_tOutlet%Col

        write(*,"(2X,A)")"Getting Mask Map of Outlet!"

        call GetMask(g_NCols,g_NRows,jj,ii,g_NoData_Value, &
                g_FDR,g_NextC,g_NextR,g_tOutlet%Mask,InBasin)

        write(*,"(2X,A)")"Writing Mask Map of Outlet to File!"

        call WriteMatrixFile_Int(trim(g_ResultPath)//"Outlet_" &
                //trim(g_tOutlet%Name)//"_Mask",   &
                g_tOutlet%Mask, g_NCols,g_NRows,     &
                g_XLLCorner,g_YLLCorner, g_CellSize, g_NoData_Value, &
                bIsError,g_BasicFormat)

    end if

    do k=0,g_NOutPixs-1
        g_tOutPix(k)%Mask=g_NoData_Value
        ii=g_tOutPix(k)%Row
        jj=g_tOutPix(k)%Col
        write(*,"(2X,A,I2,A)") &
                "Getting Mask Map of OutPix Num ",k+1," !"

        call GetMask(g_NCols,g_NRows,jj,ii,g_NoData_Value, &
                g_FDR,g_NextC,g_NextR,g_tOutPix(k)%Mask,InBasin)

        write(*,"(2X,A,I2,A)")  &
                "Writing Mask Map of OutPix Num ",k+1," to File!"

        call WriteMatrixFile_Int(trim(g_ResultPath)//"OutPix_"   &
                //trim(g_tOutPix(k)%Name)//"_Mask",   &
                g_tOutPix(k)%Mask, g_NCols,g_NRows,    &
                g_XLLCorner,g_YLLCorner, g_CellSize, g_NoData_Value, &
                bIsError,g_BasicFormat)

    end do

    write(*,"(2X,A)")"Reading Prarmeters' Data!"
    call ReadParamFile()
    write(*,"(2X,A)")"Reading Initial Conditions Data!"
    call ReadICSFile()

    select case (trim(g_sRunStyle))
    case ("CALI_SCEUA")
        write(*,"(2X,A)")"Reading calibration's Data!"
        call ReadCalib_SCEUAFile()

    end select

    write(*,*)
    return
end subroutine CREST_Main_Pre

! read the basic information of the project control file
subroutine ReadProjectFile(bIsError)
    use CREST_Project
    use CREST_Basic
    use SoilDownscale_Basic
    use Landslide_Basic

    implicit none

    integer :: error,i
    character(len=200):: sTemp
    character(len=200):: sTemp_ForNum
    logical	:: bOutPixColRow, bOutletColRow
    double precision:: dblTemp
    logical :: bIsError
    integer			 :: dtTemp(1:6)

    bIsError=.false.
    g_NCols = XXWreadLineInt(g_PrjNP,"NCols_Hydro", error)
    g_NRows = XXWReadLineInt(g_PrjNP,"NRows_Hydro", error)

    g_XLLCorner = XXWReadLineDbl(g_PrjNP,"XLLCorner_Hydro", error)
    g_YLLCorner = XXWReadLineDbl(g_PrjNP,"YLLCorner_Hydro", error)

    g_CellSize = XXWReadLineDbl(g_PrjNP,"CellSize_Hydro", error)

    g_NCols_Land = XXWreadLineInt(g_PrjNP,"NCols_Land", error)
    g_NRows_Land = XXWReadLineInt(g_PrjNP,"NRows_Land", error)

    g_XLLCorner_Land = XXWReadLineDbl(g_PrjNP,"XLLCorner_Land", error)
    g_YLLCorner_Land = XXWReadLineDbl(g_PrjNP,"YLLCorner_Land", error)

    g_CellSize_Land = XXWReadLineDbl(g_PrjNP,"CellSize_Land", error)


    g_NoData_Value = XXWReadLineDbl(g_PrjNP,"NoData_Value", error)


    ! get the Model map Coordinate System
    g_CS = XXWReadLineStr(g_PrjNP,"CoordinateSystem", error)
    call UPCASE(g_CS)

    !write(*,*)"---",g_NCols,g_NRows,g_XLLCorner,g_YLLCorner,g_CellSize,g_NoData_Value !--------------
    !Model Directory
    g_sRunStyle=XXWReadLineStr(g_PrjNP,"RunStyle", error)
    call UPCASE(g_sRunStyle)

    g_sModelCore=XXWReadLineStr(g_PrjNP,"ModelCore", error)
    call UPCASE(g_sModelCore)

    select case(trim(g_sModelCore))
        case("HYDRO")
            g_ModelCore = 1
        case("HYDROSLIDE")
            g_ModelCore = 2
        case("HYDROSLIDE3D")
            g_ModelCore = 3
        case default
            g_ModelCore = 1
    end select


    select case(g_sRunStyle(1:4))
        case("SIMU")
            g_RunStyle = 1
        case("CALI")
            g_RunStyle = 2
        case("REAL")
            g_RunStyle = 3
        case("REPE")
            g_RunStyle = 4
        case("MCAT")
            g_RunStyle = 5
        case("FORE")
            g_RunStyle = 6
        case default
            g_RunStyle = 1
    end select




    g_BasicFormat=XXWReadLineStr(g_PrjNP,"HydroBasicFormat", error)
    call UPCASE(g_BasicFormat)
    g_BasicPath=XXWReadLineStr(g_PrjNP,"HydroBasicPath", error)

    g_SoilDownscaleFormat = XXWReadLineStr(g_PrjNP,"SoilDownscaleFormat", error)
    call UPCASE(g_SoilDownscaleFormat)
    g_SoilDownscalePath = XXWReadLineStr(g_PrjNP,"SoilDownscalePath", error)

    g_LandslideFormat = XXWReadLineStr(g_PrjNP,"LandslideFormat", error)
    call UPCASE(g_LandslideFormat)
    g_LandslidePath = XXWReadLineStr(g_PrjNP,"LandslidePath", error)

    g_ParamFormat=XXWReadLineStr(g_PrjNP,"ParamFormat", error)
    call UPCASE(g_ParamFormat)
    g_ParamPath=XXWReadLineStr(g_PrjNP,"ParamPath", error)

    g_StateFormat=XXWReadLineStr(g_PrjNP,"StateFormat", error)
    call UPCASE(g_StateFormat)
    g_StatePath=XXWReadLineStr(g_PrjNP,"StatePath", error)

    g_ICSFormat=XXWReadLineStr(g_PrjNP,"ICSFormat", error)
    call UPCASE(g_ICSFormat)
    g_ICSPath=XXWReadLineStr(g_PrjNP,"ICSPath", error)

    g_OBSFormat=XXWReadLineStr(g_PrjNP,"OBSFormat", error)
    call UPCASE(g_OBSFormat)
    g_OBSPath=XXWReadLineStr(g_PrjNP,"OBSPath", error)

    g_RainFormat=XXWReadLineStr(g_PrjNP,"RainFormat", error)
    call UPCASE(g_RainFormat)
    g_RainPath=XXWReadLineStr(g_PrjNP,"RainPath", error)

    g_PETFormat=XXWReadLineStr(g_PrjNP,"PETFormat", error)
    call UPCASE(g_PETFormat)
    g_PETPath=XXWReadLineStr(g_PrjNP,"PETPath", error)

    g_ResultFormat=XXWReadLineStr(g_PrjNP,"ResultFormat", error)
    call UPCASE(g_ResultFormat)


    g_ResultPath=XXWReadLineStr(g_PrjNP,"ResultPath", error)

    if(g_sRunStyle(1:4)=="CALI")then
        g_CalibFormat=XXWReadLineStr(g_PrjNP,"CalibFormat",error)
        call UPCASE(g_CalibFormat)
        g_CalibPath=XXWReadLineStr(g_PrjNP,"CalibPath",error)
    end if

    if(g_sRunStyle(1:8)=="FORECAST")then
        g_ForecastFormat = XXWReadLineStr(g_PrjNP,"ForecastFormat",error)
        call UPCASE(g_ForecastFormat)
        g_ForecastPath=XXWReadLineStr(g_PrjNP,"ForecastPath",error)
    end if



    ! Model Run time Information
    g_TimeMark=XXWReadLineStr(g_PrjNP,"TimeMark", error)
    g_TimeStep=XXWReadLineDbl(g_PrjNP,"TimeStep", error)

    sTemp=XXWReadLineStr(g_PrjNP,"StartDate", error)
    call myStrtoD(sTemp,g_StartDate,g_TimeMark)

    sTemp=XXWReadLineStr(g_PrjNP,"WarmupDate", error)
    call myStrtoD(sTemp,g_WarmupDate,g_TimeMark)

    sTemp=XXWReadLineStr(g_PrjNP,"EndDate", error)
    call myStrtoD(sTemp,g_EndDate,g_TimeMark)

    g_NumWarmup=myDdIFf(g_TimeMark, g_StartDate, g_WarmupDate)  &
            / g_TimeStep
    g_ITMax=myDdIFf(g_TimeMark, g_StartDate, g_EndDate)  &
            / g_TimeStep + 1
    g_LoadState=XXWReadLineBln(g_PrjNP,"LoadState", error,"yes")

    g_SaveState=XXWReadLineBln(g_PrjNP,"SaveState", error,"yes")

    !write(*,*)"---",g_NumWarmup,g_ITMax !--------------

    ! Outlet Information
    g_tOutlet%HasOutlet=XXWReadLineBln(g_PrjNP,"HasOutlet",  &
            error,"yes")
    if(g_tOutlet%HasOutlet .NEQV. .true.)then
        write(*,*)"You did not assign a outlet!"
        goto 5000
    end if
    g_tOutlet%Name = XXWReadLineStr(g_PrjNP,"OutletName",error)
    bOutletColRow=XXWReadLineBln(g_PrjNP,"OutletColRow", &
            error,"yes")


    if(bOutletColRow .eqv. .true.)then
        g_tOutlet%Col = XXWReadLineInt(g_PrjNP,"OutletCol", error)

        g_tOutlet%Row = XXWReadLineInt(g_PrjNP,"OutletRow", error)
    else
        dblTemp = XXWReadLineDbl(g_PrjNP, "OutletLong", error)
        ! get the matirx Col and Row for given Long and Lati
        g_tOutlet%Col=int((dblTemp-g_XLLCorner)/g_CellSize)
        dblTemp =XXWReadLineDbl(g_PrjNP,  &
                "OutletLati", error)
        g_tOutlet%Row =int((g_YLLCorner+g_NRows*g_CellSize-dblTemp) &
                /g_CellSize)
        if(g_tOutlet%Col<0 .or. g_tOutlet%Col>=g_NCols &
                .or. g_tOutlet%Row<0 .or. g_tOutlet%Row>=g_NRows)then
            g_tOutlet%bIsOut=.true.
            write(*,*) "Your outlet is out of the basin! "  &
                    //"Please check it!"
            stop
        else
            g_tOutlet%bIsOut=.false.
        end if
    end if

    allocate(g_tOutlet%RObs(0:g_ITMax-1))
    call XXWReadRunoffObs(trim(g_OBSPath) &
            //trim(g_tOutlet%Name)//"_Obs.csv", &
            g_tOutlet%RObs,bIsError)
    if(bIsError .eqv. .true.)then
        g_tOutlet%HasOBS=.false.
        bIsError=.false.
    else
        g_tOutlet%HasOBS=.true.
        bIsError=.false.
    end if
    allocate(g_tOutlet%Mask(0:g_NCols-1,0:g_NRows-1))

    5000 continue
    !Output Pixels Information
    g_NOutPixs=XXWReadLineInt(g_PrjNP,"NOutPixs", error)
    bOutPixColRow=XXWReadLineBln(g_PrjNP,"OutPixColRow", &
            error,"yes")

    allocate(g_tOutPix(0:g_NOutPixs-1))

    Do i=lbound(g_tOutPix,1), ubound(g_tOutPix,1)
        write(sTemp_ForNum,"(I2)")i+1
        sTemp_ForNum=adjustl(sTemp_ForNum)

        g_tOutPix(i)%Name &
                =XXWReadLineStr(g_PrjNP,"OutPixName" &
                //trim(sTemp_ForNum),error)

        if(bOutPixColRow .eqv. .true.)then
            g_tOutPix(i)%Col &
                    =XXWReadLineInt(g_PrjNP,"OutPixCol" &
                    //sTemp_ForNum, error)

            g_tOutPix(i)%Row &
                    =XXWReadLineInt(g_PrjNP,"OutPixRow" &
                    //sTemp_ForNum, error)

        else
            dblTemp &
                    =XXWReadLineDbl(g_PrjNP,  &
                    "OutPixLong" // sTemp_ForNum, error)
            g_tOutPix(i)%Col=int((dblTemp-g_XLLCorner)/g_CellSize)

            dblTemp  &
                    =XXWReadLineDbl(g_PrjNP,  &
                    "OutPixLati"//sTemp_ForNum, error)
            g_tOutPix(i)%Row &
                    =int((g_YLLCorner+g_NRows*g_CellSize-dblTemp) &
                    /g_CellSize)
        end if

        if(g_tOutPix(i)%Col<0   &
                .or. g_tOutPix(i)%Col>=g_NCols &
                .or. g_tOutPix(i)%Row<0  &
                .or. g_tOutPix(i)%Row>=g_NRows)then
            g_tOutPix(i)%bIsOut=.true.
        else
            g_tOutPix(i)%bIsOut=.false.
        end if

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

        allocate(g_tOutPix(i)%Mask(0:g_NCols-1,0:g_NRows-1))
    end do


    !Grid Outputs Information
    allocate(g_bGOVar(lbound(g_sGOVarName,1) &
            :ubound(g_sGOVarName,1)))
    do i=lbound(g_sGOVarName,1),ubound(g_sGOVarName,1)
        g_bGOVar(i)=XXWReadLineBln(g_PrjNP,"GOVar_"   &
                //trim(g_sGOVarName(i)), &
                error,"yes")
    end do
    ! Output the specified date
    g_NOutDTs=XXWReadLineInt(g_PrjNP,"NumOfOutputDates", error)

    allocate(g_OutDTIndex(0:g_NOutDTs-1))

    Do i=lbound(g_OutDTIndex,1), ubound(g_OutDTIndex,1)
        write(sTemp_ForNum,"(I2)")i+1
        sTemp_ForNum=adjustl(sTemp_ForNum)

        sTemp=XXWReadLineStr(g_PrjNP,"OutputDate_"  &
                // sTemp_ForNum, error)
        call myStrtoD(sTemp,dtTemp,g_TimeMark)
        g_OutDTIndex(i)=myDdIFf(g_TimeMark, g_StartDate, dtTemp)  &
                / g_TimeStep
    end do

    bIsError=.false.

    !Read Lake Mask Data Added by Xianwu Xue 2011.4.21
    g_NLakes=XXWReadLineInt(g_PrjNP,"NumOfLakes", error)

    if(g_NLakes>0)then
        allocate(g_LakeMask(0:g_NCols-1,0:g_NRows-1))

        call ReadMatrixFile_Int(trim(g_BasicPath) // "LakeMask",   &
                g_LakeMask,g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
                g_CellSize,g_NoData_Value,bIsError,g_BasicFormat,"")
        if(bIsError .eqv. .true.)then
            write(*,"(1X,A)")  &
                    "*** Something wrong in your Lake Mask file!"
            write(g_CREST_LogFileID,"(1X,A)")  &
                    "*** Something wrong in your Lake Mask file!"
            return
        end if
    end if


    return
end subroutine ReadProjectFile

! read the basic files, e.g., DEM, FDR, FAC....
subroutine ReadBasicFile(bIsError)
    use CREST_Project
    use CREST_Basic
    use SoilDownscale_Basic
    use Landslide_Basic

    implicit none
    logical :: bIsError
    double precision,allocatable :: AreaFact(:,:)
    ! read the hydrological DEM data
    allocate(g_DEM(0:g_NCols-1,0:g_NRows-1))
    write(*,*)"     ---Reading Hydro-DEM File!"
    call ReadMatrixFile(trim(g_BasicPath) // "DEM",g_DEM,  &
            g_NCols, g_NRows,g_XLLCorner,g_YLLCorner,  &
            g_CellSize,g_NoData_Value,bIsError,g_BasicFormat,"")
    if(bIsError .eqv. .true.)then
        return
    end if


    allocate(g_FDR(0:g_NCols-1,0:g_NRows-1))
    write(*,*)"     ---Reading FDR File!"
    call ReadMatrixFile_Int(trim(g_BasicPath) // "FDR", g_FDR, &
            g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
            g_CellSize,g_NoData_Value,bIsError,g_BasicFormat,"")
    if(bIsError .eqv. .true.)then
        return
    end if

    !Convert to Standard Flow Direction Map
    if(count(g_FDR==3)/=0 &
            .or. count(g_FDR==5)/=0 &
            .or. count(g_FDR==7)/=0)then
        write(*,*)"     ---Converting DDM to FDR!"
        call ConvDDMToFDR()

        call WriteMatrixFile_Int(trim(g_ResultPath)//"FDR", &
                g_FDR, g_NCols,g_NRows,    &
                g_XLLCorner,g_YLLCorner, g_CellSize, g_NoData_Value, &
                bIsError,g_BasicFormat)
    end if

    allocate(g_FAC(0:g_NCols-1,0:g_NRows-1))
    write(*,*)"     ---Reading FAC File!"
    call ReadMatrixFile_Int(trim(g_BasicPath) // "FAC", g_FAC, &
            g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
            g_CellSize,g_NoData_Value,bIsError,g_BasicFormat,"")

    allocate(g_GridArea(0:g_NCols-1,0:g_NRows-1))
    allocate(g_NextLen(0:g_NCols-1,0:g_NRows-1))
    allocate(g_NextC(0:g_NCols-1,0:g_NRows-1))
    allocate(g_NextR(0:g_NCols-1,0:g_NRows-1))

    write(*,*)"     ---Reading GridArea File!"
    call ReadMatrixFile(trim(g_BasicPath) // "GridArea",g_GridArea,  &
            g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
            g_CellSize,g_NoData_Value,bIsError,g_BasicFormat,"")
    call AssignNextGroup(g_FDR,bIsError)

    write(*,*)"     ---Reading AreaFact File!"
    allocate(AreaFact(0:g_NCols-1,0:g_NRows-1))
    call ReadMatrixFile(trim(g_BasicPath) // "AreaFact",AreaFact,  &
            g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
            g_CellSize,g_NoData_Value,bIsError,g_BasicFormat,"")
    if(bIsError .eqv. .false.)then
        where(g_Mask/=g_NoData_Value)
            g_GridArea=AreaFact*g_GridArea
        endwhere
    end if

    !Read Stream Map
    allocate(g_Stream(0:g_NCols-1,0:g_NRows-1))
    write(*,*)"     ---Reading Stream File!"
    call ReadMatrixFile_Int(trim(g_BasicPath) // "Stream", g_Stream,  &
            g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
            g_CellSize,g_NoData_Value,bIsError,g_BasicFormat,"")
    if(bIsError .eqv. .true.)then
        call GetStream()

        call WriteMatrixFile_Int(trim(g_ResultPath)//"Stream",  &
                g_Stream, g_NCols,g_NRows,   &
                g_XLLCorner,g_YLLCorner, g_CellSize, g_NoData_Value, &
                bIsError,g_BasicFormat)
    end if

    !Read Mask Map
    allocate(g_Mask(0:g_NCols-1,0:g_NRows-1))
    write(*,*)"     ---Reading Mask File!"
    call ReadMatrixFile_Int(trim(g_BasicPath) // "Mask", g_Mask, &
            g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
            g_CellSize,g_NoData_Value,bIsError,g_BasicFormat,"")
    if(bIsError .eqv. .true.)then
        if(g_tOutlet%HasOutlet)then
            !Get Mask Map by Outlet
            write(*,*)    "Can't find the MASK file in Basics folder" ! by cgd
            write(*,*)    "Get Mask Map by Outlet" ! by cgd
            call GetMask(g_NCols,g_NRows,g_tOutlet%Col,g_tOutlet%Row, &
                    g_NoData_Value,g_FDR,g_NextC,g_NextR,  &
                    g_Mask,InBasin)
        else
            write(*,*)    "Get Mask Map by DEM in Basics folder" ! by cgd
            call GetMaskByNoData(g_NCols,g_NRows, &
                    g_NoData_Value,g_DEM,g_Mask)
        end if
        ! generate the mask based on the outlet, and write it to the folder
        call WriteMatrixFile_Int(trim(g_ResultPath)//"Mask",  &
                g_Mask, g_NCols,g_NRows,   &
                g_XLLCorner,g_YLLCorner, g_CellSize, g_NoData_Value, &
                bIsError,g_BasicFormat)
    end if


    !Read Slope Map
    allocate(g_Slope(0:g_NCols-1,0:g_NRows-1))
    allocate(g_Slope_angle(0:g_NCols-1,0:g_NRows-1))
    write(*,*)"     ---Reading Slope File!"
    call ReadMatrixFile(trim(g_BasicPath) // "Slope",g_Slope,  &
            g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
            g_CellSize,g_NoData_Value,bIsError,g_BasicFormat,"")
    if(bIsError .eqv. .true.)then

        call CalSlope_Tiger(bIsError)

        if(bIsError .eqv. .true.)then
            call CalSlope_Tiger_XXW()
        end if

    end if

    where(g_Mask == 1)
        g_Slope_angle = ATAND(g_Slope)
    elsewhere
        g_Slope_angle = g_Slope
    end where
    ! output the slope file with unit of angle
    call WriteMatrixFile(trim(g_ResultPath)//"Slope_angle",   &
            g_Slope_angle, g_NCols,g_NRows,   &
            g_XLLCorner,g_YLLCorner, g_CellSize, g_NoData_Value,  &
            bIsError,g_BasicFormat)
    
    ! Limit the matrix to the mask area
    where(g_Mask == g_NoData_Value)
        g_FDR = g_NoData_Value
        g_FAC = g_NoData_Value
        g_Stream = g_NoData_Value
    end where

    if (g_ModelCore == 3) then
        ! read the landslide mask data
        allocate(g_mask_fine(0:g_NCols_Land-1,0:g_NRows_Land-1))
        write(*,*)"Reading Landslide File!"
        call ReadMatrixFile_Int(trim(g_LandslidePath) // "mask_fine",g_mask_fine,  &
                g_NCols_Land, g_NRows_Land,g_xllCorner_Land,g_yllCorner_Land,  &
                g_CellSize_Land,g_NoData_Value,bIsError,g_LandslideFormat,"")
        if(bIsError .eqv. .true.)then
            return
        end if



        ! read the soil map with coarse resolution
        allocate(g_soil(0:g_NCols-1,0:g_NRows-1))

        call ReadMatrixFile_Int(trim(g_LandslidePath) // "Soil",g_soil,  &
                g_NCols, g_NRows,g_xllCorner,g_yllCorner,  &
                g_CellSize,g_NoData_Value,bIsError,g_LandslideFormat,"")
        if(bIsError .eqv. .true.)then
            return
        end if

        ! read the landslide DEM data
        allocate(g_DEM_fine(0:g_NCols_Land-1,0:g_NRows_Land-1))

        call ReadMatrixFile(trim(g_LandslidePath) // "DEM_fine",g_DEM_fine,  &
                g_NCols_Land, g_NRows_Land,g_xllCorner_Land,g_yllCorner_Land,  &
                g_CellSize_Land,g_NoData_Value,bIsError,g_LandslideFormat,"")
        if(bIsError .eqv. .true.)then
            return
        end if

        where (g_DEM_fine < 0)
            g_DEM_fine = 0
        end where

        ! read the landslide slope data
        allocate(g_slope_fine(0:g_NCols_Land-1,0:g_NRows_Land-1))

        call ReadMatrixFile(trim(g_LandslidePath) // "slope_fine",g_slope_fine,  &
                g_NCols_Land, g_NRows_Land,g_xllCorner_Land,g_yllCorner_Land,  &
                g_CellSize_Land,g_NoData_Value,bIsError,g_LandslideFormat,"")
        if(bIsError .eqv. .true.)then
            return
        end if

        where (g_slope_fine == 0)
            g_slope_fine = 0.1
        end where
        
        ! read the landslide slope data
        allocate(g_aspect_fine(0:g_NCols_Land-1,0:g_NRows_Land-1))

        call ReadMatrixFile(trim(g_LandslidePath) // "aspect_fine",g_aspect_fine,  &
                g_NCols_Land, g_NRows_Land,g_xllCorner_Land,g_yllCorner_Land,  &
                g_CellSize_Land,g_NoData_Value,bIsError,g_LandslideFormat,"")
        if(bIsError .eqv. .true.)then
            return
        end if

        where (g_aspect_fine < 0)
            g_aspect_fine = 1
        end where

        allocate(g_FS_3D(0:g_NCols_Land-1,0:g_NRows_Land-1))
        allocate(g_failure_volume(0:g_NCols_Land-1,0:g_NRows_Land-1))
        allocate(g_failure_area(0:g_NCols_Land-1,0:g_NRows_Land-1))
        allocate(g_probability(0:g_NCols_Land-1,0:g_NRows_Land-1))
        allocate(g_cal_count(0:g_NCols_Land-1,0:g_NRows_Land-1))
        allocate(g_unstable_count(0:g_NCols_Land-1,0:g_NRows_Land-1))

        !----------------- read the soil downscaling data------------
        allocate(g_aspect_coarse(0:g_NCols-1,0:g_NRows-1))
        write(*,*)"  Reading soil downscaling File!"
        call ReadMatrixFile(trim(g_SoilDownscalePath) // "aspect_coarse", g_aspect_coarse, &
                g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
                g_CellSize,g_NoData_Value,bIsError,g_SoilDownscaleFormat,"")
        if(bIsError .eqv. .true.)then
            return
        end if

        allocate(g_curvature_coarse(0:g_NCols-1,0:g_NRows-1))

        call ReadMatrixFile(trim(g_SoilDownscalePath) // "curvature_coarse", g_curvature_coarse, &
                g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
                g_CellSize,g_NoData_Value,bIsError,g_SoilDownscaleFormat,"")
        if(bIsError .eqv. .true.)then
            return
        end if

        allocate(g_TWI_coarse(0:g_NCols-1,0:g_NRows-1))

        call ReadMatrixFile(trim(g_SoilDownscalePath) // "TWI_coarse", g_TWI_coarse, &
                g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
                g_CellSize,g_NoData_Value,bIsError,g_SoilDownscaleFormat,"")
        if(bIsError .eqv. .true.)then
            return
        end if

        allocate(g_TWI_fine(0:g_NCols_Land-1,0:g_NRows_Land-1))

        call ReadMatrixFile(trim(g_SoilDownscalePath) // "TWI_fine", g_TWI_fine, &
                g_NCols_Land, g_NRows_Land,g_xllCorner_Land,g_yllCorner_Land, &
                g_CellSize_Land,g_NoData_Value,bIsError,g_SoilDownscaleFormat,"")
        if(bIsError .eqv. .true.)then
            return
        end if

        allocate(g_curvature_fine(0:g_NCols_Land-1,0:g_NRows_Land-1))

        call ReadMatrixFile(trim(g_SoilDownscalePath) // "curvature_fine", g_curvature_fine, &
                g_NCols_Land, g_NRows_Land,g_xllCorner_Land,g_yllCorner_Land, &
                g_CellSize_Land,g_NoData_Value,bIsError,g_SoilDownscaleFormat,"")
        if(bIsError .eqv. .true.)then
            return
        end if

        ! declare the soil moisture in fine map
        allocate(g_SM_fine(0:g_NCols_Land-1,0:g_NRows_Land-1))



    end if


    return
end subroutine ReadBasicFile



! read the parameters file
subroutine ReadParamFile()
    use CREST_Project
    use CREST_Basic
    use CREST_Param
    use LandslideModel_parameters
    use OMP_LIB
    implicit none
    integer :: error
    logical :: bIsDIS,bIsError
    character(len=200):: fileName,sParamName
    double precision :: dblTemp
    fileName=trim(g_ParamPath) // "Parameters_hydro.txt"

    call InitParamsType(g_tParams)
    ! Physical Parameters
    sParamName="Ksat"
    bIsDIS=XXWReadLineBln(fileName,trim(sParamName)//"Type",  &
            error,"Distributed")
    if(bIsDIS)then
        call ReadMatrixFile(trim(g_ParamPath) // trim(sParamName),  &
                g_tParams%Ksat, &
                g_NCols, g_NRows,g_XLLCorner,g_YLLCorner,  &
                g_CellSize,g_NoData_Value,bIsError,g_ParamFormat,"")
        where(g_Mask==g_NoData_Value)
            g_tParams%Ksat=g_NoData_Value
        end where
    else
        dblTemp=XXWReadLineDbl(fileName,trim(sParamName), error)
        where(g_Mask/=g_NoData_Value)
            g_tParams%Ksat=dblTemp
        end where
    end if

    !-----------------------------------------------------------------------
    sParamName="RainFact"
    bIsDIS=XXWReadLineBln(fileName,trim(sParamName)//"Type",   &
            error,"Distributed")
    if(bIsDIS)then
        call ReadMatrixFile(trim(g_ParamPath) // trim(sParamName),   &
                g_tParams%RainFact, &
                g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
                g_CellSize,g_NoData_Value,bIsError,g_ParamFormat,"")
        where(g_Mask==g_NoData_Value)
            g_tParams%RainFact=g_NoData_Value
        end where
    else
        dblTemp=XXWReadLineDbl(fileName,trim(sParamName), error)
        where(g_Mask/=g_NoData_Value)
            g_tParams%RainFact=dblTemp
        end where
    end if


    !-----------------------------------------------------------------------
    sParamName="WM"
    bIsDIS=XXWReadLineBln(fileName,trim(sParamName)//"Type",  &
            error,"Distributed")
    if(bIsDIS)then
        call ReadMatrixFile(trim(g_ParamPath) // trim(sParamName),  &
                g_tParams%WM, &
                g_NCols, g_NRows,g_XLLCorner,g_YLLCorner,  &
                g_CellSize,g_NoData_Value,bIsError,g_ParamFormat,"")
        where(g_Mask==g_NoData_Value)
            g_tParams%WM=g_NoData_Value
        end where
    else
        dblTemp=XXWReadLineDbl(fileName,trim(sParamName), error)
        where(g_Mask/=g_NoData_Value)
            g_tParams%WM=dblTemp
        end where
    end if

    !-----------------------------------------------------------------------
    sParamName="B"
    bIsDIS=XXWReadLineBln(fileName,trim(sParamName)//"Type",  &
            error,"Distributed")
    if(bIsDIS)then
        call ReadMatrixFile(trim(g_ParamPath) // trim(sParamName),  &
                g_tParams%B, &
                g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
                g_CellSize,g_NoData_Value,bIsError,g_ParamFormat,"")
        where(g_Mask==g_NoData_Value)
            g_tParams%B=g_NoData_Value
        end where
    else
        dblTemp=XXWReadLineDbl(fileName,trim(sParamName), error)
        where(g_Mask/=g_NoData_Value)
            g_tParams%B=dblTemp
        end where
    end if

    !-----------------------------------------------------------------------
    sParamName="IM"
    bIsDIS=XXWReadLineBln(fileName,trim(sParamName)//"Type",  &
            error,"Distributed")
    if(bIsDIS)then
        call ReadMatrixFile(trim(g_ParamPath) // trim(sParamName),  &
                g_tParams%IM, &
                g_NCols, g_NRows,g_XLLCorner,g_YLLCorner,  &
                g_CellSize,g_NoData_Value,bIsError,g_ParamFormat,"")
        where(g_Mask==g_NoData_Value)
            g_tParams%IM=g_NoData_Value
        end where
    else
        dblTemp=XXWReadLineDbl(fileName,trim(sParamName), error)
        where(g_Mask/=g_NoData_Value)
            g_tParams%IM=dblTemp
        end where
    end if
    !-----------------------------------------------------------------------
    sParamName="KE"
    bIsDIS=XXWReadLineBln(fileName,trim(sParamName)//"Type",  &
            error,"Distributed")
    if(bIsDIS)then
        call ReadMatrixFile(trim(g_ParamPath) // trim(sParamName),  &
                g_tParams%KE,  &
                g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
                g_CellSize,g_NoData_Value,bIsError,g_ParamFormat,"")
        where(g_Mask==g_NoData_Value)
            g_tParams%KE=g_NoData_Value
        end where
    else
        dblTemp=XXWReadLineDbl(fileName,trim(sParamName), error)
        where(g_Mask/=g_NoData_Value)
            g_tParams%KE=dblTemp
        end where
    end if

    !-----------------------------------------------------------------------
    sParamName="coeM"
    bIsDIS=XXWReadLineBln(fileName,trim(sParamName)//"Type",  &
            error,"Distributed")
    if(bIsDIS)then
        call ReadMatrixFile(trim(g_ParamPath) // trim(sParamName), &
                g_tParams%coeM, &
                g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
                g_CellSize,g_NoData_Value,bIsError,g_ParamFormat,"")
        where(g_Mask==g_NoData_Value)
            g_tParams%coeM=g_NoData_Value
        end where
    else
        dblTemp=XXWReadLineDbl(fileName,trim(sParamName), error)
        where(g_Mask/=g_NoData_Value)
            g_tParams%coeM=dblTemp
        end where
    end if

    !Conceptual Parameters
    !-----------------------------------------------------------------------
    sParamName="expM"
    bIsDIS=XXWReadLineBln(fileName,trim(sParamName)//"Type",  &
            error,"Distributed")
    if(bIsDIS)then
        call ReadMatrixFile(trim(g_ParamPath) // trim(sParamName),  &
                g_tParams%expM, &
                g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
                g_CellSize,g_NoData_Value,bIsError,g_ParamFormat,"")
        where(g_Mask==g_NoData_Value)
            g_tParams%expM=g_NoData_Value
        end where
    else
        dblTemp=XXWReadLineDbl(fileName,trim(sParamName), error)
        where(g_Mask/=g_NoData_Value)
            g_tParams%expM=dblTemp
        end where
    end if

    !-----------------------------------------------------------------------
    sParamName="coeR"
    bIsDIS=XXWReadLineBln(fileName,trim(sParamName)//"Type",  &
            error,"Distributed")
    if(bIsDIS)then
        call ReadMatrixFile(trim(g_ParamPath) // trim(sParamName),  &
                g_tParams%coeR, &
                g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
                g_CellSize,g_NoData_Value,bIsError,g_ParamFormat,"")
        where(g_Mask==g_NoData_Value)
            g_tParams%coeR=g_NoData_Value
        end where
    else
        dblTemp=XXWReadLineDbl(fileName,trim(sParamName), error)
        where(g_Mask/=g_NoData_Value)
            g_tParams%coeR=dblTemp
        end where
    end if
    !-----------------------------------------------------------------------
    sParamName="coeS"
    bIsDIS=XXWReadLineBln(fileName,trim(sParamName)//"Type",  &
            error,"Distributed")
    if(bIsDIS)then
        call ReadMatrixFile(trim(g_ParamPath) // trim(sParamName),  &
                g_tParams%coeS, &
                g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
                g_CellSize,g_NoData_Value,bIsError,g_ParamFormat,"")
        where(g_Mask==g_NoData_Value)
            g_tParams%coeS=g_NoData_Value
        end where
    else
        dblTemp=XXWReadLineDbl(fileName,trim(sParamName), error)
        where(g_Mask/=g_NoData_Value)
            g_tParams%coeS=dblTemp
        end where
    end if

    !-----------------------------------------------------------------------
    sParamName="KS"
    bIsDIS=XXWReadLineBln(fileName,trim(sParamName)//"Type", &
            error,"Distributed")
    if(bIsDIS)then
        call ReadMatrixFile(trim(g_ParamPath) // trim(sParamName),  &
                g_tParams%KS,  &
                g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
                g_CellSize,g_NoData_Value,bIsError,g_ParamFormat,"")
        where(g_Mask==g_NoData_Value)
            g_tParams%KS=g_NoData_Value
        end where
    else
        dblTemp=XXWReadLineDbl(fileName,trim(sParamName), error)
        where(g_Mask/=g_NoData_Value)
            g_tParams%KS=dblTemp
        end where
    end if
    !-----------------------------------------------------------------------
    sParamName="KI"
    bIsDIS=XXWReadLineBln(fileName,trim(sParamName)//"Type",  &
            error,"Distributed")
    if(bIsDIS)then
        call ReadMatrixFile(trim(g_ParamPath) // trim(sParamName),  &
                g_tParams%KI,  &
                g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
                g_CellSize,g_NoData_Value,bIsError,g_ParamFormat,"")
        where(g_Mask==g_NoData_Value)
            g_tParams%KI=g_NoData_Value
        end where
    else
        dblTemp=XXWReadLineDbl(fileName,trim(sParamName), error)
        where(g_Mask/=g_NoData_Value)
            g_tParams%KI=dblTemp
        end where
    end if

    if (g_ModelCore == 3) then
        ! landslide module is open. Read the landslide parameters

        fileName=trim(g_ParamPath) // "Parameters_land.txt"
        ellipse_density = XXWreadLineInt(fileName,"ellipse_density", error)


        min_ae = XXWReadLineDbl(fileName,"min_ae", error)
        max_ae = XXWReadLineDbl(fileName,"max_ae", error)
        min_be = XXWReadLineDbl(fileName,"min_be", error)
        max_be = XXWReadLineDbl(fileName,"max_be", error)
        min_ce = XXWReadLineDbl(fileName,"min_ce", error)
        max_ce = XXWReadLineDbl(fileName,"max_ce", error)

        ! here cell_size is the resolution of the landslide map in unit of m
        CellSize_LandInM = XXWReadLineDbl(fileName,"cell_size", error)
        
    end if
    ! read the parallel setup
    if (g_ModelCore == 1) then ! only for hydrological modeling

        fileName=trim(g_ParamPath) // "Parameters_parallel.txt"

        N_Subbasin = XXWreadLineInt(fileName,"N_Subbasin", error)
        Nthread_hydro = XXWreadLineInt(fileName,"NHydroThread", error)

    elseif (g_ModelCore == 3) then ! for both hydrology and landslide
        fileName=trim(g_ParamPath) // "Parameters_parallel.txt"

        N_Subbasin = XXWreadLineInt(fileName,"N_Subbasin", error)
        Nthread_hydro = XXWreadLineInt(fileName,"NHydroThread", error)
        total_tile_number = XXWreadLineInt(fileName,"Tot_tile", error)
        Nthread_Land = XXWreadLineInt(fileName,"NLandThread", error)

    end if


    ! check the validation of the parallel setup
    if (Nthread_hydro > OMP_GET_MAX_THREADS() .or. &
            Nthread_Land > OMP_GET_MAX_THREADS()) then
        write(*,*) "Error: Parallel startup failed! &
                You are requesting the threads more than maximum number"
        write(*,*) "Available maximum threads are: ", OMP_GET_MAX_THREADS()
        stop
    end if


    return
end subroutine ReadParamFile




! read the initial conditions
subroutine ReadICSFile()
    use CREST_Project
    use CREST_Basic
    use CREST_ICS

    implicit none
    integer :: error
    logical :: bIsDIS,bIsError
    character(len=200):: fileName,sParamName
    double precision :: dblTemp
    fileName=trim(g_ICSPath) // "InitialConditions.txt"

    allocate(g_WU0(0:g_NCols-1,0:g_NRows-1))
    allocate(g_SS0(0:g_NCols-1,0:g_NRows-1))
    allocate(g_SI0(0:g_NCols-1,0:g_NRows-1))
    g_WU0=g_NoData_Value
    g_SS0=g_NoData_Value
    g_SI0=g_NoData_Value

    sParamName="WU0"
    bIsDIS=XXWReadLineBln(fileName,trim(sParamName)//"Type",  &
            error,"Distributed")
    if(bIsDIS)then
        call ReadMatrixFile(trim(g_ICSPath) // trim(sParamName), &
                g_WU0, &
                g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
                g_CellSize,g_NoData_Value,bIsError,g_ICSFormat,"")

    else
        dblTemp=XXWReadLineDbl(fileName,trim(sParamName), error)
        where(g_Mask/=g_NoData_Value)
            g_WU0=dblTemp
        end where
    end if

    !-----------------------------------------------------------------------
    sParamName="SS0"
    bIsDIS=XXWReadLineBln(fileName,trim(sParamName)//"Type",  &
            error,"Distributed")
    if(bIsDIS)then
        call ReadMatrixFile(trim(g_ICSPath) // trim(sParamName),  &
                g_SS0,  &
                g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
                g_CellSize,g_NoData_Value,bIsError,g_ICSFormat,"")
    else
        dblTemp=XXWReadLineDbl(fileName,trim(sParamName), error)
        where(g_Mask/=g_NoData_Value)
            g_SS0=dblTemp
        end where
    end if


    !-----------------------------------------------------------------------
    sParamName="SI0"
    bIsDIS=XXWReadLineBln(fileName,trim(sParamName)//"Type",  &
            error,"Distributed")
    if(bIsDIS)then
        call ReadMatrixFile(trim(g_ICSPath) // trim(sParamName),  &
                g_SI0, &
                g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
                g_CellSize,g_NoData_Value,bIsError,g_ICSFormat,"")
    else
        dblTemp=XXWReadLineDbl(fileName,trim(sParamName), error)
        where(g_Mask/=g_NoData_Value)
            g_SI0=dblTemp
        end where
    end if

    return
end subroutine ReadICSFile

subroutine ReadCalib_SCEUAFile()
    use CREST_Project
    use CREST_Basic
    use CREST_Calib_SCEUA
    implicit none
    character(len=200)::sFileName,sTemp

    integer :: error,i,j,k

    logical	:: bIsColRow,bIsError
    double precision:: dblTemp

    sFileName=trim(g_CalibPath)//trim("Calibrations.txt")
    g_iseed=XXWReadLineInt(trim(sFileName),"iseed", error)
    g_maxn=XXWReadLineInt(trim(sFileName),"maxn", error)
    g_kstop=XXWReadLineInt(trim(sFileName),"kstop", error)
    g_pcento=XXWReadLineDbl(trim(sFileName),"pcento", error)
    g_ngs=XXWReadLineInt(trim(sFileName),"ngs", error)

    g_NCalibStas=XXWReadLineInt(trim(sFileName), &
            "NCalibStations", error)

    bIsColRow=XXWReadLineBln(trim(sFileName),"IsColRow",  &
            error,"yes")

    allocate(g_tCalibSta(0:g_NCalibStas-1))


    do i=0,g_NCalibStas-1
        write(sTemp,*)i+1
        sTemp=adjustl(sTemp)
        g_tCalibSta(i)%Name=XXWReadLineStr(trim(sFileName), &
                "Name_"//trim(sTemp), error)
        g_tCalibSta(i)%Value=XXWReadLineInt(trim(sFileName), &
                "Value_"//trim(sTemp), error)

        if(bIsColRow .eqv. .true.)then
            g_tCalibSta(i)%Col &
                    =XXWReadLineInt(trim(sFileName), &
                    "Col_"//trim(sTemp), error)

            g_tCalibSta(i)%Row &
                    =XXWReadLineInt(trim(sFileName), &
                    "Row_"//trim(sTemp), error)
        else
            dblTemp &
                    =XXWReadLineDbl(trim(sFileName), &
                    "Long_"//trim(sTemp), error)
            g_tCalibSta(i)%Col &
                    =int((dblTemp-g_XLLCorner)/g_CellSize)

            dblTemp &
                    =XXWReadLineDbl(trim(sFileName),  &
                    "Lati_"//trim(sTemp), error)
            g_tCalibSta(i)%Row &
                    =int((g_YLLCorner+g_NRows*g_CellSize-dblTemp)  &
                    /g_CellSize)

        end if

        if(InBasin2(g_tCalibSta(i)%Col,g_tCalibSta(i)%Row)  &
                .eqv. .false.)then
            g_tCalibSta(i)%bIsOut=.true.
        else
            g_tCalibSta(i)%bIsOut=.false.
        end if


        if(g_tCalibSta(i)%bIsOut .eqv. .true.)then
            cycle
        end if

        allocate(g_tCalibSta(i)%a(1:ubound(g_sParamName,1)))
        allocate(g_tCalibSta(i)%bl(1:ubound(g_sParamName,1)))
        allocate(g_tCalibSta(i)%bu(1:ubound(g_sParamName,1)))
        allocate(g_tCalibSta(i)%x(1:ubound(g_sParamName,1)))

        g_tCalibSta(i)%a=1.0
        g_tCalibSta(i)%bl=1.0
        g_tCalibSta(i)%bu=1.0

        ! Input the Extent of Parameters
        do j=1,ubound(g_sParamName,1)
            call XXWReadLineDbl3(trim(sFileName), &
                    trim(g_sParamName(j))//"_"//trim(sTemp),error, &
                    g_tCalibSta(i)%bl(j),  &
                    g_tCalibSta(i)%a(j),  &
                    g_tCalibSta(i)%bu(j))
        end do

        g_tCalibSta(i)%x=g_tCalibSta(i)%a

        g_tCalibSta(i)%Area &
                =g_FAC(g_tCalibSta(i)%Col,g_tCalibSta(i)%Row)

        allocate(g_tCalibSta(i)%Mask(0:g_NCols-1,0:g_NRows-1))

        g_tCalibSta(i)%Mask=g_NoData_Value

        !	Get Station's Mask Matrix
        call GetMask(g_NCols,g_NRows,g_tCalibSta(i)%Col, &
                g_tCalibSta(i)%Row,g_NoData_Value,g_FDR,  &
                g_NextC,g_NextR,g_tCalibSta(i)%Mask,InBasin)

        call WriteMatrixFile_Int(trim(g_ResultPath)//"Sta_" &
                //trim(g_tCalibSta(i)%Name)//"_Mask",  &
                g_tCalibSta(i)%Mask, g_NCols,g_NRows,   &
                g_XLLCorner,g_YLLCorner, g_CellSize, g_NoData_Value, &
                bIsError,g_BasicFormat)

        !	Read Station's Observated Runoff
        allocate(g_tCalibSta(i)%RObs(0:g_ITMax-1))
        allocate(g_tCalibSta(i)%RSim(0:g_ITMax-1))

        call XXWReadRunoffObs(trim(g_OBSPath) &
                //trim(g_tCalibSta(i)%Name)//"_Obs.csv",  &
                g_tCalibSta(i)%RObs,bIsError)

    end do

    !Get g_RegMask Map
    allocate(g_RegMask(0:g_NCols-1,0:g_NRows-1))
    call ReadMatrixFile_Int(trim(g_CalibPath) // "CalibMask",   &
            g_RegMask,g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
            g_CellSize,g_NoData_Value,bIsError,g_CalibFormat,"")
    if(bIsError .eqv. .true.)then
        do i=g_NCalibStas-1,0,-1
            g_RegMask=g_NoData_Value
            where(g_tCalibSta(i)%Mask==1)
                g_RegMask=g_tCalibSta(i)%Value
            end where
        end do
        call WriteMatrixFile_Int(trim(g_ResultPath) // "CalibMask",   &
                g_RegMask,g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
                g_CellSize,g_NoData_Value,bIsError,g_BasicFormat)
    end if

    return
end subroutine ReadCalib_SCEUAFile

subroutine AssignNextGroup(FDR,bIsNotExist)
    use CREST_Project
    use CREST_Basic
    implicit none
    ! character(len = 20) :: g_CS
    logical :: bGCS ,bIsNotExist ! Examine whether the GridArea.asc exist

    integer :: FDR(0:g_NCols-1,0:g_NRows-1)
    integer :: j,i
    double precision :: LenSN, LenEW, LenCross

    !Identify whether it is a Geographic Coordinate System (GCS)
    !   or Projected Coordinate System (PCS)

    if(g_XLLCorner>=-180.0 .and. g_XLLCorner<=180  &
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
            if(FDR(j,i)/=g_NoData_Value)then
                if(bGCS .eqv. .true.)then
                    LenEW=g_YLLCorner+(g_NRows-i-0.5)*g_CellSize
                    LenEW=LenSN*cos(LenEW*4.0*atan(1.0)/180.0)
                else
                    LenEW=g_CellSize
                end if
                LenCross=sqrt(LenEW**2+LenSN**2)

                if(bIsNotExist .eqv. .true.)then
                    g_GridArea(j,i)=LenSN*LenEW/1.0e6 ! Convert to km^2
                end if

                select case (FDR(j,i))
                case (64)
                    g_NextR(j,i)=i-1
                    g_NextC(j,i)=j
                    g_NextLen(j,i)=LenSN
                case (128)
                    g_NextR(j,i)=i-1
                    g_NextC(j,i)=j+1
                    g_NextLen(j,i)=LenCross
                case (1)
                    g_NextR(j,i)=i
                    g_NextC(j,i)=j+1
                    g_NextLen(j,i)=LenEW
                case (2)
                    g_NextR(j,i)=i+1
                    g_NextC(j,i)=j+1
                    g_NextLen(j,i)=LenCross
                case (4)
                    g_NextR(j,i)=i+1
                    g_NextC(j,i)=j
                    g_NextLen(j,i)=LenSN
                case (8)
                    g_NextR(j,i)=i+1
                    g_NextC(j,i)=j-1
                    g_NextLen(j,i)=LenCross
                case (16)
                    g_NextR(j,i)=i
                    g_NextC(j,i)=j-1
                    g_NextLen(j,i)=LenEW
                case (32)
                    g_NextR(j,i)=i-1
                    g_NextC(j,i)=j-1
                    g_NextLen(j,i)=LenCross
                case (256) !Outlet
                    g_NextR(j,i)=g_NRows
                    g_NextC(j,i)=g_NCols
                    g_NextLen(j,i)=LenCross
                case default
                    write(*,*) "Something is wrong " &
                            // "in your Flow Direction Map!"
                end select

                if(InBasin(g_NextC(j,i), &
                        g_NextR(j,i)) .eqv. .false.)then
                    ! 						g_NextC(j,i)=g_NoData_Value
                    ! 						g_NextR(j,i)=g_NoData_Value
                    g_NextLen(j,i)=LenSN
                end if
            else
                g_NextC(j,i)=g_NoData_Value
                g_NextR(j,i)=g_NoData_Value
                g_NextLen(j,i)=g_NoData_Value
                if(bIsNotExist .eqv. .true.)then
                    g_GridArea(j,i)=g_NoData_Value
                end if
            end if
        end do
    end do

    return
end subroutine AssignNextGroup

subroutine GetMask(NCols,NRows,jCol_outlet,iRow_outlet,NoData_Value,FDR, &
        NextC,NextR,MaskOut,InBasin)
    implicit none
    integer :: iRow_outlet,jCol_outlet !Objective Row/Col
    integer :: NCols,NRows
    double precision :: NoData_Value
    integer :: NextC(0:NCols-1,0:NRows-1)
    integer :: NextR(0:NCols-1,0:NRows-1)
    integer :: FDR(0:NCols-1,0:NRows-1)
    integer, intent(out) :: MaskOut(0:NCols-1,0:NRows-1)
    integer :: i,j,iiA,iiB,jjA,jjB
    logical,external :: InBasin

    MaskOut=NoData_Value
    MaskOut(jCol_outlet,iRow_outlet)=1
    do i = 0, NRows-1

        do j = 0, NCols-1
            !    write(*,*) i, j
            !if (i == 338 .and. j == 338) then
            !    pause
            !end if
            if(FDR(j,i)==NoData_Value)then
                cycle
            end if
            iiA=NextR(j,i)
            jjA=NextC(j,i)
            do while(InBasin(jjA,iiA) .eqv. .true.)
                if(FDR(jjA,iiA)==NoData_Value)then
                    exit
                end if
                if(iiA==iRow_outlet .and. jjA==jCol_outlet)then
                    MaskOut(j,i)=1
                    exit
                end if
                iiB=iiA
                jjB=jjA
                iiA=NextR(jjB,iiB)
                jjA=NextC(jjB,iiB)
            end do
        end do
    end do
    return
end subroutine GetMask

subroutine GetStream()
    use CREST_Project
    use CREST_Basic

    implicit none
    double precision :: TH
    logical :: fExist

    integer :: i,j
    integer :: fileid
    character(len=200):: fileName
    double precision :: dblTemp
    fileName=trim(g_BasicPath) // "Stream.def"

    inquire(file=trim(Filename), exist=fExist)
    if(fExist .eqv. .false.)then
        write(*,*)"You should give a Stream Map or 'Stream.def' file!"
        write(g_CREST_LogFileID,*) &
                "You should give a Stream Map or 'Stream.def' file!"
        stop
    end if
    call XXWGetFreeFile(fileid)
    open(fileid,file=TRIM(Filename), form="formatted")
    read(fileid,*)TH
    close(fileid)

    do i=0,g_NRows-1
        do j=0,g_NCols-1
            if(g_FAC(j,i)>TH)then
                g_Stream(j,i)=1
            else
                g_Stream(j,i)=g_NoData_Value
            end if
        end do
    end do

    return
end subroutine GetStream


integer function XXWReadLineInt(sFileNP,sKey,error)
    implicit none
    character(len=200)::sKey2
    character*(*)::sKey,sFileNP
    integer :: fileid
    integer, INTent(out) :: error
    character(len=200)::sTemp,sTemp2
    integer :: ICOL=1,ISTART,ISTOP,NCODE=1,N,IOUT,IN
    integer :: ISTART_in,ISTOP_in

    XXWReadLineInt=0 ! Added by Xianwu Xue 2011.4.21

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
            read(fileid,*,iostat=error)sTemp,sTemp2,XXWReadLineInt
            close(fileid)
            return
        end if
    end do

    close(fileid)

    return
end function

double precision function XXWReadLineDbl(sFileNP,sKey,error)
    implicit none
    character(len=200)::sKey2
    character*(*)::sKey,sFileNP
    integer :: fileid
    integer, INTent(out) :: error
    character(len=200)::sTemp,sTemp2
    integer :: ICOL=1,ISTART,ISTOP,NCODE=1,N,IOUT,IN
    integer :: ISTART_in,ISTOP_in

    XXWReadLineDbl=0 ! Added by Xianwu Xue 2011.4.21

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
            read(fileid,*,iostat=error)sTemp,sTemp2,XXWReadLineDbl
            close(fileid)
            return
        end if
    end do

    close(fileid)

    return
end function

character(len=200) function XXWReadLineStr(sFileNP,sKey,error)
    implicit none
    character(len=200)::sKey2
    character*(*)::sKey,sFileNP
    integer :: fileid
    integer, INTent(out) :: error
    character(len=200)::sTemp,sTemp2
    integer :: ICOL=1,ISTART,ISTOP,NCODE=1,N,IOUT,IN
    integer :: ISTART_in,ISTOP_in

    XXWReadLineStr=""

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
            read(fileid,*,iostat=error)sTemp,sTemp2,XXWReadLineStr
            close(fileid)
            return
        end if
    end do

    close(fileid)

    return
end function