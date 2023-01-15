module CREST_Project
    implicit none
    character(len=14), parameter:: g_CREST_Version = "V1.0"
    character(len=19) :: g_strD
    character(len = 20) :: g_CS
    character(len=200):: g_ProjectName,g_ProjectWS
    character(len=200):: g_PrjNP
    integer :: g_CREST_LogFileID
    character(len=1) :: g_TimeMark !y(year);m(month);d(day);h(hour);u(minute);s(second)
    integer :: g_TimeStep
    integer			 :: g_StartDate(1:6)

    integer			 :: g_NumWarmup,g_ITMax

    integer			 :: g_WarmupDate(1:6)
    integer			 :: g_EndDate(1:6)
    logical			 :: g_LoadState
    logical			 :: g_SaveState

    character(len=20):: g_sRunStyle
    integer			 :: g_RunStyle ! simu--1; cali--2; real--3; repe --4; fore ---6
    integer			 :: g_ModelCore
    character(len=20):: g_sModelCore
    character(len=10) :: g_BasicFormat
    character(len=200):: g_BasicPath

    character(len=10) :: g_SoilDownscaleFormat
    character(len=200):: g_SoilDownscalePath

    character(len=10) :: g_LandslideFormat
    character(len=200):: g_LandslidePath

    character(len=10) :: g_ParamFormat
    character(len=200):: g_ParamPath

    character(len=10) :: g_StateFormat
    character(len=200):: g_StatePath

    character(len=10) :: g_ICSFormat
    character(len=200):: g_ICSPath

    character(len=10) :: g_OBSFormat !Leave it to use in furture
    character(len=200):: g_OBSPath

    character(len=10) :: g_RainFormat
    character(len=200):: g_RainPath

    character(len=10) :: g_PETFormat
    character(len=200):: g_PETPath

    character(len=10) :: g_ResultFormat
    character(len=200):: g_ResultPath

    character(len=10) :: g_CalibFormat
    character(len=200):: g_CalibPath


    character(len=10) :: g_ForecastFormat
    character(len=200):: g_ForecastPath

    integer			 :: g_NOutPixs

    type:: CREST_OutPix
        character(len=200):: Name
        integer			 :: Col
        integer			 :: Row
        logical			 :: bIsOut
        integer, allocatable :: Mask(:,:)

        double precision, allocatable :: Rain(:)
        double precision, allocatable :: PET(:)
        double precision, allocatable :: EPot(:)
        double precision, allocatable :: EAct(:)
        double precision, allocatable :: W(:)
        double precision, allocatable :: SM(:) ! Soil Moisture
        double precision, allocatable :: R(:)
        double precision, allocatable :: ExcS(:),ExcI(:)
        double precision, allocatable :: RS(:),RI(:)
        double precision, allocatable :: RObs(:)
        logical :: HasOBS
    end type CREST_OutPix

    type(CREST_OutPix), allocatable :: g_tOutPix(:)


    type:: CREST_Outlet
        character(len=200):: Name
        integer :: Col
        integer :: Row
        logical :: bIsOut
        integer, allocatable :: Mask(:,:)

        double precision, allocatable :: Rain(:)
        double precision, allocatable :: PET(:)
        double precision, allocatable :: EPot(:)
        double precision, allocatable :: EAct(:)
        double precision, allocatable :: W(:)
        double precision, allocatable :: SM(:) ! Soil Moisture
        double precision, allocatable :: R(:)
        double precision, allocatable :: ExcS(:),ExcI(:)
        double precision, allocatable :: RS(:),RI(:)
        double precision, allocatable :: RObs(:)

        logical					 :: HasOutlet
        logical					 :: HasOBS
    end type CREST_Outlet

    type(CREST_Outlet) :: g_tOutlet

    integer :: g_NOutDTs
    integer, allocatable :: g_OutDTIndex(:)

    character(len = 10)::g_sGOVarName(0:14) !Grid Output Variable Name
    data g_sGOVarName /"Rain","PET","EPot","EAct","W","SM",  &
            "R","ExcS","ExcI","RS","RI", "FS3D", "PF","FVolume", "FArea"/
    logical, allocatable :: g_bGOVar(:)

    !For Lake======================================
    integer :: g_NLakes
    integer, allocatable :: g_LakeMask(:,:)


    !For Forecast
    integer			 :: g_QPFBaseDate(1:6)
    character(len=14) :: g_sQPFBaseDate


    ! User Defined Functions
    integer, external :: XXWReadLineInt
    double precision, external :: XXWReadLineDbl
    character(len=200), external :: XXWReadLineStr
    logical, external :: XXWReadLineBln
    logical, external :: InBasin
    logical, external :: InBasin2

    integer, external :: myDdIFf
    character(len=14),external :: myDtoStr
    integer, external :: myDCompare,myDCompare2
    logical, external :: myDequal
    character(len=6), external:: PosIntMsg
end module
!########################################################
module CREST_Param
    implicit none

    type CREST_Params
        double precision, allocatable :: RainFact(:,:)
        double precision, allocatable :: Ksat(:,:)
        double precision, allocatable :: WM(:,:)
        double precision, allocatable :: B(:,:)
        double precision, allocatable :: IM(:,:)
        double precision, allocatable :: KE(:,:)
        double precision, allocatable :: coeM(:,:)

        double precision, allocatable :: expM(:,:)
        double precision, allocatable :: coeR(:,:)
        double precision, allocatable :: coeS(:,:)
        double precision, allocatable :: KS(:,:)
        double precision, allocatable :: KI(:,:)

    end type CREST_Params

    type(CREST_Params):: g_tParams_Cali
    type(CREST_Params):: g_tParams
    type(CREST_Params):: g_tParamsAdj_Cali

end module


!########################################################
module CREST_ICS
    implicit none

    double precision, allocatable :: g_WU0(:,:)
    double precision, allocatable :: g_SS0(:,:)
    double precision, allocatable :: g_SI0(:,:)

end module

!########################################################
module CREST_Calib_SCEUA
    implicit none
    integer			 :: g_iSeed
    integer			 :: g_maxn
    integer			 :: g_kstop
    double precision   :: g_pcento
    integer			 :: g_ngs
    integer			 :: g_nopt

    integer,allocatable::g_RegMask(:,:)
    integer			 :: g_RegNum
    integer			 :: g_NCalibStas

    type:: CREST_CalibStations
        character(len=200):: Name
        integer			 :: Value
        integer			 :: Col
        integer			 :: Row
        logical			 :: bIsOut

        integer :: Area !Area or Number of the Cells in FAC
        integer, allocatable :: Mask(:,:)

        double precision, allocatable :: RObs(:),RSim(:)

        double precision, allocatable :: a(:)
        double precision, allocatable :: bl(:)
        double precision, allocatable :: bu(:)
        double precision, allocatable :: x(:)

    end type CREST_CalibStations

    type(CREST_CalibStations), allocatable :: g_tCalibSta(:)

    character(len=10)::g_sParamName(1:12)
    data g_sParamName /"RainFact","Ksat","WM","B","IM", &
            "KE","coeM","expM","coeR","coeS", &
            "KS","KI"/

end module
!########################################################
module CREST_Basic
    implicit none

    integer			 :: g_NCols
    integer			 :: g_NRows
    integer			 :: Nthread_hydro
    integer			 :: N_Subbasin
    double precision :: g_xllCorner
    double precision :: g_yllCorner
    double precision :: g_CellSize
    double precision :: g_NoData_Value

    double precision, allocatable :: g_DEM(:,:)
    integer, allocatable :: g_Mask(:,:) !Optional
    integer, allocatable :: g_FDR(:,:)
    integer, allocatable :: g_FAC(:,:)
    integer, allocatable :: g_Stream(:,:)

    double precision, allocatable :: g_GridArea(:,:)
    double precision, allocatable :: g_NextLen(:,:)

    integer, allocatable		  :: g_NextR(:,:)
    integer, allocatable		  :: g_NextC(:,:)
    double precision, allocatable :: g_Slope(:,:)
    double precision, allocatable :: g_Slope_angle(:,:) ! unit: degree
    ! declare for parallel setting
    integer, allocatable :: Subbasin_assemble(:,:,:)
    integer, allocatable :: ChannelIndex_Rows(:), ChannelIndex_Cols(:)
    integer, allocatable :: NextChannel(:)
    integer, allocatable :: Channel_connect(:)
    integer :: Npixel_channel
    double precision :: HydroRunTime

end module

module SoilDownscale_Basic
    implicit none
    double precision, allocatable :: g_aspect_coarse(:,:)
    double precision, allocatable :: g_curvature_coarse(:,:)
    double precision, allocatable :: g_curvature_fine(:,:)
    double precision, allocatable :: g_TWI_coarse(:,:)
    double precision, allocatable :: g_TWI_fine(:,:)
    double precision, allocatable :: g_SM_fine(:,:)


end module SoilDownscale_Basic

module Landslide_Basic
    implicit none

    integer :: g_NCols_Land
    integer :: g_NRows_Land
    integer :: Npixel_tile, Npixel_residual, ellipsoid_number
    integer :: window_extend
    double precision :: g_xllCorner_Land
    double precision :: g_yllCorner_Land
    double precision :: g_CellSize_Land
    double precision, allocatable :: g_aspect_fine(:,:)
    double precision, allocatable :: g_slope_fine(:,:)
    double precision, allocatable :: g_DEM_fine(:,:)
    double precision, allocatable :: x_all(:,:)
    double precision, allocatable :: y_all(:,:)
    double precision, allocatable :: g_FS_3D(:,:)
    double precision, allocatable :: g_failure_volume(:,:)
    double precision, allocatable :: g_failure_area(:,:)
    double precision, allocatable :: g_probability(:,:)
    double precision, allocatable :: g_cal_count(:,:)
    double precision, allocatable :: g_unstable_count(:,:)
    double precision :: LandRunTime
    integer, allocatable :: g_mask_fine(:,:)


    integer, allocatable :: g_soil(:,:)
    integer, allocatable :: ValidPixel_matrix(:,:,:)
    integer, allocatable :: ValidPixel_matrix_residual(:,:)


end module Landslide_Basic

! Initial the landslide modeling parameters
module LandslideModel_parameters

    implicit none

    integer			 :: ellipse_density
    integer			 :: total_tile_number
    integer	         :: Nthread_Land
    double precision :: min_ae, max_ae
    double precision :: min_be, max_be
    double precision :: min_ce, max_ce
    ! CellSize_LandInM: cell size of landslide map: m
    double precision :: CellSize_LandInM
    real, parameter :: pi = 3.14159
    real, parameter :: gamma_w = 9.81 ! KN/m^3
    type LandslideModel_params
        ! added by Guoding Chen: serving Landslide model

        double precision, allocatable :: Slope_LandslideModel(:,:)
        ! state/initial the soil ralated variables; added by cgd

        double precision, allocatable :: Soil_USDA_code(:,:)
        double precision, allocatable :: Cohesion(:,:)
        double precision, allocatable :: Theta_r(:,:)
        double precision, allocatable :: Theta_s(:,:)
        double precision, allocatable :: Dry_Unit_Weight(:,:)
        double precision, allocatable :: FRI(:,:)
        double precision, allocatable :: Porosity(:,:)
    end type LandslideModel_params

    type(LandslideModel_params):: g_LandParams

end module LandslideModel_parameters

module const
    ! SP: 単精度(4), DP: 倍精度(8)
    integer,     parameter :: SP = kind(1.0)
    integer(SP), parameter :: DP = selected_real_kind(2 * precision(1.0_SP))
end module const

module comp
    use const
    implicit none
    private
    public :: calc_reg_line

contains
    ! 単回帰直線計算
    !
    ! :param(in)  real(8) x(:): 説明変数配列
    ! :param(in)  real(8) y(:): 目的変数配列
    ! :param(out) real(8)    a: 切片
    ! :param(out) real(8)    b: 傾き
    subroutine calc_reg_line(x, y, a, b)
        implicit none
        real(DP), intent(in)  :: x(:), y(:)
        real(DP), intent(out) :: a, b
        integer(SP) :: size_x, size_y, i
        real(DP)    :: sum_x, sum_y, sum_xx, sum_xy

        size_x = size(x)
        size_y = size(y)
        if (size_x == 0 .or. size_y == 0) then
            print *, "[ERROR] array size == 0"
            stop
        end if
        if (size_x /= size_y) then
            print *, "[ERROR] size(X) != size(Y)"
            stop
        end if

        sum_x  = sum(x)
        sum_y  = sum(y)
        sum_xx = sum(x * x)
        sum_xy = sum(x * y)
        a = (sum_xx * sum_y - sum_xy * sum_x) &
                & / (size_x * sum_xx - sum_x * sum_x)
        b = (size_x * sum_xy - sum_x * sum_y) &
                & / (size_x * sum_xx - sum_x * sum_x)
    end subroutine calc_reg_line
end module comp