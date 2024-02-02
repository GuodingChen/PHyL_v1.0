! Created by Guoding Chen on 2022/8/7.

subroutine Parallel_hydro_pre()

    use CREST_Project
    use CREST_Basic

    implicit none
    integer :: i, j, ii, jj, i_next, j_next,  NextChannel_index, NextChannel_index_i
    integer :: channel_count, i_basin, i_channel_pixel, j_channel_pixel
    integer :: FAC_divide
    integer :: FAC_Subbasin_outlet, Index_Subbasin_outlet, outlet_index
    logical :: bIsError
    character(len=3):: i_basin_str

    integer, allocatable :: Channel_FAC(:), Residual_Channel_FAC(:)
    integer,allocatable :: g_FDR_forSub(:,:), g_SubMask(:,:)
    ! get the cell number of stream first, and then set the RAM
    write(*,"(2X,A)")  "start parallel preprocessing"
    Npixel_channel = count(g_Stream == 1)


    allocate(ChannelIndex_Rows(1:Npixel_channel))
    allocate(ChannelIndex_Cols(1:Npixel_channel))
    allocate(Channel_FAC(1:Npixel_channel))

    allocate(Residual_Channel_FAC(1:Npixel_channel))
    allocate(NextChannel(1:Npixel_channel))
    allocate(Channel_connect(1:N_Subbasin))
    allocate(g_FDR_forSub(0:g_NCols-1,0:g_NRows-1))
    allocate(g_SubMask(0:g_NCols-1,0:g_NRows-1))
    allocate(Subbasin_assemble(N_Subbasin, 0:g_NCols-1, 0:g_NRows-1))

    g_FDR_forSub = g_FDR
    g_SubMask = g_Mask

    ! record the channel cell and associated index
    channel_count = 1
    do i=0,g_NRows-1
        do j=0,g_NCols-1
            if( g_Stream(j,i) == 1 )then
                ChannelIndex_Rows(channel_count) = i
                ChannelIndex_Cols(channel_count) = j
                Channel_FAC(channel_count) = g_FAC(j,i)
                channel_count = channel_count + 1

            end if
        end do
    end do

    ! calculate the next routing index for all stream cells

    ! record the index for outlet first, and it has no next cell in stream within considered mask
    outlet_index = maxloc(Channel_FAC, 1)

    do i_channel_pixel = 1, Npixel_channel
        if (i_channel_pixel /= outlet_index) then

            i_next = g_NextR(ChannelIndex_Cols(i_channel_pixel), ChannelIndex_Rows(i_channel_pixel))
            j_next = g_NextC(ChannelIndex_Cols(i_channel_pixel), ChannelIndex_Rows(i_channel_pixel))
            ! the next routing cell must can be found in Stream
            do j_channel_pixel = 1, Npixel_channel
                if(i_next == ChannelIndex_Rows(j_channel_pixel) .and. &
                        j_next == ChannelIndex_Cols(j_channel_pixel))then

                    ! the next routing cell locates at "j_channel_pixel" index number
                    NextChannel(i_channel_pixel) = j_channel_pixel

                end if

            end do
        else 
            NextChannel(i_channel_pixel) = 2 * Npixel_channel
        end if 
    end do
   
    ! creat the sub-basins according to the parameter of N_subbasin
    FAC_divide = N_Subbasin
    Residual_Channel_FAC = 0
    do i_basin = 1, N_Subbasin - 1

        write(i_basin_str , '(i3)') i_basin

        FAC_Subbasin_outlet = maxval(Channel_FAC) / FAC_divide

        where(Channel_FAC > 0)
            Residual_Channel_FAC = Channel_FAC - FAC_Subbasin_outlet
        end where

        where(Residual_Channel_FAC < 0)
            Residual_Channel_FAC = -Residual_Channel_FAC
        end where

        where(Residual_Channel_FAC == 0)
            Residual_Channel_FAC = -g_NoData_Value
        end where

        Index_Subbasin_outlet = minloc(Residual_Channel_FAC, 1)

        ! determine the FAC value at basin outlet (FAC_mac is not divisible by Ncore)
        FAC_Subbasin_outlet = Channel_FAC(Index_Subbasin_outlet)
        ! get the coordination of the identified basin outlet pixel
        ii = ChannelIndex_Rows(Index_Subbasin_outlet)
        jj = ChannelIndex_Cols(Index_Subbasin_outlet)
        ! save the channel connection in
        Channel_connect(i_basin) = Index_Subbasin_outlet

        ! get the sunbasin mask based on Sub_outlet index (jj, ii)
        call GetMask(g_NCols,g_NRows, jj, ii, &
                g_NoData_Value,g_FDR_forSub,g_NextC,g_NextR,  &
                g_SubMask,InBasin)


        call WriteMatrixFile_Int(trim(g_ResultPath)//"Subbasin_#"//adjustl(i_basin_str),  &
                g_SubMask, g_NCols,g_NRows,   &
                g_XLLCorner,g_YLLCorner, g_CellSize, g_NoData_Value, &
                bIsError,g_BasicFormat)

        ! up date the FAC value in the loop of stream cell

        ! there is a principle strictly followed:
        ! each cell in the basin will eventually routes to the outlet point

        ! Then, all the cells that routes to subbasin outlet are excluded from 
        ! the whole basin by assign the 0 FAC values 
        do i_channel_pixel = 1, Npixel_channel
            
            NextChannel_index = NextChannel(i_channel_pixel)

            ! This means that all the cells passed by cell "i_channel_pixel" 
            ! and finally route to the subbasin outlet are found 
            do while( NextChannel_index /= NextChannel(outlet_index) )
                
                if(NextChannel_index == Index_Subbasin_outlet)then
                    Channel_FAC(i_channel_pixel) = g_NoData_Value
                    exit
                end if

                NextChannel_index = NextChannel(NextChannel_index)
            end do
        end do
        
        ! All downstream cells corresponding to the subbasin need 
        ! to update their FAC values.
        Channel_FAC(Index_Subbasin_outlet) = Channel_FAC(Index_Subbasin_outlet) - FAC_Subbasin_outlet
        NextChannel_index = NextChannel(Index_Subbasin_outlet)
        do while(NextChannel_index /= NextChannel(outlet_index))

            Channel_FAC(NextChannel_index) = Channel_FAC(NextChannel_index) - FAC_Subbasin_outlet

            NextChannel_index = NextChannel(NextChannel_index)

            if(NextChannel_index == NextChannel(outlet_index)) then
                exit
            end if

        end do

        ! update the mask file
        where(g_SubMask == 1)
            g_FDR_forSub = g_NoData_Value
        end where

        FAC_divide = FAC_divide - 1
        Residual_Channel_FAC = 0

        Subbasin_assemble(i_basin, :, :) = g_SubMask
        write(*,"(2X,A)")  "Generating Subbasin_#"//adjustl(i_basin_str)//"......done"
    end do
    ! creat the last subbasin using the default outlet (g_tOutlet%Row, g_tOutlet%Col)

    write(i_basin_str , '(i3)') N_Subbasin
    Channel_connect(N_Subbasin) = outlet_index
    call GetMask(g_NCols,g_NRows, g_tOutlet%Col, g_tOutlet%Row, &
            g_NoData_Value,g_FDR_forSub,g_NextC,g_NextR,  &
            g_SubMask,InBasin)


    call WriteMatrixFile_Int(trim(g_ResultPath)//"Subbasin_#"//adjustl(i_basin_str),  &
            g_SubMask, g_NCols,g_NRows,   &
            g_XLLCorner,g_YLLCorner, g_CellSize, g_NoData_Value, &
            bIsError,g_BasicFormat)
    Subbasin_assemble(N_Subbasin, :, :) = g_SubMask
    write(*,"(2X,A)")  "Generating Subbasin_#"//adjustl(i_basin_str)//"......done"
    write(*,"(2X,A)")  "All Subbasins have been generated."
    !write(*,*) Channel_connect
    deallocate(Channel_FAC)
    deallocate(Residual_Channel_FAC)
    deallocate(g_FDR_forSub)
    deallocate(g_SubMask)


    return
end subroutine Parallel_hydro_pre



subroutine Parallel_land_pre()
    use Landslide_Basic
    use LandslideModel_parameters
    use CREST_Basic
    implicit none
    integer ::  i, j, Ntile_count, Npixel_count, Npixel_count_residual
    double precision :: Npixel_land, As_tile


    Npixel_land = count(g_mask_fine == 1)

    Npixel_tile = INT(Npixel_land / total_tile_number)

    Npixel_residual = Npixel_land - total_tile_number * Npixel_tile
    ! calculate the area of each tile (m^2)
    As_tile = Npixel_tile * CellSize_LandInM ** 2

    ! calculate the total potential number of landslide in each tile
    ellipsoid_number = INT(ellipse_density * 16 * As_tile / &
            (pi * (min_ae + max_ae) * (min_be + max_be)))
    ! define the window extend for following calculation
    if (max_ae > max_be) then
        window_extend = NINT(max_ae / CellSize_LandInM * 2)
    else
        window_extend = NINT(max_be / CellSize_LandInM * 2)
    end if

    allocate(ValidPixel_matrix(total_tile_number, Npixel_tile, 2))

    allocate(ValidPixel_matrix_residual(Npixel_residual, 2))


    ValidPixel_matrix = g_NoData_Value
    ValidPixel_matrix_residual = g_NoData_Value

    Ntile_count = 1
    Npixel_count = 1
    Npixel_count_residual = 1

    do i=0, g_NRows_Land - 1
        do j=0, g_NCols_Land - 1

            if(g_mask_fine(j,i) == g_NoData_Value)then
                cycle
            end if

            if(Npixel_count > Npixel_tile) then
                Ntile_count = Ntile_count + 1
                Npixel_count = 1
            end if

            if (Ntile_count <= total_tile_number) then
                ValidPixel_matrix(Ntile_count, Npixel_count, 1) = i
                ValidPixel_matrix(Ntile_count, Npixel_count, 2) = j
                Npixel_count = Npixel_count + 1
            else
                ValidPixel_matrix_residual(Npixel_count_residual, 1) = i
                ValidPixel_matrix_residual(Npixel_count_residual, 2) = j
                Npixel_count_residual = Npixel_count_residual + 1
            end if

        end do

    end do
    ! prepare the Cartesian coordinate system
    allocate(x_all(0:g_NCols_Land-1,0:g_NRows_Land-1))
    allocate(y_all(0:g_NCols_Land-1,0:g_NRows_Land-1))
    x_all = g_NoData_Value
    y_all = g_NoData_Value

    do i=0, g_NRows_Land - 1
        x_all(:,i) = i * CellSize_LandInM
    end do

    do i=0, g_NCols_Land - 1
        y_all(i,:) = i * CellSize_LandInM
    end do

    return
end subroutine Parallel_land_pre




