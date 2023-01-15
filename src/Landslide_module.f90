

subroutine Landslide_module()
    use CREST_Basic
    use Landslide_Basic
    use LandslideModel_parameters
    use SoilDownscale_Basic
    use OMP_LIB

    implicit none

    integer :: i, j, i_tile, i_ellipsoid, i_window, j_window,&
                 i_return, i_LandMap, j_LandMap, random_center
    integer :: iLoc_coarse, jLoc_coarse, count_ellipse
    integer :: x_center, y_center, Npixel_ellipse
    integer :: SOIL_CODE
    integer, allocatable :: ellipse_i(:), ellipse_j(:)
    integer, allocatable :: ValidPixel_matrix_tile(:,:)
    integer, allocatable :: ValidPixel_matrix_ResiduleTile(:,:)

    double precision :: slope_center, aspect_center, z_center, project_length
    double precision :: a_e, a_e_pre, b_e, c_e
    double precision :: main_slope, main_aspect
    double precision :: LandTime_start, LandTime_end
    ! create a temporary matrix (should contain this ellipse)
    ! use the window extend value to realize this
    double precision :: x_single_all(window_extend*2+1, window_extend*2+1)
    double precision :: y_single_all(window_extend*2+1, window_extend*2+1)
    double precision :: z_single_all(window_extend*2+1, window_extend*2+1)
    double precision :: SM_fine_single(window_extend*2+1, window_extend*2+1)
    double precision :: slope_single_all(window_extend*2+1, window_extend*2+1)
    double precision :: aspect_single_all(window_extend*2+1, window_extend*2+1)

    double precision :: all_x_transition(window_extend*2+1, window_extend*2+1)
    double precision :: all_y_transition(window_extend*2+1, window_extend*2+1)
    double precision :: judge_location(window_extend*2+1, window_extend*2+1)

    double precision :: z_grid, x_transition, y_transition
    double precision :: slope_InEllipse, aspect_InEllipse
    double precision :: x_ellipsoid, y_ellipsoid, z_ellipsoid, z_grid_InSlip
    double precision, allocatable :: D_raster(:), SM_InEllipse(:), z_ellipsoid_In(:)
    double precision :: FS_3D, volumn_sum, area_sum
    ! the tiles include the all tiles in ValidPixel_matrix and the final one
    ! in ValidPixel_matrix_residual
    allocate(ValidPixel_matrix_tile(Npixel_tile, 2))

    allocate(ValidPixel_matrix_ResiduleTile(Npixel_residual, 2))

    ValidPixel_matrix_tile = g_NoData_Value
    ValidPixel_matrix_ResiduleTile = g_NoData_Value

    ! record the time
    LandTime_start = OMP_get_wtime()
    !-------------landslide parallel region start-----------------

    !$OMP PARALLEL SHARED(ValidPixel_matrix, ValidPixel_matrix_residual,total_tile_number,&
    !$OMP&  ellipsoid_number, Npixel_tile, Npixel_residual, &
    !$OMP&  min_ae, max_ae, min_be, max_be, min_ce, max_ce, window_extend, &
    !$OMP&  x_all, y_all, g_DEM_fine, g_slope_fine, g_aspect_fine, g_soil, g_SM_fine, &
    !$OMP&  g_NCols, g_NCols_Land,g_CellSize_Land, g_yllCorner, g_CellSize, &
    !$OMP&  g_xllCorner_Land, g_yllCorner_Land, g_xllCorner, g_NoData_Value, &
    !$OMP&  g_cal_count, g_unstable_count, g_failure_volume, g_failure_area, &
    !$OMP&  g_FS_3D, c_e) DEFAULT(PRIVATE)
    !$OMP DO
    do i_tile = 1, (total_tile_number + 1)
!        print *, i_tile
!        print '("Thread: ", i0)', omp_get_thread_num()
        if (i_tile <= total_tile_number) then
            ValidPixel_matrix_tile = ValidPixel_matrix(i_tile,:,:)
        else
            ValidPixel_matrix_ResiduleTile = ValidPixel_matrix_residual
        end if


        do i_ellipsoid = 1, ellipsoid_number

            ! get the random center in tile
            if (i_tile <= total_tile_number) then
                call random_uniform_int(0, Npixel_tile, random_center)
                ! make sure the random value won't exceed the range
                if (random_center > Npixel_tile) then
                    random_center = Npixel_tile
                end if
                if (random_center == 0) then
                    random_center = 1

                end if
            else
                call random_uniform_int(0, Npixel_residual, random_center)
                ! make sure the random value won't exceed the range
                if (random_center > Npixel_residual) then
                    random_center = Npixel_residual
                end if
                if (random_center == 0) then
                    random_center = 1
                end if
            end if


            ! get the random length and width of ellipsoid
            call random_uniform_float(min_ae, max_ae, a_e_pre)
            call random_uniform_float(min_be, max_be, b_e)
            call random_uniform_float(min_ce, max_ce, c_e)

            ! Note: i, j in ValidPixel_matrix_tile is the index
            ! in g_mask_fine, i.e.,
            ! i-->(0, g_NRows_Land - 1)
            ! j-->(0, g_NCols_Land - 1)
            if (i_tile <= total_tile_number) then
                i = ValidPixel_matrix_tile(random_center,1)
                j = ValidPixel_matrix_tile(random_center,2)
            else
                i = ValidPixel_matrix_ResiduleTile(random_center,1)
                j = ValidPixel_matrix_ResiduleTile(random_center,2)
            end if

            ! locate to ellipse center
            x_center = x_all(j,i)
            y_center = y_all(j,i)
            z_center = g_DEM_fine(j,i)

            ! unit: degree
            slope_center = g_slope_fine(j,i)
            aspect_center = g_aspect_fine(j,i)


            ! get the  project_length: m
            project_length = a_e_pre * COSD(slope_center)

            ! get the soil type from the USDA soil code
            iLoc_coarse = NINT( g_NCols - (((g_NCols_Land-i-1) * &
                    g_CellSize_Land + g_yllCorner_Land) &
                    - g_yllCorner) / g_CellSize - 1)

            jLoc_coarse = NINT( ((j + 1) * g_CellSize_Land + &
                    g_xllCorner_Land - g_xllCorner) &
                    / g_CellSize - 1)

            SOIL_CODE = g_soil(jLoc_coarse, iLoc_coarse)



            ! invalid soil code
            if (SOIL_CODE == 0 .or. SOIL_CODE > 12) then
                cycle
            end if


            ! create a temporary matrix (should contain this ellipse)
            SM_fine_single = g_SM_fine((j-window_extend):(j+window_extend),&
                    (i-window_extend) : (i+window_extend))
            ! check that how many invalid pixcel in SM_fine_single
            if ( count(SM_fine_single == g_NoData_Value) > 0) then
                ! More than half of the elements are invalid
                ! then go to the next loop
                cycle
            end if

            x_single_all = x_all((j-window_extend) : (j+window_extend),&
                    (i-window_extend) : (i+window_extend))

            y_single_all = y_all((j-window_extend) : (j+window_extend),&
                    (i-window_extend) : (i+window_extend))

            z_single_all = g_DEM_fine((j-window_extend) : (j+window_extend),&
                    (i-window_extend) : (i+window_extend))

            slope_single_all = g_slope_fine((j-window_extend) : (j+window_extend),&
                    (i-window_extend) : (i+window_extend))

            aspect_single_all = g_aspect_fine((j-window_extend) : (j+window_extend),&
                    (i-window_extend) : (i+window_extend))


            all_x_transition = (x_single_all-x_center) * COSD(aspect_center) &
                            + (y_single_all-y_center) * SIND(aspect_center)

            all_y_transition = (y_single_all-y_center) * COSD(aspect_center) &
                            - (x_single_all-x_center) * SIND(aspect_center)

            !------find the involved ellipse---------
            ! jugge_location is the index in small single region which can
            ! include a single ellipse
            judge_location = all_x_transition ** 2 / (project_length ** 2) + &
                            all_y_transition ** 2 / (b_e ** 2)
            Npixel_ellipse = count(judge_location<1)

            main_slope = SUM(slope_single_all, MASK = judge_location<1) / Npixel_ellipse
            main_aspect = SUM(aspect_single_all, MASK = judge_location<1) / Npixel_ellipse
            a_e = project_length/COSD(main_slope)

            if (MINVAL(SM_fine_single, MASK = judge_location<1) < 0) then
                ! this means the ellipse is invalid
                cycle
            end if
            !---------------------------get Ellipsoidal region----------------
            allocate(D_raster(Npixel_ellipse))
            allocate(ellipse_i(Npixel_ellipse))
            allocate(ellipse_j(Npixel_ellipse))
            allocate(SM_InEllipse(Npixel_ellipse))
            allocate(z_ellipsoid_In(Npixel_ellipse))
            ! initial the allocated array
            D_raster = g_NoData_Value
            ellipse_i = g_NoData_Value
            ellipse_j = g_NoData_Value
            SM_InEllipse = g_NoData_Value
            z_ellipsoid_In = g_NoData_Value
            ! start the loop in ellipse
            count_ellipse = 1
            do i_window = 1, window_extend*2+1
                do j_window = 1, window_extend*2+1

                    if (judge_location(j_window, i_window) > 1) then
                        ! only pixel in ellipse is interested
                        cycle
                    end if
                    ! Note:
                    ! ellipse_i-->(1, window_extend*2+1)
                    ! ellipse_j-->(1, window_extend*2+1)
                    ellipse_i(count_ellipse) = i_window
                    ellipse_j(count_ellipse) = j_window

                    ! count the pixels that is calculated
                    ! calculate the return index
                    i_LandMap = i + (i_window - window_extend - 1)
                    j_LandMap = j + (j_window - window_extend - 1)

                    g_cal_count(j_LandMap, i_LandMap) = g_cal_count(j_LandMap, i_LandMap) + 1

                    z_grid = z_single_all(j_window, i_window)
                    x_transition = all_x_transition(j_window, i_window)
                    y_transition = all_y_transition(j_window, i_window)


                    ! calculate the coordinate in ellipsoid system
                    x_ellipsoid = x_transition / COSD(main_slope)
                    y_ellipsoid = y_transition
                    ! error check
                    if (x_ellipsoid**2/a_e**2+y_ellipsoid**2/b_e**2 - 1 > 0.01) then
                        write(*,*) "check the a_e and delta_x"
                        stop
                    end if
                    c_e = 1
                    z_ellipsoid = (-2*x_ellipsoid/(a_e**2)-SQRT((2*x_ellipsoid/(a_e**2))&
                            **2-4*(1/a_e**2+1/(c_e**2*(TAND(main_slope))**2))*&
                            (x_ellipsoid**2/a_e**2+y_ellipsoid**2/b_e**2-1)))&
                            /(2*(1/a_e**2+1/(c_e**2*(TAND(main_slope))**2))) &
                            / TAND(main_slope)


                    z_ellipsoid_In(count_ellipse) = z_ellipsoid
                    z_grid_InSlip = z_center + (z_ellipsoid - x_transition &
                                    * SIND(main_slope)) / COSD(main_slope)

                    z_single_all(j_window, i_window) = z_grid_InSlip

                    D_raster(count_ellipse) = z_grid - z_grid_InSlip
                    SM_InEllipse(count_ellipse) = SM_fine_single(j_window, i_window)

                    count_ellipse = count_ellipse + 1

                end do
            end do

            ! -------------------------3D stability loop-------------------

!            write (*,*) "z_ellipsoid",z_ellipsoid
!
!            stop

            call Stability3D_VG(FS_3D, volumn_sum, area_sum, SOIL_CODE, D_raster, SM_InEllipse&
                    , Npixel_ellipse, z_single_all, z_ellipsoid_In, &
                    ellipse_i, ellipse_j, window_extend, &
                    main_aspect, main_slope, c_e)


            ! update FS_3D value in regional FS map

            do i_return = 1, Npixel_ellipse
                ! calculate the return index
                i_LandMap = i + (ellipse_i(i_return) - window_extend - 1)
                j_LandMap = j + (ellipse_j(i_return) - window_extend - 1)

                ! count the unstable situation
                if (FS_3D < 1) then

                    g_unstable_count(j_LandMap, i_LandMap) = &
                            g_unstable_count(j_LandMap, i_LandMap) + 1
                    ! update the volume map (seek the maximum volume)
                    if (g_failure_volume(j_LandMap, i_LandMap) < volumn_sum ) then
                        g_failure_volume(j_LandMap, i_LandMap) = volumn_sum
                    end if

                    if (g_failure_area(j_LandMap, i_LandMap) < area_sum ) then
                        g_failure_area(j_LandMap, i_LandMap) = area_sum
                    end if

                end if

                if (g_FS_3D(j_LandMap, i_LandMap) > FS_3D) then
                    ! that is to say this pixel has been calculated already
                    ! and the FS value is smaller than current landslide.
                    ! So keep the old value and go to the next loop


                    g_FS_3D(j_LandMap, i_LandMap) =  FS_3D


                end if


            end do


            deallocate(D_raster)
            deallocate(ellipse_i)
            deallocate(ellipse_j)
            deallocate(SM_InEllipse)
            deallocate(z_ellipsoid_In)


        end do

    end do
    !$OMP END DO
    !$OMP END PARALLEL
    !-------------Landslide parallel region end-----------------
    LandTime_end = OMP_get_wtime()
    LandRunTime = LandRunTime + (LandTime_end - LandTime_start)

    ! return NoData_value to g_FS_3D
    where (g_FS_3D == - g_NoData_Value)
        g_FS_3D = g_NoData_Value
    end where

    where (g_cal_count > 0)
        g_probability = g_unstable_count / g_cal_count
    end where

    deallocate(ValidPixel_matrix_tile)
    deallocate(ValidPixel_matrix_ResiduleTile)

    return
end subroutine Landslide_module



! this subroutine return the random value ranges from a and b
! for example, given a = 0 and b = 3, the output random values are:
! x = 1, x = 2, x = 3 (uniform distribution)
subroutine random_uniform_int(a,b,x)
    implicit none
    integer, intent(in) :: a,b
    integer, intent(out) :: x
    real :: u, r
    call random_number(r)
    u = 1 - r
    x = INT((b-a)*u + a) + 1
    return
end subroutine random_uniform_int

subroutine random_uniform_float(a,b,x)
    implicit none
    double precision, intent(in) :: a,b
    double precision, intent(out) :: x
    double precision :: u, r
    call random_number(r)
    u = 1 - r
    x = (b-a)*u + a
    return
end subroutine random_uniform_float

