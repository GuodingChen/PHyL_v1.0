
subroutine SoilDownscale_pre(SM_coarse)
    use CREST_Basic
    use CREST_Project ! not necessary
    use SoilDownscale_Basic
    use Landslide_Basic
    use const
    use comp
    implicit none

    integer :: i, j, Npixel_coarse, Npixel_fine, Npixel_coarse_count, iLoc_coarse, jLoc_coarse
    integer :: k_e, ka_coarse, ka_fine
    integer :: slope_coarse_mean, slope_fine_mean, SM_coarse_mean
    double precision,intent(in) :: SM_coarse(0:g_NCols-1,0:g_NRows-1)

    double precision, allocatable :: kw_coarse(:,:), kw_fine(:,:)
    real(DP) :: p, q
    double precision, allocatable :: fit_c_coarse(:)
    double precision, allocatable :: fit_TWI_coarse(:)
    logical :: bIsError
    !allocate(C_90(0:g_NCols-1,0:g_NRows-1))
    Npixel_coarse = count(g_Mask == 1)
    Npixel_fine = count(g_mask_fine == 1)

    allocate(fit_c_coarse(1 : Npixel_coarse))
    allocate(fit_TWI_coarse(1 : Npixel_coarse))
    allocate(kw_coarse(0:g_NCols-1,0:g_NRows-1))
    allocate(kw_fine(0:g_NCols_Land-1,0:g_NRows_Land-1))

    ! initial the matrix
    fit_c_coarse = g_NoData_Value
    fit_TWI_coarse = g_NoData_Value
    kw_coarse = g_NoData_Value
    kw_fine = g_NoData_Value
    ! the slope data for both coarse and fine resolution are all in unit of degree
    slope_coarse_mean = sum(g_Slope_angle, mask = g_Mask == 1) / Npixel_coarse
    SM_coarse_mean = sum(SM_coarse, mask = g_Mask == 1) / Npixel_coarse
    slope_fine_mean = sum(g_slope_fine, mask = g_mask_fine == 1) / Npixel_fine

    where(g_Mask/=g_NoData_Value)
        kw_coarse = SM_coarse / SM_coarse_mean
    end where
!    write(*,*) SM_coarse_mean, SM_coarse(200,200), kw_coarse(200,200)
!    stop
    Npixel_coarse_count = 1

    do i=0, g_NRows-1
        do j=0, g_NCols-1
            if(g_Mask(j,i) == g_NoData_Value)then
                cycle
            end if
            if(g_aspect_coarse(j,i) > 45 .and. g_aspect_coarse(j,i) <= 135) then
                k_e = 0.002
            else if (g_aspect_coarse(j,i) > 135 .and. g_aspect_coarse(j,i) <= 225) then
                k_e = 0.005
            else if (g_aspect_coarse(j,i) > 225 .and. g_aspect_coarse(j,i) <= 315) then
                k_e = -0.003
            else
                k_e = -0.01
            end if
            ka_coarse = (1 - k_e * g_Slope_angle(j,i)) / (1- k_e * slope_coarse_mean)

            if (g_curvature_coarse(j,i) <= 0) then
                fit_c_coarse(Npixel_coarse_count) = kw_coarse(j,i) / ka_coarse - &
                        0.16 * COSD(g_aspect_coarse(j,i)) - 0.09 * SIND(g_aspect_coarse(j,i))
            else
                fit_c_coarse(Npixel_coarse_count) = kw_coarse(j,i) / ka_coarse - &
                        0.14 * COSD(g_aspect_coarse(j,i)) - 0.10 * SIND(g_aspect_coarse(j,i)) &
                        + 0.02 * COSD(2 * g_aspect_coarse(j,i))
            end if

            fit_TWI_coarse(Npixel_coarse_count) = g_TWI_coarse(j,i)
            Npixel_coarse_count = Npixel_coarse_count + 1
        end do
    end do

    ! linear regress fitting algorithm for fit_TWI_coarse and fit_c_coarse
    ! p: incline, q: intercept
    call calc_reg_line(fit_TWI_coarse, fit_c_coarse, q, p)

    ! start the loop in finer-resolution map

    do i=0, g_NRows_Land - 1
        do j=0, g_NCols_Land - 1
            if(g_mask_fine(j,i) == g_NoData_Value)then
                cycle
            end if

            iLoc_coarse = NINT( g_NRows - (((g_NRows_Land-i-1) * &
                    g_CellSize_Land + g_yllCorner_Land) &
                    - g_yllCorner) / g_CellSize - 1)

            jLoc_coarse = NINT( ((j + 1) * g_CellSize_Land + &
                    g_xllCorner_Land - g_xllCorner) &
                    / g_CellSize - 1)

            ! old version
            ! iLoc_coarse = NINT( g_NCols - (((g_NCols_Land-i-1) * &
            !         g_CellSize_Land + g_yllCorner_Land) &
            !         - g_yllCorner) / g_CellSize - 1)

            ! jLoc_coarse = NINT( ((j + 1) * g_CellSize_Land + &
            !         g_xllCorner_Land - g_xllCorner) &
            !         / g_CellSize - 1)

            if (SM_coarse(jLoc_coarse,iLoc_coarse) == -9999) then
                cycle
            end if


            if(g_aspect_fine(j,i) > 45 .and. g_aspect_fine(j,i) <= 135) then
                k_e = 0.002
            else if (g_aspect_fine(j,i) > 135 .and. g_aspect_fine(j,i) <= 225) then
                k_e = 0.005
            else if (g_aspect_fine(j,i) > 225 .and. g_aspect_fine(j,i) <= 315) then
                k_e = -0.003
            else
                k_e = -0.01
            end if
            ka_fine = (1 - k_e * g_slope_fine(j,i)) / (1- k_e * slope_fine_mean)

            if (g_curvature_fine(j,i) <= 0) then

                kw_fine(j,i) = ( p * g_TWI_fine(j,i) + q + 0.16 * COSD(g_aspect_fine(j,i)) + &
                                0.09 * SIND(g_aspect_fine(j,i)) ) * ka_fine

            else
                kw_fine(j,i) = ( p * g_TWI_fine(j,i) + q + 0.14 * COSD(g_aspect_fine(j,i)) + &
                                 0.10 * SIND(g_aspect_fine(j,i)) - 0.02 * COSD(2*g_aspect_fine(j,i)) ) * ka_fine
            end if


            g_SM_fine(j,i) = kw_fine(j,i) * SM_coarse(jLoc_coarse,iLoc_coarse)
            if (g_SM_fine(j,i) > 100) then
                g_SM_fine(j,i) = 100
            end if

        end do
    end do


    return
end subroutine SoilDownscale_pre