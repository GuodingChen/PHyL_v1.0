
subroutine Stability3D_VG(FS_3D, volumn_sum, area_sum, SOIL_CODE, D_raster, SM_InEllipse&
                    , Npixel_ellipse, z_single_all, z_ellipsoid_In,&
                        ellipse_i, ellipse_j, window_extend, &
                    main_aspect, main_slope, c_e)

    use LandslideModel_parameters

    implicit none
    integer, intent(in) :: SOIL_CODE, Npixel_ellipse, window_extend
    integer :: i_SoilColumn
    integer, intent(in) :: ellipse_i(Npixel_ellipse), ellipse_j(Npixel_ellipse)
    ! soil basic parameters: read from SOIL_CODE
    double precision :: SoilCohesion, Friction_angle, Porosity, Dry_Unit_Weight, &
                        Theta_r, Thera_s, VG_alpha, VG_n

    double precision, intent(in) :: D_raster(Npixel_ellipse), SM_InEllipse(Npixel_ellipse)
    double precision, intent(in) :: z_ellipsoid_In(Npixel_ellipse)
    double precision, intent(in) :: main_aspect, main_slope, c_e
    double precision, intent(in) :: z_single_all(window_extend*2+1, window_extend*2+1)
    double precision ::SlipSurface_aspect, SlipSurface_dip, &
                        SlipApparent_dip, slipe_surface

    double precision :: A, lambda, alpha, GroundWater_aspect, Sr, Se, Sr_resi, &
                        m_t, c, z_w, G_soil, Suction_stress, &
                        S, S_h, S_v, S_ch, S_mh, S_c, S_m, beta_Sc, beta_Sm,&
                        N_s, T_s, resist_force, drive_force, &
                        resist_sum, drive_sum
    double precision, intent(out) :: FS_3D, volumn_sum, area_sum


    ! get the soil parameters for this potential landslide
    call GetSoilParaFrom_SoilCode(SOIL_CODE, SoilCohesion, Friction_angle, &
            Porosity,Dry_Unit_Weight, Theta_r, Thera_s, VG_alpha, VG_n)

    ! start the stability loop
    resist_sum = 0.0
    drive_sum = 0.0
    volumn_sum = 0.0
    area_sum = 0.0
    do i_SoilColumn = 1, Npixel_ellipse

        if (D_raster(i_SoilColumn) <= 0) then
            cycle
        end if


        call CalSlip_GeoInfo(ellipse_i(i_SoilColumn),ellipse_j(i_SoilColumn), &
                z_single_all, window_extend*2+1, window_extend*2+1, main_aspect, &
                SlipSurface_aspect,SlipSurface_dip,SlipApparent_dip,slipe_surface)

        ! 3D FS calculation

        Sr_resi = Theta_r / Thera_s

        GroundWater_aspect = main_aspect
        Sr = SM_InEllipse(i_SoilColumn) / 100.0

        if (Sr < Sr_resi ) then
            Sr = Sr_resi + 0.01
        end if
        ! calculate the effective saturation,
        Se = (Sr - Sr_resi) / (1 - Sr_resi)
        Suction_stress = - (Se / VG_alpha) * ((Se ** (VG_n/(1-VG_n)) - 1)**(1/VG_n))



        m_t = Sr
        c = SoilCohesion

        z_w = (ABS(z_ellipsoid_In(i_SoilColumn)) - &
                (c_e - m_t * c_e * COSD(main_slope))) / COSD(main_slope)

        if (z_w < 0) then
            z_w = 0.01
        end if

        G_soil = CellSize_LandInM * CellSize_LandInM * D_raster(i_SoilColumn) &
                * (Dry_Unit_Weight + Sr * gamma_w)




        S = gamma_w *  z_w * CellSize_LandInM * CellSize_LandInM * SIND(main_slope)
        S_h = S * COSD(main_slope)
        S_v = S * SIND(main_slope)
        S_ch = S_h * COSD(GroundWater_aspect - SlipSurface_aspect)
        S_mh = S_h * COSD(GroundWater_aspect - main_aspect)
        S_c = SQRT(S_v**2 + S_ch**2)
        S_m = SQRT(S_v**2 + S_mh**2)
        beta_Sc = ACOSD(S_ch / S_c)
        beta_Sm = ACOSD(S_mh / S_m)

        !compute the seepage force in two directions
        N_s = S_c * SIND(beta_Sc - SlipSurface_dip)
        T_s = S_m * COSD(beta_Sm - SlipApparent_dip)

        if (z_w == 0) then
            N_s = 0.0
            T_s = 0.0
        end if
        ! kN / KN
        resist_force = (c * slipe_surface + &
                (G_soil * COSD(SlipSurface_dip) - Suction_stress * slipe_surface + N_s) &
                * TAND(Friction_angle)) * COSD(SlipApparent_dip)

        drive_force = (G_soil * SIND(SlipApparent_dip) + T_s) &
                        * COSD(SlipApparent_dip)
        resist_sum = resist_sum + resist_force
        drive_sum = drive_sum + drive_force
        volumn_sum = volumn_sum + D_raster(i_SoilColumn) * CellSize_LandInM ** 2
        area_sum = area_sum + CellSize_LandInM ** 2

    end do


    FS_3D = resist_sum / drive_sum


    ! add an error check here: FS value cannot be negative
    if (FS_3D < 0) then
        write(*,*) "Invalid value of safety factor! Please check the program."
        stop
    end if

    return
end subroutine Stability3D_VG


subroutine CalSlip_GeoInfo(i,j, DEM, Dim1, Dim2, main_aspect, &
        SlipSurface_aspect, SlipSurface_dip, &
        SlipApparent_dip, slipe_surface)

    use LandslideModel_parameters

    implicit none
    integer, intent(in) :: i,j, Dim1, Dim2
    double precision, intent(in) :: DEM(Dim1,Dim2), main_aspect
    double precision :: z_a, z_b, z_c, z_d, z_f, z_g, z_h, z_i
    double precision :: dz_dx, dz_dy, beta_xz, beta_yz
    double precision, intent(out) :: SlipSurface_aspect, SlipSurface_dip, &
                                    slipe_surface, SlipApparent_dip
    ! Find Neighbors pixel
    z_a = DEM(j-1, i-1)
    z_b = DEM(j-1, i)
    z_c = DEM(j-1, i+1)
    z_d = DEM(j, i-1)
    z_f = DEM(j, i+1)
    z_g = DEM(j+1, i-1)
    z_h = DEM(j+1, i)
    z_i = DEM(j+1, i+1)
    dz_dx = ((z_c+2*z_f+z_i)-(z_a+2*z_d+z_g)) / (8 * CellSize_LandInM)
    dz_dy = ((z_g+2*z_h+z_i)-(z_a+2*z_b+z_c)) / (8 * CellSize_LandInM)

    if (dz_dx == 0 .and. dz_dy == 0) then
        SlipSurface_dip = 0
        SlipSurface_aspect  = 0
        SlipApparent_dip = 0
    else 
        beta_xz = ATAND( dz_dx )
        beta_yz = ATAND( dz_dy )
        SlipSurface_dip = ATAND( SQRT(dz_dx ** 2 + dz_dy ** 2) )

        SlipSurface_aspect = ATAN2D( dz_dy * CellSize_LandInM, -dz_dx * CellSize_LandInM) 
        if (SlipSurface_aspect < 0) then
            SlipSurface_aspect = 90 - SlipSurface_aspect
        else if (SlipSurface_aspect > 90) then
            SlipSurface_aspect = 360 - SlipSurface_aspect + 90
        else
            SlipSurface_aspect = 90 - SlipSurface_aspect
        end if
        
        SlipApparent_dip = ATAND( TAND(SlipSurface_dip) * &
                        ABS( COSD(SlipSurface_aspect - main_aspect) ) ) 

    end if


    slipe_surface = CellSize_LandInM * CellSize_LandInM * SQRT( 1 - (SIND(beta_xz)) ** 2 &
                    *(SIND(beta_yz))**2 ) / ( COSD(beta_xz) * COSD(beta_yz) )
    return
end subroutine CalSlip_GeoInfo



subroutine GetSoilParaFrom_SoilCode(SOIL_CODE, SoilCohesion, Friction_angle, &
                                Porosity,Dry_Unit_Weight, Theta_r, Thera_s, VG_alpha, VG_n)

    implicit none

    integer, intent(in):: SOIL_CODE
    double precision, intent(out) :: SoilCohesion, Friction_angle, Porosity, Dry_Unit_Weight, &
                                    Theta_r, Thera_s, VG_alpha, VG_n

!    get the soil information
!    please read me !!!!!!
!    -----------------------------------------------------
!    unit define
!    Soil parameters unit when read from the soil USDA code
!    Cohesion: (kpa)
!    Friction_angle: (degree)
!    Porosity: Non
!    K_s: (cm/day)
!    Theta_r: (cm^3/cm^3)
!    Thera_s: (cm^3/cm^3)
!    Dry_Unit_Weight: kN/m^3
!    water_weight = 9.8KN/m^3
!    VG1980_param_alpha = kPa^(-1)
!    VG1980_param_n = unitless
!    ------------------------------------------------------
    select case (SOIL_CODE)

        case (1) ! clay
            SoilCohesion = 50
            Friction_angle = 35
            Porosity = 0.4
            Dry_Unit_Weight = 25
            Theta_r = 0.2
            Thera_s = 0.4
            VG_alpha = 0.013 * 100 / 9.8
            VG_n = 1.15
        case (2) ! silty clay
            SoilCohesion = 30
            Friction_angle = 18.5
            Porosity = 0.49
            Dry_Unit_Weight = 18
            Theta_r = 0.198
            Thera_s = 0.481
            VG_alpha = 0.012 * 100 / 9.8
            VG_n = 2.0
        case (3) ! Sandy clay
            SoilCohesion = 24.5
            Friction_angle = 22.5
            Porosity = 0.41
            Dry_Unit_Weight = 18.5
            Theta_r = 0.237
            Thera_s = 0.385
            VG_alpha = 0.028 * 100 / 9.8
            VG_n = 1.35

        case (4) !Clay loam
            SoilCohesion = 35
            Friction_angle = 20
            Porosity = 0.46
            Dry_Unit_Weight = 14
            Theta_r = 0.171
            Thera_s = 0.438
            VG_alpha = 0.51
            VG_n = 1.31

        case (5) ! Silty clay loam
            SoilCohesion = 50
            Friction_angle = 16.5
            Porosity = 0.48
            Dry_Unit_Weight = 14
            Theta_r = 0.172
            Thera_s = 0.481
            VG_alpha = 0.010 * 100 / 9.8
            VG_n = 1.92

        case (6) ! Sandy clay loam
            SoilCohesion = 29
            Friction_angle = 20
            Porosity = 0.39
            Dry_Unit_Weight = 15
            Theta_r = 0.169
            Thera_s = 0.382
            VG_alpha = 0.028 * 100 / 9.8
            VG_n = 1.64

        case (7) ! Loam
            SoilCohesion = 20
            Friction_angle = 27.5
            Porosity = 0.43
            Dry_Unit_Weight = 13
            Theta_r = 0.126
            Thera_s = 0.403
            VG_alpha = 0.33
            VG_n = 1.21

        case (8) ! Silt loam
            SoilCohesion = 9
            Friction_angle = 24
            Porosity = 0.46
            Dry_Unit_Weight = 14
            Theta_r = 0.115
            Thera_s = 0.439
            VG_alpha = 0.017 * 100 / 9.8
            VG_n = 1.41

        case (9) ! Sandy loam
            SoilCohesion = 6
            Friction_angle = 32
            Porosity = 0.4
            Dry_Unit_Weight = 15
            Theta_r = 0.087
            Thera_s = 0.387
            VG_alpha = 0.037 * 100 / 9.8
            VG_n = 3.7

        case (10) ! Silt
            SoilCohesion = 9
            Friction_angle = 26.5
            Porosity = 0.52
            Dry_Unit_Weight = 16.5
            Theta_r = 0.126
            Thera_s = 0.489
            VG_alpha = 0.017 * 100 / 9.8
            VG_n = 1.33

        case (11) ! Loamy sand
            SoilCohesion = 7.5
            Friction_angle = 28.5
            Porosity = 0.42
            Dry_Unit_Weight = 20.5
            Theta_r = 0.077
            Thera_s = 0.390
            VG_alpha = 0.042 * 100 / 9.8
            VG_n = 3.13

        case (12) ! Sand
            SoilCohesion = 5
            Friction_angle = 40
            Porosity = 0.43
            Dry_Unit_Weight = 21
            Theta_r = 0.061
            Thera_s = 0.375
            VG_alpha = 0.040 * 100 / 9.8
            VG_n = 6.67


        case default
            write(*,*) "An invalid soil code has been detected; please check your soil map."
            stop
    end select





    return
end subroutine GetSoilParaFrom_SoilCode
