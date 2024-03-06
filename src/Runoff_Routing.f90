

!-----------------------------------------------------------------
subroutine CREST_RouteTreat(NextTime,toRowA,toColA,toPerA,  &
        toRowB,toColB,toPerB, PassCell_Row_pre, PassCell_Col_pre, &
        PassCell_StartIndex, PassCell_EndIndex, RAM_pre)
    use CREST_Project
    use CREST_Basic
    use CREST_Param

    implicit none

    double precision :: NextTime(0:g_NCols-1,0:g_NRows-1)

    integer, intent(out)::toRowA(0:g_NCols-1,0:g_NRows-1)
    integer, intent(out)::toRowB(0:g_NCols-1,0:g_NRows-1)
    integer, intent(out)::toColA(0:g_NCols-1,0:g_NRows-1)
    integer, intent(out)::toColB(0:g_NCols-1,0:g_NRows-1)

    integer :: StartIndex_value, EndIndex_value, RAM_pre
    integer, intent(out) :: PassCell_Row_pre(0:RAM_pre)
    integer, intent(out) :: PassCell_col_pre(0:RAM_pre)
    integer, intent(out)::PassCell_StartIndex(0:g_NCols-1,0:g_NRows-1)
    integer, intent(out)::PassCell_EndIndex(0:g_NCols-1,0:g_NRows-1)
    integer :: i,j
    double precision, intent(out)::toPerA(0:g_NCols-1,0:g_NRows-1)
    double precision, intent(out)::toPerB(0:g_NCols-1,0:g_NRows-1)


    toRowA = g_NoData_Value
    toRowB = g_NoData_Value
    toColA = g_NoData_Value
    toColB = g_NoData_Value

    toPerA = g_NoData_Value
    toPerB = g_NoData_Value
    PassCell_StartIndex = g_NoData_Value
    PassCell_EndIndex = g_NoData_Value
    PassCell_Row_pre = g_NoData_Value
    PassCell_col_pre = g_NoData_Value

    EndIndex_value = 0
    do i=0, g_NRows-1
        do j=0, g_NCols-1
            if(g_Mask(j,i)/=g_NoData_Value)then
                toRowB(j,i)=i
                toColB(j,i)=j
                toPerB(j,i)=0
                StartIndex_value = EndIndex_value
                do while(toPerB(j,i) < g_TimeStep)


                    toRowA(j,i)=toRowB(j,i)
                    toColA(j,i)=toColB(j,i)
                    toPerA(j,i)=toPerB(j,i)

                    if(toRowA(j,i)/=g_NoData_Value)then
                        if( g_Mask(toColA(j,i),toRowA(j,i)) /= g_NoData_Value )then

                            toRowB(j,i) = g_NextR(toColA(j,i),toRowA(j,i))

                            toColB(j,i) = g_NextC(toColA(j,i),toRowA(j,i))

                            toPerB(j,i)=toPerB(j,i) + NextTime(toColA(j,i),toRowA(j,i))

                            PassCell_Row_pre(EndIndex_value) = toRowB(j,i)
                            PassCell_col_pre(EndIndex_value) = toColB(j,i)
                            EndIndex_value = EndIndex_value + 1

                        else

                            toRowB(j,i)=g_NoData_Value
                            toColB(j,i)=g_NoData_Value
                            toPerB(j,i)=toPerB(j,i)+9999.0

                        end if

                    else
                        toRowB(j,i)=g_NoData_Value
                        toColB(j,i)=g_NoData_Value
                        toPerB(j,i)=toPerB(j,i)+9999.0
                    end if
                end do
                ! follows happen when toPerB(j,i) > g_TimeStep, meaning the flow
                ! destination of the pixcel (j,i) is found during timestep
                ! The end point won't happen to be the pixel, so the fraction is used
                ! to assign the value to last and next pixel
                ! toPerB: fraction of value exceeds to next pixel (toColB(j,i), toRowB(j,i))
                ! toPerA: fraction of value that dosen't fully flow to next pixel
                toPerB(j,i) = (g_TimeStep-toPerA(j,i)) / (toPerB(j,i)-toPerA(j,i))
                toPerA(j,i) = 1.0 - toPerB(j,i)

                PassCell_EndIndex(j,i) = EndIndex_value - 1
                PassCell_StartIndex(j,i) = StartIndex_value
            end if
        end do
    end do
    !write(*,*) PassCell_StartIndex(231,69), PassCell_EndIndex(231,69)
    return
end subroutine CREST_RouteTreat


!----------------------------------------------------------------------
subroutine CalNextTime(NextTime)
    use CREST_Project
    use CREST_Basic
    use CREST_Param

    implicit none
    double precision :: SpeedVegLocal,SpeedVegNext
    logical :: bIsError = .false.
    integer :: i,j,ii,jj
    double precision, allocatable :: Speed(:,:)
    double precision :: NextTime(0:g_NCols-1,0:g_NRows-1)
    allocate(Speed(0:g_NCols-1,0:g_NRows-1))
    do i=0, g_NRows-1
        do j=0, g_NCols-1
            ii=g_NextR(j,i)
            jj=g_NextC(j,i)
            if(g_Mask(j,i)/=g_NoData_Value)then
                SpeedVegLocal=0.5
                if(InBasin(jj,ii) .eqv. .false.)then
                    SpeedVegNext=SpeedVegLocal
                else
                    SpeedVegNext=0.5
                end if
                Speed(j,i) = g_tParams%coeM(j,i) * ((SpeedVegLocal+SpeedVegNext)/2.0) &
                        *g_Slope(j,i)**g_tParams%expM(j,i)

                if(g_Stream(j,i)==1)then		!g_FAC(j,i)>g_tParams%TH(j,i))then
                    Speed(j,i)=Speed(j,i)*g_tParams%coeR(j,i)
                end if

                NextTime(j,i)=g_NextLen(j,i)/Speed(j,i)/3600.0 !Unit=h, second--->hour
                !NextTime(j,i)=g_NextLen(j,i)/Speed(j,i) ! cgd: for synthetic simulation
            else
                Speed(j,i)=g_NoData_Value
                NextTime(j,i)=g_NoData_Value
            end if
        end do
    end do
    ! check the speed file
    !    call WriteMatrixFile(trim(g_ResultPath)//"Speed",  &
    !            Speed, g_NCols,g_NRows,    &
    !            g_XLLCorner,g_YLLCorner, g_CellSize, g_NoData_Value, &
    !            bIsError,g_ResultFormat)
    !    call WriteMatrixFile(trim(g_ResultPath)//"g_NextLen",  &
    !            g_NextLen, g_NCols,g_NRows,    &
    !            g_XLLCorner,g_YLLCorner, g_CellSize, g_NoData_Value, &
    !            bIsError,g_ResultFormat)

    deallocate(Speed)

    return
end subroutine CalNextTime