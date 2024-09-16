module setPrecision

      !!!!!!! Defining the precision of floating point !!!!!!!
   !integer, parameter :: rk = selected_real_kind(6, 37)          ! Single precision
   integer, parameter  :: rk = selected_real_kind(15, 307)    ! Double precision
   !integer, parameter :: rk = selected_real_kind(33, 4931)   ! Quadruple precision

end module setPrecision
