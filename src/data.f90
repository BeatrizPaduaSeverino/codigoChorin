MODULE data

   USE parameters
   USE setPrecision
   USE variables

CONTAINS

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!!!!      SUBROUTINE printdata
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE printdata
      IMPLICIT NONE

      !open(1,file=dirgraf//'INIT_'//fname//'.dat',status='unknown')
      !open(1,file=dirgraf//'INIT_data.dat',status='unknown')
      write (*, *) '****************************************'
      write (*, *) '****************************************'
      write (*, '(a33,i5)') 'nx........................................:', nx
      write (*, '(a33,i5)') 'ny........................................:', ny
      write (*, '(a33,i5)') 'FDq xorder........................:', xorder
      write (*, '(a33,i5)') 'FDq yorder........................:', yorder
      write (*, *)
      write (*, '(a33,F16.8)') 'Re...................................:', Re
      write (*, '(a33,F16.8)') 'Pr..................................:', Pr
      write (*, '(a33,F16.8)') 'Pe...................................:', Pe
      write (*, '(a33,F16.8)') 'LeF.............................:', LeF
      write (*, '(a33,F16.8)') 'LeO.............................:', LeO
      !write(*,'(a33,a1,F10.4,a1,F10.4,a1)')'x \in .....................&
      !&    ....:','[',x0,',',xf,']'
      !write(*,'(a33,a1,F10.4,a1,F10.4,a1)')'y \in .....................&
      !&   ....:','[',y0,',',yf,']'

      !write(*,*)
      !write(*,*)'******************************************************************************'
      !write(*,*)'                                                 CPU TIME                                                     '
      !write(*,*)'******************************************************************************'
      !write(*,'(a25,F16.8)')'CPU/Time (init_data)................:', cpu_end(2) - cpu_start(2)
      !write(*,'(a25,F16.8)')'CPU/Time (matriz LHS/RHS)..:', cpu_end(3) - cpu_start(3)
      !write(*,'(a25,F16.8)')'CPU/Time (bc LHS/RHS)........:', cpu_end(4) - cpu_start(4)
      !write(*,'(a25,F16.8)')'CPU/Time (solver)....................:', cpu_end(5) - cpu_start(5)
      !write(*,'(a25,F16.8)')'CPU/Time (total).......................:', cpu_end(1) - cpu_start(1)
      !write(*,*)'******************************************************************************'

      RETURN
   END SUBROUTINE printdata

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!!!!      SUBROUTINE transient
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE transient(um, vm, p, rho, T, Z, k)

      IMPLICIT NONE

      INTEGER                                                                                 :: i, j, k
      REAL(8), INTENT(IN), DIMENSION(2:imax, 1:jmax)      :: um
      REAL(8), INTENT(IN), DIMENSION(1:imax, 2:jmax)      :: vm
      REAL(8), INTENT(IN), DIMENSION(1:imax, 1:jmax)        :: rho, T, Z
      REAL(8), INTENT(IN), DIMENSION(2:imax - 1, 2:jmax - 1) :: P

      character(len=100) :: filename2

      write (filename2, *) (k/n_tr) + 100
      filename2 = adjustl(filename2)

      open (unit=550, file='transient/data/x.dat', ACTION="write", STATUS="replace")
      do i = 1, imax
         write (550, *) x(i)
      end do
      CLOSE (550)

      open (unit=550, file='transient/data/y.dat', ACTION="write", STATUS="replace")
      do j = 1, jmax
         write (550, *) y(j)
      end do
      CLOSE (550)

      open (unit=550, file='transient/data/ym.dat', ACTION="write", STATUS="replace")
      do j = 2, jmax
         write (550, *) ym(j)
      end do
      CLOSE (550)

      open (unit=550, file='transient/data/xm.dat', ACTION="write", STATUS="replace")
      do i = 2, imax
         write (550, *) xm(i)
      end do
      CLOSE (550)

      open (unit=550, file='transient/data/xp.dat', ACTION="write", STATUS="replace")
      do i = 2, imax - 1
         write (550, *) x(i)
      end do
      CLOSE (550)

      open (unit=550, file='transient/data/yp.dat', ACTION="write", STATUS="replace")
      do j = 2, jmax - 1
         write (550, *) y(j)
      end do
      CLOSE (550)

      open (unit=550, file='transient/data/um'//trim(filename2)//'.dat', ACTION="write", STATUS="replace")
      do i = 2, imax
         write (550, *) (real(um(i, j)), j=1, jmax)
      end do
      CLOSE (550)

      open (unit=550, file='transient/data/vm'//trim(filename2)//'.dat', ACTION="write", STATUS="replace")
      do i = 1, imax
         write (550, *) (real(vm(i, j)), j=2, jmax)
      end do
      CLOSE (550)

      open (unit=550, file='transient/data/P'//trim(filename2)//'.dat', ACTION="write", STATUS="replace")
      do i = 2, imax - 1
         write (550, *) (real(P(i, j)), j=2, jmax - 1)
      end do
      CLOSE (550)

      open (unit=550, file='transient/data/rho'//trim(filename2)//'.dat', ACTION="write", STATUS="replace")
      do i = 1, imax
         write (550, *) (real(rho(i, j)), j=1, jmax)
      end do
      CLOSE (550)

      open (unit=550, file='transient/data/T'//trim(filename2)//'.dat', ACTION="write", STATUS="replace")
      do i = 1, imax
         write (550, *) (real(T(i, j)), j=1, jmax)
      end do
      CLOSE (550)

      open (unit=550, file='transient/data/Z'//trim(filename2)//'.dat', ACTION="write", STATUS="replace")
      do i = 1, imax
         write (550, *) (real(Z(i, j)), j=1, jmax)
      end do
      CLOSE (550)

      !
      open (550, file='transient/data/time'//trim(filename2)//'.dat')

      write (550, *) time

      close (550)

      !you can activate this block of call system to plot error for each time step
      !-------------------------------------------------------------------------------------
      !CALL SYSTEM('python3 error-code.py')
      !CALL SYSTEM('mv output/error.png output/errors/')
      !CALL SYSTEM('mv output/residual.png output/errors/')
      !CALL RENAME('output/errors/error.png','output/errors/error'//trim(filename2)//'.png')
      !CALL RENAME('output/errors/residual.png','output/errors/residual'//trim(filename2)//'.png')
      !-------------------------------------------------------------------------------------

      RETURN
   END SUBROUTINE transient

   SUBROUTINE output(um, vm, rho, p, Z, T, H, k)
      USE vars
      IMPLICIT NONE
      INTEGER :: i, j, k

      REAL(8), INTENT(IN), DIMENSION(2:imax, 1:jmax) :: um
      REAL(8), INTENT(IN), DIMENSION(1:imax, 2:jmax) :: vm
      REAL(8), INTENT(IN), DIMENSION(1:imax, 1:jmax) :: rho, Z, T, H
      REAL(8), INTENT(IN), DIMENSION(2:imax - 1, 2:jmax - 1) :: P

      REAL(8), DIMENSION(1:imax, 1:jmax) :: Yi
      character*16 filename
      character(len=100) :: filename2

      do i = 2, imax
         do j = 2, jmax

            if (Z(i, j) .gt. (1.d0)) then
               Yi(i, j) = (Z(i, j) - 1.d0)/S
            else
               Yi(i, j) = -Z(i, j) + 1.d0
            end if

         end do
      end do

      open (unit=550, file='data/x.dat', ACTION="write", STATUS="replace")
      do i = 1, imax
         write (550, *) x(i)
      end do
      CLOSE (550)

      open (unit=550, file='data/y.dat', ACTION="write", STATUS="replace")
      do j = 1, jmax
         write (550, *) y(j)
      end do
      CLOSE (550)

      open (unit=550, file='data/ym.dat', ACTION="write", STATUS="replace")
      do j = 2, jmax
         write (550, *) ym(j)
      end do
      CLOSE (550)

      open (unit=550, file='data/xm.dat', ACTION="write", STATUS="replace")
      do i = 2, imax
         write (550, *) xm(i)
      end do
      CLOSE (550)

      open (unit=550, file='data/xp.dat', ACTION="write", STATUS="replace")
      do i = 2, imax - 1
         write (550, *) x(i)
      end do
      CLOSE (550)

      open (unit=550, file='data/yp.dat', ACTION="write", STATUS="replace")
      do j = 2, jmax - 1
         write (550, *) y(j)
      end do
      CLOSE (550)

      open (unit=550, file='data/um.dat', ACTION="write", STATUS="replace")
      do i = 2, imax
         write (550, *) (real(um(i, j)), j=1, jmax)
      end do
      CLOSE (550)

      open (unit=550, file='data/vm.dat', ACTION="write", STATUS="replace")
      do i = 1, imax
         write (550, *) (real(vm(i, j)), j=2, jmax)
      end do
      CLOSE (550)

      open (unit=550, file='data/P.dat', ACTION="write", STATUS="replace")
      do i = 2, imax - 1
         write (550, *) (real(P(i, j)), j=2, jmax - 1)
      end do
      CLOSE (550)

      open (unit=550, file='data/rho.dat', ACTION="write", STATUS="replace")
      do i = 1, imax
         write (550, *) (real(rho(i, j)), j=1, jmax)
      end do
      CLOSE (550)

      open (unit=550, file='data/T.dat', ACTION="write", STATUS="replace")
      do i = 1, imax
         write (550, *) (real(T(i, j)), j=1, jmax)
      end do
      CLOSE (550)

      open (unit=550, file='data/Z.dat', ACTION="write", STATUS="replace")
      do i = 1, imax
         write (550, *) (real(Z(i, j)), j=1, jmax)
      end do
      CLOSE (550)

      open (unit=550, file='data/H.dat', ACTION="write", STATUS="replace")
      do i = 1, imax
         write (550, *) (real(H(i, j)), j=1, jmax)
      end do
      CLOSE (550)

      open (550, file='data/time.dat')

      write (550, *) time

      close (550)

      !############ RESTART ###############

      write (filename, *) k
      filename2 = adjustl(filename)
      open (550, file='data/results/U/restartU'//trim(filename2)//'.dat', status='unknown')

      do i = 2, imax
         do j = 1, jmax

            write (550, *) um(i, j)

         end do
      end do

      close (550)

      open (550, file='data/results/V/restartV'//trim(filename2)//'.dat', status='unknown')

      do i = 1, imax
      do j = 2, jmax

         write (550, *) vm(i, j)

      end do
      end do

      close (550)

      open (550, file='data/results/PTZH/restartPTZH'//trim(filename2)//'.dat', status='unknown')

      do i = 1, imax
         do j = 1, jmax

            write (550, *) T(i, j), Z(i, j), H(i, j)

         end do
      end do

      close (550)

      !########## RESTART/RESTART.dat

      open (550, file='data/restart/restartU.dat', status='unknown')

      do i = 2, imax
         do j = 1, jmax

            write (550, *) um(i, j)

         end do
      end do

      close (550)

      open (550, file='data/restart/restartV.dat', status='unknown')

      do i = 1, imax
      do j = 2, jmax

         write (550, *) vm(i, j)

      end do
      end do

      close (550)

      open (550, file='data/restart/restartPTZH.dat', status='unknown')

      do i = 1, imax
         do j = 1, jmax

            write (550, *) T(i, j), Z(i, j), H(i, j)

         end do
      end do

      close (550)

      open (550, file='data/restart/restartP.dat', status='unknown')

      do i = 2, imax - 1
         do j = 2, jmax - 1

            write (550, *) P(i, j)

         end do
      end do

      close (550)

      RETURN
   END SUBROUTINE output

END MODULE data
