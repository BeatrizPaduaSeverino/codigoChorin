MODULE dataWrite

   USE parameters
   USE setPrecision
   USE variables

CONTAINS

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!!!!      SUBROUTINE printdata
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE printdata
      IMPLICIT NONE

      write (*, *) '****************************************'
      write (*, *) '****************************************'

      write (*, '(a33,F16.8)') 'Grid spacing - x (min, max)', minval(xSpacing), ',', maxval(xSpacing)
      write (*, '(a33,F16.8)') 'Grid spacing - y (min, max)', minval(ySpacing), ',', maxval(ySpacing)

      !open(1,file=dirgraf//'INIT_'//fname//'.dat',status='unknown')
      !open(1,file=dirgraf//'INIT_data.dat',status='unknown')
      write (*, *) '****************************************'
      write (*, *) '****************************************'
      write (*, '(a33,i5)') 'nx........................................:', nx
      write (*, '(a33,i5)') 'ny........................................:', ny
      write (*, '(a33,i5)') 'FDq xorder........................:', xorder
      write (*, '(a33,i5)') 'FDq yorder........................:', yorder
      write (*, *)
      write (*, '(a33,F16.8)') 'Pr..................................:', Pr
      write (*, '(a33,F16.8)') 'Pe...................................:', Pe
      write (*, '(a33,F16.8)') 'Re...................................:', Pe/Pr
      write (*, '(a33,F16.8)') 'LeF.............................:', LeF
      write (*, '(a33,F16.8)') 'LeO.............................:', LeO
      write (*, '(a33,F16.8)') 'NF.............................:', NF
      write (*, '(a33,F16.8)') 'NO.............................:', NO
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

   SUBROUTINE transient(um, vm, p, rho, k)

      IMPLICIT NONE
      INTEGER                                   :: i, j, k
      REAL(8), INTENT(IN), DIMENSION(:, :)      :: um
      REAL(8), INTENT(IN), DIMENSION(:, :)      :: vm
      REAL(8), INTENT(IN), DIMENSION(:, :)      :: rho
      REAL(8), INTENT(IN), DIMENSION(:, :)      :: P

      character(len=100) :: filename2

      write (filename2, *) (k/1000) + 100
      filename2 = adjustl(filename2)

      open (unit=550, file='transient/data/x.dat', ACTION="write", STATUS="replace")
      do i = 1, nx
         write (550, *) x(i)
      end do
      CLOSE (550)

      open (unit=550, file='transient/data/y.dat', ACTION="write", STATUS="replace")
      do j = 1, ny
         write (550, *) y(j)
      end do
      CLOSE (550)

      open (unit=550, file='transient/data/ym.dat', ACTION="write", STATUS="replace")
      do j = 1, ny
         write (550, *) y(j)
      end do
      CLOSE (550)

      open (unit=550, file='transient/data/xm.dat', ACTION="write", STATUS="replace")
      do i = 1, nx
         write (550, *) x(i)
      end do
      CLOSE (550)

      open (unit=550, file='transient/data/xp.dat', ACTION="write", STATUS="replace")
      do i = 1, nx
         write (550, *) x(i)
      end do
      CLOSE (550)

      open (unit=550, file='transient/data/yp.dat', ACTION="write", STATUS="replace")
      do j = 1, ny - 1
         write (550, *) y(j)
      end do
      CLOSE (550)

      open (unit=550, file='transient/data/um'//trim(filename2)//'.dat', ACTION="write", STATUS="replace")
      do i = 1, nx
         write (550, *) (real(um(i, j)), j=1, ny)
      end do
      CLOSE (550)

      open (unit=550, file='transient/data/vm'//trim(filename2)//'.dat', ACTION="write", STATUS="replace")
      do i = 1, nx
         write (550, *) (real(vm(i, j)), j=1, ny)
      end do
      CLOSE (550)

      open (unit=550, file='transient/data/P'//trim(filename2)//'.dat', ACTION="write", STATUS="replace")
      do i = 1, nx
         write (550, *) (real(P(i, j)), j=1, ny - 1)
      end do
      CLOSE (550)

      open (unit=550, file='transient/data/rho'//trim(filename2)//'.dat', ACTION="write", STATUS="replace")
      do i = 1, nx
         write (550, *) (real(rho(i, j)), j=1, ny)
      end do
      CLOSE (550)

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

   SUBROUTINE output(um, vm, rho, p, k)

      IMPLICIT NONE
      INTEGER                                        :: i, j, k
      REAL(8), INTENT(IN), DIMENSION(0:nx, 0:ny)      :: um
      REAL(8), INTENT(IN), DIMENSION(0:nx, 0:ny)      :: vm
      REAL(8), INTENT(IN), DIMENSION(0:nx, 0:ny)      :: rho
      REAL(8), INTENT(IN), DIMENSION(0:nx, 0:ny)      :: P

      REAL(8), DIMENSION(0:nx, 0:ny) :: Yi
      character*16 filename
      character(len=100) :: filename2

      open (unit=550, file='data/x.dat', ACTION="write", STATUS="replace")
      do i = 1, nx
         write (550, *) x(i)
      end do
      CLOSE (550)

      open (unit=550, file='data/y.dat', ACTION="write", STATUS="replace")
      do j = 1, ny
         write (550, *) y(j)
      end do
      CLOSE (550)

      open (unit=550, file='data/ym.dat', ACTION="write", STATUS="replace")
      do j = 1, ny
         write (550, *) y(j)
      end do
      CLOSE (550)

      open (unit=550, file='data/xm.dat', ACTION="write", STATUS="replace")
      do i = 1, nx
         write (550, *) x(i)
      end do
      CLOSE (550)

      open (unit=550, file='data/xp.dat', ACTION="write", STATUS="replace")
      do i = 1, nx
         write (550, *) x(i)
      end do
      CLOSE (550)

      open (unit=550, file='data/yp.dat', ACTION="write", STATUS="replace")
      do j = 1, ny - 1
         write (550, *) y(j)
      end do
      CLOSE (550)

      open (unit=550, file='data/um.dat', ACTION="write", STATUS="replace")
      do i = 1, nx
         write (550, *) (real(um(i, j)), j=0, ny)
      end do
      CLOSE (550)

      open (unit=550, file='data/vm.dat', ACTION="write", STATUS="replace")
      do i = 1, nx
         write (550, *) (real(vm(i, j)), j=0, ny)
      end do
      CLOSE (550)

      open (unit=550, file='data/P.dat', ACTION="write", STATUS="replace")
      do i = 1, nx
         write (550, *) (real(P(i, j)), j=0, ny - 1)
      end do
      CLOSE (550)

      open (unit=550, file='data/rho.dat', ACTION="write", STATUS="replace")
      do i = 1, nx
         write (550, *) (real(rho(i, j)), j=0, ny)
      end do
      CLOSE (550)

      open (550, file='data/time.dat')

      write (550, *) time

      close (550)

      !############ RESTART ###############

      write (filename, *) k
      filename2 = adjustl(filename)
      open (550, file='data/results/U/restartU'//trim(filename2)//'.dat', status='unknown')

      do i = 1, nx
         do j = 1, ny

            write (550, *) um(i, j)

         end do
      end do

      close (550)

      open (550, file='data/results/V/restartV'//trim(filename2)//'.dat', status='unknown')

      do i = 1, nx
      do j = 2, ny

         write (550, *) vm(i, j)

      end do
      end do

      close (550)


      RETURN
   END SUBROUTINE output

END MODULE dataWrite
