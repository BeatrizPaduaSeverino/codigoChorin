MODULE initialCondition

   USE parameters
   USE setPrecision

CONTAINS

   SUBROUTINE ic(x, y, rho, u, v, p, xSpacing, ySpacing)

      IMPLICIT NONE
      INTEGER                                               :: i, j
      REAL(KIND=rk), DIMENSION(:,:), INTENT(INOUT)  :: rho, u, v, p
      real(kind=rk), dimension(:), INTENT(IN)          :: xSpacing
      real(kind=rk), dimension(:), INTENT(IN)          :: ySpacing
      real(kind=rk), dimension(:), INTENT(IN)            :: x
      real(kind=rk), dimension(:), INTENT(IN)            :: y

      u = 0.0000000000000001_rk
      v = 0.0000000000000001_rk
      p = 1._rk
      rho = 1._rk

      ! START FROM A POTENTIAL FLOW
        !!!!!!! counterflow potential velocity !!!!!!!
      !       do i=0, nx
      !            do j=0, ny
      !              u(i,j) = x(i)
      !              v(i,j) = -y(j)
      !            enddo
      !        enddo

        !!!!!!! double Tsuji potential velocity !!!!!!!
      DO i = 1, nx - 1
         DO j = 1, ny - 1

            IF (x(i)**2._rk + y(j)**2._rk .GE. (1._rk - 1.1_rk*max(xSpacing(i), ySpacing(j)))**2) THEN
               u(i, j) = x(i)*(1._rk - (x(i)**2._rk - 3*y(j)**2._rk)/(x(i)**2._rk + y(j)**2._rk)**3._rk &
                               + ub/(x(i)**2._rk + y(j)**2._rk))
               v(i, j) = -y(j)*(1._rk + (3*x(i)**2._rk - y(j)**2._rk)/(x(i)**2._rk + y(j)**2._rk)**3._rk &
                                - ub/(x(i)**2._rk + y(j)**2._rk))
            ELSE
               u(i, j) = 0._rk
               v(i, j) = 0._rk
            END IF

         END DO
      END DO

      !----------------------------------------------------------------

      RETURN
   END SUBROUTINE ic

END MODULE initialCondition
