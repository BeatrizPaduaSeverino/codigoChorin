MODULE boundaryConditions

   USE setPrecision
   USE parameters

CONTAINS

 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!      SUBROUTINE  bcU
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE bcU(u, x, y, xSpacing, ySpacing)

      IMPLICIT NONE
      INTEGER                                             :: i, j
      REAL(KIND=rk), DIMENSION(:, :), INTENT(INOUT)       :: u
      real(kind=rk), dimension(:), INTENT(IN)             :: xSpacing
      real(kind=rk), dimension(:), INTENT(IN)             :: ySpacing
      real(kind=rk), dimension(:), INTENT(IN)             :: x
      real(kind=rk), dimension(:), INTENT(IN)             :: y


      !------- vertical symmetry axis -------
      u(1, :) = 0._rk

      !------- outlet -------
      u(nx, :) = u(nx - 1, :)

      !------- horizontal symmetry axis -------
      u(:, 1) = u(:, 1)

      !------- inlet -------
      u(:, ny) = x(:)

   END SUBROUTINE bcU

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!      SUBROUTINE  bcV
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE bcV(v, x, y, xSpacing, ySpacing)

      IMPLICIT NONE
      INTEGER                                             :: i, j
      REAL(KIND=rk), DIMENSION(:, :), INTENT(INOUT)       :: v
      real(kind=rk), dimension(:), INTENT(IN)             :: xSpacing
      real(kind=rk), dimension(:), INTENT(IN)             :: ySpacing
      real(kind=rk), dimension(:), INTENT(IN)             :: x
      real(kind=rk), dimension(:), INTENT(IN)             :: y

   
      !------- vertical symmetry axis -------
      v(1, :) = v(1, :)

      !------- outlet -------
      v(nx, :) = v(nx - 1, :)

      !------- horizontal symmetry axis -------
      v(:, 1) = 0._rk

      !------- inlet -------
      v(:, ny) = -y(ny)

   END SUBROUTINE bcV

END MODULE boundaryConditions
