MODULE discretisation

   USE parameters
   USE setPrecision
   USE variables

CONTAINS

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!!!!      Subroutine  FD
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE FD(nz, z0, zf, zm, gridz, stencilz, stencilzz)

      IMPLICIT NONE

      INTEGER                                                  ::  i
      INTEGER, INTENT(IN)                                      ::  nz
      REAL(KIND=rk), ALLOCATABLE, DIMENSION(:)                 ::  grid
      REAL(KIND=rk), ALLOCATABLE, DIMENSION(:, :)              ::  df1, df2
      REAL(KIND=rk), ALLOCATABLE, DIMENSION(:), INTENT(OUT)    ::  gridz
      REAL(KIND=rk), ALLOCATABLE, DIMENSION(:, :), INTENT(OUT) ::  stencilz, stencilzz
      REAL(KIND=rk), INTENT(IN)                                ::  z0, zf, zm
      REAL(KIND=rk)                                            ::  delta

      !grid
      ALLOCATE (grid(nz), gridz(nz), df1(nz, nz), df2(nz, nz))

      !Caldula a distância uniforme entre os pontos da mallha espacial
      delta = 1.0_rk/REAL(nz - 1)

      !Inicializa a matriz da malha e das derivadas primeira e segunda, todas com valores zerados
      grid = 0.0_rk
      df1 = 0.0_rk
      df2 = 0.0_rk

      !Estabelece que a malha em seu ponto final(ou seja, no último x ou no último y, vale 1)
      !critério utilizado para normalizar o intervalo da malha de [0,1]
      grid(nz) = 1.0_rk

      !Estabelece a discretização da primeira derivada na borda 'inferior', utilizando pontos descentrados
      df1(1, 1) = -1.5_rk/delta
      df1(1, 1 + 1) = 2._rk/delta
      df1(1, 1 + 2) = -0.5_rk/delta

      !Estabelece a discretização da segunda derivada na borda 'inferior', utilizando pontos descentrados
      df2(1, 1) = 2._rk/(delta*delta)
      df2(1, 1 + 1) = -5._rk/(delta*delta)
      df2(1, 1 + 2) = 4._rk/(delta*delta)
      df2(1, 1 + 3) = -1._rk/(delta*delta)

      !Estabelece a discretização da primeira derivada na borda 'superior', utilizando pontos descentrados
      df1(nz, nz) = 1.5_rk/delta
      df1(nz, nz - 1) = -2._rk/delta
      df1(nz, nz - 2) = 0.5_rk/delta

      !Estabelece a discretização da segunda derivada na borda 'superior', utilizando pontos descentrados
      df2(nz, nz) = 2._rk/(delta*delta)
      df2(nz, nz - 1) = -5._rk/(delta*delta)
      df2(nz, nz - 2) = 4_rk/(delta*delta)
      df2(nz, nz - 3) = -1_rk/(delta*delta)

      !intera no interior da malha (do segundo ao penúltimo ponto)
      !Calcula a malha normalizada no intervalo [0,1]
      !Calcula a derivada primeira e segunda baseadas na discretização utilizando pontos centrados e normalizando no intervalo [0,1]
      DO i = 2, nz - 1
         grid(i) = (i - 1)*delta

         df1(i, i - 1) = -0.5_rk/delta
         df1(i, i + 1) = 0.5_rk/delta

         df2(i, i - 1) = 1.0_rk/(delta*delta)
         df2(i, i) = -2.0_rk/(delta*delta)
         df2(i, i + 1) = 1.0_rk/(delta*delta)
      END DO

      !Escalona a Malha para o intervalo original [z0, zf], através de transformação linear
      !Os operadores diferenciais são escalonados de acordo com a transformação da malha
      gridz = (zf - z0)*grid + z0
      stencilz = df1/(zf - z0)
      stencilzz = df2/(zf - z0)**2.0_rk

      DEALLOCATE (grid, df1, df2)

      RETURN
   END SUBROUTINE FD

   !Transfere as variáveis do nó para a intersecção (para serem utilizadas nas equações de fluxo)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!!!!      Subroutine  FD_NI
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE FD_NI(nz, z0, zf, zm, stencilz)

      IMPLICIT NONE

      INTEGER                                    :: i, j
      INTEGER, INTENT(IN)                        :: nz
      REAL(KIND=rk), ALLOCATABLE, DIMENSION(:)   :: grid
      REAL(KIND=rk), ALLOCATABLE, DIMENSION(:, :) :: df1, df2
      REAL(KIND=rk), ALLOCATABLE, DIMENSION(:)   :: gridz
      REAL(KIND=rk), DIMENSION(:, :), INTENT(OUT) :: stencilz
      REAL(KIND=rk), ALLOCATABLE, DIMENSION(:, :) :: stencilzz
      REAL(KIND=rk), INTENT(IN)                  :: z0, zf, zm
      REAL(KIND=rk)                              :: delta

      !grid z
      ALLOCATE (grid(nz), df1(nz, nz), df2(nz, nz))

      delta = 1.0_rk/REAL(nz - 1)

      grid = 0.0_rk
      df1 = 0.0_rk
      df2 = 0.0_rk

      grid(nz) = 1.0_rk

      DO i = 2, nz - 1
         grid(i) = (i - 1)*delta

         df1(i, i) = -1.0_rk/delta
         df1(i, i + 1) = 1.0_rk/delta
      END DO

      ALLOCATE (gridz(nz), stencilzz(nz, nz))

      gridz = (zf - z0)*grid + z0
      stencilz = df1/(zf - z0)

      DEALLOCATE (grid, df1, df2, gridz, stencilzz)

      RETURN
   END SUBROUTINE FD_NI

   !!Transfere as variáveis da intersecção para o nó (para serem utilizadas nas equações das outras grandezas)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!!!!      Subroutine  FD_IN
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   SUBROUTINE FD_IN(nz, z0, zf, zm, stencilz)

      IMPLICIT NONE

      INTEGER                                    :: i, j
      INTEGER, INTENT(IN)                        :: nz
      REAL(KIND=rk), ALLOCATABLE, DIMENSION(:)   :: grid
      REAL(KIND=rk), ALLOCATABLE, DIMENSION(:, :) :: df1, df2
      REAL(KIND=rk), ALLOCATABLE, DIMENSION(:)   :: gridz
      REAL(KIND=rk), DIMENSION(:, :), INTENT(OUT) :: stencilz
      REAL(KIND=rk), ALLOCATABLE, DIMENSION(:, :) :: stencilzz
      REAL(KIND=rk), INTENT(IN)                  :: z0, zf, zm
      REAL(KIND=rk)                              :: delta

      !grid z

      ALLOCATE (grid(nz), df1(nz, nz), df2(nz, nz))

      delta = 1.0_rk/REAL(nz - 1)

      grid = 0.0_rk
      df1 = 0.0_rk
      df2 = 0.0_rk

      grid(nz) = 1.0_rk

      DO i = 2, nz - 1
         grid(i) = (i - 1)*delta

         df1(i, i) = 1.0_rk/delta
         df1(i, i - 1) = -1.0_rk/delta
      END DO

      ALLOCATE (gridz(nz), stencilzz(nz, nz))

      gridz = (zf - z0)*grid + z0
      stencilz = df1/(zf - z0)

      DEALLOCATE (grid, df1, df2, gridz, stencilzz)

      RETURN
   END SUBROUTINE FD_IN

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!!!!      SUBROUTINE  InterpNI_x
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE InterpNI_x(f)

      IMPLICIT NONE

      INTEGER                                      ::  i
      REAL(KIND=rk), DIMENSION(:, :), INTENT(INOUT) ::  f
      REAL(KIND=rk), ALLOCATABLE, DIMENSION(:, :)   ::  fI

      ALLOCATE (fI(nx, ny))

      fI = 0.0_rk

      DO i = 1, nx - 1
         fI(i, :) = 0.5_rk*(f(i, :) + f(i + 1, :))
      END DO

      f = fI

      DEALLOCATE (fI)

      RETURN
   END SUBROUTINE InterpNI_x

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!!!!      SUBROUTINE  InterpNI_y
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE InterpNI_y(f)

      IMPLICIT NONE

      INTEGER                                      ::  j
      REAL(KIND=rk), DIMENSION(:, :), INTENT(INOUT) ::  f
      REAL(KIND=rk), ALLOCATABLE, DIMENSION(:, :)   ::  fI

      ALLOCATE (fI(nx, ny))

      fI = 0.0_rk

      DO j = 1, ny - 1
         fI(:, j) = 0.5_rk*(f(:, j) + f(:, j + 1))
      END DO

      f = fI

      DEALLOCATE (fI)

      RETURN
   END SUBROUTINE InterpNI_y

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!!!!      SUBROUTINE  InterpIN_x
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE InterpIN_x(f)

      IMPLICIT NONE

      INTEGER                                      ::  i
      REAL(KIND=rk), DIMENSION(:, :), INTENT(INOUT) ::  f
      REAL(KIND=rk), ALLOCATABLE, DIMENSION(:, :)   ::  fN

      ALLOCATE (fN(nx, ny))

      fN = 0.0_rk

      DO i = 2, nx - 1
         fN(i, :) = 0.5_rk*(f(i - 1, :) + f(i, :))
      END DO

      f = fN

      DEALLOCATE (fN)

      RETURN
   END SUBROUTINE InterpIN_x

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!!!!      SUBROUTINE  InterpIN_y
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE InterpIN_y(f)

      IMPLICIT NONE

      INTEGER                                      ::  j
      REAL(KIND=rk), DIMENSION(:, :), INTENT(INOUT) ::  f
      REAL(KIND=rk), ALLOCATABLE, DIMENSION(:, :)   ::  fN

      ALLOCATE (fN(nx, ny))

      fN = 0.0_rk

      DO j = 2, ny - 1
         fN(:, j) = 0.5_rk*(f(:, j - 1) + f(:, j))
      END DO

      f = fN

      DEALLOCATE (fN)

      RETURN
   END SUBROUTINE InterpIN_y

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!!!!      FUNCTION  dx
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   FUNCTION dx(f) RESULT(dfdx)

      IMPLICIT NONE

      INTEGER                                    :: j
      REAL(KIND=rk), DIMENSION(:, :), INTENT(IN) :: f
      REAL(KIND=rk), DIMENSION(:, :), ALLOCATABLE :: dfdx

      ALLOCATE (dfdx(nx, ny))

      DO j = 1, ny
         dfdx(:, j) = MATMUL(stencilx, f(:, j))
      END DO

      RETURN
      DEALLOCATE (dfdx)

   END FUNCTION dx

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!!!!      FUNCTION  dx1
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   FUNCTION dx1(f, stencilx1) RESULT(dfdx1)

      IMPLICIT NONE

      INTEGER                                    :: j
      REAL(KIND=rk), DIMENSION(:, :), INTENT(IN)  :: f
      REAL(KIND=rk), DIMENSION(:, :), INTENT(IN)  :: stencilx1
      REAL(KIND=rk), DIMENSION(:, :), ALLOCATABLE :: dfdx1

      ALLOCATE (dfdx1(nx, ny))
      DO j = 1, ny
         dfdx1(:, j) = MATMUL(stencilx1, f(:, j))
      END DO

      RETURN
      DEALLOCATE (dfdx1)

   END FUNCTION dx1

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!!!!      FUNCTION  dxx
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   FUNCTION dxx(f) RESULT(dfdxx)

      IMPLICIT NONE

      INTEGER                                    :: j
      REAL(KIND=rk), DIMENSION(:, :), INTENT(IN)  :: f
      REAL(KIND=rk), DIMENSION(:, :), ALLOCATABLE :: dfdxx

      ALLOCATE (dfdxx(nx, ny))
      DO j = 1, ny
         dfdxx(:, j) = MATMUL(stencilxx, f(:, j))
      END DO

      RETURN
      DEALLOCATE (dfdxx)

   END FUNCTION dxx

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!!!!      FUNCTION  dy
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   FUNCTION dy(f) RESULT(dfdy)

      IMPLICIT NONE

      INTEGER                                    :: i
      REAL(KIND=rk), DIMENSION(:, :), INTENT(IN)  :: f
      REAL(KIND=rk), DIMENSION(:, :), ALLOCATABLE :: dfdy

      ALLOCATE (dfdy(nx, ny))
      DO i = 1, nx
         dfdy(i, :) = MATMUL(stencily, f(i, :))
      END DO

      RETURN
      DEALLOCATE (dfdy)

   END FUNCTION dy

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!!!!      FUNCTION  dy1
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   FUNCTION dy1(f, stencily1) RESULT(dfdy1)

      IMPLICIT NONE

      INTEGER                                    :: i
      REAL(KIND=rk), DIMENSION(:, :), INTENT(IN)  :: f
      REAL(KIND=rk), DIMENSION(:, :), INTENT(IN)  :: stencily1
      REAL(KIND=rk), DIMENSION(:, :), ALLOCATABLE :: dfdy1

      ALLOCATE (dfdy1(nx, ny))
      DO i = 1, nx
         dfdy1(i, :) = MATMUL(stencily1, f(i, :))
      END DO

      RETURN
      DEALLOCATE (dfdy1)

   END FUNCTION dy1

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!!!!      FUNCTION  dyy
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   FUNCTION dyy(f) RESULT(dfdyy)

      IMPLICIT NONE

      INTEGER                                    :: i
      REAL(KIND=rk), DIMENSION(:, :), INTENT(IN)  :: f
      REAL(KIND=rk), DIMENSION(:, :), ALLOCATABLE :: dfdyy

      ALLOCATE (dfdyy(nx, ny))
      DO i = 1, nx
         dfdyy(i, :) = MATMUL(stencilyy, f(i, :))
      END DO

      RETURN
      DEALLOCATE (dfdyy)

   END FUNCTION dyy

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!!!!      FUNCTION dt
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   FUNCTION dt(fNew, f, fOld, order) RESULT(dfdt)

      IMPLICIT NONE

      INTEGER, INTENT(IN)                        :: order
      REAL(KIND=rk), DIMENSION(:, :), INTENT(IN)  :: fNew, f, fOld
      REAL(KIND=rk), DIMENSION(:, :), ALLOCATABLE :: dfdt

      ALLOCATE (dfdt(nx, ny))
      IF (order .EQ. 1) THEN
         dfdt = (fNew - f)
      ELSE
         dfdt = (1.5_rk*fNew - 2.0_rk*f + 0.5_rk*fOld)
      END IF

      RETURN
      DEALLOCATE (dfdt)

   END FUNCTION dt

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!!!!      FUNCTION fSpacing
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   FUNCTION fSpacing(z, nz) RESULT(zSpacing)

      IMPLICIT NONE

      INTEGER, INTENT(IN)                      :: nz
      REAL(KIND=rk), DIMENSION(:), INTENT(IN)  :: z
      REAL(KIND=rk), DIMENSION(:), ALLOCATABLE :: zSpacing

      ALLOCATE (zSpacing(nz - 1))
      zSpacing = z(2:nz) - z(1:nz - 1)

      RETURN
      DEALLOCATE (zSpacing)

   END FUNCTION fSpacing

END MODULE discretisation
