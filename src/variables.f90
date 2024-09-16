MODULE variables

   USE parameters
   USE setPrecision

   INTEGER                                        :: tr, itc
   REAL(KIND=rk), ALLOCATABLE, DIMENSION(:)       :: x, y, xSpacing, ySpacing
   REAL(KIND=rk), ALLOCATABLE, DIMENSION(:)       :: gridx, gridy
   REAL(KIND=rk), ALLOCATABLE, DIMENSION(:, :)    :: stencilx, stencilxx
   REAL(KIND=rk), ALLOCATABLE, DIMENSION(:, :)    :: stencily, stencilyy
   REAL(KIND=rk), ALLOCATABLE, DIMENSION(:, :)    :: rhoTau, rhoNew, rho, rhoOld
   REAL(KIND=rk), ALLOCATABLE, DIMENSION(:, :)    :: uTau, uNew, u, uOld
   REAL(KIND=rk), ALLOCATABLE, DIMENSION(:, :)    :: vTau, vNew, v, vOld
   REAL(KIND=rk), ALLOCATABLE, DIMENSION(:, :)    :: pTau, pNew, p, pOld
   REAL(KIND=rk), ALLOCATABLE, DIMENSION(:, :)    :: tauSpacing
   REAL(KIND=rk), ALLOCATABLE, DIMENSION(:, :)    :: resU, resV, resP
   REAL(KIND=rk)                                  :: tSpacing
   REAL(KIND=rk)                                  :: time
   REAL(KIND=rk)                                  :: residueP, residueU, residueV

CONTAINS

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!!!!      SUBROUTINE init
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   SUBROUTINE init

      IMPLICIT NONE

      !initialize variables related to the iterations
      tr = 1
      itc = 1 !iteração inicial
      time = 0._rk

      tSpacing = 0._rk

      ALLOCATE (x(nx), xSpacing(nx - 1), &
                y(ny), ySpacing(ny - 1))

      x = 0._rk
      y = 0._rk

      xSpacing = 0._rk
      ySpacing = 0._rk

      residueP = 1._rk
      residueU = 1._rk
      residueV = 1._rk

      ALLOCATE (tauSpacing(nx, ny))

      tauSpacing = 0._rk

      ALLOCATE (stencilx(nx, nx), stencilxx(nx, nx), &
                stencily(ny, ny), stencilyy(ny, ny))

      !call CPU_TIME(cpu_start(2))

      stencilx = 0._rk
      stencilxx = 0._rk
      stencily = 0._rk
      stencilyy = 0._rk

      ALLOCATE (rhoTau(nx, ny), rhoNew(nx, ny), rho(nx, ny), rhoOld(nx, ny), &
                uTau(nx, ny), uNew(nx, ny), u(nx, ny), uOld(nx, ny), &
                vTau(nx, ny), vNew(nx, ny), v(nx, ny), vOld(nx, ny), &
                pTau(nx, ny), pNew(nx, ny), p(nx, ny), pOld(nx, ny))

      rhoTau = 0._rk
      rhoNew = 0._rk
      rho = 0._rk
      rhoOld = 0._rk

      uTau = 0._rk
      uNew = 0._rk
      u = 0._rk
      uOld = 0._rk

      vTau = 0._rk
      vNew = 0._rk
      v = 0._rk
      vOld = 0._rk

      pTau = 0._rk
      pNew = 0._rk
      p = 0._rk
      pOld = 0._rk

      ALLOCATE (resU(nx, ny), resV(nx, ny), resP(nx, ny))

      resU = 0._rk
      resV = 0._rk
      resP = 0._rk

      write (*, *) 'Data initialised'

      RETURN
   END SUBROUTINE init

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!!!!      SUBROUTINE fin
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   SUBROUTINE fin

      IMPLICIT NONE

      DEALLOCATE (x, y)

      DEALLOCATE (stencilx, stencilxx, stencily, stencilyy)

      DEALLOCATE (rhoTau, rhoNew, rho, rhoOld, &
                  uTau, uNew, u, uOld, &
                  vTau, vNew, v, vOld, &
                  pTau, pNew, p, pOld)

      write (*, *) 'Finished'

      RETURN
   END SUBROUTINE fin

END MODULE variables
