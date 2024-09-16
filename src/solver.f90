MODULE solver

   USE parameters
   USE setPrecision
   USE variables
   USE boundaryConditions
   USE rhs
   USE residue

CONTAINS

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!      SUBROUTINE solve
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE solve(tr, rhoNew, rho, rhoOld, uNew, u, uOld, vNew, &
                    v, vOld, pNew, p, pOld, rhoTau, uTau, vTau, &
                    pTau, resP, resU, resV)

      IMPLICIT NONE
      INTEGER, INTENT(IN)                              :: tr
      INTEGER                                          :: iter

      REAL(KIND=rk), DIMENSION(:, :), INTENT(INOUT)  :: rhoNew, pNew, uNew
      REAL(KIND=rk), DIMENSION(:, :), INTENT(INOUT)  :: vNew
      REAL(KIND=rk), DIMENSION(:, :), INTENT(IN)     :: rho, p, u, v
      REAL(KIND=rk), DIMENSION(:, :), INTENT(IN)     :: rhoOld, pOld, uOld
      REAL(KIND=rk), DIMENSION(:, :), INTENT(IN)     :: vOld
      REAL(KIND=rk), DIMENSION(nx, ny)                 :: rhsP, rhsU, rhsV
      REAL(KIND=rk), DIMENSION(nx, ny)                 :: dP, dU, dV
      REAL(KIND=rk), DIMENSION(nx, ny)                 :: pTau, uTau, vTau
      REAL(KIND=rk), DIMENSION(nx, ny)                 :: rhoTau
      REAL(KIND=rk), DIMENSION(nx, ny)                 :: pTau1, uTau1, vTau1
      REAL(KIND=rk), DIMENSION(nx, ny)                 :: rhoTau1
      REAL(KIND=rk), DIMENSION(:, :), INTENT(OUT)    :: resP, resU, resV
      REAL(KIND=rk)                                    :: errMax, errP, errU, errV

      pTau = pNew
      uTau = uNew
      vTau = vNew
      rhoTau = rhoNew

      iter = 0
      errMax = 1._rk
      errP = 1._rk
      errU = 1._rk
      errV = 1._rk

      DO WHILE (errMax .GT. tol)

         iter = iter + 1

            !!!-------- Pressure (continuity equation)

         rhsP = rhs_P(rhoTau, beta, uTau, vTau)
         resP = res_P(tr, pTau, p, pOld, rhsP)
         dP = resP*tauSpacing*beta

            !!!-------- Velocity component u (momentum equation in the x-direction)
         CALL bcU(uTau, x, y, xSpacing, ySpacing)
         rhsU = rhs_U(uTau, vTau, pTau)
         dU = res_Phi(tr, rhoTau, rho, rhoOld, uTau, u, uOld, resP, rhsU)*tauSpacing

            !!!-------- Velocity component v (momentum equation in the y-direction)
         CALL bcV(vTau, x, y, xSpacing, ySpacing)
         rhsV = rhs_V(uTau, vTau, pTau)
         dV = res_Phi(tr, rhoTau, rho, rhoOld, vTau, v, vOld, resP, rhsV)*tauSpacing

         pTau1 = pNew + dP
         uTau1 = uNew + du
         vTau1 = vNew + dv

         CALL bcU(uTau1, x, y, xSpacing, ySpacing)
         CALL bcV(vTau1, x, y, xSpacing, ySpacing)

         errP = MAXVAL(ABS((pTau1 - pTau)/(tauSpacing*beta)))
         errU = MAXVAL(ABS((uTau1 - uTau)/tauSpacing))
         errV = MAXVAL(ABS((vTau1 - vTau)/tauSpacing))


         errMax = MAX(errP, errU, errV)

         !WRITE(*,*) errMax

         pTau = pTau1
         uTau = uTau1
         vTau = vTau1

            !!!-------- Solve for contant rho 
         rhoTau = rho

      END DO

      rhsP = rhs_P(rhoTau, uTau, vTau)
      resP = res_P(tr, pTau, pNew, p, rhsP)

      CALL bcU(uTau, x, y, xSpacing, ySpacing)
      rhsU = rhs_U(rhoTau, uTau, vTau, pTau)
      resU = res_Phi(tr, rhoTau, rhoNew, rho, uTau, uNew, u, resP, rhsU)

      CALL bcV(vTau, x, y, xSpacing, ySpacing)
      rhsV = rhs_V(rhoTau, uTau, vTau, pTau)
      resV = res_Phi(tr, rhoTau, rhoNew, rho, vTau, vNew, v, resP, rhsV)

      !update the variables
      pNew = pTau
      uNew = uTau
      vNew = vTau
      rhoNew = rhoTau

      !update boundary conditions
      !CALL bcRho(rhoNew, x, y, xSpacing, ySpacing)
      CALL bcU(uNew, x, y, xSpacing, ySpacing)
      CALL bcV(vNew, x, y, xSpacing, ySpacing)

      RETURN

   END SUBROUTINE solve

END MODULE solver
