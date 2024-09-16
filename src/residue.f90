MODULE residue

   USE parameters
   USE setPrecision
   USE discretisation

CONTAINS

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!      FUNCTION  res_P
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   FUNCTION res_P(tr, pNew, p, pOld, rhsP) result(resP)

      IMPLICIT NONE
      INTEGER, INTENT(IN)                              :: tr
      REAL(KIND=rk), INTENT(IN), DIMENSION(nx, ny)     ::  pNew, p, pOld, rhsP
      REAL(KIND=rk), DIMENSION(nx, ny)  :: resP

      resP = -dt(pNew, p, pOld, tr) + rhsP

   END FUNCTION res_P

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!      FUNCTION  res_Phi
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   FUNCTION res_Phi(tr, rhoNew, rho, rhoOld, phiNew, phi, phiOld, resP, rhsPhi) result(resPhi)

      IMPLICIT NONE
      INTEGER, INTENT(IN)                                                            :: tr
      REAL(KIND=rk), INTENT(IN), DIMENSION(nx, ny)     :: rhoNew, rho, rhoOld
      REAL(KIND=rk), INTENT(IN), DIMENSION(nx, ny)     :: phiNew, phi, phiOld
      REAL(KIND=rk), INTENT(IN), DIMENSION(nx, ny)     :: resP, rhsPhi
      REAL(KIND=rk), DIMENSION(nx, ny) :: resPhi

      resPhi = (-dt(rhoNew*phiNew, rho*phi, rhoOld*phiOld, tr) + rhsPhi &
                - phiNew*(resP/beta))/rhoNew
      !resPhi = - dt(phiNew, phi, phiOld, tr)  + rhsPhi

   END FUNCTION res_Phi

END MODULE residue
