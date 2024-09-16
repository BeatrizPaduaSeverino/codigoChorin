MODULE auxiliary

   USE setPrecision
   USE parameters

CONTAINS

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!      FUNCTION  thetaEps
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   FUNCTION thetaEps(x) RESULT(tEps) !Smoothed Heaviside step function

      IMPLICIT NONE
      REAL(KIND=rk)             :: tEps
      REAL(KIND=rk), INTENT(IN) :: x
      REAL(KIND=rk)             :: k

      k = 2._rk*Pi/eps
      tEps = 0.5_rk*(1._rk + tanh(k*x))

   END FUNCTION thetaEps

END MODULE auxiliary

