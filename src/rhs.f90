MODULE rhs

   USE parameters
   USE setPrecision
   USE discretisation
  
CONTAINS

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!      FUNCTION  rhs_P
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   FUNCTION rhs_P(rho, beta, u, v) result(rhsP)

      IMPLICIT NONE
      REAL(KIND=rk), INTENT (IN)                   :: beta
      REAL(KIND=rk), INTENT(IN), DIMENSION(nx, ny) :: rho, u, v
      REAL(KIND=rk), DIMENSION(nx, ny)             :: rhsP

      rhsP = - beta*(dx(u) + dy(v))

   END FUNCTION rhs_P

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!      FUNCTION  rhs_U
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   FUNCTION rhs_U( u, v, p) result(rhsU)

      IMPLICIT NONE
      INTEGER                                           :: i, j
      REAL(KIND=rk), INTENT(IN), DIMENSION(nx, ny)      :: u, v, p
      REAL(KIND=rk), DIMENSION(nx, ny)                  :: rhsU

      rhsU = - ( u*dx(u) + v*dy(u) + (1/Re)*(dx(dx(u))+dy(dy(u))) - dx(p) )

   END FUNCTION rhs_U

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!      FUNCTION  rhs_V
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   FUNCTION rhs_V(u, v, p) result(rhsV)

      IMPLICIT NONE
      INTEGER                                           :: i, j
      REAL(KIND=rk), INTENT(IN), DIMENSION(nx, ny)      :: u, v, p
      REAL(KIND=rk), DIMENSION(nx, ny)                  :: rhsV

      rhsV = - ( u*dx(v) + v*dy(v) ) + ( (1/Re)*(dx(dx(v))+dy(dy(v))) - dy(p) )

   END FUNCTION rhs_V

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!      FUNCTION  rhs_Z
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE rhs
