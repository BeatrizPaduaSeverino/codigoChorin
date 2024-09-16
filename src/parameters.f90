MODULE parameters

   USE setPrecision

   INTEGER                   :: nx, ny, xorder, yorder
   INTEGER                   :: sx, sy
   INTEGER                   :: itcMax

   REAL(KIND=rk)             :: Pi
   REAL(KIND=rk)             :: Re
   REAL(KIND=rk)             :: x0, xf, y0, yf
   REAL(KIND=rk)             :: xm, ym
   REAL(KIND=rk)             :: Pr, Pe, Fr, LeF, LeO, Ub
   REAL(KIND=rk)             :: Tb, S, Q
   REAL(KIND=rk)             :: XF0, TF0
   REAL(KIND=rk)             :: beta, sigma
   REAL(KIND=rk)             :: eps
   REAL(KIND=rk)             :: tFinal
   REAL(KIND=rk)             :: NF, NO
   REAL(KIND=rk)             :: g
   REAL(KIND=rk)             :: tol

   CHARACTER(LEN=5)          :: dataDir

      !!!!!!! Definition Parameters  !!!!!!!
   PARAMETER(dataDir='data/', & !dir to save data
             Pi=4._rk*atan(1._rk))      !pi number

   PARAMETER(dataDir='data/', & !dir to save data
             Pi=4._rk*atan(1._rk))      !pi number

      !!!!!!! Control parameters !!!!!!!
   PARAMETER(itcMax=100000, & !max number of iterations
             tFinal=10._rk)                  !final time

      !!!!!!! Parameters of discretisation !!!!!!!
   PARAMETER(nx=40, & !number of points in the x-direction (even)
             ny=40, & !number of points in the y-direction (even)
             xorder=8, & !spatial precision in the x-direction (even)
             yorder=8)                    !spatial precision in the y-direction (even)

      !!!!!!! Domain extension parameters !!!!!!!
   PARAMETER(x0=0._rk, & !x initial
             xf=20._rk, & !x final
             y0=0._rk, & !y initial
             yf=10._rk)                      !y final

      !!!!!!! Grid stretching/accumulation parameters !!!!!!!
   PARAMETER(sx=1, & !streching/accumulation in x-direction?
             sy=1, & !streching/accumulation in y-direction?
             xm=5.0_rk, & !accumulate half the points up to this position in the x-direction
             ym=3.0_rk)                      !accumulate half the points up to this position in the y-direction

      !!!!!!! Hydrodynamic parameters !!!!!!!
   PARAMETER(Ub=1.0_rk, & !fuel ejection velocity (on the burner surface)
             Pr=0.72_rk, & !Prandlt number
             Pe=1._rk, & !Peclet number
             Fr=1._rk, & !Froud number
             g=0._rk)                      !Considering gravity effect?

      !!!!!!! Combustion/thermal parameters !!!!!!!
   PARAMETER(Tb=1._rk, & !burner temperature
             S=3._rk, & !stoichiometric coefficient
             Q=10._rk, & !combustion heat
             sigma=0.7_rk)                   !Power of transport coefficients

      !!!!!!! Differential / preferential diffusion !!!!!!!
   PARAMETER(LeF=1._rk, & !Lewis number of the fuel
             LeO=1._rk, & !Lewis number of the oxidiser
             NF=(LeF - 1._rk)/(LeF*S), & !Source term LeF != 1
             NO=(1._rk - LeO)/LeO)        !Source term LeO != 1

      !!!!!!! Parameters for potencial solution used as initial condition !!!!!!!
   PARAMETER(XF0=2._rk*S*Ub*SQRT(Pe), & !estimate for flame width
             TF0=6._rk)                 !estimate for (adiabatic) flame temperature

      !!!!!!! Compressibility parameter !!!!!!!
   PARAMETER(beta=1._rk)               !compressibility factor

      !!!!!!! Smoothing thickness parameter !!!!!!!
   PARAMETER(eps=0.1_rk)               !thickness of Smoothed Heavide / Dirac delta function

      !!!!!!! Tolerance parameter !!!!!!!
   PARAMETER(tol=0.001)               !tolerance for implicit Euler calculation

END MODULE
