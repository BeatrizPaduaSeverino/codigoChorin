PROGRAM main

   USE parameters
   USE setPrecision
   USE variables
   USE initialCondition
   USE boundaryConditions
   USE solver
   USE dataWrite

   IMPLICIT NONE
   INTEGER :: i, j

   !Allocate and initialise variables with zeros
   CALL init

   !Calculate grid and stencils
   CALL FD(nx, x0, xf, xm, gridx, stencilx, stencilxx) !x-direction
   CALL FD(ny, y0, yf, ym, gridy, stencily, stencilyy) !y-direction

   !Impose initial condition
   CALL ic(x, y, rho, u, v, p, xSpacing, ySpacing)

   !initialize boundary conditions for the first iteration
   !CALL bcRho(rho, x, y, xSpacing, ySpacing)
   CALL bcU(u, x, y, xSpacing, ySpacing)
   CALL bcV(v, x, y, xSpacing, ySpacing)

   !pressure reference
   !p(0,ny)   = 0._rk

   !next time variables
   UNew = u
   vNew = v
   rhoNew = rho
   pNew = p

   !grid spacing in the x-direction
   xSpacing = fSpacing(x, nx)

   !grid spacing in the y-direction
   ySpacing = fSpacing(y, ny)

   !pseudo time step
  ! tauSpacing = Ub*ABS(MINVAL(ySpacing))/(Ub + SQRT(Ub*Ub*(1._rk + beta)))
  ! tauSpacing = 0.0001_rk*tauSpacing

   !physical time step
   tSpacing = MINVAL(ySpacing)*10.0_rk

   !Print information
   CALL printdata

   !-------- output initial conditions
   CALL transient(uNew, vNew, pNew, rhoNew, tr)

   !------ output data file ------------------------
   CALL output(uNew, vNew, rhoNew, pNew, tr)

   DO WHILE ((tr .LE. tFinal/tSpacing))

      !CALL SYSTEM('rm -fv data/error.dat')
      !CALL SYSTEM('rm -fv data/residual.dat')

      time = time + tSpacing
      write (*, *) '#########################################'
      write (*, *) 'Iteration:', tr
      write (*, *) 'PHYSICAL TIME:', time

            !!-------- pseudo-time calculation starts ---------------------------------------------------
      DO WHILE (itc .LT. itcMax)

                !!!-------- Solve the system of equations
         CALL solve(tr, rhoNew, rho, rhoOld, uNew, u, uOld, vNew, v, vOld, pNew, p, pOld, &
                    rhoTau, uTau, vTau, pTau, resP, resU, resV)

                !!!-------- Calculate maximum residue
         DO j = 1, ny - 1
            DO i = 1, nx - 1

               IF (x(i)**2._rk + y(j)**2._rk .LE. (1._rk + 1.1_rk*max(xSpacing(i), ySpacing(j)))**2) THEN
                  resP(i, j) = 0._rk
                  resU(i, j) = 0._rk
                  resV(i, j) = 0._rk
               END IF

            END DO
         END DO

         resP(1, :) = 0._rk
         resU(1, :) = 0._rk
         resV(1, :) = 0._rk

         resP(nx, :) = 0._rk
         resU(nx, :) = 0._rk
         resV(nx, :) = 0._rk

         resP(:, ny) = 0._rk
         resU(:, ny) = 0._rk
         resV(:, ny) = 0._rk

         resP(:, 1) = 0._rk
         resU(:, 1) = 0._rk
         resV(:, 1) = 0._rk

         residueP = MAXVAL(ABS(resP))
         residueU = MAXVAL(ABS(resU))
         residueV = MAXVAL(ABS(resV))

         OPEN (UNIT=550, FILE='resP.dat', ACTION="write", STATUS="replace")
         DO i = 1, nx
            WRITE (550, *) (REAL(resP(i, j)), j=1, ny)
         END DO
         CLOSE (550)

         OPEN (UNIT=551, FILE='resU.dat', ACTION="write", STATUS="replace")
         DO i = 1, nx
            WRITE (551, *) (REAL(resU(i, j)), j=1, ny)
         END DO
         CLOSE (551)

         OPEN (UNIT=552, FILE='resV.dat', ACTION="write", STATUS="replace")
         DO i = 1, nx
            WRITE (552, *) (REAL(resV(i, j)), j=1, ny)
         END DO
         CLOSE (552)

         IF (MOD(itc, 100) .EQ. 0) THEN
            WRITE (*, *) '--------------------------------------------------------------------------------------'
            write (*, *) tr, '--Physical time:', time
            WRITE (*, *) '         dtau:', maxval(tauSpacing)
            WRITE (*, *) '         dt:', tSpacing
            WRITE (*, *) ' Art Compressibility (c^2):', beta
            WRITE (*, *) ' dtau x c^2 :', maxval(tauSpacing)*beta
            WRITE (*, *) ' dtau x c^2 / dt :', maxval(tauSpacing)*beta/tSpacing
            WRITE (*, *) ' dtau / dt :', maxval(tauSpacing)/tSpacing
            WRITE (*, *) '--------------------------------------------------------------------------------------'
            WRITE (*, *) itc, 'res  ', residueP, residueU, residueV
            WRITE (*, *) '--------------------------------------------------------------------------------------'
         END IF

         !pressure reference
         !pNew(0,ny)   = 0._rk

         itc = itc + 1

      END DO
            !!------ END OF pseudo-time CALCULATION ---------------------------------------------------

      !update the physical variables
      pOld = p
      uOld = u
      vOld = v
      rhoOld = rho

      p = pNew
      u = uNew
      v = vNew
      rho = rhoNew

      !print the physical time step results
      WRITE (*, *) '-----------------------------------------------------------'
      write (*, *) tr, '--Physical time:', time
      WRITE (*, *) '         dtau:', maxval(tauSpacing)
      WRITE (*, *) '         dt:', tSpacing
      WRITE (*, *) ' (Art Compressibility) beta:', beta
      WRITE (*, *) '-----------------------------------------------------------'
      WRITE (*, *) '    Accuracy P:', residueP
      WRITE (*, *) '    Accuracy U:', residueU
      WRITE (*, *) '    Accuracy V:', residueV
      WRITE (*, *) '-----------------------------------------------------------'
      WRITE (*, *) '    Pseudo-iterations to convergence:', itc
      WRITE (*, *) '-----------------------------------------------------------'

      WRITE (*, *) 'Pseudo-iterations to convergence:', itc
      write (*, *) '#########################################'

      !update convergence criteria
      itc = 0

      !next physical time step
      tr = tr + 1

      !-------- output initial conditions
      CALL transient(uNew, vNew, pNew, rhoNew, tr)

      !------ output data file ------------------------
      CALL output(uNew, vNew, rhoNew, pNew, tr)

   END DO
!!------ END of physical CALCULATION -----------------------------------------------------

   !Deallocate variaibles
   CALL fin

   STOP
END PROGRAM main
