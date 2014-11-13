!Program written by Nathan Cosbie-Ross Last edited 06/11/14
!Program designed to simulate an electron in a 2d electron gas
!interacting in a uniform magnetic field.
!Allows input of varying initial velocities and displacements

PROGRAM Uni_Field

  IMPLICIT NONE

  !Defines variables as real but with twice the decimal places for
  !optimal accuracy
  !q = charge of electron, dt = time difference, n = count
  !m = mass of electron
  !b = magnetic field strength, r = separation, rc = separation in plane
  !f = force, v = velocity !theta = angle around plane
  
  DOUBLE PRECISION :: qe, dt, n, m, h, b, d, pi, nmax
  DOUBLE PRECISION :: rx, ry, rc, fx, fy, vx, vy
  
  !All constants necessary for the formulae are defined below 
  qe = 1.602 * 10 ** (-19.0)
  m = 9.10938291 * 10 **(-31.0)
  pi = 3.14159265359

  !The initial velocities, displacements and magnetic strength
  !are requested from the user
  WRITE(*,*) 'Please enter the initial x displacement'
  WRITE(*,*) 'of the electron (nm)'
  READ(*,*) rx
  WRITE(*,*) 'Please enter the initial y displacement'
  WRITE(*,*) 'of the electron (nm)'
  READ(*,*) ry
  WRITE(*,*) 'Please enter the initial x velocity'
  WRITE(*,*) 'of the electron (nm/s)'
  READ(*,*) vx
  WRITE(*,*) 'Please enter the initial y velocity'
  WRITE(*,*) 'of the electron (nm/s)'
  READ(*,*) vy
  WRITE(*,*) 'Please enter the strength of the field (nT)'
  READ(*,*) b
  WRITE(*,*) 'Please enter the timestep (ns)'
  READ(*,*) dt
  WRITE(*,*) 'Please enter the number of iterations'
  READ(*,*) nmax
  !Converts Magnetic field and displacements/velocities
  b = b * 10 ** (-9.0)
  rx = rx * 10 ** (-9.0)
  ry = ry * 10 ** (-9.0)
  vx = vx * 10 ** (-9.0)
  vy = vy * 10 ** (-9.0)
  !Initialises the count n
  n = 0

  !Opens the output file
  OPEN(1, FILE='Uni_Field.out')
    
  !Repeats the iterations of the formulae within the loop till n = 1000
  DO WHILE (n .LE. nmax)
   
    !Formulae using lorentz equation to get the force at a given time 
    fx = -(qe * vy * b)
    fy = (qe * vx * b)
	WRITE(1,*) fx, fy
    !Adds the change in velocity due to the velocity
    vx = vx + (fx / m) * dt * 10 ** (-9.0)
    vy = vy + (fy / m) * dt * 10 ** (-9.0)
	WRITE(1,*) vx, vy
    !Adds to the displacement due to velocity
    rx = rx + vx * dt
    ry = ry + vy * dt

    !Adds to the count
    n = n + 1
    
    !Outputs the displacement in terms of x and y to the output file
    WRITE(1,*) rx, ry 
  
  !Ends the loop
  END DO
  WRITE(*,*) 'Iterations Complete'
  !Closes the output file
  CLOSE(1)

!Ends the program
END PROGRAM

