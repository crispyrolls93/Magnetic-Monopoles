!Program written by Nathan Cosbie-Ross Last edited 06/11/14
!Program designed to simulate an electron in a 2d electron gas
!interacting above an artificial magnetic monopole.
!Allows input of varying initial velocities, displacements and monopole strengths



!Add perentage tracker

PROGRAM Mag_Mon

  IMPLICIT NONE

  !Defines variables as real but with twice the decimal places for
  !optimal accuracy
  !q = charge of electron, dt = time difference, n = count
  !m = mass of electron, h = height of plane above monopole
  !b = magnetic field strength, r = separation
  !rp = separation from projection, f = fore, v = velocity
  
  DOUBLE PRECISION :: qe, qm, dt, n, m, h, b, bc, bp, pi, nmax, bp2
  DOUBLE PRECISION :: r, rx, ry, fx, fy, vx, vy, mu0, ep0, qd, ek, bp1
  
  
  !All constants necessary for the formulae are defined below 
  qe = 1.602D-19
  m = 9.10938291D-31
  mu0 = 1.25663706D-6
  pi = 3.14159265359

  !The initial velocities, displacements and magnetic strength
  !are requested from the user
  ek = 0.1466D-24
  qd = 3.3D-9
  rx = -2.403D-6
  ry = 0
  vx = 0
  vy = ((2.0 * ek)/m) ** (0.5)
  qm = 4.0 * qd 
  h = 3.117D-6
  dt = 1D-6
  nmax = 1D6
  
  !Initialises the count n
  n = 0

  !Opens the output file
  OPEN(1, FILE='Mag_Mon_Test.txt')

  !Writes the initial x and y displacement to the output file
 

  !Repeats the iterations of the formulae within the loop till set number of iterations
  DO WHILE (n .LE. nmax)

    !Adds to the count
    n = n + 1
	
    !Calculates separation from projection of monopole
    r = SQRT((rx ** 2.0) + (ry ** 2.0))
    
    !Calculates magnetic field strength at point perpendicular to the plane.
    bp1 = ((mu0 / (4.0 * pi)))
    bp2 =((qm * h) / (((h ** 2.0) + (r ** 2.0)) ** (1.5)))
	bp = bp1 * bp2
    !Formulae using lorentz equation to get the force at a given time 
    fx = (qe * vy * bp)
    fy = -(qe * vx * bp)

    !Adds the change in velocity due to the velocity
    vx = vx + ((fx / m) * dt)
    vy = vy + ((fy / m) * dt)

    !Adds to the displacement due to velocity
    rx = rx + (vx * dt)
    ry = ry + (vy * dt)
    
    !Outputs the displacement in terms of x and y to the output file
    WRITE(1,*) rx, ry 

  !Ends the loop
  END DO

  !Closes the output file
  CLOSE(1)

!Ends the program
END PROGRAM

