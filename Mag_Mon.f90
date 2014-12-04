!Program written by Nathan Cosbie-Ross Last edited 06/11/14
!Program designed to simulate an electron in a 2d electron gas
!interacting above an artificial magnetic monopole.
!Allows input of varying initial velocities, displacements and monopole strengths

PROGRAM Mag_Mon

  IMPLICIT NONE

  !Defines variables as real but with twice the decimal places for
  !optimal accuracy
  !q = charge of electron, dt = time difference, n = count
  !m = mass of electron, h = height of plane above monopole
  !b = magnetic field strength, r = separation
  !rp = separation from projection, f = fore, v = velocity
  
  DOUBLE PRECISION :: qe, qm, dt, n, m, h, b, bx, by, bc, bp, pi, nmax
  DOUBLE PRECISION :: r, rx, ry, r, rp, fx, fy, vx, vy, mu0, ep0
  
  !All constants necessary for the formulae are defined below 
  qe = 1.602 * 10 ** (-19.0)
  m = 9.10938291 * 10 **(-31.0)
  mu0 = 1.25663706 * 10 ** (-6.0)
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
  WRITE(*,*) 'Please enter the strength of the monopole'
  READ(*,*) qm
  WRITE(*,*) 'Please enter the separation of the monopole'
  WRITE(*,*) 'from the plane'
  READ(*,*) rp
  WRITE(*,*) 'Please enter the timestep (ns)'
  READ(*,*) dt
  WRITE(*,*) 'Please enter the number of iterations'
  READ(*,*) nmax
  
  !Initialises the count n
  n = 0
  !Converts into SI Units
  rx = rx * 10.0 ** (-9.0)
  ry = ry * 10.0 ** (-9.0)
  vx = vx * 10.0 ** (-9.0)
  vy = vy * 10.0 ** (-9.0)
  dt = dt * 10.0 ** (-9.0)

  !Opens the output file
  OPEN(1, FILE='Mag_Mon.out')

  !Writes the initial x and y displacement to the output file
  WRITE(1,*) rx, ry

  !Repeats the iterations of the formulae within the loop till set number of iterations
  DO WHILE (n .LE. nmax)

    !Calculates separation from projection of monopole
    r = SQRT((rx ** 2.0) + (ry ** 2.0))
    
    !Calculates magnetic field strength at point perpendicular to the plane.
    bp = ((mu0 / 4.0 * pi)) * ((qm * rp) / (((rp ** 2.0) + (r ** 2.0)) ** (3.0 / 2.0)))
    
    !Formulae using lorentz equation to get the fore at a given time 
    fx = (qe * vy * bp)
    fy = -(qe * vx * bp)

    !Adds the change in velocity due to the velocity
    vx = vx + ((fx / m) * dt)
    vy = vy + ((fy / m) * dt)

    !Adds to the displacement due to velocity
    rx = rx + vx * dt
    ry = ry + vy * dt
    r = SQRT((rx ** 2.0) + (ry ** 2.0) + (h ** 2.0))

    !Adds to the count
    n = n + 1
    
    !Outputs the displacement in terms of x and y to the output file
    WRITE(1,*) rx, ry 

  !Ends the loop
  END DO

  !Closes the output file
  CLOSE(1)

!Ends the program
END PROGRAM

