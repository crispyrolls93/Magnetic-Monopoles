!Need to add how the magnetic field strength falls with distance 1/r^3 (relationship)

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
  !b = magnetic field strength, r = separation, rc = separation in plane
  !rp = separation from projection, f = force, v = velocity
  !theta = angle around plane, phi = angle perpendicular to plane
  
  DOUBLE PRECISION :: q, dt, n, m, h, b, bx, by, bc, btot, theta, phi
  DOUBLE PRECISION :: r, rx, ry, rc, rp, rtot, fx, fy, vx, vy
  
  !All constants neccesary for the formulae are defined below 
  q = 1.602 * 10 ** (-19)
  m = 9.10938291 * 10 **(-31)
  dt = 1.0 * 10.0 ** (-5.0)

  !The intial velocities, displacements and magnetic strength
  !are requested from the user
  WRITE(*,*) 'Please enter the initial x displacement'
  WRITE(*,*) 'of the electron'
  READ(*,*) rx
  WRITE(*,*) 'Please enter the initial y displacement'
  WRITE(*,*) 'of the electron'
  READ(*,*) ry
  WRITE(*,*) 'Please enter the initial x velocity'
  WRITE(*,*) 'of the electron'
  READ(*,*) vx
  WRITE(*,*) 'Please enter the initial y velocity'
  WRITE(*,*) 'of the electron'
  READ(*,*) vy
  WRITE(*,*) 'Please enter the strength of the monopole'
  READ(*,*) btot
  WRITE(*,*) 'Please enter the separtaion of the monopole'
  WRITE(*,*) 'from the plane'
  READ(*,*) rp
  
  !Initialises the count n
  n = 0

  !Opens the output file
  OPEN(1, FILE='Mag_Mon.out')

  !Writes the initial x and y displacement to the output file
  WRITE(1,*) x, y

  !Repeats the iterations of the formulae within the loop till n = 1000
  DO WHILE (n .LE. 1000)

    !Calculates separation from projection of monopole
    rc = SQRT((rx ** 2) + (ry ** 2))
    !Calculates total separation from monopole
    rtot = SQRT((rc ** 2) + (rp ** 2))
    
    !Calculates angle counter-clockwise from projection of monopole in x axis
    theta = atan(ry / rx)
    !Calculates angle from monopole perpendicular to x-y plane
    phi = atan(rc / rp)
    
    !Calculates magnetic field strength at point and then in x and y respectively
    bc = btot * sin(phi)
    bx = bc * cos(theta)
    by = bc * sin(theta)

    !Formulae using lorentz equation to get the force at a given time 
    fx = (q * vy * by)
    fy = (q * vx * bx)

    !Adds the change in velocity due to the velocity
    vx = vx + (fx / m) * dt
    vy = vy + (fy / m) * dt

    !Adds to the displacement due to velocity
    rx = rx + vx * dt
    ry = ry + vy * dt
    r = SQRT((rx ** 2) + (ry ** 2) + (h ** 2))

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
