!Retrofitting an old Rutherford Scattering Simulation

!Need to add definitions magnetic field strengthand include in the do loop

!Program written by Nathan Cosbie-Ross Last edited 22/10/14
!Program designed to  to simulate an electron in a 2d electron gas
!interacting above an artificial magnetic monopole.
!Allows input of varying initial velocities and displacements

PROGRAM Mag_Mon

  IMPLICIT NONE

  !Defines variables as real but with twice the decimal places for
  !optimal accuracy
  !q = charge of electron, dt = time difference, n = count
  !m = mass of electron, h = height of plane above monopole
  !b = magnetic field strength, r = separation, rp = separation in plane
  !rc = separation from projection, f = force, v = velocity
  !theta = angle
  
  DOUBLE PRECISION :: q, dt, n, m, h, b, bx, by, bxi, byi, bi, btot, theta, phi
  DOUBLE PRECISION :: r, ri, rxi, ryi, rx, ry, rc, rp, rtot, fx, fy, vx, vy, v, vxi, vyi
  
  !All constants neccesary for the formulae are defined below 
  q = 1
  m = 9.10938291 * 10 **(-31)
  dt = 1.0 * 10.0 ** (-5.0)

  !The intial velocity and x displacement are requested from the user
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

    !Formulae using lorentz equation to get the force at a given time and 
    !the the change in velocity and position of the electron
    n = n + 1
    fx = (q * vx * bx)
    fy = (q * vy * by)
    vx = vx + (fx / m) * dt
    vy = vy + (fy / m) * dt
    rx = rx + vx * dt
    ry = ry + vy * dt
    r = SQRT((rx ** 2) + (ry ** 2) + (h ** 2))

   !Calculates separation from projection of monopole
    rc = SQRT((rx ** 2) + (ry ** 2))
    !Calculates angle counter-clockwise from projection of monopole in x axis
    theta = asin(ry / rc)
    !Calculates total separation from monopole
    rtot = SQRT((rc ** 2) + (rp ** 2))
    !Calculates angle from monopole perpendicular to x-y plane
    phi = asin(rc / rtot)
    !Calculates magnetic field strength at point and then in x and y respectively
    bi = rtot * sin(phi)
    bx = bi * cos(theta)
    by = bi * sin(theta)
    
     !Outputs the displacement in terms of x and y to the output file
    WRITE(1,*) rx, ry 

  !Ends the loop
  END DO

  !Closes the output file
  CLOSE(1)

!Ends the program
END PROGRAM
