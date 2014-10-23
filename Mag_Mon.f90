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
  !b = magnetic field strength, r = separation
  !rc = separation from projection, f = force, v = velocity
  
  DOUBLE PRECISION :: q, dt, n, m, h, b, bx, by, bxi, byi, bi
  DOUBLE PRECISION :: r, ri, rxi, ryi, rx, ry, rc, fx, fy, vx, vy, v, vxi, vyi
  
  !All constants neccesary for the formulae are defined below 
  q = 1
  m = 9.10938291 * 10 **(-31)
  dt = 1.0 * 10.0 ** (-5.0)
  y = -0.005

  !The intial velocity and x displacement are requested from the user
  WRITE(*,*) 'Please enter the initial x displacement'
  WRITE(*,*) 'of the electron'
  READ(*,*) x
  WRITE(*,*) 'Please enter the initial y displacement'
  WRITE(*,*) 'of the electron'
  READ(*,*) y
  WRITE(*,*) 'Please enter the initial x velocity'
  WRITE(*,*) 'of the electron'
  READ(*,*) vxi
  WRITE(*,*) 'Please enter the initial y velocity'
  WRITE(*,*) 'of the electron'
  READ(*,*) vyi
  WRITE(*,*) 'Please enter the strength of the monopole'
  READ(*,*) bi

  !Defines inital velocities and positions in both directions
  vx = vxi
  vy = vyi
  rx = x
  ry = y
  bxi = 
  byi = 

  !Calculates the initial value of the total separation
  ri = SQRT((x ** 2) + (y ** 2))

  !Initialises the count n
  n = 0

  !Opens the output file
  OPEN(1, FILE='Mag_Mon.out')

  !Writes the initial x and y displacement to the output file
  WRITE(1,*) x, y

  !Repeats the iterations of the formulae within the loop till r > rlim
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
    rc = SQRT((rx ** 2) + (ry ** 2))
    r = SQRT((rx ** 2) + (ry ** 2) + (h ** 2))
 
    !Outputs the displacement in terms of x and y to the output file
    WRITE(1,*) rx, ry 

  !Ends the loop
  END DO

  !Closes the output file
  CLOSE(1)

!Ends the program
END PROGRAM
