!Retrofitting an old Rutherford Scattering Simulation

!Program written by Nathan Cosbie-Ross Last edited 22/10/14
!Program designed to  to simulate an electron in a 2d electron gas
!interacting above an artificial magnetic monopole.
!Allows input of varying initial velocities and displacements

PROGRAM Mag_Mon

  IMPLICIT NONE

  !Defines variables as real but with twice the decimal places for
  !optimal accuracy
  DOUBLE PRECISION :: m, qa, qg, rlim, c, dt, vc
  DOUBLE PRECISION :: x, y, fx, fy, vx, vy, rx, ry, r, v, ri
  
  !All constants neccesary for the formulae are defined below 
  qa = 2
  qg = 79
  c = 137.035999
  m = 7294.3
  dt = 1.0 * 10.0 ** (-5.0)
  y = -0.005

  !The intial velocity and x displacement are requested from the user
  WRITE(*,*) 'Please enter the initial horizontal displacement'
  WRITE(*,*) 'of the alpha particle'
  READ(*,*) x
  WRITE(*,*) 'Please enter the initial velocity in terms of c'
  WRITE(*,*) 'of the alpha particle'
  READ(*,*) vc

  !The velocity is read in from the equation in terms of c so this converts
  !to atomic units
  v = vc * c

  !Defines inital velocities and positions in both directions
  vx = 0
  vy = v
  rx = x
  ry = y

  !Calculates the initial value of the total separation
  ri = SQRT((x ** 2) + (y ** 2))

  !Provides the limiting factor
  rlim = ABS(1.1 * ri)
  
  !Initialise the r variable so that rlim > r
  r = 0

  !Opens the output file
  OPEN(1, FILE='assign_11_1.out')

  !Writes the initial x and y displacement to the output file
  WRITE(1,*) x, y

  !Repeats the iterations of the formulae within the loop till r > rlim
  DO WHILE (rlim .GE. r)

    !Formulae using coulombs equation to get the force at a given time and 
    !the the change in velocity and position of the electron
    fx = (rx / SQRT(rx ** 2 + ry ** 2)) * ((qa * qg) / (rx ** 2 + ry ** 2))
    fy = (ry / SQRT(rx ** 2 + ry ** 2)) * ((qa * qg) / (rx ** 2 + ry ** 2))
    vx = vx + (fx / m) * dt
    vy = vy + (fy / m) * dt
    rx = rx + vx * dt
    ry = ry + vy * dt
    r = SQRT((rx ** 2) + (ry ** 2))
 
    !Outputs the displacement in terms of x and y to the output file
    WRITE(1,*) rx, ry 

  !Ends the loop
  END DO

  !Closes the output file
  CLOSE(1)

!Ends the program
END PROGRAM
