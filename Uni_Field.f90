!Program Written by Nathan Cosbie-Ross Last Edited 21/02/15
!Program Designed to Simulate an Electron in a 2D Electron Gas Interacting in a Uniform Magnetic Field.

!-----Begins Program-----!
PROGRAM Uni_Field

!-----Fix For Legacy Flaw-----!
IMPLICIT NONE

!-----Defines All Variable Types-----!
!q = charge of electron, dt = time difference, n = count, m = mass of electron
!b = magnetic field strength, r = separation, rc = separation in plane
!f = force, v = velocity !theta = angle around plane
DOUBLE PRECISION :: qe, dt, n, m, h, b, d, i, imax, tmax, t
DOUBLE PRECISION :: rx, ry, rc, fx, fy, vx, vy , mu

!-----All Constants Necessary For The Formulae Are Defined-----!
qe = 1
m = 1

!-----Initial Conditions Defined-----!
rx = 1
ry = 0
vx = 0
vy = 1
b = 1
dt = 0.001
imax = 10000

!-----Initialise Count-----!
n = 0
i = 0

!Opens the output file
OPEN(1, FILE='Uni_Field.dat')

!Repeats the iterations of the formulae within the loop till n = 1000
DO WHILE (i .LE. imax)

	!-----Iterates Lorentz Force-----!
	fx = (qe * vy * b)
	fy = -(qe * vx * b)

	vx = vx + (fx / m) * dt
	vy = vy + (fy / m) * dt

	rx = rx + vx * dt
	ry = ry + vy * dt

	!-----Adds To The Count-----!
	i = i + 1 
	
	!-----Updates Terminal With Percentage Completion-----!
	IF (t >= (n/10) * tmax) THEN

		WRITE(*,*) n * 10, '% Complete'
		n = n + 1
	
	END IF


	!-----Writes To Output File-----!
	WRITE(1,*) rx, ry 

!-----Ends Do Loop-----!
END DO

!-----Closes Output File-----!
CLOSE(1)

!-----Ends Program-----!
END PROGRAM
