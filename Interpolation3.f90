! This is yet another way to implement the Boys function interpolation,
! hopefully in a more efficient way
! The coefficients in "Coef_nword.dat" are ordered as:
! For a given x [from 0 to 0.05]
! m=0{
! a0
! a1
! a2
! a3
! a4}
! m=1{
! a0
! a1
! a2
! a3
! a4} ... until m = 3
! Only coefficients are written, x and m values are implicit in position
program interpolation3
  implicit none

  real(kind=16), dimension(480016) :: coeff
  real(kind=16), dimension(4) :: Delta
  integer :: m, i, aux
  real(kind=16) :: x, a0, a1, a2, a3, nm, aux2, t, boys, aux1
  real(kind=16) :: deltaux

  open(101, file="Cof_0_30_siglecol.dat", status="unknown")

  do i=1, 480016
    read(101,*) coeff(i)
  enddo

  close(101)

  write(*,*) "Ingresar el valor de m [0-3]"
  read(*,*) m
  write(*,*) "Ingresar el valor de x"
  read(*,*) x

  aux = INT(x*1000)
  aux1 = aux
  aux2 = aux1/1000

  a0 = coeff(1 + 4*m + 4*4*aux)
  a1 = coeff(2 + 4*m + 4*4*aux)
  a2 = coeff(3 + 4*m + 4*4*aux)
  a3 = coeff(4 + 4*m + 4*4*aux)

!  write(*,*) "Los coeficientes a utilizar son, en orden:", a0, a1, a2, a3

  open(102, file="scalf.dat", status="unknown")
    do i = 1, 4
      read(102, *) Delta(i)
    enddo
  close(102)

  deltaux = Delta(m+1)
  t = (x - aux2)/deltaux

  boys = a0 + (a1*t) + (a2*(-1 + (2*(t**2)))) + (a3*((-3*t) + (4*(t**3))))

  Write(*,*) "X", aux2
  Write(*,*) "La función de Boys en ese punto es", boys

end program interpolation3
