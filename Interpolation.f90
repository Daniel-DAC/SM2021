! This program interpolates the Boys function employing
! Chebyshev's interpolation

program interpolation
  implicit none

  real(kind=8), dimension(1001) :: equis, ac, au, ad, at
  integer :: i, aux, j
  real :: dif, x, aux2, Delta, t, boys

  ! We store all the columns in a vector for each
  open(101, file="a_km0.dat",status="unknown")
  do i =1, 1001
    read(101,*) equis(i), ac(i), au(i), ad(i), at(i)
  enddo

  close(101)
  ! x for evaluation input
  Write(*,*) "Ingresar el valor de x para evaluar la función de Boys"
  read(*,*) x

  aux = 1
  j = 1
  do while (aux == 1)
      dif = 0
      aux2 = 0
      aux2 = equis(j)
      dif = x - aux2

      if (dif .GE. 0.0) THEN
        j = j+1
      elseif (dif .LT. 0.0) then
        aux = 2
      endif

  enddo

!  write(*,*) "El punto a utilizar en la interpolación es", equis(j-1)

  Delta =  0.0064474195909412466976
  t = (x - equis(j-1))/Delta

  boys = ac(j-1) + (au(j-1)*t) + (ad(j-1)*(-1 + (2*(t**2)))) + (at(j-1)*((-3*t) + (4*(t**3))))

  Write(*,*) "La función de Boys interpolada en ese punto es", boys

end program interpolation
