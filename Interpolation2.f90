! This program interpolates the Boys function employing
! Chebyshev's interpolation

program interpolation
  implicit none

  real(kind=8), dimension(40001) :: equis, ac, au, ad, at
  real(kind=8), dimension(4) :: Delta
  integer :: i, aux, j, m
  real :: dif, x, aux2, deltaux, t, boys
  !Variables for timing
  integer :: clock_start, clock_end, clock_rate
  real :: time_taken

  ! We store all the columns in a vector for each

  Write(*,*) "Ingresar el valor de m para la funciónd e Boys"
  Read(*,*) m
  Write(*,*) "Ingresar el valor de x para evaluar la función de Boys"
  read(*,*) x

  call system_clock(COUNT_RATE = clock_rate)
  call system_clock(COUNT = clock_start)

  if (m==0) THEN
      open(101, file="ak_m0.dat",status="unknown")
    elseif(m==1) THEN
      open(101, file="ak_m1.dat", status="unknown")
    elseif(m==2) THEN
      open(101, file="ak_m2.dat", status="unknown")
    elseif(m==3) THEN
      open(101, file="ak_m3.dat", status="unknown")
    endif

  do i =1, 40001
    read(101,*) equis(i), ac(i), au(i), ad(i), at(i)
  enddo

  close(101)

  open(102, file="scalf.dat", status="unknown")
    do i = 1, 4
      read(102, *) Delta(i)
    enddo
  close(102)

  ! x for evaluation input

  aux = x*1000
  j = INT(aux)
!  do while (aux == 1)
!      dif = 0
!      aux2 = 0
!      aux2 = equis(j)
!      dif = x - aux2
!
!      if (dif .GE. 0.0) THEN
!        j = j+1
!      elseif (dif .LT. 0.0) then
!        aux = 2
!      endif

!  enddo

  write(*,*) "El punto a utilizar en la interpolación es", equis(j+1)

  deltaux =  Delta(m+1)

  t = (x - equis(j+1))/deltaux

  boys = ac(j+1) + (au(j+1)*t) + (ad(j+1)*(-1 + (2*(t**2)))) + (at(j+1)*((-3*t) + (4*(t**3))))

  Write(*,*) "La función de Boys interpolada en ese punto es", boys

  call system_clock(COUNT=clock_end)

  time_taken = real((clock_end - clock_start)/clock_rate)
  write(*,*) "Tiempo", time_taken

end program interpolation
