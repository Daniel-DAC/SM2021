! This program evaluates the Boys function in Fortran 90
module functions1
contains

  real(kind=8) function upfact(arg1, n)
    implicit none
    integer, intent(in) :: n
    real, intent(in) :: arg1
    integer :: cont1
    real :: aux1, acc
    acc = 1
    do cont1 = 0, n-1
      aux1 = arg1 + cont1
      acc = acc*aux1
    end do
    upfact = acc
    return

  end function

  real(kind=8) function fact(k)
    implicit none
    integer, intent(in) :: k
    integer :: cont2, aux2
    aux2 = 1
    if (k==0) THEN
      aux2 = 1
  else
    do cont2 = 1, k
      aux2 = aux2 * cont2
    enddo
  endif
    fact = aux2
    return

  end function

end module

program Boys1
  use functions1
  implicit none

  real :: n, hypgeo, a, b, z, T, p1, p2, hgeo, bois!, debugger
  integer :: i, j, k !, a, b

    Write(*,*) "Provide argument of the Boys function"
    Read(*,*) T

    Write(*,*) "Provide n of the desired Boys function"
    Read(*,*) n

    a = n + 0.5
    b = n + 1.5

!    write(*,*) "Para m igual a", n

      z = (-1)*T
      hgeo = 0
      ! Generate hypergeometric function
      do i = 0, 50
        p1 = (upfact(a, i)) / (upfact(b, i))
        if (i == 0) THEN
          p2 = 1
        endif
        p2 = (z**i)/fact(i)
        hgeo = (p1*p2) + hgeo
      enddo
      ! Go from hypergeometric to Boys
      bois = hgeo/((2*n)+1)
      !  debugger = upfact(2.4, 5)
      !  Write(*,*) debugger
      Write(*,*) bois

end program Boys1
