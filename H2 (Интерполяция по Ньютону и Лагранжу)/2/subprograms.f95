module subprograms ! Модуль с процедурами
implicit none

contains

subroutine cheb_points(x,a,b,n) ! Процедура вычисления узлов Чебышевской сетки
implicit none

real(8) a, b           ! Границы промежутка
integer n              ! Число узлов, уменьшенное на единицу
real(8) x(:)           ! Массив узлов
real(8) pi, s1, s2, s3 ! Вспомогательные переменные
integer i

! Вычисление констант
s1=2*n+2
s2=(a+b)/2
s3=(b-a)/2

pi=4d0*datan(1.d0)

! Вычисление узлов
do i=0, n
x(i+1)=s2+s3*dcos(((1+2*i)*pi/s1))
enddo

end subroutine

function Lagrange(x,y,n,z) result(r) ! Функция расчёта интерполяции по многочлену в форме Лагранжа
implicit none

real(8) x(:), y(:) ! Массивы вычисленных аргументов и значений функции в них
real(8) z          ! Аргумент, в котором будем искать значение функции
real(8) m          ! Слагаемое при фиксированном i
real(8) r          ! Результат
integer n          ! Число узлов
integer i, j       ! Вспомогательные переменные

r=0; m=1
do i=1, n

do j=1, n
if (j .ne. i) m=m*(z-x(j))/(x(i)-x(j))
enddo

r=r+m*y(i); m=1

enddo

end function

end
