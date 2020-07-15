module subprograms ! Модуль с процедурами
implicit none

contains

subroutine splines(x,y,z,n,a_c,b_c,c_c,d_c,res) ! Процедура расчёта интерполяции сплайнами
implicit none

real(8) x(0:n), y(0:n) ! Массивы вычисленных аргументов и значений функции в них

real(8) z   ! Аргумент, в котором будем искать значение аппроксимирующей функции
real(8) res ! Значение аппроксимирующей функции в точке x = z
integer n   ! Число промежутков деления
integer i   ! Вспомогательные переменные

real(8) k(2:n), l(2:n)                 ! Прогоночные коэффициенты
real(8) h(n), h_i_m1                   ! x_i - x_(i-1) и x_(i-1) - x_(i-2) соответственно
real(8) r, s                           ! Величины, необходимые для вычисления k и l
real(8) a_c(:), b_c(:), c_c(:), d_c(:) ! Коэффициенты аппроксимирующей функции
real(8) koef                           ! (z - x_(i-1)) для вычисления результата

! Прямой ход метода прогонки

do i = 2, n
        
        h(i) = x(i) - x(i-1)
        h_i_m1 = x(i-1) - x(i-2)   

        r = 3d0 * ( ( y(i) - y(i-1) ) / h(i) - (y(i-1) - y(i-2)) / h_i_m1 )
        s = 2d0 * (h_i_m1 + h(i))

        k(i) = r / s
        l(i) = h(i) / s

enddo

! Обратный ход метода прогонки

c_c(1) = 0d0
c_c(n) = k(n)
c_c(n+1) = 0d0

do i = n-1, 2, -1

        c_c(i) = k(i) - l(i) * c_c(i+1)

enddo

! Вычисление оставшихся коэффициентов

h(1) = x(1) - x(0)

do i = 1, n
        
        a_c(i) = y(i-1)
        d_c(i) = (c_c(i+1) - c_c(i)) / (3d0 * h(i))
        b_c(i) = (y(i) - y(i-1)) / h(i) - (c_c(i+1) + 2d0 * c_c(i)) * h(i) / 3d0 

enddo

! Вычисление значения аппроксимирующей функции в точке x = z

do i = 1, n
        if (z .ge. x(i-1) .and. z .le. x(i)) exit
enddo

koef = z - x(i-1)
res = a_c(i) + b_c(i) * koef  + c_c(i) * koef * koef + d_c(i) * koef * koef * koef

end subroutine

end
