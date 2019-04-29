module subprograms ! Модуль с процедурами
implicit none

contains

function Lagrange(x,y,n,z) result(r) ! Функция расчёта интерполяции по многочлену в форме Лагранжа
implicit none

real(8) x(:), y(:) ! Массивы вычисленных аргументов и значений функции в них
real(8) z          ! Аргумент, в котором будем искать значения y, y', y"
real(8) m          ! Слагаемое при фиксированном i
real(8) r          ! Результат
integer n          ! Число промежутков деления
integer i, j       ! Вспомогательные переменные

r = 0d0; m = 1d0
do j = 0, n

        do i = 0, n
        if (i .ne. j) m = m * (z - x(i)) / (x(j) - x(i))
        enddo

r = r + m * y(j)
m = 1d0

enddo

end function

end
