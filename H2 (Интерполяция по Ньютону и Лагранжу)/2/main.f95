program main ! Интерполяция по многочлену в форме Лагранжа
use subprograms ! Модуль с процедурами
implicit none

real(8) a, b                                ! Границы промежутка
integer n                                   ! Число узлов
real(8) h                                   ! Шаг (для равномерной сетки)
real(8) z                                   ! Аргумент, в котором будем искать значение функции
integer k                                   ! Идентификатор режима (см. описание в файле input1)
real(8), allocatable, dimension(:) :: x, y  ! Массивы вычисляемых аргументов и значений функции в них
integer ier, i                              ! Вспомогательные переменные
real(8) t1, t2                              ! Переменные времени

! Считывание аргумента, в котором будем искать значение функции
read(*,'()'); read(*,*) z; read(*,'(/)')

! Считывание границ промежутка
read(*,*) a, b; read(*,'(/)')

! Считывание числа узлов интерполяции
read(*,*) n; read(*,'(/)')

! Считывание идентификатора режима интерполяции
read(*,*) k

! Выделение памяти под массивы
allocate (x(n), stat=ier); if (ier .ne. 0) stop 'Не могу выделить память для массива x'
allocate (y(n), stat=ier); if (ier .ne. 0) stop 'Не могу выделить память для массива y'

! Формирование массива x, считывание массива y
open(10, file = 'input2'); read(10,'(/)')                                            ! Одно из двух:
if (k .eq. 0) then; h=(b-a)/(n-1); do i=1, n; x(i)=a+(i-1)*h; read(10,*) y(i); enddo ! Формирование равномерной сетки
else; call cheb_points(x,a,b,n-1); read(10,*) y; endif                               ! Формирование Чебышевской сетки
close(10)

! Вывод известной информации:
write(*,'(/,4x,a)') 'Интерполяция по многочлену в форме Лагранжа'

if (k .eq. 0) then; write(*,'(4x,a,/)') 'Режим: равномерная сетка'
else; write(*,'(4x,a,/)') 'Режим: Чебышевская сетка'; endif

write(*,'(4x,a,/,4x,a,e22.10,/,4x,a,e22.10,/)') 'Границы промежутка:', 'a = ', a, 'b = ', b

write(*,'(4x,a,//,7x,a,14x,a,21x,a)') 'Узлы и значения в них:', '№', 'X', 'Y'
write(*,'(t3,i6,2x,e20.10,2x,e20.10)') (i,x(i),y(i),i=1,n)

if (k .eq. 0) write(*,'(/,4x,a,e16.10)') 'Шаг: ', h

write(*,'(/,4x,a,/,4x,a,1x,e22.10)') 'Ищем значение функции при аргументе', 'x =', z

! Вызов функции, вывод результата
write(*,'(/,4x,a)') 'Результат:'
call cpu_time(t1); write(*,'(4x,a,e22.10,/)') 'y = ', Lagrange(x,y,n,z); call cpu_time(t2)
write(*,'(4x,a,e15.7,/)') 'Время выполнения:', t2-t1

deallocate(x,y)

end program
