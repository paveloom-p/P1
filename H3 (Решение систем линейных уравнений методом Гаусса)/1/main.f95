program main ! Решение систем линейных уравнений методом Гаусса
use subprograms ! Модуль с процедурами
implicit none

integer n                                 ! Число незвестных в уравнениях
real(8), allocatable, dimension(:,:) :: A ! Матрица коэффициентов и свободных членов
integer ier, i , j                        ! Вспомогательные переменные
character(20) f1, f2                      ! Вспомогательные переменные для автоформатирования
real(8) t1, t2                            ! Переменные времени

! Считывание числа неизвестных
read(*,'()'); read(*,*) n; read(*,'(//)')

! Выделение памяти под матрицу
allocate (A(n,n+1), stat=ier); if (ier .ne. 0) stop 'Не могу выделить память для матрицы A'

! Считывание коэффицентов и свободных членов в матрицу
read(*,*) ((A(i,j),j=1,n+1),i=1,n)

! Вывод известной информации:
write(*,'(/,4x,a,/)') 'Решение систем линейных уравнений методом Гаусса'

write(f1,*) n+1
f2='(/'//trim(adjustl(f1))//'(4x,'//trim(adjustl(f1))//'e15.7/))'

write(*,'(4x,a)') 'Матрица коэффициентов и свободных членов:'
write(*,f2) ((A(i,j),j=1,n+1),i=1,n)

write(f1,*) n
write(*,'(4x,a,1x,a,/)') 'Число неизвестных: ', trim(adjustl(f1))

! Вызовы процедур, вывод результата
call cpu_time(t1)

call choice_test(A,n,f2)
call forward(A,n,f2)
call backward(A,n)

call cpu_time(t2)
write(*,'(4x,a,e15.7,/)') 'Время выполнения:', t2-t1

deallocate(A)

end program
