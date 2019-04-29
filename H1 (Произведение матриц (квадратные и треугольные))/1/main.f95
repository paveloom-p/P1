program main ! Произведение матриц вида A[n,m] x B[m,n]
use subprograms
implicit none

integer n, m         ! Размерности матриц
character(20) f1, f2 ! Вспомогательные переменные
real(8) t1, t2       ! Переменные времени

! Считывание размерностей матриц
read(*,'()'); read(*,*) n, m; read(*,'(////)')

! Автоматическая подгонка формата вывода
write(f1,*) n
f2='(/'//trim(adjustl(f1))//'('//trim(adjustl(f1))//'i15/))'

! Вызов функции, вывод результата
write(*,'(/,13x,a)') 'Результат:'
call cpu_time(t1); write(*,f2) mult(n,m); call cpu_time(t2)
write(*,'(13x,a,e15.7,/)') 'Время выполнения:', t2-t1

end program
