program main ! Произведение треугольных матриц
use subprograms
implicit none

integer n            ! Размерность матриц
character(20) f1, f2 ! Вспомогательные переменные
real(8) t1, t2       ! Переменные времени

! Считывание размерности матриц
read(*,'()'); read(*,*) n; read(*,'(////)')

! Автоматическая подгонка формата вывода
write(f1,*) n
f2='(/'//trim(adjustl(f1))//'('//trim(adjustl(f1))//'i15/))'

! Вызов функции, вывод результата
write(*,'(/,14x,a)') 'Результат:'
call cpu_time(t1); write(*,f2) mult(n); call cpu_time(t2)
write(*,'(14x,a,e15.7,/)') 'Время выполнения:', t2-t1

end program
