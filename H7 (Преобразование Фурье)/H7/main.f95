program main ! Дискретное преобразование Фурье
use subprograms ! Модуль с процедурами
implicit none

complex(8), allocatable, dimension(:) :: X, Y ! Входной и выходной массивы

integer n        ! Количество комплексных чисел
integer P        ! Идентификатора режима преобразования
integer do_check ! Проводить проверку обратным преобразованием?

integer i, ier         ! Вспомогательные переменные
real(8) t1, t2, t3, t4 ! Переменные времени

! Считывание количества комплексных чисел
read(*,'()'); read(*,*) n 

! Считывание идентификатора режима преобразования
read(*,'(/)'); read(*,*) P

! Считывание ответа на вопрос о проверке
read(*,'(/)'); read(*,*) do_check

! Выделение памяти под массивы
allocate (X(0:n-1), stat=ier); if (ier .ne. 0) stop 'Не могу выделить память для массива X'
allocate (Y(0:n-1), stat=ier); if (ier .ne. 0) stop 'Не могу выделить память для массива Y'

! Считывание массива X
read(*,'(//)'); read(*,*) X

! Вывод известной информации

write(*,'(/,4x,a)') 'Дискретное преобразование Фурье'

write(*,'(/,4x,a,/)') 'Последовательность комплексных чисел X:'
write(*,'(4x,2x,a,7x,a,10x,a)') 'i', 'Re(X(i))', 'Im(X(i))'
write(*,'(4x,i3,2x,e16.7,2x,e16.7)') (i, dble(X(i)), dimag(X(i)), i = 0, n-1)

! Вывод сообщения о режиме, вызов функции, проверка

    if (P .gt. 0) then

        write(*,'(/,4x,a,/)') 'Режим: прямое дискретное преобразование Фурье.'
        
        call cpu_time(t1)
                
                call DFT(X,Y,n)
                
        call cpu_time(t2)
        
        write(*,'(4x,a,/)') 'Результат:'
        write(*,'(4x,2x,a,7x,a,10x,a)') 'i', 'Re(Y(i))', 'Im(Y(i))'
        write(*,'(4x,i3,2x,e16.7,2x,e16.7)') (i, dble(Y(i)), dimag(Y(i)), i = 0, n-1)
        
        if (do_check .eq. 0) then
        
                call cpu_time(t3)
        
                        call IDFT(Y,X,n)
                
                call cpu_time(t4)
                
                
                write(*,'(/,4x,a,/)') 'Результат проверки:'
                write(*,'(4x,2x,a,7x,a,10x,a)') 'i', 'Re(X(i))', 'Im(X(i))'
                write(*,'(4x,i3,2x,e16.7,2x,e16.7)') (i, dble(X(i)), dimag(X(i)), i = 0, n-1)
        
        endif     

elseif (P .lt. 0) then

        write(*,'(/,4x,a,/)') 'Режим: обратное дискретное преобразование Фурье.'
        
        call cpu_time(t1)
                
                call IDFT(X,Y,n)
                
        call cpu_time(t2)
        
        write(*,'(4x,a,/)') 'Результат:'
        write(*,'(4x,2x,a,7x,a,10x,a)') 'i', 'Re(Y(i))', 'Im(Y(i))'
        write(*,'(4x,i3,2x,e16.7,2x,e16.7)') (i, dble(Y(i)), dimag(Y(i)), i = 0, n-1)

        if (do_check .eq. 0) then
        
                call cpu_time(t3)
        
                        call DFT(Y,X,n)
                
                call cpu_time(t4)
                
                
                write(*,'(/,4x,a,/)') 'Результат проверки:'
                write(*,'(4x,2x,a,7x,a,10x,a)') 'i', 'Re(X(i))', 'Im(X(i))'
                write(*,'(4x,i3,2x,e16.7,2x,e16.7)') (i, dble(X(i)), dimag(X(i)), i = 0, n-1)
        
        endif 

  else

        write(*,'(/,4x,a,/)') 'Укажите параметр P в допустимых диапазонах.'
        stop

 endif


write(*,'(/,4x,a,9x,e15.7)') 'Время выполнения:',       t2-t1

if (do_check .eq. 0) then

        write(*,'(4x,a,e15.7,/)') 'Время выполнения проверки:', t4-t3

                     else

        write(*,'()')

endif

deallocate(X,Y)

end
