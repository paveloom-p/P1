module subprograms ! Модуль с процедурами
implicit none

contains

subroutine DFT(X,Y,n) ! Функция прямого преобразования Фурье 
implicit none

complex(8) X(0:n-1), Y(0:n-1) ! Входной и выходной массивы
integer n                     ! Количество комплексных чисел

real(8) pi   ! Число pi
real(8) koef ! Коэффициент 2d0 * pi * k_d * i_d / n_d

real(8) s_real, s_im  ! Суммы вещественной и мнимой частей
real(8) sin_value     ! Держатель для sin(koef)
real(8) cos_value     ! Держатель для cos(koef)
real(8) n_d, k_d, i_d ! Овеществления
integer k, i          ! Вспомогательные переменные

! Переменные для проверки степени числа
character(16) f1
integer(2)    j, f

Y = 0d0
n_d = n

! Вычисление числа pi
pi = 4d0*datan(1d0)

do k = 0, n - 1
        
        s_real = 0d0
        s_im   = 0d0
        
        k_d = k
        
        do i = 0, n - 1
        
                i_d = i
        
                koef = 2d0 * pi * k_d * i_d / n_d
                
                sin_value = dsin(koef)
                cos_value = dcos(koef)
                
                        ! Проверка на ошибку округления sin_value и cos_value
                        write(f1,'(e16.7)') sin_value
                        read(f1(15:16),'(i2)') j
                
                        if (j .gt. 1) sin_value = 0d0
                        
                        write(f1,'(e16.7)') cos_value
                        read(f1(15:16),'(i2)') j
                
                        if (j .gt. 1) cos_value = 0d0
                
                ! Вычисление вещественной и мнимой частей суммы
                s_real = s_real + dble(X(i)) * cos_value + dimag(X(i)) * sin_value
                s_im   = s_im + dble(X(i)) * (-1d0) * sin_value + dimag(X(i)) * cos_value
                
                        ! Проверка на ошибку округления s_real и s_im
                        ! (трюк с форматным считыванием знака степени:
                        !  -01 считывает как -1, а +01 как 0)
                        
                        write(f1,'(e16.7)') s_real
                        read(f1(14:16),'(i2)') j
                        read(f1(15:16),'(i2)') f
                
                        if (j*f .lt. -10) s_real = 0d0
                        
                        write(f1,'(e16.7)') s_im
                        read(f1(14:16),'(i2)') j
                        read(f1(15:16),'(i2)') f
                
                        if (j*f .lt. -10) s_im = 0d0
        
        enddo
        
        Y(k) = complex(s_real, s_im)
        
enddo

end subroutine


subroutine IDFT(X,Y,n) ! Функция обратного преобразования Фурье 
implicit none

complex(8) X(0:n-1), Y(0:n-1) ! Входной и выходной массивы
integer n                     ! Количество комплексных чисел

real(8) pi   ! Число pi
real(8) koef ! Коэффициент 2d0 * pi * k_d * i_d / n_d

real(8) s_real, s_im  ! Суммы вещественной и мнимой частей
real(8) sin_value     ! Держатель для sin(koef)
real(8) cos_value     ! Держатель для cos(koef)
real(8) n_d, k_d, i_d ! Овеществления
integer k, i          ! Вспомогательные переменные

! Переменные для проверки степени числа
character(16) f1
integer(2)    j, f

Y = 0d0
n_d = n

! Вычисление числа pi
pi = 4d0*datan(1d0)

do k = 0, n - 1
        
        s_real = 0d0
        s_im   = 0d0
        
        k_d = k
        
        do i = 0, n - 1
        
                i_d = i
        
                koef = 2d0 * pi * k_d * i_d / n_d
                                
                sin_value = dsin(koef)
                cos_value = dcos(koef)
                                
                        ! Проверка на ошибку округления sin_value и cos_value
                        write(f1,'(e16.7)') sin_value
                        read(f1(15:16),'(i2)') j
                
                        if (j .gt. 1) sin_value = 0d0
                        
                        write(f1,'(e16.7)') cos_value
                        read(f1(15:16),'(i2)') j
                
                        if (j .gt. 1) cos_value = 0d0
                               
                ! Вычисление вещественной и мнимой частей суммы         
                s_real = s_real + dble(X(i)) * cos_value - dimag(X(i)) * sin_value
                s_im   = s_im + dble(X(i)) * sin_value + dimag(X(i)) * cos_value
                
                        ! Проверка на ошибку округления s_real и s_im
                        ! (трюк с форматным считыванием знака степени:
                        !  -01 считывает как -1, а +01 как 0)
                        
                        write(f1,'(e16.7)') s_real
                        read(f1(14:16),'(i2)') j
                        read(f1(15:16),'(i2)') f
                
                        if (j*f .lt. -10) s_real = 0d0
                        
                        write(f1,'(e16.7)') s_im
                        read(f1(14:16),'(i2)') j
                        read(f1(15:16),'(i2)') f
                
                        if (j*f .lt. -10) s_im = 0d0
        
        enddo
        
        Y(k) = complex(s_real, s_im)
        Y(k) = 1d0 / n_d * Y(k)
        
enddo

end subroutine

end
