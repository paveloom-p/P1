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
real(8) n_d, k_d, i_d ! Овеществления
integer k, i          ! Вспомогательные переменные

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
                
                s_real = s_real + dble(X(i)) * dcos(koef) + dimag(X(i)) * dsin(koef)
                s_im   = s_im + dble(X(i)) * (-1d0) * dsin(koef) + dimag(X(i)) * dcos(koef)
        
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
real(8) n_d, k_d, i_d ! Овеществления
integer k, i          ! Вспомогательные переменные

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
                                
                s_real = s_real + dble(X(i)) * dcos(koef) - dimag(X(i)) * dsin(koef)
                s_im   = s_im + dble(X(i)) * dsin(koef) + dimag(X(i)) * dcos(koef)
        
        enddo
        
        Y(k) = complex(s_real, s_im)
        Y(k) = 1d0 / n_d * Y(k)
        
enddo

end subroutine

end
