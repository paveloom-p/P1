module subprograms
implicit none

contains

function mult(n) result(C)
implicit none

integer n, A(n,n), B(n,n), C(n,n)
integer i, j

! Считывание той части матрицы, которая не ниже главной диагонали
open(10, file='input2'); read(10,'(////)')
do i=1, n
        do j=i, n
                read(*,*)  A(i,j)
                read(10,*) B(i,j)
        enddo
enddo
close(10)

! Произведение
C=0
do i=1, n; do j=i, n; 
        C(i,j)=dot_product(A(i,i:j+i-1), B(i:j,j))
enddo; enddo

! Транспонирование
C=transpose(C)

!write(*,11) ((C(i,j),j=1,n),i=1,n)
!11 format (/5(10x,3i15/))

end function

end
