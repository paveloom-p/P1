module subprograms
implicit none

contains

function mult(n,m) result(C)
implicit none

integer n, m, A(n,m), B(m,n), C(n,n)
integer i, j

! Считывание матриц
open(10, file='input2'); read(10,'(////)')
do i=1, n; do j=1, m; read(*,*)  A(i,j); enddo; enddo
do i=1, m; do j=1, n; read(10,*) B(i,j); enddo; enddo
close(10)

! Произведение
do i=1, n; do j=1, n; C(i,j)=dot_product(A(i,:),B(:,j)); enddo; enddo

! Транспонирование
C=transpose(C)

end

end
