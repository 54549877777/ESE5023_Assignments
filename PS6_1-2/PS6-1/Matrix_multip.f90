subroutine Matrix_multip(a,b,c,n)
implicit none
integer                 ::n
real(4),dimension(n,n)  ::a,b,c
c = matmul(a,b)
return
end
