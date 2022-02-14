PROGRAM SOLVE
    IMPLICIT NONE
    INTEGER, PARAMETER :: n = 3
    REAL (kind=8), DIMENSION(n) :: x, b 
    REAL (kind=8), DIMENSION(n,n) :: a
    INTEGER :: i, info, lda, ldb, nrhs
    INTEGER, DIMENSION(n) :: ipiv

    a = RESHAPE((/ 3.0, 2.0, -1.0, 2.0, -2.0, 0.5, -1.0, 4.0, -1.0 /),(/n,n/))
    b = (/1.0,-2.0,0.0/)
    x = b

    nrhs = 1 ! number of right hand sides in b
    lda = n
    ldb = n

    call dgesv (n, nrhs, a, lda, ldb, ipiv, x, ldb, info)

    print *, "the solution using the lapack subroutine is:"

    DO i = 1,n
        print '("x",i1,"is:",f16.6)', i,x(i)
    end do

end program SOLVE
