program lab2

    use omp_lib
    implicit none

    ! region vars start   
    integer, parameter :: M=8, M1=2, M2=2, M3=2;
    real :: a(M);
    real :: b(M);

    real :: a3(M1,M2,M3);
    real :: b3(M1,M2,M3);
    real :: sumArray(M1,M2,M3);

    real :: min_val, max_val;

    integer :: i, j, k;
    real*8 :: tStart, tEnd;
    ! region vars end

    ! region fill-array start
    do i = 1, size(a);
        a(i) = i;
    end do;
    do i = 1, size(b);
        b(i) = i * 10;
    end do;
    a3 = reshape(a, (/M1,M2,M3/));
    b3 = reshape(b, (/M1,M2,M3/));
    ! region fill-array end

    tStart = omp_get_wtime();
    ! region task start
    !$OMP PARALLEL DO PRIVATE(i,j,k) SHARED(a3,b3,sumArray)
    do i = 1, size(sumArray, 1)
        do j = 1, size(sumArray, 2)
            do k = 1, size(sumArray, 3)
                sumArray(i,j,k) = a3(i,j,k) + b3(i,j,k);
            enddo
        enddo
    enddo
    !$OMP END PARALLEL DO
    min_val = sumArray(1,1,1);
    max_val = sumArray(1,1,1);
    !$OMP PARALLEL DO PRIVATE(i,j,k) SHARED(sumArray) REDUCTION(MIN:min_val) REDUCTION(MAX:max_val)
    do i = 1, size(sumArray, 1)
        do j = 1, size(sumArray, 2)
            do k = 1, size(sumArray, 3)
                min_val = min(min_val, sumArray(i,j,k));
                max_val = max(max_val, sumArray(i,j,k));
            enddo
        enddo
    enddo
    !$OMP END PARALLEL DO
    ! region task end
    tEnd = omp_get_wtime();

    ! region output start
    write(*,"((f7.1))") sumArray;
    write(*,'("Min value = ",f7.1)') min_val;
    write(*,'("Max value = ",f7.1)') max_val;
    write(*,'("Time = ",f6.5," seconds.")') tEnd - tStart;
    ! region output end
end program lab2