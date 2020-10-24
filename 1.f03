program lab1

    use omp_lib
    implicit none

    ! region vars start
    integer, parameter :: M=8, M1=2, M2=2, M3=2;
    real :: a(M);
    real :: b(M);

    real :: a3(M1,M2,M3);
    real :: b3(M1,M2,M3);
    real :: sumArray(M1,M2,M3);

    real :: min_val;
    real :: max_val;

    real*8 :: tStart, tEnd;

    integer :: i;
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
    sumArray = a3 + b3;
    min_val = minval(sumArray);
    max_val = maxval(sumArray);
    ! region task end
    tEnd = omp_get_wtime();

    ! region output start
    write(*,"((f7.1))") sumArray;
    write(*,'("Min value = ",f7.1)') min_val;
    write(*,'("Max value = ",f7.1)') max_val;
    write(*,'("Time = ",f6.5," seconds.")') tEnd - tStart;
    ! region output end   
    
end program lab1