
integer function get_min(array, dim)
    integer :: dim;
    integer, dimension(dim) :: array;
    get_min = array(4);
    return;
end function get_min

integer function get_max()
    return;
end function get_max

PROGRAM scatter_mpi
    integer mpi_proc, mpi_size, iError;
    integer, dimension(:), allocatable :: scattered_data_a;
    integer, dimension(:), allocatable :: scattered_data_b;
    integer, dimension(:), allocatable :: scattered_data_sum;
    integer :: a(4) = (/1, 2, 3, 4/);
    integer :: b(4) = (/100, 200, 300, 400/);
    integer :: d(4) = (/100, 10, 200, 1/);
    integer :: c(4);
    integer :: cNew(4);
    integer :: k(2);
    integer :: n = 4;
    integer :: min;
    integer :: min_local;
    integer, dimension(:), allocatable :: sumArray;
    integer clusterSize;

    ! !$OMP PARALLEL DO PRIVATE(i,j) SHARED(a,b,c)
    ! do i = 1, 4, 1
    !     c(i) = b(i) + a(i);
    ! enddo
    ! !$OMP END PARALLEL DO

    ! min = d(1);
    ! !$OMP PARALLEL PRIVATE(min_local) SHARED(d,min)
    ! min_local = min;
    ! !$OMP PARALLEL DO PRIVATE(i)
    ! do i = 2, 4, 1
    !     if (min_local > d(i)) then
    !         min_local = d(i);
    !     endif
    ! enddo
    ! !$OMP END PARALLEL DO

    ! !$OMP CRITICAL
    ! if (min_local < min) then
    !     min = min_local;
    ! endif
    ! !$OMP END CRITICAL

    ! !$OMP END PARALLEL

    ! !$OMP PARALLEL DO PRIVATE(i) SHARED(d) REDUCTION(MIN:n)
    ! do i = 2, 4, 1
    !     n = min(min_local, d(i))
    ! enddo
    ! !$OMP END PARALLEL

    min = integer :: get_min(b, size(a));
    write(*,*)min;
    
END PROGRAM