program lab3

    ! region usings start
    use omp_lib
    implicit none
    include 'mpif.h'
    ! region usings end

    ! region vars start
    integer :: i,j;
    integer mpi_rank, mpi_size, iError;

    integer, dimension(:), allocatable :: scattered_data_a;
    integer, dimension(:), allocatable :: scattered_data_b;
    integer, dimension(:), allocatable :: scattered_data_sum;

    integer, parameter :: M=8, M1=2, M2=2, M3=2;
    integer :: clusterSize;
    integer :: a(M);
    integer :: b(M);
    integer :: a3(M1,M2,M3);
    integer :: b3(M1,M2,M3);
    integer :: sumArray(M1,M2,M3);
    integer, dimension(:), allocatable :: minArray, maxArray;
    integer :: minValue, maxValue;

    real*8 :: tStart, tEnd;
    ! region vars end

    ! region mpi-init start
    call mpi_init(iError);
    call mpi_comm_size(mpi_comm_world,mpi_size,iError);
    call mpi_comm_rank(mpi_comm_world,mpi_rank,iError);
    ! region mpi-init end

    ! region fill-array start
    if (mpi_rank == 0) then
        do i = 1, size(a);
            a(i) = i;
        end do;
        do i = 1, size(b);
            b(i) = i * 10;
        end do;
        a3 = reshape(a, (/M1,M2,M3/));
        b3 = reshape(b, (/M1,M2,M3/));
    endif
    call MPI_BARRIER(mpi_comm_world, iError);
    call MPI_BCAST(a3,M,MPI_INT,0,mpi_comm_world,iError);
    call MPI_BCAST(b3,M,MPI_INT,0,mpi_comm_world,iError);
    ! region fill-array end

    tStart = omp_get_wtime();
    ! region main-program start
    clusterSize = M / mpi_size;
    allocate(minArray(M / clusterSize));
    allocate(maxArray(M / clusterSize));
    allocate(scattered_data_a(clusterSize));
    allocate(scattered_data_b(clusterSize));
    allocate(scattered_data_sum(clusterSize));
    call MPI_Scatter(a3, clusterSize, MPI_INT, scattered_data_a, clusterSize, MPI_INT, 0, mpi_comm_world, iError);
    call MPI_Scatter(b3, clusterSize, MPI_INT, scattered_data_b, clusterSize, MPI_INT, 0, mpi_comm_world, iError);
    call MPI_Scatter(sumArray, clusterSize, MPI_INT, scattered_data_sum, clusterSize, MPI_INT, 0, mpi_comm_world, iError);
    scattered_data_sum = scattered_data_a + scattered_data_b;
    minValue = minval(scattered_data_sum);
    maxValue = maxval(scattered_data_sum);
    call MPI_ALLGATHER(scattered_data_sum,clusterSize,MPI_INT,sumArray,clusterSize,MPI_INT,mpi_comm_world,iError);
    call MPI_ALLGATHER(minValue, 1, MPI_INT, minArray, 1 ,MPI_INT, mpi_comm_world, iError);
    call MPI_ALLGATHER(maxValue, 1, MPI_INT, maxArray, 1, MPI_INT, mpi_comm_world, iError);
    call MPI_BARRIER(mpi_comm_world, iError);
    if (mpi_rank == 0) then
        minValue = minval(minArray);
        maxValue = maxval(maxArray);
    endif
    call MPI_BCAST(minValue,1,MPI_INT,0,mpi_comm_world,iError);
    call MPI_BCAST(maxValue,1,MPI_INT,0,mpi_comm_world,iError);
    ! region main-program end
    tEnd = omp_get_wtime();

    ! region mpi-finalize start
    call mpi_finalize(iError);
    ! region mpi-finalize end

    ! region output start
    if (mpi_rank == 0) then
        write(*,*) sumArray;
        write(*,'("Min value = ",i7)') minValue;
        write(*,'("Max value = ",i7)') maxValue;
        write(*,'("Time = ",f6.5," seconds.")') tEnd - tStart;
    endif
    ! region output end

end program