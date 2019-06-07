!compile it with mpif90 and run it like:
! $mpif90 main.f90 && sbatch submit_sbatch.csh
program simple2
  
  implicit none
  
  integer ierr,my_rank,size
  
  include 'mpif.h'
  
  call mpi_init(ierr)
  
  call mpi_comm_rank(MPI_COMM_WORLD,my_rank,ierr)
  call mpi_comm_size(MPI_COMM_WORLD,size,ierr)
  
  !     print rank and size to screen
  
  write(6,100) my_rank, size
  
  call mpi_finalize(ierr)
  
100 format('Hello World! I am rank ', I2, ' of size ', I2)
  
end program simple2
