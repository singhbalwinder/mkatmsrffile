#!/bin/csh
# Submit this script as : sbatch ./[script-name]

#SBATCH  --job-name=regrid
#SBATCH  --nodes=1
#SBATCH  --time=00:20:00
#SBATCH  --exclusive
#SBATCH -A e3sm
#SBATCH -e err.slurm
#SBATCH -o out.slurm



module load intel/19.0.3 netcdf/4.6.3 mvapich2/2.3.1

srun --mpi=none  --ntasks=2 --cpu_bind=sockets --cpu_bind=verbose --kill-on-bad-exit ./a.out

#srun -N1 -n8  ./mkatmsrffile
