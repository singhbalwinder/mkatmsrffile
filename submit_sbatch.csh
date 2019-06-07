#!/bin/csh
# Submit this script as : sbatch ./[script-name]

#SBATCH  --job-name=regrid
#SBATCH  --nodes=1 #12
#SBATCH  --time=00:20:00
#SBATCH  --exclusive
#SBATCH -A e3sm
#SBATCH -e err.slurm
#SBATCH -o out.slurm
#SBATCH --mail-user=balwinder.singh@pnnl.gov
#SBATCH --mail-type=ALL


module load intel/19.0.3 netcdf/4.6.3 mvapich2/2.3.1

srun --mpi=none --ntasks=1 --cpu_bind=sockets --cpu_bind=verbose --kill-on-bad-exit ./mkatmsrffile

#srun -N1 -n8  ./mkatmsrffile
