#!/bin/csh
# Submit this script as : sbatch ./[script-name]

#SBATCH  --job-name=regrid
#SBATCH  --nodes=1 #12
#SBATCH  --time=24:20:00
#SBATCH  --exclusive
#SBATCH -A e3sm
#SBATCH -e err.slurm
#SBATCH -o out.slurm
#SBATCH --mail-user=balwinder.singh@pnnl.gov
#SBATCH --mail-type=ALL


source env_src
echo START
date
srun --mpi=none --ntasks=1 --cpu_bind=sockets --cpu_bind=verbose --kill-on-bad-exit ./exe_mkatmsrffile
echo END
date

