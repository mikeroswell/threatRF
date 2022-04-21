#!/bin/bash

#SBATCH --partition=main
# Name of the partition
#SBATCH --ntasks=1 # Number of tasks
#SBATCH --cpus-per-task=42 # Number of CPUs per task
#SBATCH --mem=192000 # Requested memory
#SBATCH --array=1 # Array job will submit 1 jobs
#SBATCH --time=32:00:00 # Total run time limit (HH:MM:SS)
#SBATCH --output=slurm.%N.%j.out # STDOUT file
#SBATCH --error=slurm.%N.%j.err  # STDERR file
#SBATCH --requeue # Return job to the queue if preempted
#SBATCH --export=ALL # Export you current env to the job env
#SBATCH --job-name="fitRF1" # Name of the job

# This is to get e-mail notifications
# when the jobs start and end
#SBATCH --mail-type=begin
#SBATCH --mail-type=end
#SBATCH --mail-user=mroswell@umd.edu


module load intel/17.0.4
module load gcc
module load R-Project/3.4.1
echo -n "Executing on the machine: " hostname echo "Array Task ID : " $SLURM_ARRAY_TASK_ID

# cd /scratch/mr984

Rscript /home/mr984/threatRF/code/fit_RF.R
