#!/bin/bash
#
#SBATCH --account=def-srivast   # replace this with your own account
#SBATCH --job-name=mod0_3    		# Job name
#
#SBATCH --ntasks=4               # number of processes
#SBATCH --mem-per-cpu=7000M      # memory; default unit is megabytes
#SBATCH --time=7-00:00           # time (DD-HH:MM)
#SBATCH --mail-user=guzman@zoology.ubc.ca # Send email updates to you or someone else
#SBATCH --mail-type=ALL          # send an email in all cases (job started, job ended, job aborted)

echo "Running on hostname `hostname`"

cd $SLURM_SUBMIT_DIR
echo "Working directory is `pwd`"
echo "Starting R at `date`."

module load gcc
module load r/3.5.0
R --vanilla 
Rscript mod0.R

echo "Finished R at `date`."
