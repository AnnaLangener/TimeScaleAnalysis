#!/bin/bash
#SBATCH --time=00:15:00
#SBATCH --nodes=1
#SBATCH --cpus-per-task=8
#SBATCH --job-name=ParallelComputingML
#SBATCH --mem=10GB
 
module load R/4.1.0-foss-2021a
 
Rscript ParallelComputingML_per.R