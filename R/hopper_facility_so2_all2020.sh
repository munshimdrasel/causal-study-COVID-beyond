#!/bin/bash

#SBATCH --job-name=causal_study
#SBATCH --partition=normal
#SBATCH --output=/scratch/%u/%x-%N-%j.out  # Output file
#SBATCH --error=/scratch/%u/%x-%N-%j.err   # Error file
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=mrasel@gmu.edu
#SBATCH --mem-per-cpu=2GB
#SBATCH --time=3-00:00:00
#SBATCH --ntasks=32
#SBATCH --nodes=2

cd /projects/HAQ_LAB/mrasel/R/causal-study-COVID-beyond/R

module load r-disperseR/0.1.0  

Rscript --no-restore --quiet --no-save causal_SO2_whole_2020.R

