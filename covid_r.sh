#!/bin/bash

#SBATCH -p general
#SBATCH -N 1
#SBATCH --mem=10g
#SBATCH -n 1
#SBATCH -t 1-

R CMD BATCH --no-save mycode.R
