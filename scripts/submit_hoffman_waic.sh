#!/bin/bash
#$ -cwd
#$ -j y
#$ -o output_compute_waic.log
#$ -l h_rt=23:50:00   # CRITICAL: Always bound to just under 24 hours
#$ -l h_data=8G       # 8G memory per core (8G x 2 cores = 16GB total)
#$ -pe shared 2       # 2 cores

# Initialize module system in non-interactive Grid Engine shells
source /u/local/Modules/default/init/bash

# Must load modern GCC before R
module load gcc/10.2.0
module load R

export cmdstanr_no_ver_check=TRUE

# Ensure CmdStan backend is properly loaded and configured
Rscript -e "
  library(cmdstanr)
  cmdstanr::set_cmdstan_path('~/.cmdstan/cmdstan-2.33.1')
"

# Execute WAIC computation script across all completed taxonomy models
Rscript scripts/compute_taxonomy_waic.R
