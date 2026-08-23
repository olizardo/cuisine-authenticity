#!/bin/bash
#$ -cwd
#$ -j y
#$ -o output_ext_relaxed_job.log
#$ -l h_rt=23:50:00   # CRITICAL: Always bound to just under 24 hours
#$ -l h_data=4G       # RAM per core (4G x 4 cores = 16GB total)
#$ -pe shared 4       # Number of cores (4 cores)

# Initialize module system in non-interactive Grid Engine shells
source /u/local/Modules/default/init/bash

# Must load modern GCC before R
module load gcc/10.2.0
module load R

# Pass allocated cores to R
export CMDSTANR_CORES=$NSLOTS
export cmdstanr_no_ver_check=TRUE

# Ensure CmdStan backend is properly loaded and configured
Rscript -e "
  library(cmdstanr)
  cmdstanr::set_cmdstan_path('~/.cmdstan/cmdstan-2.33.1')
"

# Execute R modeling script
Rscript scripts/fit_ext_models_relaxed.R
