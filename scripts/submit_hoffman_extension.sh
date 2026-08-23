#!/bin/bash
#$ -cwd
#$ -j y
#$ -o output_ext_job.log
#$ -l h_rt=23:50:00   # Bound to just under 24 hours
#$ -l h_data=4G       # RAM per core
#$ -pe shared 8       # Number of cores

# Initialize module system in non-interactive Grid Engine shells
source /u/local/Modules/default/init/bash

# Load modern GCC before R
module load gcc/10.2.0
module load R

# Pass allocated cores to R
export CMDSTANR_CORES=$NSLOTS
export cmdstanr_no_ver_check=TRUE

# Stagger concurrent tasks by 15 mins if running array jobs
if [ "${SGE_TASK_ID:-0}" -eq 2 ]; then
  sleep 900
fi

# Ensure CmdStan backend is properly loaded and configured
Rscript -e "
  library(cmdstanr)
  cmdstanr::set_cmdstan_path('~/.cmdstan/cmdstan-2.33.1')
"

# Execute R modeling script
Rscript scripts/fit_ext_models.R
