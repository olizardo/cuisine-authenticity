#!/bin/bash
#$ -cwd
#$ -j y
#$ -t 1-9             # Array job covering all 9 missing taxonomy cells across the 3 domains
#$ -o output_ext_taxonomy_task_$TASK_ID.log
#$ -l h_rt=23:50:00   # Bound to just under 24 hours
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

# Stagger concurrent task launches by 5 minutes to avoid compiler race conditions
if [ ! -z "$SGE_TASK_ID" ] && [ "$SGE_TASK_ID" -gt 1 ]; then
  sleep_seconds=$(( (SGE_TASK_ID - 1) * 300 ))
  echo "Staggering Task $SGE_TASK_ID for $sleep_seconds seconds..."
  sleep $sleep_seconds
fi

# Ensure CmdStan backend is properly loaded and configured
Rscript -e "
  library(cmdstanr)
  cmdstanr::set_cmdstan_path('~/.cmdstan/cmdstan-2.33.1')
"

# Execute taxonomy R script for specific task ID
Rscript scripts/fit_ext_taxonomy.R
