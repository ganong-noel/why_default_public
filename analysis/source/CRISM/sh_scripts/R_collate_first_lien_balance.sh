#!/bin/bash
#SBATCH --job-name=script_06_upb
#SBATCH --account=pi-ganong
#SBATCH --time=01:00:00
#SBATCH --partition=broadwl
#SBATCH --nodes=1
#SBATCH --mail-type=ALL
#SBATCH --mem=20G

source analysis/source/CRISM/sh_scripts/file_names.sh

module load R
Rscript --quiet --no-restore --no-save analysis/source/CRISM/R_scripts/06_reduce_find_at_default.R $scratch_dir $crism_data $corelogic_dir $output_dir
