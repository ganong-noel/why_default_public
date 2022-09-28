#!/bin/bash
#SBATCH --job-name=script_02
#SBATCH --account=pi-ganong
#SBATCH --time=04:00:00
#SBATCH --partition=bigmem2
#SBATCH --nodes=1
#SBATCH --mail-type=ALL
#SBATCH --mem=100G

source analysis/source/CRISM/sh_scripts/file_names.sh

module load R

Rscript --quiet --no-restore --no-save analysis/source/CRISM/R_scripts/02_write_loan_to_rds.R $scratch_dir $crism_data $corelogic_dir
