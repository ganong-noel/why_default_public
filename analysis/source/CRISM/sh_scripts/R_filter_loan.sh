#!/bin/bash
#SBATCH --job-name=script_04_05
#SBATCH --account=pi-ganong
#SBATCH --time=1:00:00
#SBATCH --partition=bigmem2
#SBATCH --nodes=1
#SBATCH --mail-type=ALL
#SBATCH --mem=100G

source analysis/source/CRISM/sh_scripts/file_names.sh

module load R

Rscript --quiet --no-restore --no-save analysis/source/CRISM/R_scripts/04_filter_mcdash_loan.R $scratch_dir $crism_data $corelogic_dir
Rscript --quiet --no-restore --no-save analysis/source/CRISM/R_scripts/05_loan_level_match.R  $scratch_dir $crism_data $corelogic_dir