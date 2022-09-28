#!/bin/bash
#SBATCH --job-name=script_05
#SBATCH --account=pi-ganong
#SBATCH --time=15:00:00
#SBATCH --partition=broadwl
#SBATCH --nodes=1
#SBATCH --mail-type=ALL
#SBATCH --mem=42G

source analysis/source/CRISM/sh_scripts/file_names.sh

module load R
Rscript --quiet --no-restore --no-save analysis/source/CRISM/R_scripts/05_loan_level_match.R $scratch_dir $crism_data $corelogic_dir $output_dir
