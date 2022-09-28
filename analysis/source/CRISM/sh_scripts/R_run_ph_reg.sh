#!/bin/bash
#SBATCH --job-name=script_14_to_16
#SBATCH --account=pi-ganong
#SBATCH --time=28:00:00
#SBATCH --partition=bigmem2
#SBATCH --nodes=1
#SBATCH --mail-type=ALL
#SBATCH --mem=128G

source analysis/source/CRISM/sh_scripts/file_names.sh

module load R

Rscript --quiet --no-restore --no-save analysis/source/CRISM/R_scripts/14_ph_reg_data.R $scratch_dir $crism_data $corelogic_dir $output_dir
Rscript --quiet --no-restore --no-save analysis/source/CRISM/R_scripts/15_ph_regressions.R $scratch_dir $crism_data $corelogic_dir $output_dir
