#!/bin/bash
#SBATCH --job-name=script_01
#SBATCH --account=pi-ganong
#SBATCH --time=04:00:00
#SBATCH --partition=broadwl
#SBATCH --nodes=1
#SBATCH --mail-type=ALL
#SBATCH --mem=8G

source analysis/source/CRISM/sh_scripts/file_names.sh

module load R

Rscript --quiet --no-restore --no-save analysis/source/CRISM/R_scripts/01_clean_hpi.R $scratch_dir $crism_data $corelogic_dir
