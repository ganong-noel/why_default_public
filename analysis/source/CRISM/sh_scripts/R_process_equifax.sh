#!/bin/bash
#SBATCH --job-name=script_03
#SBATCH --account=pi-ganong
#SBATCH --time=23:00:00
#SBATCH --partition=broadwl
#SBATCH --nodes=1
#SBATCH --mail-type=ALL
#SBATCH --mem=30G

source analysis/source/CRISM/sh_scripts/file_names.sh

module load R

Rscript --quiet --no-restore --no-save analysis/source/CRISM/R_scripts/03_clean_equifax.R $scratch_dir $crism_data $corelogic_dir
