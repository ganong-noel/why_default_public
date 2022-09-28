#!/bin/bash
#SBATCH --job-name=script_06_to_12
#SBATCH --account=pi-ganong
#SBATCH --time=24:00:00
#SBATCH --partition=broadwl
#SBATCH --nodes=1
#SBATCH --mail-type=ALL
#SBATCH --mem=32G

source analysis/source/CRISM/sh_scripts/file_names.sh

module load R
echo ${1}
Rscript --quiet --no-restore --no-save analysis/source/CRISM/R_scripts/06_reduce_find_at_default.R  $scratch_dir $crism_data $corelogic_dir $output_dir
Rscript --quiet --no-restore --no-save analysis/source/CRISM/R_scripts/07_calculate_second_lien_balance_at_default.R $scratch_dir $crism_data $corelogic_dir ${1}
Rscript --quiet --no-restore --no-save analysis/source/CRISM/R_scripts/08_find_all_equifax_del.R $scratch_dir $crism_data $corelogic_dir ${1}
