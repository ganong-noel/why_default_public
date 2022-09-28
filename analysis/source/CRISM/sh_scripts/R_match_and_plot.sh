#!/bin/bash
#SBATCH --job-name=script_06_to_12
#SBATCH --account=pi-ganong
#SBATCH --time=03:00:00
#SBATCH --partition=broadwl
#SBATCH --nodes=1
#SBATCH --mail-type=ALL
#SBATCH --mem=15G

source analysis/source/CRISM/sh_scripts/file_names.sh

module load R

Rscript --quiet --no-restore --no-save analysis/source/CRISM/R_scripts/09_reduce.R $scratch_dir $crism_data $corelogic_dir $output_dir
Rscript --quiet --no-restore --no-save analysis/source/CRISM/R_scripts/10_keys_straight_default_fig.R $scratch_dir $crism_data $corelogic_dir $output_dir
Rscript --quiet --no-restore --no-save analysis/source/CRISM/R_scripts/11_benchmarking.R $scratch_dir $crism_data $corelogic_dir $output_dir
Rscript --quiet --no-restore --no-save analysis/source/CRISM/R_scripts/12_plot_ltv_distribution_time.R $scratch_dir $crism_data $corelogic_dir $output_dir
