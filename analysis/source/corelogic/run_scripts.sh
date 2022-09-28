#!/bin/bash
#SBATCH --job-name=clean-deed-data
#SBATCH --account=pi-ganong
#SBATCH --time=28:00:00
#SBATCH --partition=bigmem2
#SBATCH --nodes=1
#SBATCH --mail-type=ALL

#SBATCH --mem=72G


module load R
Rscript --quiet --no-restore --no-save analysis/source/corelogic/read_all.R
Rscript --quiet --no-restore --no-save analysis/source/corelogic/merge_hpi.R
Rscript --quiet --no-restore --no-save analysis/source/corelogic/plot_and_tabulate.R
