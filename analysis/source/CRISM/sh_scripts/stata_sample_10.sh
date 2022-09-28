#!/bin/bash
#SBATCH --job-name=stata_sample_equifax
#SBATCH --account=pi-ganong
#SBATCH --time=18:00:00
#SBATCH --partition=broadwl
#SBATCH --nodes=1
#SBATCH --mail-type=ALL
#SBATCH --mem=16G

source analysis/source/CRISM/sh_scripts/file_names.sh

module load stata

stata -b analysis/source/CRISM/stata_scripts/sample_equifax.do $scratch_dir $crism_data $corelogic_dir
