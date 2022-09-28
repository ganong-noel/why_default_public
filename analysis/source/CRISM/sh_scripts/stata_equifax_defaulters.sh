#!/bin/bash

#SBATCH --job-name=stata_sample_payment_stat
#SBATCH --account=pi-ganong
#SBATCH --time=12:00:00
#SBATCH --partition=broadwl
#SBATCH --nodes=1
#SBATCH --mail-type=ALL
#SBATCH --mem=10G

source analysis/source/CRISM/sh_scripts/file_names.sh

module load stata

stata -b analysis/source/CRISM/stata_scripts/sample_payment_stat.do $scratch_dir $crism_data $corelogic_dir
