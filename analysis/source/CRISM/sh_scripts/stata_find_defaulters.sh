#!/bin/bash
#SBATCH --job-name=stata_status
#SBATCH --account=pi-ganong
#SBATCH --time=18:00:00
#SBATCH --partition=broadwl
#SBATCH --nodes=1
#SBATCH --mail-type=ALL
#SBATCH --mem=10G

source analysis/source/CRISM/sh_scripts/file_names.sh

module load stata

stata -b analysis/source/CRISM/stata_scripts/select_upb_and_payment_status.do $scratch_dir $crism_data $corelogic_dir
