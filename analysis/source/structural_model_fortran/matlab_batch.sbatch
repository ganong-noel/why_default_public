#!/bin/bash

#SBATCH --partition=broadwl
#SBATCH --time=10:00:00
#SBATCH --mem=30G

input=$1
model_id=$2
n=$3

# For reproducibility, add version numbers to the "module load" calls.
module load matlab/2018b


echo "Running s${input}.m"
date
for i in `seq 1 $n`
do
  matlab -nodisplay < s${input}.m
  mv statsa800x50.raw simulation_${model_id}_${i}.raw
done
