#! /bin/bash

source analysis/source/CRISM/sh_scripts/file_names.sh

temp_dir=${scratch_dir/scratch_dir:/}
out_dir=${output_dir/output_dir:/}

#mkdir -p $temp_dir/stata_sample/defaulters
#mkdir -p $temp_dir/r_output/month/consumer_level
#mkdir -p $temp_dir/r_output/month/month_balance
#mkdir -p $temp_dir/r_output/month/equity
#mkdir -p $temp_dir/r_output/month/loan_lv
#mkdir -p $temp_dir/r_output/overall
#mkdir -p $temp_dir/corelogic

# mkdir -p $out_dir


jobid1=$(sbatch --parsable analysis/source/CRISM/sh_scripts/R_write_loan_to_rds.sh)
sleep 10
jobid2=$(sbatch --parsable analysis/source/CRISM/sh_scripts/R_clean_hpi.sh)
sleep 10
jobid4=$(sbatch --parsable analysis/source/CRISM/sh_scripts/stata_sample_10.sh)
sleep 10
jobid5=$(sbatch --parsable analysis/source/CRISM/sh_scripts/stata_find_defaulters.sh)
sleep 10
jobid6=$(sbatch --parsable analysis/source/CRISM/sh_scripts/stata_equifax_defaulters.sh)
sleep 10
jobid7=$(sbatch --parsable --dependency=afterok:$jobid4  analysis/source/CRISM/sh_scripts/R_process_equifax.sh)
sleep 10
jobid8=$(sbatch --parsable --dependency=afterok:$jobid7:$jobid1 analysis/source/CRISM/sh_scripts/R_filter_loan.sh)
jobid11=$(sbatch --parsable --dependency=afterok:$jobid8 analysis/source/CRISM/sh_scripts/R_match.sh)

for i in {1..5}
do
  iteration="iteration:"$i
  echo $iteration
  sbatch --dependency=afterok:$jobid5:$jobid11 analysis/source/CRISM/sh_scripts/R_first_lien_balance.sh $iteration
  sleep 10
done

jobid15=$(sbatch --parsable --dependency=singleton analysis/source/CRISM/sh_scripts/R_collate_first_lien_balance.sh)

for i in {1..5}
do
  iteration="iteration:"$i
  echo $iteration
  sbatch --dependency=afterok:$jobid15 analysis/source/CRISM/sh_scripts/R_match_equity.sh $iteration
  sleep 10
done


jobid9=$(sbatch --parsable --dependency=singleton analysis/source/CRISM/sh_scripts/R_match_and_plot.sh)
sleep 10
jobid10=$(sbatch --parsable --dependency=afterok:$jobid9 analysis/source/CRISM/sh_scripts/R_run_ph_reg.sh)

