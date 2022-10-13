# Ganong and Noel "Why Do Borrowers Default on Mortgages?" (2022)

Please send feedback and questions to ganong@uchicago.edu.

## Driver scripts

There are four master scripts in this directory:

- `master.R` in `analysis/source` executes files that run locally. This script calls `load_packages.R`, which installs the version of all packages used at the time of the last full build. The complete run takes about 10 minutes.
  - `CRISM/master_default.sh` produces the CRISM plots, tables, and regression results. The full run takes up to a week.
  - `structural_model_fortran/master.sbatch` runs the structural model, producing output that will be plotted by running  `analysis/source/master.R`
  - `run_script.sh` runs the measurement error in LTV analysis that is in `analysis/source/corelogic`. It takes up to 25 hours to run.

## Package Versioning

To ensure that there aren't any problems with package versioning, there is the option to re-install the needed packages with the version from the last complete run. To do this locally, one has to have the package `versions` installed and has to set `reinstall_packages` to `TRUE` in the master script. To do this on the RCC, one has to manually install the package `gt`.

- `load_packages.R` is called from the master script and loads in the packages that are necessary to run the local files.

## CRISM

The code in `analysis/source/CRISM` creates the results in Table 6, Figure A-1, Figure A-13, and the benchmarks in Table A-2. Each R and Stata script is called from a sh-file. Each sh-file starts a job on the RCC. All files can be executed at once with `master_default.sh`. To run the files one needs to change the scratch to directory in `file_names.sh` or gain access to the specified scratch directory. Below is the hierarchy and order in which each file is called.

- `00_prelim.R`, `file_names.sh`, and `00_get_bash_args.R` define file paths and auxiliary functions from `prelim.R` that are passed to the remaining scripts.
- `00_cleaning_function.R` has cleaning functions specific to CRISM, including the functions that import the Equifax and McDash data. This file defines the thresholds that match the second lien to the first lien.
- `00_guren_controls.R` prepares the cyclicality sensitivity and industry-share controls from Guren et al. (2021). To get the file with the original controls in `analysis/input/data/guren/guren_controls.csv`, one needs to run the script `/FiguresTables Package/FiguresTables/Table1.do` in their replication kit until line 215.
- `R_write_loan_to_rds.sh`
  - `02_write_load_rds.R` imports the raw loan-level McDash data and saves `all_loans.Rds`.
- `R_clean_hpi.sh`
  - `01_clean_hpi.R` cleans the CoreLogic indices and creates the price indices `hpi_zip.csv` and `hpi_cbsa.csv`. They are used to adjust the house value for the LTV calculation.
- `stata_sample_10.sh`
  - `sample_equifax.do` creates the 10% random sample based on the last digit of the customer ID.
- `stata_find_defaulters.sh`
  - `select_ubp_and_payment_status.do` selects the variables form the loan-month level McDash data we use.
- `R_process_equifax.sh`
  - `03_clean_equifax.R` calls functions from `00_cleaning_function.R` to import the 10% Equifax sample. It outputs the list of customer IDs (`loan_cid.Rds`) we use to reduce the McDash data.
- `R_filter_loan.sh`
  - `04_filter_mcdash_loan.R` filters `all_loans.Rds` with`loan_cid.Rds` to create `loan.rds`.
  - `05_loan_level_match.R` matches the Equifax customers to the McDash mortgage data.
- `R_match_equity.sh`
  - `06_reduce_find_at_default.R` merges multiple files that were split by month initially to increase execution speed.
  - `07_calculate_second_lien_balance_at_default.R` matches second liens to first liens based on the parameters set in `00_cleaning_function.R`, and open at least three months later and have a lower balance than the first lien. Then it calculates the outstanding balance for each month and inflates prices with the CoreLogic HPI to get a correct LTV.
  - `08_find_all_equifax_del.R` creates a data frame with all delinquent customers in Equifax.
- `R_match_and_plot.sh`
  - `09_reduce_equity.R` merges the monthly equity and delinquency data to get the LTV at default.
  - `10_keys_straight_default.R` produces Figure A-13.
  - `11_benchmarking.R` calculates the statistics used in Table A-2. The MBA statistics were calculated from the MBA National Delinquency Survey. This data can be purchased by emailing mbaresearch@mba.org.
  - `12_plot_ltv_distribution_time.R` creates Figure A-13.
- `R_run_ph_reg.sh`
  - `14_ph_reg_data.R` creates the loan-level data for the IV regressions. Here the data is further reduced to a 1% sample.
  - `15_ph_regressions.R` runs the regressions in Table 6 and the regression with LTV bins shown in the appendix.

## Simulations

The simulations to illustrate the method described in Section 2, the measurement error simulation, and the relaxation of assumption 3 are in `analysis/source/meas_error_sim`.

- `relax_assumption_3.R` produces the simulation outlined in Appendix C.3, which creates Figure A-18 to A-22 and Table A-13.
- `sim_bias_var_tradeoff.R` produces the simulation outlined in Appendix C.4 and creates Figure A-23.

## JPMCI Plots constructed on the outside

The files in `analysis/source/plot_chase` produce plots that use JPMCI disclosures but are built on the outside.

- `comparison_of_sd_empirics.R` creates
  - the BDS estimates used to build Figure 3, the share of strategic default by LTV.
  - the average share of defaults that are strategic according to BDS.
- `plot_chase.R` produces Figure A-9.

## Structural Model

This code is based on code originally created by John Campbell and Joao Cocco. We are very grateful that they shared their code with us. The files in `analysis/source/structural_model_fortran` run the structural model of mortgage default from Campbell and Cocco and determine the optimal value for stigma. Detailed documentation for the model, including the code structure and how to run it, can be found in `analysis/source/structural_model_fortran/README.md`

## PSID analysis

The files in `analysis/source/dinc_ltv_in_psid` run the PSID analysis outlined in Section 5. The PSID analysis uses Gerardi, Herkenhoff, Ohanian and Willen (2017)'s PSID extract (available as part of the replication kit on Kyle Herkenhoff's [website](https://sites.google.com/site/kyleherkenhoff/research)), saved here as `analysis/input/data/psid_kfh_2013_3.dta`

- `ability_to_pay_by_ltv.R` runs the PSID analysis. The output are Figures 6a and the numbers in Section 5 of the paper.
- `psid_benchmarks.R` produces the tables derived from the PSID analysis, which include Table 3, Table A-3, Table A-12, and Table A-16. This script has to be run after `ability_to_pay_by_ltv.R` to have the necessary data loaded.

## CoreLogic

The files for the measurement error in LTV ratio analysis described in Section 4.3 are in `analysis/source/corelogic/`.

- `corelogic_clean.Rmd` and `corelogic_clean.html` contains explanations that walk through the three R-files in the folder.
- `run_script.sh` creates the job on the Research Computing Cluster that runs the measurement error analysis.
  - `read_all.R` imports the CoreLogic data and cleans it for the next script.
  - `merge_hpi.R` uses the cleaned CoreLogic HPI and merges it to the deed data.
  - `plot_and_tabulate.R` uses the final dataset to create Figure A-11, panel a and b.


## JPMCI
Some of the data used for this paper were prepared in JPMorganChase Insitute's (JPMCI) secure computing facilities. Due to JPMCI's rules on access and confidentiality, the programming code and analysis files cannot be made available publicly. The analysis files and programming code created by the authors will be available within JPMCI's secure computing facilities until 2027, and can be requested by researchers with approved projects (email institute@jpmchase.com). We grant any researchers with appropriate approval to conduct research on JPMCI's secure computing facilities access to these files. Below, we list the tables needed to replicate the analysis as well as some key variables

### 1. Archived tables:
- `institute_archive.ganong_strategic_qje_do_not_delete_tb_all_underwater_hist`: all underwater income drops for histogram and CDF
- `institute_archive.ganong_strategic_qje_do_not_delete_tb_all_abovewater_hist`: all above water sample for UI rate
- `institute_archive.ganong_strategic_qje_do_not_delete_tb_all_underwater_micro`: all underwater microdata for phi regression
- `institute_archive.ganong_strategic_qje_do_not_delete_tb_income_modification`: mortgages in servicing table that receive a modification
- `institute_archive.ganong_strategic_qje_do_not_delete_pg_strategic_mtg_12_08_21`: i) rows where `acctdim_sk` is not null: main analysis table, ii) rows where `acctdim_sk` is null: observations to be added to main analysis table for post-default analysis
- `institute_archive.ganong_strategic_qje_do_not_delete_tb_ss_sample`: used for Social Security subsample
- `institute_archive.ganong_strategic_qje_do_not_delete_tb_strategic_ui_05_13_21`: unfiltered sample for UI analysis (contains rows where `acctdim_sk` is null)

#### Key variables:
`institute_archive.ganong_strategic_qje_do_not_delete_tb_all_underwater_hist`:
- `acctdim_sk2`
- `delin_period`
- `inc_ratio`
- `received_ui`



`institute_archive.ganong_strategic_qje_do_not_delete_tb_all_abovewater_hist`:
- `acctdim_sk2`
- `delin_period`
- `received_ui`

`institute_archive.ganong_strategic_qje_do_not_delete_tb_all_underwater_micro`:
- `acctdim_sk2`
- `delin_period`
- `mos_since_def90`
- `inflow`
- `outflow`
- `schd_pi_am`


`institute_archive.ganong_strategic_qje_do_not_delete_tb_income_modification`:
- `customerdim_sk`
- `periodid`
- `monthly_income`
- `mod_period`
- `mod_type_cd`
- `eff_cltv_rt`
- `schd_pi_am`
- `paid_pi_am`


`institute_archive.ganong_strategic_qje_do_not_delete_pg_strategic_mtg_12_08_21`:
- `acctdim_sk2`
- `customerdim_sk2`
- `periodid`
- `period_index`
- `per_chk_inflow_total_am`
- `per_chk_outflow_total_am`
- `lien_pstn_cd`
- `rllp_prod1_tx`
- `bnd_mba_cyc_delq_tx`
- `fc_sts_cd`
- `paid_pi_am`
- `schd_pi_am`
- `hpi_prop_valn_am`
- `curr_bal_am`
- `dpd_cd`

`institute_archive.ganong_strategic_qje_do_not_delete_tb_ss_sample`:
- `customerdim_sk2`
- `periodid`
- `per_chk_inflow_total_am`
- `per_chk_outflow_total_am`
- `cbsa_cd`
- `age`


`institute_archive.ganong_strategic_qje_do_not_delete_tb_strategic_ui_05_13_21`:
- `acctdim_sk2`
- `customerdim_sk2`
- `periodid`
- `per_chk_inflow_total_am`
- `ui_hist`
- `labor_hist`
- `lien_pstn_cd`
- `rllp_prod1_tx`
- `period_index`
- `first_ui_period`
- `first_ui_period_index`
- `count_ui_txns`
- `eff_cltv_rt`
- `bnd_mba_cyc_delq_tx`

### 2. Inputs from repository inside JPMCI firewall:
- `csv_counts_df.csv`: sample counts for `stats_text`
- `implied_second_lien.csv`: table for implied second lien rate
- `age_orig_benchmarking.csv`: origination statistics for benchmarking
- `count_benchmark_table.csv`: benchmark table
- `csv_tb_all_underwater_subprime.csv`: all underwater income drops for subprime borrowers
- `csv_tb_all_underwater_agg.csv`: all underwater aggregate income drops
- `csv_tb_all_underwater_recourse.csv`: all underwater income drops by recourse/non-recourse states
- `csv_tb_benchmark_ltv_bins.csv`: mortgage benchmarking data by LTV
- `csv_tb_all_underwater_type.csv`: all underwater income drops by mortgage type
- `csv_tb_all_underwater_ltv.csv`: all underwater income drops by LTV



### 3. External data dependencies
The following tables are from the repkit of Guren et al (2021):
- `gmns_gammas.dta`: CBSA level gamma (house price sensitivity) values from Guren et al. (This data can be obtained from Adam Guren's website; link here: https://people.bu.edu/guren/gmns_gammas.zip.)
- `freddie_foranalysis_longdiff_pc.dta`: Freddie Mac regional HPI changes used in Guren et al.
- `guren_monthly_crtl.csv`: monthly data with controls from Guren et al
- `guren_yearly_crtl.csv`: yearly data with controls from Guren et al

The last two files are constructed using code from Guren et al's repkit; the script that constructs these tables is saved as `00_guren_controls.R` in this repo.

_Note:_ Guren et al (2021) refers to the paper: Guren, Adam M, Alisdair McKay, Emi Nakamura, Jón Steinsson. 2021. "Housing Wealth Effects: The Long View." _The Review of Economic Studies,_ 88(2): 669-707.
