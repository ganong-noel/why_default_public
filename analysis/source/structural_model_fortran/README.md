# Structural Model

We are able to release a full repkit for this section

## Running the model

### Overview

To run the model, we use a remote computing cluster with 28 Intel E5-2680v4 2.4GHz CPUs, 64 GB of memory, and EDR and FDR Infiniband interconnect. Each run of the model takes around 10 hours and requires up to 30G of memory. If you would like to skip this part of the replication, please email ganong@uchicago.edu or pascal.noel@chicagobooth.edu to request the simulation data.

An important note: this code is based on code originally created by John Campbell and Joao Cocco. We are very grateful that they shared their code with us.

### Instructions

1. Pull the repository onto your cluster.
1. Create a directory for storing the large output of the model. We create a `run` directory in the scratch area of our cluster.
    ```
    mkdir $SCRATCH/run
    ```
    If you choose to make a different directory, you will need to edit the value of the `model_dir` object in the `master.sbatch` script. Set it equal to the filepath that leads to the directory you make for the output.  
1. The four model runs that you will need to do are in the table below. Choose one, and edit the `stigma` and  `boost_prob_bad_house_shock` parameters in `config_nml.nml` to the values corresponding to your chosen run.

    | model_id | stigma | boost_prob_bad_house_shock |
    | --- | --- | --- |
    | stigma_0_bh_shock_0 | 0 | 0 |
    | stigma_80_bh_shock_20 | 0.8 | 0.2 |
    | stigma_90_bh_shock_20 | 0.9 | 0.2 |
    | stigma_100_bh_shock_20 | 1.0 | 0.2 |
1. Set your working directory to `/analysis/source/structural_model_fortran`. Run the simulation using the code below:
    ```
    sbatch master.sbatch --armi --model_id [Insert the model_id from the table here] --config config_nml.nml --n 10
    ```
    Note that there is a preamble in `master.sbatch` that is specific to our computing cluster. You will have to edit that preamble according to your own cluster's requirements.
1. Repeat steps three and four until you've run all four models.
1. On your local machine, create a directory called `data_cc_simulations` within the same parent directory where you cloned/saved the repkit. Then, create a repo within `data_cc_simulations` called `data`. This will maintain the file paths we use in the R code that analyzes the simulations. If you'd like a different setup, you will have to edit the `cc_simulations` object in `analysis/config.yml`.
1. After you've run the models, secure copy (scp) the output files with "simulation" at the beginning of their name from the directory you made in step one to the directory `[parent directory]/data_cc_simulations/data` that you made in step six.

### File Descriptions

#### Fortran scripts (creates policy functions)
Fortran code must be compiled. This is automated with [compile_code.sh](https://github.com/ganong-noel/why_default_public/blob/6f0ee1c84e2d7576a4a62a7352d785c7e59d7d52/analysis/source/structural_model_fortran/compile_code.sh).

- `Renti` is executable from Fortran run by the master script.
    - Needs: behavioral parameters (`config_nml.nml`)
    - Byproduct (console output): `renti.out`
    - Output: `rear{01,20}`
    - Purpose: gives values if you switch from paying mortgage to renting
- `Armi` is executable from Fortran run by the master script.
    - Needs:  `rear{01,20}`, behavioral + mortgage parameters (`config_nml.nml`)
    - Byproduct (console output): `armi.out`
    - Output: `year{01,20}`, `v01arm`
    - Purpose: gives values if you keep paying mortgage
    - The code is described section by section [here](https://github.com/ganong-noel/why_default_public/blob/541b027115671bda95ee037e5eb9932bf2886210/analysis/source/structural_model_fortran/armi_analysis.pdf)

#### Matlab scripts (runs simulations)
- `sarmi.m` is run by the master script
    - Needs: `rear{01,20}` and `year{01,20}` files, behavioral + mortgage parameters (`config_nml.nml`)
    - Output: `statsa800x50.raw` (we rename this immediately to `simulation_<model_id>_i.raw`).
    - The code is described section by section [here](https://github.com/ganong-noel/why_default_public/blob/541b027115671bda95ee037e5eb9932bf2886210/analysis/source/structural_model_fortran/sarmi_m_analysis.pdf)

#### Processing scripts
To smooth the process we wrote scripts:
- `config_nml.nml`: a Fortran nameslist file that sets specified paramaters in `renti`, `armi` and `sarmi.` The list can be extended by updating those three files.
- `master.sbatch`: an sbatch script that flexibly runs the simulation process from start to finish.
- `compile_code.sh`: a shell script that compiles Fortran code to create `renti` or `armi`. First, it compiles helper functions individually using `-c` flag, and then it compiles everything into a single executable file.  (Helper functions are described below).
- `matlab_batch.sbatch`: an sbatch script that runs the matlab simulation. This is called by `master.sbatch`.


#### Helper files
Renti, armi and sarmi.m rely on helper functions.

##### Fortran
- `ntoil.f90`: n to index location. takes a value and determines the nearest index in a set of grid points. (Confusing point: `ntoil` takes an input called `n` and another called `value`. `n` is the number of grid points in the grid, while `value` is the value we determine the best index for.)  
- `Spline.f90`: `Spline` determines values between grid points
- `Splint.f90`: `Splint` is called by irev* functions. Similar to spline, it determines values between grid points.
- `Gettrans.f90`: takes quadrature points and creates a transition matrix.
- `irutil2.f90`: takes consumption grid and returns a utilty grid
- `irevi.f90`: cycles through various states of the world to produce an expected value of utility in the next period for mortgagors (uncertain)
- `irevrenti.f90`: cycles through various states of the world to produce an expected value of utility in the next period for renters

##### Matlab
- `ntoi.m`: same functionality as ntoil.f90
- `gettrans.m`: same functionality as Gettrans.f90
- `condnorm.m`: calculates conditional probabilities of quadrature points for `gettrans.m`
- `read_namelist.m`: a function found online to read Fortran's native namelists



#### output files
- `simulation_<model_id>_<i>.raw`: the output from the matlab simulation
- `rear{01, 20}`: output from `renti`. For each year, we have a value function and policy function (table) that is indexed by:
    - a grid point from `ncash` (`ind2`: 1 to 185)
    - number of high inflation in the past: 1 stands for zero (`ind4 or 8`: 1 to t+1)
    - number of low permanent income in the past (`ind5`: 1 to t)
    - number of low housing price innovations in past (`ind8`: 1 to t)
    - Current inflation rate (`ind3`: 1 or 2)
    - Current real interest rate (`ind9`: 1 or 2)
    - The value functions are used by `armi`, while the policy functions are used in simulations.
- `year{01, 20}`: output from `armi`. For each year, we have a policy function and default function (table) indexed as `rear{01, 20}`.
- `varm01`: output from `armi` includes a one year policy function for arms.
- `armi.out`/`renti.out`: the output from running the eponymous fortran command. This includes a print out of the config_nml.nml file.
- `slurm_*`: each run produces two slurm files which capture the standard output from the simulation. The first file is from running the fortran section of the simulation and the second reports the matlab output. The fortran section includes warnings from compilation, which are normal. The output from fortran and matlab could be used for debugging.

## Analyzing the model

### Instructions

To recreate our analysis of the model, all you have to do is run `master.R` saved here, `analysis/source/structural_model_fortran/analysis/master.R`.

### File Descriptions

#### Scripts

- `master.R` runs all analysis scripts in `analysis/source/structural_model_fortran/analysis`
- `prelim.R` loads required parameters, libraries, and functions
- `config.yml` defines relevant file paths
- `cocco_data.R` hardcodes data from Campbell and Cocco (2015) to be used for comparison to our model runs in `cc_table_2_dot_plots.R`
- `sarmi_processing_functions.R` creates functions for cleaning and analyzing the model output from `sarmi.m`
- `cc_table_2_dot_plots.R` produces Figure A-17
- `optimal_stigma.R` compares runs of the model with different stigma values to the Chase data to determine which stigma value creates the best fit
- `chase_vs_cc_simulations.R` produces Figure 7
- `stigma_cost_value_functions.R` calculates the consumption cost of stigma at our optimal level calculated in `optimal_stigma.R`

#### Inputs

- Simulation data from running the model
  - `[parent directory]/data_cc_simulations/data/simulation_hh_0_stigma_0_bh_rand_large_test_wealth.raw`
  - `[parent directory]/data_cc_simulations/data/simulationhh_80_stigma_.2_bh_rand_large_test_wealth.raw`
  - `[parent directory]/data_cc_simulations/data/simulation_hh_90_stigma_.2_bh_rand_large_aux.raw`
  - `[parent directory]/data_cc_simulations/data/simulation_hh_100_stigma_.2_bh_rand.raw`

- Chase data
  - `[parent directory]/why_default_public/analysis/input/disclose/latest/gn_strategic_latest.xls`

#### Outputs

- `table_2_rep_hh.png` is Figure A-16
- `optimal_stigma_value.csv` contains the stigma value that best fits the Chase data
- `delta_inc_at_default_cc_v_chase_no_stigma.png` is the top half of Figure 7
- `delta_inc_at_default_cc_v_chase_with_stigma.png` is the bottom half of Figure 7
- `stigma_cost.csv` contains the consumption cost of stigma at our optimal level.
- All outputs are saved in `[parent directory]/why_default_public/analysis/release/structural_model_fortran`
