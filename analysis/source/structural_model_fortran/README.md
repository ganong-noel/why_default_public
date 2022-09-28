# Diagram with flow of code
[See google drawing](https://docs.google.com/drawings/d/1nRx3OgYSFPtJHKkdUWSJ5zO_m7YMtIWhBQ15pEPH-mo/edit?usp=sharing)

# Running code from Fortran to R

## Overview / quick start

1. Pull the repository on to the RCC.
1. Create a `run` directory on scratch area for large output storage
    ```
    mkdir $SCRATCH/run
    ```
1. Set parameters in `config_nml.nml`
1. Run the simulation (This may take up to 10 hrs)
    ```
    sbatch master.sbatch --armi --model_id <something to identify the model> --config config_nml.nml --n 5
    ```
1. Secure copy (scp) the output files to your computer
1. Analyze with R

## Configuration
`config_nml.nml` is a fortran namelist that will be parsed by fortran and matlab code. (Note: see  fortran (e.g. armi.f90) and matlab code (sarmi.m) to understand how the parameters are incorporated to the simulation).

## master script: simulations with Fortran + matlab
`master.sbatch` is an sbatch script written to work on the RCC server. It takes a config file and a few additional arguments and automates the entire simulation process.

- `--armi` use arm code
- `--frmi` use frm code
- `--model_id <str>` <str> will be part of the names of output files and folder that holds it.
- `--config <file>` use <file> for configuration
- `--n <int>` produce <int> simulations (matlab)
- `--rent <dir_path>` use rent policy function files found in <dir_path> instead of producing new files

This is run from `./strategic/analysis/source/structural_model_fortran`. The files output into `/scratch/midway2/<user>/run/`.

The output files are deterministically created based on parameters. The 40 files are roughly 2GB and are called `rear<int>` or `year<int>` with int from `01` to `20`.

### Fortran
Fortran is a compiled language. This means that you must process the code with a compiler to make an executable in order to run the code.

`compile_code.sh` automates the compilation process. It relies on _gfortran_ a fortran compiler that is standard on linux and works on mac. This file is called by `master.sbatch`.

### matlab
The matlab code is memory intensive and so is instantiated by a separate sbatch script.

`matlab_batch.sbatch` requests a larger memory node and runs the matlab simulation which relies on the policy functions produced by fortran and stored on scratch.

The matlab output files are ~200MB.

## Analyzing simulation output in R
### moving files
I manually move the output files I want to `/scratch/midway2/<user>/run/out/` and then copy them to a local directory. e.g.
```
scp -r <user>@midway2.rcc.uchicago.edu:/scratch/midway2/<user>/run/out/* /home/<user>/current_project/out/
```
### analyzing data with `master.R`

`master.R` holds productionized analysis of simulation output.

### data for `master.R`

Data for `master.R` is stored in the repo `data_cc_simulation`.

| script |data name | seed | data description | 
|---|---|---|---|
| `cc_table_2_dot_plots.R`| "cocco_original_sigty_high_indcase_3_2019_06_11" | cocco's seed | HH parameter's as given |
| `cc_table_2_dot_plots.R`| "cocco_original_sigty_low_indcase_3_2019_06_11" |  cocco's seed | HL parameter's as given |
| `cc_table_2_dot_plots.R`| "cocco_original_sigty_high_indcase_1_2019_06_11" | cocco's seed | LH parameter's as given |
| `cc_table_2_dot_plots.R`| "cocco_original_sigty_low_indcase_1_2019_06_11" | cocco's seed | LL parameter's as given |
| `chase_v_cc_simulations.R`| "cocco_original_sigty_high_indcase_3" | 5x random seed| HH parameter's as given |      


# Letter from Joao Cocco
The zip file entitled “Rent” has the fortran programs for the default state, in which the agent rents for the remaining of the time horizon. The value functions that are generated from these programs are the input for the others for the choices of whether to default or to sell and move to rental.

The zip file entitled “ARM_HH”  has the fortran programs for the ARM that solve the program and generate the policy and value functions and the matlab programs that are used to simulate the model. The file is entitled ARM_HH since these programs are setup for High initial inflation and High initial real interest rates. These are just initial conditions and can easily be changed in the programs.

Finally, the zip file entitled FRM_HH has the programs for the FRM. As before, fortran programs are for solving the model and matlab programs are for simulating it. There is quite a bit of repetition in the matlab programs since my computer did not have enough memory to load all policy functions at the same time. So I had to do it sequentially.

The FRM programs with the refinancing option are the trickiest, and they need to be solved sequentially. That is: first one needs to solve the FRM program for the lowest level of initial interest rates. Then one uses the output from this program as an input for the one with the second lowest level of initial interest rates. If and when interest rates decline the agent decides whether to refinance the FRM mortgage. Then one uses the input of these two programs as an input for the one with the third lowest level of initial interest rates. Again this is to model the refinancing decisions. And so on.

The FRM programs that I am sending attached are the most general, for the highest level of initial interest rates. This can easily be changed by changing the initial conditions and the inputs that one needs to feed in for the refinancing option.

Thanks.

# Further documentation
The following resources may be helpful for further understanding of parts of the model:
- [Algebra to code](https://github.com/ganong-noel/strategic/wiki/Understanding-the-Campbell-and-Cocco-(2015)-model-of-mortgage-default:-mapping-from-algebra-to-code)
- [Code process](https://github.com/ganong-noel/strategic/wiki/Campbell-and-Cocco-(2015)-code-process)
- [Data naming conventions](https://github.com/ganong-noel/data_cc_simulations)
- [Code section-by-section documentation](https://github.com/ganong-noel/strategic/tree/149b286dde3706645579d4070a1bc6114ebf8b33/issues/issue_226_document_model)
- [The paper itself](https://onlinelibrary.wiley.com/doi/full/10.1111/jofi.12252)
