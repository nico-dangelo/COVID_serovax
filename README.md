# Summary
This repository contains the code used in simulations comparing fixed-time interval COVID-19 vaccination strategies in the US state Georgia. 

# Code description
## Main forward model simulation and analysis
Code for the main modeling work and analysis can be found in [main_simulation](https://github.com/lopmanlab/COVID_serovax_Mozambique/tree/main/2_main_simulation). The 1-year forward model simulations were run using a High Performance Cluster. Relevant pieces of the main code are detailed in the table below. 

| File                   | Description |Category|
| ---------------------- | ------------- |------------- 
| [1_sweep_int](2_main_simulation/1_sweep_int.RDS)| Data frame of model parameters for fixed time interval vax scenarios| Fixed-interval model sims|
| [model_setup_two](2_main_simulation/model_setup_two.R)         |Setup model without seroprevalence vax trigger |Fixed-interval model sims|
| [1d_model_code_int](2_main_simulation/model_code_int_two.R)      | Model code function with fixed time vax|Fixed-interval model sims|
| [2_compile_res_hpc](2_main_simulation/2_compile_res_hpc.R)      | Takes raw outputs from simulations and summarizes cumulative outcomes over 10-years|Compile results &summarise| stored in  [0_res](2_main_simulation/0_res)
| [2_compile_annual_hpc](2_main_simulation/2_compile_annual_hpc.R)      | Takes raw outputs from simulations and summarizes for annual NNT
| [9_last_Rrand](2_main_simulation/9_last_Rrand.RDS)      | Distribution of compartments at end of calibration|Model input|
| [mixing_matrix_gmix](2_main_simulation/mixing_matrix_gmix.R)      | Social mixing matrix input|Model input|



## Model calibration
The code used for model calibration implemented using Approximate Bayesian Approach can be found in [1_calibration](1_calibration)
