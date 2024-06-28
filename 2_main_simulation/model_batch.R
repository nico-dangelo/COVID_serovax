##Code below is Rscript that is executed through the bash script calib_bash.sh

library(deSolve)
library(tidyverse)
require(plyr)
library(here)
library(foreach)
library(progressr)
library(doParallel)
library(tictoc)

tic()

rm(list=ls())
cl <- makeCluster(4)
registerDoParallel(cl)

##For the HPC
#registerDoParallel(cores=32)
#options(scipen=999)

##Edit beginning and end of sweep to execute the sweeps across the four cores

sweep_beg <- 1
sweep_end <- 4

mod_scenarios <- foreach(i = sweep_beg:sweep_end) %dopar% {
  #mod_scenarios <- for(i in 1:num_sweep){
  library(deSolve)
  library(tidyverse)
  library(plyr)
  
  sweep<-sweep
  source("~/COVID_serovax/2_main_simulation/model_setup_two.R")
  
  loop<- model_sims(i)
}
toc()

# saveRDS(mod_scenarios,"/restricted/projectnb/shiogrp/Nico/COVID_serovax_modified/output")

