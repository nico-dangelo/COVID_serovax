


source("mixing_matrix_gmix.R")
## Model flows
## The subscripts denote the number of times someone as been exposed through either infection or vaccination
COVID_sero_vax <- function(t, start, params) {
  with(as.list(c(start, params)), {
    sd <- input(t)
    # Derived variables
    
    
    #Urban child
    Ncu0v0 = sum(Scu0v0, Ecu1v0, Acu1v0, Icu1v0, Hcu1v0,  Rncu1v0) #Unvax and no prior exposure
    Ncu1v0 = sum(Sncu1v0,
                 Ecu2v0,
                 Acu2v0,
                 Icu2v0,
                 Hcu2v0,
                 Rpcu2v0,
                 Rncu2v0) #Unvax and one prior
    
    
    Ncu0v1 = sum(Vncu0v1,
                 Sncu0v1,
                 Ecu1v1,
                 Acu1v1,
                 Icu1v1,
                 Hcu1v1,
                 
                 Rncu1v1) #vax 1 dose and no prior exposure
    Ncu1v1 = sum(Sncu1v1,
                 Ecu2v1,
                 Acu2v1,
                 Icu2v1,
                 Hcu2v1,
                 
                 Rncu2v1) #vax 1 dose and one prior
    
    
    Ncu0v2 = sum(Vncu0v2,
                 Sncu0v2,
                 Ecu1v2,
                 Acu1v2,
                 Icu1v2,
                 Hcu1v2,
                 
                 Rncu1v2) #vax 2 dose and no prior exposure
    Ncu1v2 = sum(Sncu1v2,
                 Ecu2v2,
                 Acu2v2,
                 Icu2v2,
                 Hcu2v2,
                 
                 Rncu2v2) #vax 2 dose and one prior
    
    
    
    #Urban adult
    Nau0v0 = sum(Sau0v0, Eau1v0, Aau1v0, Iau1v0, Hau1v0,  Rnau1v0) #Unvax and no prior exposure
    Nau1v0 = sum(Snau1v0,
                 Eau2v0,
                 Aau2v0,
                 Iau2v0,
                 Hau2v0,
                 
                 Rnau2v0) #Unvax and one prior
    
    
    Nau0v1 = sum(Vnau0v1,
                 Snau0v1,
                 Eau1v1,
                 Aau1v1,
                 Iau1v1,
                 Hau1v1,
                 
                 Rnau1v1) #vax 1 dose and no prior exposure
    Nau1v1 = sum(Snau1v1,
                 Eau2v1,
                 Aau2v1,
                 Iau2v1,
                 Hau2v1,
                 
                 Rnau2v1) #vax 1 dose and one prior
    
    Nau0v2 = sum(Vnau0v2,
                 Snau0v2,
                 Eau1v2,
                 Aau1v2,
                 Iau1v2,
                 Hau1v2,
                 
                 Rnau1v2) #vax 2 dose and no prior exposure
    Nau1v2 = sum(Snau1v2,
                 Eau2v2,
                 Aau2v2,
                 Iau2v2,
                 Hau2v2,
                 
                 Rnau2v2) #vax 2 dose and one prior
    
    
    
    
    
    
    
    #Urban elderly
    Neu0v0 = sum(Seu0v0, Eeu1v0, Aeu1v0, Ieu1v0, Heu1v0,  Rneu1v0) #Unvax and no prior exposure
    Neu1v0 = sum(Sneu1v0,
                 Eeu2v0,
                 Aeu2v0,
                 Ieu2v0,
                 Heu2v0,
                 
                 Rneu2v0) #Unvax and one prior
    
    
    Neu0v1 = sum(Vneu0v1,
                 Sneu0v1,
                 Eeu1v1,
                 Aeu1v1,
                 Ieu1v1,
                 Heu1v1,
                 
                 Rneu1v1) #vax 1 dose and no prior exposure
    Neu1v1 = sum(Sneu1v1,
                 Eeu2v1,
                 Aeu2v1,
                 Ieu2v1,
                 Heu2v1,
                 
                 Rneu2v1) #vax 1 dose and one prior
    
    Neu0v2 = sum(Vneu0v2,
                 Sneu0v2,
                 Eeu1v2,
                 Aeu1v2,
                 Ieu1v2,
                 Heu1v2,
                 
                 Rneu1v2) #vax 2 dose and no prior exposure
    Neu1v2 = sum(Sneu1v2,
                 Eeu2v2,
                 Aeu2v2,
                 Ieu2v2,
                 Heu2v2,
                 
                 Rneu2v2) #vax 2 dose and one prior
    
    ##Population sums
    
    
    Nchildu = sum(Ncu0v0,
                  Ncu1v0,
                 
                  Ncu0v1,
                  Ncu1v1,
                  
                  Ncu0v2,
                  Ncu1v2
                 )
    Nadultu = sum(Nau0v0,
                  Nau1v0,
                 
                  Nau0v1,
                  Nau1v1,
                  
                  Nau0v2,
                  Nau1v2)
    
    Noldu   = sum(Neu0v0,
                  Neu1v0,
                 
                  Neu0v1,
                  Neu1v1,
                  
                  Neu0v2,
                  Neu1v2)
    
    ## Vax values (0 unless triggered)
    
    ## Vax values (0 unless triggered)
    
    delta1_cu <- 0
    delta1_au <- 0
    delta1_eu <- 0
    delta2_cu <- 0
    delta2_au <- 0
    delta2_eu <- 0
    
    
    ##Social distancing stuff
    #create the seasonality parameter
    #years = t/365
    #seas  = 1 + beta1 *(cos(2*3.141593*years + 1))      #seasonal forcing
    
    ##r0_hyp <- r00
    #sd <- (r0_hyp)/r0
    
    #if ((t/365)%%1>(270/365)){
    #yr<-floor(years)
    #  if(yr==0){
    #    r0_hyp <- r00
    #  } else if(yr==1){
    #    r0_hyp <- r01
    #  } else if(yr==2){
    #    r0_hyp <- r02
    #  } else if(yr==3){
    #    r0_hyp <- r03
    #  } else if(yr==4){
    #    r0_hyp <- r04
    #  } else if(yr==5){
    #    r0_hyp <- r05
    #  } else if(yr==6){
    #    r0_hyp <- r06
    #  } else if (yr==7){
    #    r0_hyp <- r07
    #  } else if (yr==8){
    #    r0_hyp <- r08
    #  } else {
    #    r0_hyp <- r09
    #  }
    #sd <- ((r0_hyp)/r0)*seas
    #} else {
    #  sd <- 1.8/r0
    #}
    
    #if(t<=100){
    #  sd <- sd2
    #} else if(t<=320){
    #  sd <- rel_newvar
    #} else {
    #  sd <- rel_newvar ##keep on for now and see what happens
    #}
    
    ### imunne escape factor for those with either prior exposure or prior vaccination
    #if (t <= 100){
    #imm_esc_factor <- imm_esc_factor_omi
    #} else {
    #  imm_esc_factor <- imm_esc_factor_newvar
    #}
    
    
    
    
    
    
    #StartODES
    # Force of infection
    # Foi_cr: Interpreted as FOI exerted on child rural susceptibles
    
    foi_cu_0v0 = beta_c * ((
      CM[1, 1] * (
        Icu1v0 + Icu2v0  + Icu1v1 + Icu2v1  + Icu1v2 + Icu2v2 +
          alpha * (Acu1v0 + Acu2v0  + Acu1v1 + Acu2v1 + Acu1v2 + Acu2v2)
      ) / Nchildu
    ) +
      (
        CM[2, 1] * (
          Iau1v0 + Iau2v0  + Iau1v1 + Iau2v1  + Iau1v2 + Iau2v2  + alpha * (Aau1v0 + Aau2v0 + Aau1v1 + Aau2v1 + Aau1v2 + Aau2v2)
        ) / Nadultu
      ) +
      (
        CM[3, 1] * (
          Ieu1v0 + Ieu2v0  + Ieu1v1 + Ieu2v1  + Ieu1v2 + Ieu2v2  + alpha * (Aeu1v0 + Aeu2v0  + Aeu1v1 + Aeu2v1 + Aeu1v2 + Aeu2v2)
        ) / Noldu
      ))
    
    foi_au_0v0 = beta_a * ((
      CM[1, 2] * (
        Icu1v0 + Icu2v0 + Icu1v1 + Icu2v1  + Icu1v2 + Icu2v2  + alpha * (Acu1v0 + Acu2v0  + Acu1v1 + Acu2v1  + Acu1v2 + Acu2v2)
      ) / Nchildu
    ) +
      (
        CM[2, 2] * (
          Iau1v0 + Iau2v0  + Iau1v1 + Iau2v1  + Iau1v2 + Iau2v2  + alpha * (Aau1v0 + Aau2v0  + Aau1v1 + Aau2v1 + Aau1v2 + Aau2v2)
        ) / Nadultu
      ) +
      (
        CM[3, 2] * (
          Ieu1v0 + Ieu2v0  + Ieu1v1 + Ieu2v1 + Ieu1v2 + Ieu2v2  + alpha * (Aeu1v0 + Aeu2v0  + Aeu1v1 + Aeu2v1  + Aeu1v2 + Aeu2v2)
        ) / Noldu
      ))
    
    foi_eu_0v0 = beta_e *  ((
      CM[1, 3] * (
        Icu1v0 + Icu2v0  + Icu1v1 + Icu2v1  + Icu1v2 + Icu2v2  + alpha * (Acu1v0 + Acu2v0  + Acu1v1 + Acu2v1  + Acu1v2 + Acu2v2)
      ) / Nchildu
    ) +
      (
        CM[2, 3] * (
          Iau1v0 + Iau2v0  + Iau1v1 + Iau2v1  + Iau1v2 + Iau2v2 + alpha * (Aau1v0 + Aau2v0 + Aau1v1 + Aau2v1  + Aau1v2 + Aau2v2)
        ) / Nadultu
      ) +
      (
        CM[3, 3] * (
          Ieu1v0 + Ieu2v0  + Ieu1v1 + Ieu2v1  + Ieu1v2 + Ieu2v2  + alpha * (Aeu1v0 + Aeu2v0  + Aeu1v1 + Aeu2v1  + Aeu1v2 + Aeu2v2)
        ) / Noldu
      ))
    
    
    foi_cu_0v1 = foi_cu_0v0 * (1 - vei1)
    
    foi_au_0v1 = foi_au_0v0 * (1 - vei1)
    
    foi_eu_0v1 = foi_eu_0v0 * (1 - vei1)
    
   
    
    foi_cu_0v2 = foi_cu_0v0 * (1 - vei2)
    
    foi_au_0v2 = foi_au_0v0 * (1 - vei2)
    
    foi_eu_0v2 = foi_eu_0v0 * (1 - vei2)
    
    
    
    ### After first exposure
    
    foi_cu_1v0 = foi_cu_0v0 * red_inf_1
    
    foi_au_1v0 = foi_au_0v0 * red_inf_1
    
    foi_eu_1v0 = foi_eu_0v0 * red_inf_1 #* imm_esc_factor
    
    
    foi_cu_1v1 = foi_cu_0v1 * red_inf_1 #* imm_esc_factor
    
    foi_au_1v1 = foi_au_0v1 * red_inf_1 #* imm_esc_factor
    
    foi_eu_1v1 = foi_eu_0v1 * red_inf_1 #* imm_esc_factor
    
    
    foi_cu_1v2 = foi_cu_0v2 * red_inf_1 #* imm_esc_factor
    
    foi_au_1v2 = foi_au_0v2 * red_inf_1 #* imm_esc_factor
    
    foi_eu_1v2 = foi_eu_0v2 * red_inf_1 #* imm_esc_factor
    
    
    
    ### After second exposure
    
    foi_cu_2v0 = foi_cu_0v0 * red_inf_2 #* imm_esc_factor * 0.7
    
    foi_au_2v0 = foi_au_0v0 * red_inf_2 #* imm_esc_factor
    
    foi_eu_2v0 = foi_eu_0v0 * red_inf_2 #* imm_esc_factor
    
    
    foi_cu_2v1 = foi_cu_0v1 * red_inf_2 #* imm_esc_factor
    
    foi_au_2v1 = foi_au_0v1 * red_inf_2 #* imm_esc_factor
    
    foi_eu_2v1 = foi_eu_0v1 * red_inf_2 #* imm_esc_factor
    
    
    foi_cu_2v2 = foi_cu_0v2 * red_inf_2 #* imm_esc_factor
    
    foi_au_2v2 = foi_au_0v2 * red_inf_2 #* imm_esc_factor
    
    foi_eu_2v2 = foi_eu_0v2 * red_inf_2 #* imm_esc_factor
    
    ################################
    ##Unvaccinated and unexposed####
    ################################
    #Susceptible
    
    dScu0v0  = -Scu0v0 * sd * foi_cu_0v0 - delta1_cu * Scu0v0
    dSau0v0  = -Sau0v0 * sd * foi_au_0v0 - delta1_au * Sau0v0
    dSeu0v0  = -Seu0v0 * sd * foi_eu_0v0 - delta1_eu * Seu0v0
    
    #Exposed
    
    dEcu1v0  =  Scu0v0 * sd * foi_cu_0v0 -  sigma * Ecu1v0  - delta1_cu *
      Ecu1v0
    
    dEau1v0  =  Sau0v0 * sd * foi_au_0v0 -  sigma * Eau1v0  - delta1_au *
      Eau1v0
    
    dEeu1v0  =  Seu0v0 * sd * foi_eu_0v0 -  sigma * Eeu1v0  - delta1_eu *
      Eeu1v0
    
    # Asymptomatic
    
    dAcu1v0  =  (1 - nu_c) * sigma * Ecu1v0 - gamma_A * Acu1v0 - delta1_cu *
      Acu1v0
    
    dAau1v0  =  (1 - nu_a) * sigma * Eau1v0 - gamma_A * Aau1v0 - delta1_au *
      Aau1v0
    
    dAeu1v0  =  (1 - nu_e) * sigma * Eeu1v0 - gamma_A * Aeu1v0 - delta1_eu *
      Aeu1v0
    
    #Symptomatic
    
    dIcu1v0  =  nu_c * sigma * Ecu1v0 - gamma_I * Icu1v0
    
    dIau1v0  =  nu_a * sigma * Eau1v0 - gamma_I * Iau1v0
    
    dIeu1v0  =  nu_e * sigma * Eeu1v0 - gamma_I * Ieu1v0
    
    # Hospitalized
    
    dHcu1v0  =  phi_c * gamma_I * Icu1v0 - gamma_H * Hcu1v0
    
    dHau1v0  =  phi_a * gamma_I * Iau1v0 - gamma_H * Hau1v0
    
    dHeu1v0  =  phi_e * gamma_I * Ieu1v0 - gamma_H * Heu1v0
    
    
    #Recovered and not seropositive
    
    dRncu1v0  =   (1 - phi_c) * gamma_I * Icu1v0 +
      gamma_A * Acu1v0 +  (1 - mu_c) * gamma_H * Hcu1v0 - omega_nc *
      Rncu1v0 - delta1_cu * Rncu1v0
    
    dRnau1v0  =   (1 - phi_a) * gamma_I * Iau1v0 +
      gamma_A * Aau1v0 +  (1 - mu_a) * gamma_H * Hau1v0  - omega_na *
      Rnau1v0 - delta1_au * Rnau1v0
    
    dRneu1v0  =   (1 - phi_e) * gamma_I * Ieu1v0 +
      gamma_A * Aeu1v0 +  (1 - mu_e) * gamma_H * Heu1v0 - omega_ne *
      Rneu1v0 - delta1_eu * Rneu1v0
    
    #Deaths
    
    dDcu1v0  =  mu_c * gamma_H * Hcu1v0
    
    dDau1v0  =  mu_a * gamma_H * Hau1v0
    
    dDeu1v0  =  mu_e * gamma_H * Heu1v0
    
    ################################
    ##Unvaccinated and exposed once####
    ################################
    #Susceptible
    ##Note vaccination rate here doesnt differ by previous vax status
    
    
    ##Note vaccination rate here doesnt differ by previous vax status
    dSncu1v0  = omega_nc * Rncu1v0  - Sncu1v0 * sd * foi_cu_1v0 - delta1_cu * Sncu1v0
    
    dSnau1v0  = omega_na * Rnau1v0  - Snau1v0 * sd * foi_au_1v0 - delta1_au * Snau1v0
    
    dSneu1v0  = omega_ne * Rneu1v0  - Sneu1v0 * sd * foi_eu_1v0 - delta1_eu * Sneu1v0
    
    #Exposed
    
    dEcu2v0  =    Sncu1v0 * sd * foi_cu_1v0 -  sigma * Ecu2v0  - delta1_cu *
      Ecu2v0
    
    dEau2v0  =   Snau1v0 * sd * foi_au_1v0 -  sigma * Eau2v0  - delta1_au *
      Eau2v0
    
    dEeu2v0  =   Sneu1v0 * sd * foi_eu_1v0 -  sigma * Eeu2v0  - delta1_eu *
      Eeu2v0
    
    # Asymptomatic
    
    dAcu2v0  =  (1 - nu_c) * sigma * Ecu2v0 - gamma_A * Acu2v0 - delta1_cu *
      Acu2v0
    
    dAau2v0  =  (1 - nu_a) * sigma * Eau2v0 - gamma_A * Aau2v0 - delta1_au *
      Aau2v0
    
    dAeu2v0  =  (1 - nu_e) * sigma * Eeu2v0 - gamma_A * Aeu2v0 - delta1_eu *
      Aeu2v0
    
    #Symptomatic
    
    dIcu2v0  =  nu_c * sigma * Ecu2v0 - gamma_I * Icu2v0
    
    dIau2v0  =  nu_a * sigma * Eau2v0 - gamma_I * Iau2v0
    
    dIeu2v0  =  nu_e * sigma * Eeu2v0 - gamma_I * Ieu2v0
    
    # Hospitalized
    
    dHcu2v0  =  phi_c * gamma_I * Icu2v0 - gamma_H * Hcu2v0
    
    dHau2v0  =  phi_a * gamma_I * Iau2v0 - gamma_H * Hau2v0
    
    dHeu2v0  =  phi_e * gamma_I * Ieu2v0 - gamma_H * Heu2v0
    
    
    
    #Recovered and not seropositive
    
    dRncu2v0  =   (1 - phi_c) * gamma_I * Icu2v0 +
      gamma_A * Acu2v0 +  (1 - mu_c) * gamma_H * Hcu2v0  - omega_nc *
      Rncu2v0 - delta1_cu * Rncu2v0
    
    dRnau2v0  =   (1 - phi_a) * gamma_I * Iau2v0 +
      gamma_A * Aau2v0 +  (1 - mu_a) * gamma_H * Hau2v0 - omega_na *
      Rnau2v0 - delta1_au * Rnau2v0
    
    dRneu2v0  =   (1 - phi_e) * gamma_I * Ieu2v0 +
      gamma_A * Aeu2v0 +  (1 - mu_e) * gamma_H * Heu2v0  - omega_ne *
      Rneu2v0 - delta1_eu * Rneu2v0
    
    #Deaths
    
    dDcu2v0  =  mu_c * gamma_H * Hcu2v0
    
    
    dDau2v0  =  mu_a * gamma_H * Hau2v0
    
    dDeu2v0  =  mu_e * gamma_H * Heu2v0
    
    
    ################################
    ##Vaccinated one dose and unexposed####
    ################################
    
    
    # Susceptible and seronegative for S-spike and total IGg
    
    dSncu0v1  = omegav_nc * Vncu0v1 - Sncu0v1 * sd * foi_cu_0v1 - delta2_cu * Sncu0v1
    
    dSnau0v1  = omegav_na * Vnau0v1 - Snau0v1 * sd * foi_au_0v1 - delta2_au * Snau0v1
    
    dSneu0v1  = omegav_ne * Vneu0v1 - Sneu0v1 * sd * foi_eu_0v1 - delta2_eu * Sneu0v1
    
    #Exposed
    
    dEcu1v1  =   Sncu0v1 * sd * foi_cu_0v1 - sigma * Ecu1v1  - delta2_cu *
      Ecu1v1 + delta1_cu * Ecu1v0
    
    dEau1v1  =   Snau0v1 * sd * foi_au_0v1 -  sigma * Eau1v1  - delta2_au *
      Eau1v1 + delta1_au * Eau1v0
    
    dEeu1v1  =    Sneu0v1 * sd * foi_eu_0v1 -  sigma * Eeu1v1  - delta2_eu *
      Eeu1v1 + delta1_eu * Eeu1v0
    
    # Asymptomatic
    
    dAcu1v1  =  (1 - nu_c) * sigma * Ecu1v1 - gamma_A * Acu1v1 - delta2_cu *
      Acu1v1 + delta1_cu * Acu1v0
    
    dAau1v1  =  (1 - nu_a) * sigma * Eau1v1 - gamma_A * Aau1v1 - delta2_au *
      Aau1v1 + delta1_au * Aau1v0
    
    dAeu1v1  =  (1 - nu_e) * sigma * Eeu1v1 - gamma_A * Aeu1v1 - delta2_eu *
      Aeu1v1 + delta1_eu * Aeu1v0
    
    #Symptomatic
    
    dIcu1v1  =  nu_c * sigma * Ecu1v1 - gamma_I * Icu1v1
    
    dIau1v1  =  nu_a * sigma * Eau1v1 - gamma_I * Iau1v1
    
    dIeu1v1  =  nu_e * sigma * Eeu1v1 - gamma_I * Ieu1v1
    
    # Hospitalized
    dHcr1v1  =  phi_cv1 * gamma_I * Icr1v1 - gamma_H * Hcr1v1
    dHcu1v1  =  phi_cv1 * gamma_I * Icu1v1 - gamma_H * Hcu1v1
    
    dHau1v1  =  phi_av1 * gamma_I * Iau1v1 - gamma_H * Hau1v1
    
    dHeu1v1  =  phi_ev1 * gamma_I * Ieu1v1 - gamma_H * Heu1v1
    
    
    
    #Recovered and not seropositive
    
    dRncu1v1  =   (1 - phi_cv1) * gamma_I * Icu1v1 +
      gamma_A * Acu1v1 +  (1 - mu_c) * gamma_H * Hcu1v1  - omega_nc *
      Rncu1v1 - delta2_cu * Rncu1v1 + delta1_cu * Rncu1v0  + delta1_cu *
      Sncu1v0 
    
    dRnau1v1  =   (1 - phi_av1) * gamma_I * Iau1v1 +
      gamma_A * Aau1v1 +  (1 - mu_a) * gamma_H * Hau1v1 - omega_na *
      Rnau1v1 - delta2_au * Rnau1v1 + delta1_au * Rnau1v0  + delta1_au *
      Snau1v0 * (1 - rho_v1)
    
    dRneu1v1  =   (1 - phi_ev1) * gamma_I * Ieu1v1 +
      gamma_A * Aeu1v1 +  (1 - mu_e) * gamma_H * Heu1v1  - omega_ne *
      Rneu1v1 - delta2_eu * Rneu1v1 + delta1_eu * Rneu1v0  + delta1_eu *
      Sneu1v0 
    
    #Deaths
    
    dDcu1v1  =  mu_c * gamma_H * Hcu1v1
    
    dDau1v1  =  mu_a * gamma_H * Hau1v1
    
    dDeu1v1  =  mu_e * gamma_H * Heu1v1
    
    ################################
    ##Vaccinated one dose and exposed once####
    ################################
    #Susceptible
    
    
    
    
    dSncu1v1  = omega_nc * Rncu1v1  - Sncu1v1 * sd * foi_cu_1v1 - delta2_cu * Sncu1v1
    
    dSnau1v1  = omega_na * Rnau1v1  - Snau1v1 * sd * foi_au_1v1 - delta2_au * Snau1v1
    
    dSneu1v1  = omega_ne * Rneu1v1  - Sneu1v1 * sd * foi_eu_1v1 - delta2_eu * Sneu1v1
    
    #Exposed
    
    dEcu2v1  =    Sncu1v1 * sd * foi_cu_1v1 -  sigma * Ecu2v1  - delta2_cu *
      Ecu2v1 + delta1_cu * Ecu2v0
    
    dEau2v1  =    Snau1v1 * sd * foi_au_1v1 -  sigma * Eau2v1  - delta2_au *
      Eau2v1 + delta1_au * Eau2v0
    
    dEeu2v1  =    Sneu1v1 * sd * foi_eu_1v1 -  sigma * Eeu2v1  - delta2_eu *
      Eeu2v1 + delta1_eu * Eeu2v0
    
    # Asymptomatic
    
    dAcu2v1  =  (1 - nu_c) * sigma * Ecu2v1 - gamma_A * Acu2v1 - delta2_cu *
      Acu2v1 + delta1_cu * Acu2v0
    
    dAau2v1  =  (1 - nu_a) * sigma * Eau2v1 - gamma_A * Aau2v1 - delta2_au *
      Aau2v1 + delta1_au * Aau2v0
    
    dAeu2v1  =  (1 - nu_e) * sigma * Eeu2v1 - gamma_A * Aeu2v1 - delta2_eu *
      Aeu2v1 + delta1_eu * Aeu2v0
    
    #Symptomatic
    
    dIcu2v1  =  nu_c * sigma * Ecu2v1 - gamma_I * Icu2v1
    
    dIau2v1  =  nu_a * sigma * Eau2v1 - gamma_I * Iau2v1
    
    dIeu2v1  =  nu_e * sigma * Eeu2v1 - gamma_I * Ieu2v1
    
    # Hospitalized
    
    dHcu2v1  =  phi_cv1 * gamma_I * Icu2v1 - gamma_H * Hcu2v1
    
    dHau2v1  =  phi_av1 * gamma_I * Iau2v1 - gamma_H * Hau2v1
    
    dHeu2v1  =  phi_ev1 * gamma_I * Ieu2v1 - gamma_H * Heu2v1
    
    
    
    #Recovered and not seropositive
    
    dRncu2v1  =   (1 - phi_cv1) * gamma_I * Icu2v1 +
      gamma_A * Acu2v1 +  (1 - mu_c) * gamma_H * Hcu2v1  - omega_nc *
      Rncu2v1 - delta2_cu * Rncu2v1 + delta1_cu * Rncu2v0  + delta1_cu *
      Sncu2v0 
    
    dRnau2v1  =   (1 - phi_av1) * gamma_I * Iau2v1 +
      gamma_A * Aau2v1 +  (1 - mu_a) * gamma_H * Hau2v1 - omega_na *
      Rnau2v1 - delta2_au * Rnau2v1 + delta1_au * Rnau2v0  + delta1_au *
      Snau2v0 
    
    dRneu2v1  =   (1 - phi_ev1) * gamma_I * Ieu2v1 +
      gamma_A * Aeu2v1 +  (1 - mu_e) * gamma_H * Heu2v1  - omega_ne *
      Rneu2v1 - delta2_eu * Rneu2v1 + delta1_eu * Rneu2v0  + delta1_eu *
      Sneu2v0 
    
    #Deaths
    
    dDcu2v1  =  mu_c * gamma_H * Hcu2v1
    
    dDau2v1  =  mu_a * gamma_H * Hau2v1
    
    dDeu2v1  =  mu_e * gamma_H * Heu2v1
    
    
    ################################
    ##Vaccinated two dose and unexposed####
    ################################
    
    #Susceptible
    
    dSncu0v2  = omegav_nc * Vncu0v2 - Sncu0v2 * sd * foi_cu_0v2
    
    dSnau0v2  = omegav_na * Vnau0v2 - Snau0v2 * sd * foi_au_0v2
    
    dSneu0v2  = omegav_ne * Vneu0v2 - Sneu0v2 * sd * foi_eu_0v2
    
    #Exposed
    
    dEcu1v2  =  Sncu0v2 * sd * foi_cu_0v2 -  sigma * Ecu1v2 + delta2_cu * Ecu1v1
    
    dEau1v2  =   Snau0v2 * sd * foi_au_0v2 -  sigma * Eau1v2  + delta2_au * Eau1v1
    
    dEeu1v2  =   Sneu0v2 * sd * foi_eu_0v2 -  sigma * Eeu1v2   + delta2_eu * Eeu1v1
    
    # Asymptomatic
    
    dAcu1v2  =  (1 - nu_c) * sigma * Ecu1v2 - gamma_A * Acu1v2  + delta2_cu * Acu1v1
    
    dAau1v2  =  (1 - nu_a) * sigma * Eau1v2 - gamma_A * Aau1v2 + delta2_au * Aau1v1
    
    dAeu1v2  =  (1 - nu_e) * sigma * Eeu1v2 - gamma_A * Aeu1v2  + delta2_eu * Aeu1v1
    
    #Symptomatic
    
    dIcu1v2  =  nu_c * sigma * Ecu1v2 - gamma_I * Icu1v2
    
    dIau1v2  =  nu_a * sigma * Eau1v2 - gamma_I * Iau1v2
    
    dIeu1v2  =  nu_e * sigma * Eeu1v2 - gamma_I * Ieu1v2
    
    # Hospitalized
    
    dHcu1v2  =  phi_cv2 * gamma_I * Icu1v2 - gamma_H * Hcu1v2
    
    dHau1v2  =  phi_av2 * gamma_I * Iau1v2 - gamma_H * Hau1v2
    
    dHeu1v2  =  phi_ev2 * gamma_I * Ieu1v2 - gamma_H * Heu1v2
    
    
    
    #Recovered and not seropositive
    
    dRncu1v2  =   (1 - phi_cv2) * gamma_I * Icu1v2 +
      gamma_A * Acu1v2 +  (1 - mu_c) * gamma_H * Hcu1v2  - omega_nc *
      Rncu1v2  + delta2_cu * Rncu1v1  + delta2_cu *
      Sncu1v1 
    
    dRnau1v2  =   (1 - phi_av2) * gamma_I * Iau1v2 +
      gamma_A * Aau1v2 +  (1 - mu_a) * gamma_H * Hau1v2  - omega_na *
      Rnau1v2  + delta2_au * Rnau1v1  + delta2_au *
      Snau1v1 
    
    dRneu1v2  =   (1 - phi_ev2) * gamma_I * Ieu1v2 +
      gamma_A * Aeu1v2 +  (1 - mu_e) * gamma_H * Heu1v2  - omega_ne *
      Rneu1v2  + delta2_eu * Rneu1v1  + delta2_eu *
      Sneu1v1 
    
    #Deaths
    
    dDcu1v2  =  mu_c * gamma_H * Hcu1v2
    
    dDau1v2  =  mu_a * gamma_H * Hau1v2
    
    dDeu1v2  =  mu_e * gamma_H * Heu1v2
    
    ################################
    ##Vaccinated two dose and exposed once####
    ################################
    
    
    
    dSncu1v2  = omega_nc * Rncu1v2  - Sncu1v2 * sd * foi_cu_1v2
    
    dSnau1v2  = omega_na * Rnau1v2  - Snau1v2 * sd * foi_au_1v2
    
    dSneu1v2  = omega_ne * Rneu1v2 - Sneu1v2 * sd * foi_eu_1v2
    
    #Exposed
    
    dEcu2v2  =  Sncu1v2 * sd * foi_cu_1v2 -  sigma * Ecu2v2   + delta2_cu * Ecu2v1
    
    dEau2v2  =    Snau1v2 * sd * foi_au_1v2 -  sigma * Eau2v2   + delta2_au * Eau2v1
    
    dEeu2v2  =   Sneu1v2 * sd * foi_eu_1v2 -  sigma * Eeu2v2   + delta2_eu * Eeu2v1
    
    # Asymptomatic
    
    dAcu2v2  =  (1 - nu_c) * sigma * Ecu2v2 - gamma_A * Acu2v2 + delta2_cu * Acu2v1
    
    dAau2v2  =  (1 - nu_a) * sigma * Eau2v2 - gamma_A * Aau2v2 + delta2_au * Aau2v1
    
    dAeu2v2  =  (1 - nu_e) * sigma * Eeu2v2 - gamma_A * Aeu2v2  + delta2_eu * Aeu2v1
    
    #Symptomatic
    
    dIcu2v2  =  nu_c * sigma * Ecu2v2 - gamma_I * Icu2v2
    
    dIau2v2  =  nu_a * sigma * Eau2v2 - gamma_I * Iau2v2
    
    dIeu2v2  =  nu_e * sigma * Eeu2v2 - gamma_I * Ieu2v2
    
    # Hospitalized
    
    dHcu2v2  =  phi_cv2 * gamma_I * Icu2v2 - gamma_H * Hcu2v2
    
    dHau2v2  =  phi_av2 * gamma_I * Iau2v2 - gamma_H * Hau2v2
    
    dHeu2v2  =  phi_ev2 * gamma_I * Ieu2v2 - gamma_H * Heu2v2
    
    
    
    
    #Recovered and not seropositive
    
    dRncu2v2  =   (1 - phi_cv2) * gamma_I * Icu2v2 +
      gamma_A * Acu2v2 +  (1 - mu_c) * gamma_H * Hcu2v2  - omega_nc *
      Rncu2v2 + delta2_cu * Rncu2v1 
    
    dRnau2v2  =   (1 - phi_av2) * gamma_I * Iau2v2 +
      gamma_A * Aau2v2 +  (1 - mu_a) * gamma_H * Hau2v2 - omega_na *
      Rnau2v2  + delta2_au * Rnau2v1 
    
    dRneu2v2  =   (1 - phi_ev2) * gamma_I * Ieu2v2 +
      gamma_A * Aeu2v2 +  (1 - mu_e) * gamma_H * Heu2v2  - omega_ne *
      Rneu2v2  + delta2_eu * Rneu2v1 
    
    #Deaths
    
    dDcu2v2  =  mu_c * gamma_H * Hcu2v2
    
    dDau2v2  =  mu_a * gamma_H * Hau2v2
    
    dDeu2v2  =  mu_e * gamma_H * Heu2v2
    
    
    
    
    ##Cumulative infections
    ## No vaccine
    
    dEcum1v0_cu = Scu0v0 * sd * foi_cu_0v0
    
    dEcum1v0_au = Sau0v0 * sd * foi_au_0v0
    
    dEcum1v0_eu = Seu0v0 * sd * foi_eu_0v0
    
    
    dEcum2v0_cu =  Sncu1v0 * sd * foi_cu_1v0
    
    dEcum2v0_au = Snau1v0 * sd * foi_au_1v0 ##
    
    dEcum2v0_eu = Sneu1v0 * sd * foi_eu_1v0
    
    
    ##One dose vaccine
    
    dEcum1v1_cu =  Sncu0v1 * sd * foi_cu_0v1
    
    dEcum1v1_au =  Snau0v1 * sd * foi_au_0v1
    
    dEcum1v1_eu =   Sneu0v1 * sd * foi_eu_0v1
    
    
    dEcum2v1_cu =  Sncu1v1 * sd * foi_cu_1v1
    
    dEcum2v1_au =  Snau1v1 * sd * foi_au_1v1 ##
    
    dEcum2v1_eu = Sneu1v1 * sd * foi_eu_1v1 ##
    
    
    
    
    ##Two dose vaccine
    
    dEcum1v2_cu =  Sncu0v2 * sd * foi_cu_0v2
    
    dEcum1v2_au = Snau0v2 * sd * foi_au_0v2
    
    dEcum1v2_eu =  Sneu0v2 * sd * foi_eu_0v2
    
    ##
    dEcum2v2_cu = Sncu1v2 * sd * foi_cu_1v2 ##
    ##
    dEcum2v2_au =  Snau1v2 * sd * foi_au_1v2 ##
    ##
    dEcum2v2_eu =  Sneu1v2 * sd * foi_cu_1v2 ##
    
    
    
    
    
    ##Cumulative symptomatic cases
    
    dIcum1v0_cu = nu_c * sigma * Ecu1v0
    #
    dIcum1v0_au = nu_a * sigma * Eau1v0 #
    #
    dIcum1v0_eu = nu_e * sigma * Eeu1v0 #
    
    
    dIcum2v0_cu = nu_c * sigma * Ecu2v0
    #
    dIcum2v0_au = nu_a * sigma * Eau2v0 #
    #
    dIcum2v0_eu = nu_e * sigma * Eeu2v0 #
    
    
    
    
    dIcum1v1_cu = nu_c * sigma * Ecu1v1
    #
    dIcum1v1_au = nu_a * sigma * Eau1v1 #
    #
    dIcum1v1_eu = nu_e * sigma * Eeu1v1 #
    
    
    dIcum2v1_cu = nu_c * sigma * Ecu2v1
    #
    dIcum2v1_au = nu_a * sigma * Eau2v1 #
    #
    dIcum2v1_eu = nu_e * sigma * Eeu2v1 #
    
    
    
    dIcum1v2_cu = nu_c * sigma * Ecu1v2
    #
    dIcum1v2_au = nu_a * sigma * Eau1v2 #
    #
    dIcum1v2_eu = nu_e * sigma * Eeu1v2 #
    
    
    dIcum2v2_cu = nu_c * sigma * Ecu2v2
    #
    dIcum2v2_au = nu_a * sigma * Eau2v2 #
    #
    dIcum2v2_eu = nu_e * sigma * Eeu2v2 #
    
    
    
    
    
    
    
    
    
    ##Classes of temporary immunity from those who were vaccinated
    ## First dose
    
    
    ## First dose and seronegative
    
    dVncu0v1 = delta1_cu * Scu0v0  - delta2_cu * Vncu0v1 - omegav_nc *
      Vncu0v1
    
    dVnau0v1 =  delta1_au * Sau0v0 - delta2_au * Vnau0v1 - omegav_na *
      Vnau0v1
    
    dVneu0v1 = delta1_eu * Seu0v0  - delta2_eu * Vneu0v1 - omegav_ne *
      Vneu0v1
    
    ##Second dose
    
    
    ##Seronegative
    
    dVncu0v2 = delta2_cu * Sncu0v1 + delta2_cu * Vncu0v1 + kappa2 * Vpcu0v2  - omegav_nc *
      Vncu0v2
    
    dVnau0v2 = delta2_au * Snau0v1 + delta2_au * Vnau0v1 + kappa2 * Vpau0v2  - omegav_na *
      Vnau0v2
    
    dVneu0v2 = delta2_eu * Sneu0v1 + delta2_eu * Vneu0v1 + kappa2 * Vpeu0v2 - omegav_ne *
      Vneu0v2
    
    
    
    
    
    
    
    
    vswitch = 1 #switch for swtiching off the vaccination
    
    delta1_cu = 0
    
    
    
    
    
    res = c(
      dScu0v0,
      
      dSau0v0,
      
      dSeu0v0,
      
      dEcu1v0,
      
      dEau1v0,
      
      dEeu1v0,
      
      dAcu1v0,
      
      dAau1v0,
      
      dAeu1v0,
      
      dIcu1v0,
      
      dIau1v0,
      
      dIeu1v0,
      
      dHcu1v0,
      
      dHau1v0,
      
      dHeu1v0,
      
      
      dRncu1v0,
      
      dRnau1v0,
      
      dRneu1v0,
      
      dDcu1v0,
      
      dDau1v0,
      
      dDeu1v0,
      
      #Unvax one prior exposure
      
      
      dSncu1v0,
      
      dSnau1v0,
      
      dSneu1v0,
      
      dEcu2v0,
      
      dEau2v0,
      
      dEeu2v0,
      
      dAcu2v0,
      
      dAau2v0,
      
      dAeu2v0,
      
      dIcu2v0,
      
      dIau2v0,
      
      dIeu2v0,
      
      dHcu2v0,
      
      dHau2v0,
      
      dHeu2v0,
      
      
      dRncu2v0,
      
      dRnau2v0,
      
      dRneu2v0,
      
      dDcu2v0,
      
      dDau2v0,
      
      dDeu2v0,
      
      
      
      ##One vax dose no exposure
      
      
      
      dSncu0v1,
      
      dSnau0v1,
      
      dSneu0v1,
      
      dEcu1v1,
      
      dEau1v1,
      
      dEeu1v1,
      
      dAcu1v1,
      
      dAau1v1,
      
      dAeu1v1,
      
      dIcu1v1,
      
      dIau1v1,
      
      dIeu1v1,
      
      dHcu1v1,
      
      dHau1v1,
      
      dHeu1v1,
      
      dRncu1v1,
      
      dRnau1v1,
      
      dRneu1v1,
      
      dDcu1v1,
      
      dDau1v1,
      
      dDeu1v1,
      
      #One vax dose one prior exposure
      
      
      dSncu1v1,
      
      dSnau1v1,
      
      dSneu1v1,
      
      dEcu2v1,
      
      dEau2v1,
      
      dEeu2v1,
      
      dAcu2v1,
      
      dAau2v1,
      
      dAeu2v1,
      
      dIcu2v1,
      
      dIau2v1,
      
      dIeu2v1,
      
      dHcu2v1,
      
      dHau2v1,
      
      dHeu2v1,
      
      
      dRncu2v1,
      
      dRnau2v1,
      
      dRneu2v1,
      
      dDcu2v1,
      
      dDau2v1,
      
      dDeu2v1,
      
      
      
      ##Two vax dose no prior exposure
      
      
      dSncu0v2,
      
      dSnau0v2,
      
      dSneu0v2,
      
      dEcu1v2,
      
      dEau1v2,
      
      dEeu1v2,
      
      dAcu1v2,
      
      dAau1v2,
      
      dAeu1v2,
      
      dIcu1v2,
      
      dIau1v2,
      
      dIeu1v2,
      
      dHcu1v2,
      
      dHau1v2,
      
      dHeu1v2,
      
      
      dRncu1v2,
      
      dRnau1v2,
      
      dRneu1v2,
      
      dDcu1v2,
      
      dDau1v2,
      
      dDeu1v2,
      
      #Two vax dose one prior exposure
      
      
      dSncu1v2,
      
      dSnau1v2,
      
      dSneu1v2,
      
      dEcu2v2,
      
      dEau2v2,
      
      dEeu2v2,
      
      dAcu2v2,
      
      dAau2v2,
      
      dAeu2v2,
      
      dIcu2v2,
      
      dIau2v2,
      
      dIeu2v2,
      
      dHcu2v2,
      
      dHau2v2,
      
      dHeu2v2,
      
      
      dRncu2v2,
      
      dRnau2v2,
      
      dRneu2v2,
      
      dDcu2v2,
      
      dDau2v2,
      
      dDeu2v2,
      
      
      
      
      
      
      
      
      dEcum1v0_cu,
      
      dEcum1v0_au,
      
      dEcum1v0_eu,
      
      dEcum2v0_cu,
      
      dEcum2v0_au,
      
      dEcum2v0_eu,
      
      
      dEcum1v1_cu,
      
      dEcum1v1_au,
      
      dEcum1v1_eu,
      
      dEcum2v1_cu,
      
      dEcum2v1_au,
      
      dEcum2v1_eu,
      
      
      
      dEcum1v2_cu,
      
      dEcum1v2_au,
      
      dEcum1v2_eu,
      
      dEcum2v2_cu,
      
      dEcum2v2_au,
      
      dEcum2v2_eu,
      
      
      
      
      
      dIcum1v0_cu,
      
      dIcum1v0_au,
      
      dIcum1v0_eu,
      
      dIcum2v0_cu,
      
      dIcum2v0_au,
      
      dIcum2v0_eu,
      
      
      dIcum1v1_cu,
      
      dIcum1v1_au,
      
      dIcum1v1_eu,
      
      dIcum2v1_cu,
      
      dIcum2v1_au,
      
      dIcum2v1_eu,
      
      
      
      dIcum1v2_cu,
      
      dIcum1v2_au,
      
      dIcum1v2_eu,
      
      dIcum2v2_cu,
      
      dIcum2v2_au,
      
      dIcum2v2_eu,
      
      
      
      
      
      
      
      dVncu0v1,
      
      dVnau0v1,
      
      dVneu0v1,
      
      dVncu0v2,
      
      dVnau0v2,
      
      dVneu0v2,
      
      
      
    )
    
    time_varying_pars = c(
      'foi_cu_0v0' = foi_cu_0v0,
      'foi_au_0v0' = foi_au_0v0,
      'foi_eu_0v0' = foi_eu_0v0,
      'foi_cu_0v1' = foi_cu_0v1,
      'foi_au_0v1' = foi_au_0v1,
      'foi_eu_0v1' = foi_eu_0v1,
      'foi_cu_0v2' = foi_cu_0v2,
      'foi_au_0v2' = foi_au_0v2,
      'foi_eu_0v2' = foi_eu_0v2,
      
      
      'foi_cu_1v0' = foi_cu_1v0,
      'foi_au_1v0' = foi_au_1v0,
      'foi_eu_1v0' = foi_eu_1v0,
      'foi_cu_1v1' = foi_cu_1v1,
      'foi_au_1v1' = foi_au_1v1,
      'foi_eu_1v1' = foi_eu_1v1,
      'foi_cu_1v2' = foi_cu_1v2,
      'foi_au_1v2' = foi_au_1v2,
      'foi_eu_1v2' = foi_eu_1v2,
      
      
      'foi_cu_2v0' = foi_cu_2v0,
      'foi_au_2v0' = foi_au_2v0,
      'foi_eu_2v0' = foi_eu_2v0,
      'foi_cu_2v1' = foi_cu_2v1,
      'foi_au_2v1' = foi_au_2v1,
      'foi_eu_2v1' = foi_eu_2v1,
      'foi_cu_2v2' = foi_cu_2v2,
      'foi_au_2v2' = foi_au_2v2,
      'foi_eu_2v2' = foi_eu_2v2,
      
      
      'delta1_cu' = delta1_cu,
      
      
      
      
    )
    
    #cat("Time=", t, "foi_eu_1v3=", foi_eu_1v3, "foi_au_1v3=", foi_au_1v3, "foi_cu_2v0=",foi_cu_2v0,"\n")
    list(res, time_varying_pars, sd = sd)
  }) #closing brackets for as.list(parames) loop
}# closing bracket for function 