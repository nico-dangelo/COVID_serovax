library(dplyr)
library(deSolve)
library(ggplot2)
source("~/COVID_serovax/2_main_simulation/model_code_int_two.R")
sweep<- readRDS("~/COVID_serovax/2_main_simulation/1_sweep_int.RDS")
spec_humid <- read.csv("~/COVID_serovax/2_main_simulation/9_spec_humid.csv")[,c("day","avg_sm2")]
# start <- readRDS("~/COVID_serovax/2_main_simulation/9_last_Rrand.RDS")
start <- readRDS("~/COVID_serovax/2_main_simulation/start_two.RDS")
trigger<-F
sero_e1 <-rep(0,5)
yroot1 <- rep(0,5)
yroot2 <- rep(0,5)



########################## 
##Model initiation stuff##
##########################
model_sims <- function(i){
  
  
  print(paste(Sys.time(),"iteration start", i, sep=" "))
  ##Future pandemic
  inc_trans<-sweep$inc_trans[i]
  r00<- sweep$r00[i]*inc_trans^0
  r01<- sweep$r01[i]*inc_trans^0
  r02<- sweep$r02[i]*inc_trans^1
  r03<- sweep$r03[i]*inc_trans^2
  r04<- sweep$r04[i]*inc_trans^3
  r05<- sweep$r05[i]*inc_trans^4
  r06<- sweep$r06[i]*inc_trans^5
  r07<- sweep$r07[i]*inc_trans^6
  r08<- sweep$r08[i]*inc_trans^6
  r09<- sweep$r09[i]*inc_trans^6
  
  fac1 <- sweep$fac1[i]
  fac2 <- sweep$fac2[i]
  
  total_time=3650           
  t = seq(0,total_time,1)
  
  tt=3650
  tt2= seq(0,tt,1)
  signal <- data.frame(t = tt2, 
                       yr = floor(tt2/365),
                       day = c(rep(c(seq(from=0, to=364, by=1)),times=10),364),
                       fac = rep(0, length(tt2)),
                       #r0_hyp = rep(0,length(times)),
                       r0t = rep(0, length(tt2)))
  
  signal<- merge(signal, spec_humid, by.x="day", by.y="day")
  
  r0hyp_list <- data.frame(yr = seq(from=0, to=10, by=1),
                           r0_hyp = c(r00,r01,r02,r03,r04,r05,r06,r07,r08,r09,r09))

  
  signal<- merge(signal,r0hyp_list, by.x= "yr", by.y = "yr")
  signal <- signal[order(signal$t),]
  
  r0min<-sweep$r0_base[i]
  #signal$r0t= exp(-227.5*signal$avg_sm2 + log(signal$r0_hyp-r0min))+r0min
  #signal$r0t= exp(-227.5*signal$avg_sm2 + log((signal$r0_hyp)+4-r0min))+r0min
  signal$r0t= exp(fac1*signal$avg_sm2 + log(signal$r0_hyp*fac2-r0min*0.85))+r0min
  signal$import <- signal$r0t/sweep$r0[i]
  
  signal <-signal[,c("t","import")]
  
  #signal%>%
  #  ggplot(aes(x=t, y=r0t))+
  #  geom_line(aes(x=t,y=r0t))
  
  input <<- approxfun(signal, rule = 2)
  
  trigger <<-F

  ##Matrix
  #CM <<- mixing_matrix[[1]]
  
  ####Scenario sweeps####
  sweep <- sweep
  num_sweep <- nrow(sweep)
  trigger<-F
  ##########################
  ##Model initiation stuff##
  ##########################
  ##Initial state values##
  
  #start = unlist(start_all[i,])
  start = unlist(start)

  ##Transmission parameters
  ## Beta is the probability of transmission when contacted with susceptible of clases c, a, e
  bl <- sweep$bl[i]
  rel_c <- sweep$relbeta_c[i]
  rel_a <- sweep$relbeta_a[i]
  
  beta_c <- bl*rel_c
  beta_a <- bl*rel_a    ##Guesses
  beta_e <- bl*1    ##Guesses
  r0 <- sweep$r0[i]
  r0_hyp <- sweep$r0_hyp[i]
 # inc_trans<-sweep$inc_trans[i]
  
  ##alpha is relative infectiousness of asymptomatic
  alpha <- 0.6
  
  
  # Relative infectiousness of omicron
  #rel_delta <- sweep$rel_delta[i]    ## Delta increase
  #rel_omi <-sweep$rel_omi[i] ##Omicron increase
  #sd1<-sweep$sd1[i]    ##Omicron decrease step 1
  #sd2 <- sweep$sd2[i]  ## Omicron decrease step 2
  #rel_newvar <- sweep$rel_newvar[i]
  
  #mu_c <-0
  #mu_a <-0
  #mu_e <-0
  ## Vaccine parameters
  epsilon_1<-0.50 ## VE against infection
  zeta_1<-0.40 ## VE against hospitalization
  epsilon_2<-0.60 ## VE against infection
  zeta_2<-0.67 ## VE against hospitalization
  
  # vei3<-0.7 ## VE against infection
  # vep3<-0.9 ## VE against hospitalization
  

  
  
  
  ##Natural history parameters
  theta <- 1/5.5  #latent period
  
  gamma_I <-1/7      #duration of infection for symptomatically infectious
  gamma_A <-1/7      #duration of infection for asymptomatically infectious
  gamma_H <-1/5      #hospital length of stay
  
  kappa_c<- 0.45         #Probability of symptomatic infection for children
  kappa_a<- 0.55         #Probability of symptomatic infection for adults
  kappa_e<- 0.65         #Probability of symptomatic infection for elderly
  
  phi_c<- 0.004     #Prob of hospitalization (children) among unvaxed
  phi_a<- 0.03       #Prob of hospitalization (adults) among unvaxed
  phi_e<- 0.2       #Prob of hospitalization (elderly) among unvaxed
  
  phi_cv1 <- phi_c*(1-zeta_1)
  phi_av1 <- phi_a*(1-zeta_1)
  phi_ev1 <- phi_e*(1-zeta_1)
  
  phi_cv2 <- phi_c*(1-zeta_2)
  phi_av2 <- phi_a*(1-zeta_2)
  phi_ev2 <- phi_e*(1-zeta_2)
  
  # phi_cv3 <- phi_c*(1-vep3)
  # phi_av3 <- phi_a*(1-vep3)
  # phi_ev3 <- phi_e*(1-vep3)
  
  
  
  
  mu_c<-0.005            #Prob of death (children)
  mu_a<-0.0365       #Prob of death (adults)
  mu_e<-0.15        #Prob of death (elderly)
  
  ##Protection post-infection
  #red_inf_1 <- 0.35  ## FOI among susceptible individuals after period of immunity is 35%%
  #red_inf_2 <- 0.15
  #red_inf_1<-0.5
  #red_inf_2<-0.4
  red_inf_1 <- sweep$red_inf_1[i]
  red_inf_2 <- red_inf_1*sweep$rel_red_inf_2[i]
  #red_inf_1 <- 0.45
  #red_inf_2 <- red_inf_1*0.7
 
  
  ## Waning immunity after infection
  ##Among seropositives
  omega_pc <- sweep$omega_pc[i]
  # omega_pa <- omega_pc
  # omega_pe <- omega_pc
  
  ##Among seronegatives
  omega_nc <- omega_pc
  omega_na <- omega_nc
  omega_ne <- omega_nc
  
  ##Waning immunity after vaccination
  omegav_pc <- omega_pc
  omegav_pa <- omegav_pc
  omegav_pe <- omegav_pc

  omegav_nc <- omega_pc
  omegav_na <- omegav_nc
  omegav_ne <- omegav_nc



  ## After third infection (two prior exposure class), wane from R3 to S2
  # omega2_pc <-sweep$omega2_pc[i]
  # omega2_pa <-omega2_pc
  # omega2_pe <-omega2_pc
  # 
  # omega2_nc <-omega2_pc
  # omega2_na <-omega2_nc
  # omega2_ne <-omega2_nc
  
  ##After third vaccination, wane from V3 to V2
  # omega3_pc <-sweep$omega2_pc[i]   ##Keep cyclical waning from V3 to V2 the same as cyclical waning from immune third infection to susc
  # omega3_pa <-omega3_pc
  # omega3_pe <-omega3_pc
  # 
  # omega3_nc <-omega3_pc    ##Keep waning of seronegative the same as seropositive
  # omega3_na <-omega3_nc
  # omega3_ne <-omega3_nc
  
  # ## Additional waning from S3 -> S2
  # omega4_pc <- 1/365
  # omega4_pa <- omega4_pc
  # omega4_pe <- omega4_pc
  # 
  # omega4_nc <- omega4_pc
  # omega4_na <- omega4_pc
  # omega4_ne <- omega4_pc
  # 
  
  # ## immune escape factor
  # imm_esc_factor_t1 <- sweep$imm_esc_factor_t1[i]
  # imm_esc_factor_omi<- sweep$imm_esc_factor_omi[i]
  # add_imm <- sweep$add_imm[i]
  #imm_esc_factor_newvar <- sweep$imm_esc_factor_newvar[i]
  
  ##Serology
  # Probability of seroconversion after infection
  # pi <- 0.9
  
  # Prob of seroconversion after vaccination
  # rho_v1 <- 0.85
  # rho_v2 <- 0.6 ## Among those seronegative after first dose
  # rho_v3 <-0.9  ##Among those seronegative after second dose, or waned

  # 
  # sero_thresh <- sweep$sero_thresh[i]
  # 
  
  ## Vax interval
  vax_int <- sweep$vax_int[i]
  vax_start <- sweep$vax_start[i]
  vax_end <- sweep$vax_end[i]
  vax_first <- sweep$vax_first[i]
  
  
  beta1<-sweep$beta1[i]
  w <- sweep$w[i]

  params<-c('beta_c'= beta_c, 'beta_a' = beta_a, 'beta_e' = beta_e,
            'alpha' = alpha, 
            'epsilon_1'=epsilon_1, 'epsilon_2'=epsilon_2,  'zeta_1'=zeta1, 'zeta_2'=zeta_2, 
            'theta' = theta,
            'gamma_I'=gamma_I, 'gamma_A'=gamma_A, 'gamma_H'=gamma_H,
            'kappa_c'=kappa_c, 'kappa_a'=kappa_a, 'kappa_e'=kappa_e,
            'phi_c'=phi_c, 'phi_a'=phi_a, 'phi_e'=phi_e,
            'phi_cv1'=phi_cv1, 'phi_av1'=phi_av1, 'phi_ev1'=phi_ev1,
            'phi_cv2'=phi_cv2, 'phi_av2'=phi_av2, 'phi_ev2'=phi_ev2,
            
            'mu_c'=mu_c, 'mu_a'=mu_a, 'mu_e'=mu_e,'red_inf_1' = red_inf_1, 'red_inf_2'=red_inf_2,
            
            'omega_nc'=omega_nc, 'omega_na'=omega_na, 'omega_ne'=omega_ne,
           
            'omegav_nc'=omegav_nc, 'omegav_na'=omegav_na, 'omegav_ne' =omegav_ne,
            
            
            
            
            'r0'=r0, 'r0_hyp'=r0_hyp, 'inc_trans'=inc_trans,
            
            'r00'=r00, 'r01'=r01, 'r02'=r02, 'r03'=r03, 'r04'=r04, 'r05'=r05, 'r06'=r06, 'r07'=r07,'r08'=r08, 'r09'=r09,
            
             'beta1'=beta1, 'w'=w,
            
            
            'vax_int'=vax_int, 'vax_start'=vax_start, 'vax_end'=vax_end, 'vax_first'=vax_first
            #'sd1'=sd1,'sd2'=sd2,'rel_newvar'=rel_newvar,
  )
  
  
  model_out <- as.data.frame(ode(y = start, times = t, fun = COVID_sero_vax, parms = params))
  mod_foi <- model_out %>% select(time| contains("foi"))
  
  
  mod_inc <- model_out %>% select(time| contains("Ecum")|contains("Icum")) %>% mutate(
   
    Ecum_cu = rowSums(select(.,contains("Ecum")&(contains("_cu")))),
   
    Ecum_au = rowSums(select(.,contains("Ecum")&(contains("_au")))),
   
    Ecum_eu = rowSums(select(.,contains("Ecum")&(contains("_eu")))),
    
    Ecum_c = Ecum_cu,
    Ecum_a = Ecum_au,
    Ecum_e = Ecum_eu,
    
    Icum_c = rowSums(select(.,contains("Icum")&(contains("_cu")))),
    Icum_a = rowSums(select(.,contains("Icum")&(contains("_au")))),
    Icum_e = rowSums(select(.,contains("Icum")&(contains("_eu")))),
    
    new_E_c = Ecum_c - lag(Ecum_c),          
    new_E_a = Ecum_a - lag(Ecum_a),
    new_E_e = Ecum_e - lag(Ecum_e),
    
    new_I_c = Icum_c - lag(Icum_c),
    new_I_a = Icum_a - lag(Icum_a),
    new_I_e = Icum_e - lag(Icum_e),  
    
    Ecum = Ecum_c+Ecum_a+Ecum_e,
    Icum = Icum_c+Icum_a+Icum_e,
    new_E = new_E_c+new_E_a+new_E_e,
    new_I = new_I_c+new_I_a+new_I_e
  ) 
    
    #mutate(date =  seq(from = as.Date("2020-05-06"), to=as.Date(dt_end), by =1)) %>%
    #select(time, Ecum_cr:date)
  
  model_out <- model_out %>% select(-contains("foi")) %>%
    select(-(contains("Ecum")|contains("Icum")))
  
  ## Population totals and deaths
  pop_num <- model_out %>% mutate(
    
       ## Total in the population (exclude deaths)
    Nchildu = rowSums(select(.,contains('cu')&(-starts_with("D")))),
    
    Nadultu = rowSums(select(.,contains('au')&(-starts_with("D")))),
   
    Noldu =   rowSums(select(.,contains('eu')&(-starts_with("D")))),
    NTot =  Nchildu  +Nadultu +Noldu,           ## Total alive in compartments
    
    
    Deaths_c = rowSums(select(.,contains('Dc'))),
    Deaths_a = rowSums(select(.,contains('Da'))),
    Deaths_e = rowSums(select(.,contains('De'))),
    
    Deaths_tot = Deaths_c+Deaths_a+Deaths_e,
    
    new_Deaths_c = Deaths_c -lag(Deaths_c),
    new_Deaths_a = Deaths_a - lag(Deaths_a),
    new_Deaths_e = Deaths_e - lag(Deaths_e),
    
    new_Deaths_tot = Deaths_tot - lag(Deaths_tot)) %>%
    
    select(time, Nchildu:new_Deaths_tot)
  
  
  # seroprev<- model_out %>% left_join(pop_num%>%select(time,Nchildr:NTot), by= "time")%>%
  #   
  #   mutate(
  #   
  #   
  #   SpRp_cu = rowSums(select(., contains('cu')& (contains('Sp')|contains('Rp')|contains('Vp')))),
  #   
  #   SpRp_au = rowSums(select(., contains('au')& (contains('Sp')|contains('Rp')|contains('Vp')))),
  #   
  #   SpRp_eu = rowSums(select(., contains('eu')& (contains('Sp')|contains('Rp')|contains('Vp')))),
  #   
  #   
  #  
  #   active_cu = rowSums(select(.,(contains('Ecu')|contains('Acu')|contains('Icu')|contains('Hcu')), -ends_with('1v0'))),
  #  
  #   active_au = rowSums(select(.,(contains('Eau')|contains('Aau')|contains('Iau')|contains('Hau')), -ends_with('1v0'))),
  #   
  #   active_eu = rowSums(select(.,(contains('Eeu')|contains('Aeu')|contains('Ieu')|contains('Heu')), -ends_with('1v0'))),
  #   
  #   seropos_cr = SpRp_cr + active_cr,
  #   seropos_cu = SpRp_cu + active_cu,
  #   seropos_ar = SpRp_ar + active_ar,
  #   seropos_au = SpRp_au + active_au,
  #   seropos_er = SpRp_er + active_er,
  #   seropos_eu = SpRp_eu + active_eu,
  #   
  #   seroprev_cr = seropos_cr/Nchildr,
  #   seroprev_cu = seropos_cu/Nchildu,
  #   seroprev_ar = seropos_ar/Nadultr,
  #   seroprev_au = seropos_au/Nadultu,
  #   seroprev_er = seropos_er/Noldr,
  #   seroprev_eu = seropos_eu/Noldu,
  #   
  #   seroprev_c = (seropos_cr+seropos_cu)/(Nchildr+Nchildu),
  #   seroprev_a = (seropos_ar+seropos_au)/(Nadultr+Nadultu),
  #   seroprev_e = (seropos_er+seropos_eu)/(Noldr+Noldu),
  #   
  #   seroprev_ul = (seropos_cu+seropos_au+seropos_eu)/(Nchildu+Nadultu+Noldu),
  #   seroprev_rl = (seropos_cr+seropos_ar+seropos_er)/(Nchildr+Nadultr+Noldr),
  #   
  #   seroprev_to = (seropos_cr+seropos_cu+seropos_ar+seropos_au+seropos_er+seropos_eu)/(NTot))%>%
  #   select(time,seroprev_cr:seroprev_to)
  # 
  # 
  vax_dist <- model_out%>% mutate(
    V0cum_c = rowSums(select(.,contains("v0")&contains("cu")&(-starts_with("D")))),
    V1cum_c = rowSums(select(.,contains("v1")&contains("cu")&(-starts_with("D")))),
    V2cum_c = rowSums(select(.,contains("v2")&contains("cu")&(-starts_with("D")))),
    
    
    V0cum_a = rowSums(select(.,contains("v0")&contains("au")&(-starts_with("D")))),
    V1cum_a = rowSums(select(.,contains("v1")&contains("au")&(-starts_with("D")))),
    V2cum_a = rowSums(select(.,contains("v2")&contains("au")&(-starts_with("D")))),
    
    
    V0cum_e = rowSums(select(.,contains("v0")&contains("eu")&(-starts_with("D")))),
    V1cum_e = rowSums(select(.,contains("v1")&contains("eu")&(-starts_with("D")))),
    V2cum_e = rowSums(select(.,contains("v2")&contains("eu")&(-starts_with("D")))),
    )%>%
    
    select(time, V0cum_c:V2cum_e)
  
  exp_dist <- model_out %>%mutate(
        e0cum_c = rowSums(select(.,contains("0v")&contains("cu")&(-starts_with("D")))),
        e1cum_c = rowSums(select(.,contains("1v")&contains("cu")&(-starts_with("D")))),
        e2cum_c = rowSums(select(.,contains("2v")&contains("cu")&(-starts_with("D")))),
        
        e0cum_a = rowSums(select(.,contains("0v")&contains("au")&(-starts_with("D")))),
        e1cum_a = rowSums(select(.,contains("1v")&contains("au")&(-starts_with("D")))),
        e2cum_a = rowSums(select(.,contains("2v")&contains("au")&(-starts_with("D")))),
        
        e0cum_e = rowSums(select(.,contains("0v")&contains("eu")&(-starts_with("D")))),
        e1cum_e = rowSums(select(.,contains("1v")&contains("eu")&(-starts_with("D")))),
        e2cum_e = rowSums(select(.,contains("2v")&contains("eu")&(-starts_with("D"))))) %>%
      select(time,  e0cum_c:e2cum_e)
  
  imm_class <- model_out %>%
    mutate(
      s0v0_c = rowSums(select(., contains('Scu'))),
      s0v0_a = rowSums(select(., contains('Sau'))),
      s0v0_e = rowSums(select(., contains('Seu'))),
      
      s1v0_c = rowSums(select(.,  Sncu1v0)),
      s1v0_a = rowSums(select(.,  Snau1v0)),
      s1v0_e = rowSums(select(.,  Sneu1v0)),
      
      
      
      s0v1_c = rowSums(select(., Sncu0v1)),
      s0v1_a = rowSums(select(., Snau0v1)),
      s0v1_e = rowSums(select(., Sneu0v1)),
      
      s1v1_c = rowSums(select(., Sncu1v1)),
      s1v1_a = rowSums(select(., Snau1v1)),
      s1v1_e = rowSums(select(., Sneu1v1)),
      
      
      
      s0v2_c = rowSums(select(., Sncu0v2)),
      s0v2_a = rowSums(select(., Snau0v2)),
      s0v2_e = rowSums(select(., Sneu0v2)),
      
      s1v2_c = rowSums(select(., Sncu1v2)),
      s1v2_a = rowSums(select(., Snau1v2)),
      s1v2_e = rowSums(select(., Sneu1v2)),
      
      
     
      
      sus_c = rowSums(select(., ((contains('cu'))&(contains('Sn')))|contains('Scu'))),
      sus_a = rowSums(select(., ((contains('au'))&(contains('Sn')))|contains('Sau'))),
      sus_e = rowSums(select(., ((contains('eu'))&(contains('Sn')))|contains('Seu'))),
      
      rec_c = rowSums(select(., (contains('cu'))&(contains('Rn')))),
      rec_a = rowSums(select(., (contains('au'))&(contains('Rn')))),
      rec_e = rowSums(select(., (contains('eu'))&(contains('Rn')))),
      
      vac_c = rowSums(select(., (contains('cu'))&(contains('Vn')))), 
      vac_a = rowSums(select(., (contains('au'))&(contains('Vn')))), 
      vac_e = rowSums(select(., (contains('eu'))&(contains('Vn'))))) %>% 
    
    select(time, s0v0_c:vac_e)
  
    vax_elig <- model_out %>%
    ##Calculate number of individuals eligible for vaccination
    mutate(
           vax_elig_eu = rowSums(select(.,contains("eu")&(-starts_with("D")&-starts_with("I")&-starts_with("H")))),
           vax_elig_au = rowSums(select(.,contains("au")&(-starts_with("D")&-starts_with("I")&-starts_with("H")))),
           
           vax_elig_e = vax_elig_eu,
           vax_elig_a =  vax_elig_au)%>%
    select(time, vax_elig_eu :vax_elig_a) %>%
    
    left_join(model_out %>% select(time, delta1_cu))
    
    sd<-model_out$sd
  
  model_out$sweepnum <- i
  # model_summary$sweepnum <- i
  print(paste(i, "iteration complete"))
  
  res_out <- list(pop_num = pop_num, start = start, params = params,
              model_out=model_out,
              mod_inc=mod_inc, 
              mod_foi=mod_foi, imm_class=imm_class,
              #seroprev=seroprev, 
              vax_elig=vax_elig,
              sd=sd)
  #saveRDS(res_out,paste("sw_run_",i,".RDS",sep=""))
  
  num <- if(nchar(i)==1){paste("000",i,sep="")} else if(nchar(i)==2){paste("00",i,sep="")}else if(nchar(i)==3){paste("0",i,sep="")}else{paste(i)}
  # saveRDS(res_out,paste("/projects/blopman/vger/cliu/0_interpol_wanehi_int_firstvar/sw_run_",num,".RDS",sep=""))
  return(res_out)

}

