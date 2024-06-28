library(tidyverse)
vars_d<-stringr::str_split_1("dScu0v0,

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

dIcum2v2_eu"
, pattern=",\n\n"


)
vars <- gsub("d","",vars_d)
start <- readRDS("~/COVID_serovax/2_main_simulation/9_last_Rrand.RDS")
start <- subset(start, select=vars)
#read in averaged case data 

avg_cases <- readxl::read_xlsx("Georgia Average Daily COVID-19 Cases December 2020.xlsx", sheet = "Age Stratified")
#Correct date type
avg_cases$Date <- as.Date(avg_cases$Date)
#GA Age distribution
start_Ns <- c(2808333,6460268,1644275)
#Start from first day of vaccinations in GA
vax_start_date <- "2020-12-13"
avg_cases_vax <- subset(avg_cases,Date==vax_start_date) |> select(!Date)
#Initialize compartments
colnames (avg_cases_vax) <- colnames(start_two |> select(starts_with("I") & contains("u1v0")))
#Infectious and Asymptomatic
start_two$Icu1v0 <- avg_cases_vax$Icu1v0
start_two$Iau1v0 <- avg_cases_vax$Iau1v0
start_two$Ieu1v0 <- avg_cases_vax$Ieu1v0
under_report <- 3
start_two$Acu1v0 <- start_two$Icu1v0 * under_report
start_two$Aau1v0 <- start_two$Iau1v0 * under_report
start_two$Aeu1v0 <- start_two$Ieu1v0 * under_report
#Exposed, Dead, Hospitalized, and cumulative infections set to zero
start_two <- start_two %>% mutate(.,across((starts_with("E")|starts_with("D")|starts_with("H")|starts_with("Icum")),function(x){x <-0}))
#Multiple vaccinations and infections set to zero
start_two <- start_two %>% mutate(.,across((contains("v1")|contains("v2")|contains("2v")),function(x){x <-0}))
#initial immunity from serosurvey data
sero_init_R <- c(0.1,0.2,0.08)
start_two[,c("Rncu1v0","Rnau1v0","Rneu1v0")] <- start_Ns * sero_init_R
# Susceptible
start_two <- start_two %>% mutate(.,across((starts_with("Sn")&contains("1v")),function(x){x <-0}))
start_two[,c("Scu0v0","Sau0v0","Seu0v0")] <- start_two %>% 
  
saveRDS(start,file="~/COVID_serovax/2_main_simulation/start_two.RDS")
