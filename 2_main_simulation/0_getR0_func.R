library(dplyr)
library(lhs) ##Needs the right RJAGs version

source("~/COVID_serovax/2_main_simulation/mixing_matrix_gmix.r")
##Calculating r0 from next generation matrix
getr0 <- function(bl,rel_c,rel_a,rel_e){
  ## Mixing matrix
  ## The columns are contact age group (infectee) whereas the rows are participant age groups (infector)
  ## Flipped from the visualizations

  # CM_u <- matrix(c(4.369811, 3.579099, 3.581121,
  #                  2.689841, 3.910767, 4.198945,
  #                  0.6558402,1.0232138,1.5153399),
  #                nrow = 3,
  #                dimnames = list(x=c("0-17","18-49","50+"), y=c("0-17","18-49","50+")))

  # CM_r <- matrix(c(5.718741, 3.954180, 4.162091,
  #                  2.971730, 4.720997, 5.519682,
  #                  0.7622381,1.3450557,2.2524737), 
  #                nrow = 3, 
  #                dimnames = list(x=c("0-17","18-49","50+"), y=c("0-17","18-49","50+")))
  
  
  ## Travel probabilities
  #p_ru <- 0.008 # Daily prob of urban contact made by rural people/all contacts made by rural
  #p_ur <- 0.005 # Daily ptob of rural contact made by urban people/all contacts made by urban
  # p_ru <- 0.05 # Prop of urban contact made by rural people/all contacts made by rural
  # p_ur <- 0.01 # Prop of rural contact made by urban people/all contacts made by urban
  #Attempt to create a 6x6 matrix incorporating a dimension of urban/rural
  
  # CM_rr <- (1-p_ru)*CM_r
  # CM_ru <- (p_ru)*CM_r
  # CM_uu <- (1-p_ur)*CM_u
  # CM_ur <- p_ur*CM_u
  
  # CM <- cbind(rbind(CM_rr, CM_ur), rbind(CM_ru,CM_uu))
  colnames(CM) <- c("sus_u_0_19","sus_u_20_64","sus_u_65")
  rownames(CM) <- c("inf_u_0_19","inf_u_20_64","inf_u_65")
  
  ##population
  start.Ns <- c(2808333,6460268,1644275)
  
  pop_dist = data.frame(age_ur = c("cu","au","eu"),
                        pop = start.Ns,
                        sus_pop = c("sus_u_0_19","sus_u_20_64","sus_u_65"))
  
  alpha <- 0.6
  kappa_c<- 0.45         #Probability of symptomatic infection for children
  kappa_a<- 0.55         #Probability of symptomatic infection for adults
  kappa_e<- 0.65         #Probability of symptomatic infection for elderly
  
  prop_inf_c = kappa_c+(1-kappa_c)*alpha
  prop_inf_a = kappa_a + (1-kappa_a)*alpha
  prop_inf_e = kappa_e +(1-kappa_e)*alpha
  
  #rel_c<-1
  #rel_a<-1
  #rel_e<-1
  
  beta_c <- bl*rel_c
  beta_a <- bl*rel_a    
  beta_e <- bl*rel_e    
  ## vector of infectiousness
  v_sus<- data.frame(var = c("sus_u_0_19","sus_u_20_64","sus_u_65"),
                     beta = c(beta_c, beta_a, beta_e))
  v_inf<- data.frame(inf = row.names(CM),
                     prop_infector = c(prop_inf_c,prop_inf_a,prop_inf_e))
  
  ngm <- as.data.frame(CM)%>% mutate(inf=row.names(.))%>%tidyr::pivot_longer(cols=sus_u_0_19:sus_u_65, names_to = "sus") %>%
    left_join(v_sus, by = c("sus"="var")) %>%
    left_join(v_inf, by= c("inf"="inf")) %>%
    left_join(pop_dist %>% select(sus_pop,pop), by = c("sus"="sus_pop"))%>%
    mutate(r0 = value*beta*prop_infector*7)
  
  m_ngm<-ngm %>% select(-value,-beta,-prop_infector,-pop)%>%tidyr::pivot_wider(names_from="sus",values_from="r0")%>%select(-inf)
  
  ##Dominant eigenvalue =1.29
  eigen<-max(eigen(as.matrix(m_ngm))$values)
  print(eigen)
}

getr0(0.00775, 0.35581,0.48272,1) ##R0 = 2.113207

## Loop through a bunch of parameter values and find the corresponding R0 value
## Use Latin hypercube sampling over a uniform distribution over plausible values
#Setting bounds for drawing relative age-specific infectiousness from uniform distribution
set.seed(12345)
total.set.size <- 20000  ##set to large number so we can filter on those that give R0 in a reasonable range
l <- randomLHS(total.set.size, 3)

relbeta_c_parms<- c(0.35,0.39) # relative infectiousness of children compared to older adults
relbeta_a_parms<- c(0.40,0.53) # relative infectiousness of adults compared to older adults
bl_parms <- c(0.005,0.02)     # beta for older adults

relbeta_c <- round((l[,1]*(relbeta_c_parms[2]-relbeta_c_parms[1]))+relbeta_c_parms[1],5)
relbeta_a <- round((l[,2]*(relbeta_a_parms[2]-relbeta_a_parms[1]))+relbeta_a_parms[1],5)
bl        <- round((l[,3]*(bl_parms[2]-bl_parms[1]))+bl_parms[1],5)

sweep <- data.frame(relbeta_c=relbeta_c,relbeta_a=relbeta_a, bl=bl)

##Figure out the estimated r0 for each combination
for(i in 1:nrow(sweep)){
  sweep$r0[i]<-getr0(bl=sweep$bl[i],rel_c = sweep$relbeta_c[i],rel_a = sweep$relbeta_a[i],rel_e=1)
}

