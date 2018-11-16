source("./03_analyses/01_clean.R")
source("./04_functions/nma_cont_gemtc.R")
library(beepr)



te_calc <- function(data){
temp <- pairwise(treat = list(t_2, t_1),
              event = list(r_2, r_1),
              n = list(n_2, n_1),
              studlab = study_name,
              data = data, sm = "OR") %>% filter(!is.na(TE))

print(netconnection(treat1, treat2, studlab, data = temp))

temp}

match_codes <- function(data, out){
  
  data$t_1 <- all_codes[match(data$t_1, all_codes[[out]]),2][[1]]
  data$t_2 <- all_codes[match(data$t_2, all_codes[[out]]),2][[1]]
  
  data
}


#Get right treatment codes across outcomes
all_outs$bpd <- match_codes(all_outs$bpd, "bpd")
all_outs$rpt_rx <- match_codes(all_outs$rpt_rx, "rpt_rx")
all_outs$oligo <- match_codes(all_outs$oligo, "oligo")
all_outs$closure <- all_outs$closure %>% mutate(r_1 = n_1 - r_1,
                                                r_2 = n_2 - r_2)



data <- list(
  alpha = 1-0.5^2, #Assume correlation between outcomes is 0.5
  #Closure
  t = all_outs$closure %>% dplyr::select(starts_with("t_")) %>% as.matrix,
  r = dplyr::select(all_outs$closure, starts_with("r")) %>% as.matrix,
  n = dplyr::select(all_outs$closure, starts_with("n")) %>% as.matrix,
  na = rep(2, nrow(all_outs$closure)),
  ns = nrow(all_outs$closure),
  nt = 10,
  zero = matrix(rep(rep(0, nrow(all_outs$closure)),2), ncol = 2),
  
  #Repeat Rx
  t2 = all_outs$rpt_rx %>% dplyr::select(starts_with("t_")) %>% as.matrix,
  r2 = dplyr::select(all_outs$rpt_rx, starts_with("r")) %>% as.matrix,
  n2 = dplyr::select(all_outs$rpt_rx, starts_with("n")) %>% as.matrix,
  na2 = rep(2, nrow(all_outs$rpt_rx)),
  ns2 = nrow(all_outs$rpt_rx),
  zero2 = matrix(rep(rep(0, nrow(all_outs$rpt_rx)),2), ncol = 2),
  
  #Sx
  t3 = all_outs$sx %>% dplyr::select(starts_with("t_")) %>% as.matrix,
  r3 = dplyr::select(all_outs$sx, starts_with("r")) %>% as.matrix,
  n3 = dplyr::select(all_outs$sx, starts_with("n")) %>% as.matrix,
  na3 = rep(2, nrow(all_outs$sx)),
  ns3 = nrow(all_outs$sx),
  zero3 = matrix(rep(rep(0, nrow(all_outs$sx)),2), ncol = 2),
  
  #Mortality
  t4 = all_outs$mort %>% dplyr::select(starts_with("t_")) %>% as.matrix,
  r4 = dplyr::select(all_outs$mort, starts_with("r")) %>% as.matrix,
  n4 = dplyr::select(all_outs$mort, starts_with("n")) %>% as.matrix,
  na4 = rep(2, nrow(all_outs$mort)),
  ns4 = nrow(all_outs$mort),
  zero4 = matrix(rep(rep(0, nrow(all_outs$mort)),2), ncol = 2),
  
  #NEC
  t5 = all_outs$nec %>% dplyr::select(starts_with("t_")) %>% as.matrix,
  r5 = dplyr::select(all_outs$nec, starts_with("r")) %>% as.matrix,
  n5 = dplyr::select(all_outs$nec, starts_with("n")) %>% as.matrix,
  na5 = rep(2, nrow(all_outs$nec)),
  ns5 = nrow(all_outs$nec),
  zero5 = matrix(rep(rep(0, nrow(all_outs$nec)),2), ncol = 2),
  
  #BPD
  t6 = all_outs$bpd %>% dplyr::select(starts_with("t_")) %>% as.matrix,
  r6 = dplyr::select(all_outs$bpd, starts_with("r")) %>% as.matrix,
  n6 = dplyr::select(all_outs$bpd, starts_with("n")) %>% as.matrix,
  na6 = rep(2, nrow(all_outs$bpd)),
  ns6 = nrow(all_outs$bpd),
  zero6 = matrix(rep(rep(0, nrow(all_outs$bpd)),2), ncol = 2),

  #IVH
  t7 = all_outs$ivh %>% dplyr::select(starts_with("t_")) %>% as.matrix,
  r7 = dplyr::select(all_outs$ivh, starts_with("r")) %>% as.matrix,
  n7 = dplyr::select(all_outs$ivh, starts_with("n")) %>% as.matrix,
  na7 = rep(2, nrow(all_outs$ivh)),
  ns7 = nrow(all_outs$ivh),
  zero7 = matrix(rep(rep(0, nrow(all_outs$ivh)),2), ncol = 2),
  
  #Oligo
  t8 = all_outs$oligo %>% dplyr::select(starts_with("t_")) %>% as.matrix,
  r8 = dplyr::select(all_outs$oligo, starts_with("r")) %>% as.matrix,
  n8 = dplyr::select(all_outs$oligo, starts_with("n")) %>% as.matrix,
  na8 = rep(2, nrow(all_outs$oligo)),
  ns8 = nrow(all_outs$oligo),
  zero8 = matrix(rep(rep(0, nrow(all_outs$oligo)),2), ncol = 2)

  
  

)


test_mod_pp <- jags.parallel(data = data, parameters.to.save = c("or6", "or5", "sd_oa"), model.file = "./05_models/binom_re_mvar2power_prior.txt",
                 n.iter = 50000, n.burnin = 20000)
beep()


