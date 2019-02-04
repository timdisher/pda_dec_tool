#User input
library(readxl)
source("./03_analyses/01_clean.R")
source("./04_functions/nma_cont_gemtc.R")
load("./multi_out_res.rda")
library(smaa)
library(hitandrun)


#----------------------------------------------------------------------------- -
#----------------------------------------------------------------------------- -
# SMAA in R
#----------------------------------------------------------------------------- -
#----------------------------------------------------------------------------- -

#================================= ==
#Functions for this project====
#================================= ==


# te_calc <- function(data){
#   temp <- pairwise(treat = list(t_2, t_1),
#                    event = list(r_2, r_1),
#                    n = list(n_2, n_1),
#                    studlab = study_name,
#                    data = data, sm = "OR") %>% filter(!is.na(TE))
# 
#   print(netconnection(treat1, treat2, studlab, data = temp))
# 
#   temp}
# 
# match_codes <- function(data, out){
# 
#   data$t_1 <- all_codes[match(data$t_1, all_codes[[out]]),2][[1]]
#   data$t_2 <- all_codes[match(data$t_2, all_codes[[out]]),2][[1]]
# 
#   data
# }
# 
# 
# #Get right treatment codes across outcomes
# all_outs$bpd <- match_codes(all_outs$bpd, "bpd")
# all_outs$rpt_rx <- match_codes(all_outs$rpt_rx, "rpt_rx")
# all_outs$oligo <- match_codes(all_outs$oligo, "oligo")
# all_outs$closure <- all_outs$closure %>% mutate(r_1 = n_1 - r_1,
#                                                 r_2 = n_2 - r_2)
# 
# 
# 
# data <- list(
#   alpha = 1-0.5^2, #Assume correlation between outcomes is 0.5
#   #Closure
#   t = all_outs$closure %>% dplyr::select(starts_with("t_")) %>% as.matrix,
#   r = dplyr::select(all_outs$closure, starts_with("r")) %>% as.matrix,
#   n = dplyr::select(all_outs$closure, starts_with("n")) %>% as.matrix,
#   na = rep(2, nrow(all_outs$closure)),
#   ns = nrow(all_outs$closure),
#   nt = 10,
#   zero = matrix(rep(rep(0, nrow(all_outs$closure)),2), ncol = 2),
# 
#   #Repeat Rx
#   t2 = all_outs$rpt_rx %>% dplyr::select(starts_with("t_")) %>% as.matrix,
#   r2 = dplyr::select(all_outs$rpt_rx, starts_with("r")) %>% as.matrix,
#   n2 = dplyr::select(all_outs$rpt_rx, starts_with("n")) %>% as.matrix,
#   na2 = rep(2, nrow(all_outs$rpt_rx)),
#   ns2 = nrow(all_outs$rpt_rx),
#   zero2 = matrix(rep(rep(0, nrow(all_outs$rpt_rx)),2), ncol = 2),
# 
#   #Sx
#   t3 = all_outs$sx %>% dplyr::select(starts_with("t_")) %>% as.matrix,
#   r3 = dplyr::select(all_outs$sx, starts_with("r")) %>% as.matrix,
#   n3 = dplyr::select(all_outs$sx, starts_with("n")) %>% as.matrix,
#   na3 = rep(2, nrow(all_outs$sx)),
#   ns3 = nrow(all_outs$sx),
#   zero3 = matrix(rep(rep(0, nrow(all_outs$sx)),2), ncol = 2),
# 
#   #Mortality
#   t4 = all_outs$mort %>% dplyr::select(starts_with("t_")) %>% as.matrix,
#   r4 = dplyr::select(all_outs$mort, starts_with("r")) %>% as.matrix,
#   n4 = dplyr::select(all_outs$mort, starts_with("n")) %>% as.matrix,
#   na4 = rep(2, nrow(all_outs$mort)),
#   ns4 = nrow(all_outs$mort),
#   zero4 = matrix(rep(rep(0, nrow(all_outs$mort)),2), ncol = 2),
# 
#   #NEC
#   t5 = all_outs$nec %>% dplyr::select(starts_with("t_")) %>% as.matrix,
#   r5 = dplyr::select(all_outs$nec, starts_with("r")) %>% as.matrix,
#   n5 = dplyr::select(all_outs$nec, starts_with("n")) %>% as.matrix,
#   na5 = rep(2, nrow(all_outs$nec)),
#   ns5 = nrow(all_outs$nec),
#   zero5 = matrix(rep(rep(0, nrow(all_outs$nec)),2), ncol = 2),
# 
#   #BPD
#   t6 = all_outs$bpd %>% dplyr::select(starts_with("t_")) %>% as.matrix,
#   r6 = dplyr::select(all_outs$bpd, starts_with("r")) %>% as.matrix,
#   n6 = dplyr::select(all_outs$bpd, starts_with("n")) %>% as.matrix,
#   na6 = rep(2, nrow(all_outs$bpd)),
#   ns6 = nrow(all_outs$bpd),
#   zero6 = matrix(rep(rep(0, nrow(all_outs$bpd)),2), ncol = 2),
# 
#   #IVH
#   t7 = all_outs$ivh %>% dplyr::select(starts_with("t_")) %>% as.matrix,
#   r7 = dplyr::select(all_outs$ivh, starts_with("r")) %>% as.matrix,
#   n7 = dplyr::select(all_outs$ivh, starts_with("n")) %>% as.matrix,
#   na7 = rep(2, nrow(all_outs$ivh)),
#   ns7 = nrow(all_outs$ivh),
#   zero7 = matrix(rep(rep(0, nrow(all_outs$ivh)),2), ncol = 2),
# 
#   #Oligo
#   t8 = all_outs$oligo %>% dplyr::select(starts_with("t_")) %>% as.matrix,
#   r8 = dplyr::select(all_outs$oligo, starts_with("r")) %>% as.matrix,
#   n8 = dplyr::select(all_outs$oligo, starts_with("n")) %>% as.matrix,
#   na8 = rep(2, nrow(all_outs$oligo)),
#   ns8 = nrow(all_outs$oligo),
#   zero8 = matrix(rep(rep(0, nrow(all_outs$oligo)),2), ncol = 2)




#)
# 
# 
# multi_out <- jags.parallel(data = data, parameters.to.save = c("or1","or2","or3","or4","or5", "or6","or7","or8"), model.file = "./05_models/binom_re_mvar2power_prior.txt",
#                              n.iter = 300000, n.burnin = 30000, n.thin = 100, n.chains = 3)
#save(multi_out, file = "multi_out_res.rda")

multi_out$BUGSoutput$sims.matrix
#----------------------------------------------------------------------------- -
#----------------------------------------------------------------------------- -
# Baseline rates
# 
# These are just crudely as sums from placebo. Really should either be the random
# effect meta-analysis on proportions or something similar
#----------------------------------------------------------------------------- -
#----------------------------------------------------------------------------- -

p_samp <- function(event, n, iter = 8100){
  a <- event
  b <- n - event
  qlogis(rbeta(iter, a, b)) # logit of probility
}


# All vs indoIV
base <- NULL
base[["closure"]] <- p_samp(276, 1125)
base[["rpt_rx"]] <- p_samp(108, 601) 
base[["sx"]] <- p_samp(92, 767)
base[["mort"]] <- p_samp(111, 904)
base[["nec"]] <- p_samp(86, 931)
base[["bpd"]] <- p_samp(307, 810)
base[["ivh"]] <- p_samp(53, 285)
base[["oligo"]] <- p_samp(143, 734)

d_s <- multi_out$BUGSoutput$sims.matrix %>% as.data.frame %>% select(-deviance, -'or1[1,1]') %>% modify(., log)

p_s<- rep(list(NA),  8)

for(i in seq_along(p_s)){

temp<- d_s %>% select(starts_with(paste0("or", i))) %>% mutate_all(funs(plogis(base[[i]] + .)))

p_s[[i]] <- cbind(plogis(base[[i]]), temp) %>% purrr::set_names(all_codes$trt)

names(p_s)[[i]] <- names(base[i])
}



p_s <- map(p_s,~ 1 - .) # Choices are based on sum of partial values * weight so higher is better


#smaa requires performance across outcomes as an array
ps_array <- array(unlist(p_s), dim = c(nrow(p_s[[1]]), ncol(p_s[[1]]), length(p_s)),
      dimnames = list(NULL, names(p_s[[1]]), names(p_s)))



n <- 8 # length of weight vector
constr <- mergeConstraints(
  list(ordinalConstraint(n,1,2),
       ordinalConstraint(n,2,3),
       ordinalConstraint(n,3,4),
       ordinalConstraint(n,4,5),
       ordinalConstraint(n,5,6),
       ordinalConstraint(n,6,7),
       ordinalConstraint(n,7,8)
  )
)

transform <- simplex.createTransform(n)

constr <- simplex.createConstraints(transform, constr)

seedPoint <- createSeedPoint(constr, homogeneous=TRUE)

N <- 8100
w <- har(seedPoint, constr, N=N * (n-1)^3, thin=(n-1)^3,
         homogeneous=TRUE, transform=transform)$samples


ps_array_sm <- reorder(ps_array, dim = 3, names = c(ranks %>% arrange(rank) %>% .$outcome)) # Outcome dimension needs to be in same order as weights
ps_array_mcy <- reorder(ps_array, dim = 3, names = c(ranks2 %>% arrange(rank) %>% .$outcome))




pda_smaa_sm <- smaa(ps_array_sm, w)
pda_smaa_mcy <- smaa(ps_array_mcy, w)

values <- smaa.values(ps_array, w)
values_td <- smaa.values(ps_array_td, w)

ranks_smaa <- smaa.ranks(values)
ranks_smaa_td <- smaa.ranks(values_td)


cw <- smaa.cw(ranks_smaa, w)
cw_td <- smaa.cw(ranks_smaa_td, w)

cf <- smaa.cf(ps_array, cw)
cf_td <- smaa.cf(ps_array_td, cw_td)

## No preferences

np <- simplex.sample(8, N)[1] %>% as.data.frame()

smaa_np <- smaa(ps_array, np)

values_np <- smaa.values(ps_array, np)
ranks_smaa_np <- smaa.ranks(values_np)
cw_np <- smaa.cw(ranks_smaa_np, np)
cf <- smaa.cf(ps_array, cw_np)


