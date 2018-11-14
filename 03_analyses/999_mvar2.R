library(tidyverse)
library(readxl)
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

match_codes <- function(data){
  
  data$t_1 <- all_codes[match(data$t_1, all_codes$closure),5][[1]]
  data$t_2 <- all_codes[match(data$t_2, all_codes$closure),5][[1]]
  
  data
}

nec <- te_calc(all_outs$nec)
bpd <- te_calc(all_outs$bpd) %>% match_codes()



bpd <- match_codes(bpd)


# data <- list(
# 
# #NEC
# t = nec %>% select(starts_with("t_")) %>% as.matrix,
# y = cbind(rep(NA,nrow(nec)),select(nec, TE)) %>% as.matrix,
# se = cbind(rep(NA,nrow(nec)),select(nec, seTE)) %>% as.matrix,
# na = rep(2, nrow(nec)),
# ns = nrow(nec),
# nt = 10,
# 
# #BPD
# t2 = bpd %>% select(starts_with("t_")) %>% as.matrix,
# y2 = cbind(rep(NA,nrow(bpd)),select(bpd, TE)) %>% as.matrix,
# se2 = cbind(rep(NA,nrow(bpd)),select(bpd, seTE)) %>% as.matrix,
# na2 = rep(2, nrow(bpd)),
# ns2 = nrow(bpd),
# 
# rho = 0 # Assume rho = 0.5 in base case
# )


data <- list(
  
  #NEC
  t = all_outs$nec %>% select(starts_with("t_")) %>% as.matrix,
  r = select(all_outs$nec, starts_with("r")) %>% as.matrix,
  n = select(all_outs$nec, starts_with("n")) %>% as.matrix,
  na = rep(2, nrow(all_outs$nec)),
  ns = nrow(all_outs$nec),
  nt = 10,
  
  #BPD
  t2 = all_outs$bpd %>% select(starts_with("t_")) %>% as.matrix,
  r2 = select(all_outs$bpd, starts_with("r")) %>% as.matrix,
  n2 = select(all_outs$bpd, starts_with("n")) %>% as.matrix,
  na2 = rep(2, nrow(all_outs$bpd)),
  ns2 = nrow(all_outs$bpd)
  
  #rho = 0 # Assume rho = 0.5 in base case
)


# test_mod <- jags(data = data, parameters.to.save = c("or1", "or2", "d_oa", "sd_oa"), model.file = "./05_models/binom_re_mvar2_contrast.txt",
#      n.iter = 100000, n.burnin = 40000)

test_mod <- jags(data = data, parameters.to.save = c("or1", "or2", "d_oa", "sd_oa"), model.file = "./05_models/binom_re_mvar2.txt",
                 n.iter = 100000, n.burnin = 40000)
windows()
traceplot(test_mod)
all_codes

all_outs$nec

