#User input
library(readxl)
source("./03_analyses/01_clean.R")
source("./04_functions/nma_cont_gemtc.R")
load("reg_out_res.rda")
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


# nma_binom = function(data, params = binom_params_re,
#                   model = mf_binom_re_con,
#                   tables = c("or","SUCRA"),
#                   est = c("or","SUCRA"),
#                   treats = all_codes_temp){
# 
#   #Output list for WinBUGS
#   wb_list = wb_cont_list(data = data, cont = FALSE)
# 
#   #Run Model
#   model = jags.parallel(wb_list,NULL,params,model.file = model,
#                         n.chains = 3, n.iter = 20000, n.burnin = 10000, n.thin = 1)
# 
#   con_tabs = jags_out(model_summary = model$BUGSoutput$summary,treatments = treats,
#                       tables = tables,est = est)
# 
#   out = list(wb_data = data,trts = treats, cons = model,cons_tables = con_tabs)
# 
#   out
# }
# 
# 
# #Binomial models====================================
# #write.model(binom_re, "./05_models/binom_re.txt")
# mf_binom_re_con = c("./05_models/binom_re.txt")
# binom_params_re = c("d","or","SUCRA","rk", "sd")
# 
# 
# nma_list <- rep(list(NA), length(all_outs))
# for(i in seq_along(all_outs)){
# 
# all_codes_temp <- all_codes[,c(1, i+1)] %>% `colnames<-`(c("trt", "t"))
# nma_list[[i]] <- nma_binom(data = all_outs[[i]] %>% mutate(na = 2))
# 
# names(nma_list)[[i]] <- names(all_outs)[[i]]
# 
# }
# 
#   #----------------------------------------------------------------------------- -
# #----------------------------------------------------------------------------- -
# # Assumptions re: missing treatments
# #  Base case: retains relative efficacy from closure.Rationale is that closure
# #  was originally seen as a surrogate for everything else.
# 
# 
# d_s <- rep(list(NA), 8)
# for(i in seq_along(d_s)){
# 
# temp <- nma_list[[i]]$cons$BUGSoutput$sims.matrix %>% as.data.frame() %>% select(starts_with("d["), starts_with("rk"))
# 
# trts <- all_codes %>% select(trt, names(nma_list)[[i]]) %>% drop_na() %>% .$trt
# 
# colnames(temp) <- c(trts, paste0(trts, "_rk"))
# 
# # if(names(nma_list)[[i]] != "oligo"){
# # temp <- temp %>% mutate_all(funs(. - PLAC_NORX)) # Get everything vs placebo
# # }
# 
# d_s[[i]] <- temp
# names(d_s)[[i]] <- names(nma_list)[[i]]
# }
# 
# #Bring rank from Closure over to outcomes where missing. 
# 
# d_s$rpt_rx <- d_s$rpt_rx %>% mutate(IBUIVCONT_rk = ifelse(11 - d_s$closure$INDOIVCONT_rk == 10, 9, 11 - d_s$closure$INDOIVCONT_rk))
# d_s$bpd <- d_s$bpd %>% mutate(IBUPOHIGHDOSE_rk = ifelse(11 - d_s$closure$IBUPOHIGHDOSE_rk > 8, 8, 11 - d_s$closure$IBUPOHIGHDOSE_rk),
#                                  INDOIVCONT_rk = ifelse(11 - d_s$closure$INDOIVCONT_rk > 8, 8, 11 - d_s$closure$INDOIVCONT_rk))
# d_s$oligo <- d_s$oligo %>% mutate(PLAC_NORX_rk = ifelse(11 - d_s$closure$PLAC_NORX_rk > 9, 9, 11 - d_s$closure$PLAC_NORX_rk))
# 
# # For each treatment missing from each outcome, select the nth ranked lor that matches their rank in closure
# for(i in 1:nrow(d_s$rpt_rx)){
# d_s$rpt_rx$IBUIVCONT[[i]] <- sort(as.numeric(d_s$rpt_rx[i,1:9]))[[d_s$rpt_rx$IBUIVCONT_rk[[i]]]]
# 
# d_s$bpd$IBUPOHIGHDOSE[[i]] <- sort(as.numeric(d_s$bpd[i,1:8]))[[d_s$bpd$IBUPOHIGHDOSE_rk[[i]]]]
# d_s$bpd$INDOIVCONT[[i]] <- sort(as.numeric(d_s$bpd[i,1:8]))[[d_s$bpd$INDOIVCONT_rk[[i]]]]
# 
# d_s$oligo$PLAC_NORX[[i]] <- sort(as.numeric(d_s$oligo[i,1:9]))[[d_s$oligo$PLAC_NORX_rk[[i]]]]
# }
# 
# d_s <- d_s %>% map(., ~select(., -ends_with("_rk")))
# save(d_s, file = "reg_out_res.rda")

#----------------------------------------------------------------------------- -
#----------------------------------------------------------------------------- -
# Baseline rates
# 
# These are just crudely as sums from placebo. Really should either be the random
# effect meta-analysis on proportions or something similar
#----------------------------------------------------------------------------- -
#----------------------------------------------------------------------------- -


p_samp <- function(event, n, iter = 30000){
  a <- event
  b <- n - event
  qlogis(rbeta(iter, a, b)) # logit of probility
}
# Placebo (for all but oliguria)
base <- NULL
base[["closure"]] <- p_samp(227, 597)
base[["rpt_rx"]] <- p_samp(69, 121) 
base[["sx"]] <- p_samp(60, 321)
base[["mort"]] <- p_samp(79, 454)
base[["nec"]] <- p_samp(24, 367)
base[["bpd"]] <- p_samp(147, 333)
base[["ivh"]] <- p_samp(70, 211)
base[["oligo"]] <- p_samp(143, 734)

p_s <- rep(list(NA), length(d_s))
for(i in seq_along(d_s)){
p_s[[i]] <- d_s[[i]] %>% select(-ends_with("_rk")) %>% mutate_all(funs(plogis(base[[names(d_s)[[i]]]] + .))) %>%
  select(names(d_s[[1]] %>% select(-ends_with("_rk"))))

names(p_s)[[i]] <- names(d_s)[[i]]
}

p_s[-1] <- map(p_s[-1],~ 1 - .) # Choices are based on sum of partial values * weight so higher is better
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
N <- 30000
w <- har(seedPoint, constr, N=N * (n-1)^3, thin=(n-1)^3,
         homogeneous=TRUE, transform=transform)$samples

# save(w, file = "ordinal_weights.rda")

ps_array_sm <- reorder(ps_array, dim = 3, names = c(ranks %>% arrange(rank) %>% .$outcome)) # Outcome dimension needs to be in same order as weights
ps_array_mcy <- reorder(ps_array, dim = 3, names = c(ranks2 %>% arrange(rank) %>% .$outcome))




pda_smaa <- smaa(ps_array_sm, w)
pda_smaa_mcy <- smaa(ps_array_mcy, w)

values <- smaa.values(ps_array, w)
values_mcy <- smaa.values(ps_array_mcy, w)

ranks_smaa <- smaa.ranks(values)
ranks_smaa_mcy <- smaa.ranks(values_mcy)


cw <- smaa.cw(ranks_smaa, w)
cw_mcy <- smaa.cw(ranks_smaa_mcy, w)

cf <- smaa.cf(ps_array, cw)
cf_mcy <- smaa.cf(ps_array_mcy, cw_mcy)

## No preferences

np <- simplex.sample(8, N)[1] %>% as.data.frame()

smaa_np <- smaa(ps_array, np)

values_np <- smaa.values(ps_array, np)
ranks_smaa_np <- smaa.ranks(values_np)
cw_np <- smaa.cw(ranks_smaa_np, np)
cf <- smaa.cf(ps_array, cw_np)


