# Convert the relative output from winBUGS to absolute values using potentially custom probabilities
rel_to_ab <- function(p_clos = 0.25, 
                      p_rpt_rx = 0.18, 
                      p_sx = 0.12, 
                      p_mort = 0.12, 
                      p_nec = 0.09, 
                      p_bpd = 0.38, 
                      p_ivh = 0.19, 
                      p_oligo = 0.19,
                      d_s = d_s, p_bar = FALSE){
  
  p_samp <- function(prob, n, iter = 30000){
    a <- floor(prob*n)
    b <- n - a
    qlogis(rbeta(iter, a, b)) # logit of probility
  }
  # Versus placebo (all vs indoIV)
  
  # All vs indoIV
  base <- NULL
  base[["closure"]] <- p_samp(p_clos, 1125)
  base[["rpt_rx"]] <- p_samp(p_rpt_rx, 601) 
  base[["sx"]] <- p_samp(p_sx, 767)
  base[["mort"]] <- p_samp(p_mort, 904)
  base[["nec"]] <- p_samp(p_nec, 931)
  base[["bpd"]] <- p_samp(p_bpd, 810)
  base[["ivh"]] <- p_samp(p_ivh, 285)
  base[["oligo"]] <- p_samp(p_oligo, 734)
  
  
  p_s <- rep(list(NA), length(d_s))
  for(i in seq_along(d_s)){
    p_s[[i]] <- d_s[[i]] %>% mutate_all(funs(plogis(base[[names(d_s)[[i]]]] + .)))
    
    names(p_s)[[i]] <- names(d_s)[[i]]
  }
  
  if(p_bar == TRUE){
    
    out <- map(p_s, ~ as.data.frame(.) %>% sapply(., quantile, probs = c(0.025, 0.5, 0.975)) %>%
                     t(.) %>% as.data.frame %>% rownames_to_column %>% purrr::set_names(c("trt", "low", "med", "high")) %>%
                     mutate_at(., vars(low:high), funs(. * 100)))
    
  }
  
  
  if(p_bar == FALSE){
  p_s[-1] <- map(p_s[-1],~ 1 - .) # Choices are based on sum of partial values * weight so higher is better
  #smaa requires performance across outcomes as an array
  out <- array(unlist(p_s), dim = c(nrow(p_s[[1]]), ncol(p_s[[1]]), length(p_s)),
                    dimnames = list(NULL, names(p_s[[1]]), names(p_s)))
  
  }
  
  out
}

# Create smaa outputs

run_smaa <- function(ps_array = ps_array,
                     ranks = ranks,
                     w = w){
  ps_array_pref <- reorder(ps_array, dim = 3, names = ranks %>% arrange(rank) %>% .$outcome %>% as.character()) # Outcome dimension needs to be in same order as weights
 

  pda_smaa <- smaa(ps_array_pref, w)
  
  values <- smaa.values(ps_array_pref, w)
  
  ranks_smaa <- smaa.ranks(values)
  
  
  cw <- smaa.cw(ranks_smaa, w)
  
  cf <- smaa.cf(ps_array_pref, cw)
  
  out <- list(smaa = pda_smaa, values = values, ranks = ranks_smaa, central_weight = cw, confidence = cf)
  
  out
}
