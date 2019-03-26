load("./11_shiny/reg_out_resv2.rda")
load("./11_shiny/ordinal_weightsv2.rda")
load("./11_shiny/p_bar_datv2.rda")
source("./11_shiny/mcda_functions.R")
library(smaa)
library(hitandrun)
library(highcharter)
library(tidyverse)

# Make ranks reactive
ranks <- data.frame(outcome = c("closure", "sx", "mort", "nec"),
                rank = c(4, 3, 1, 2))

ranks2 <- ranks %>% mutate(rank = c(2, 4, 1, 3))

# Make entries reactive
p_s <- rel_to_ab(p_clos = 0.25, 
                 p_sx = 0.12, 
                 p_mort = 0.12, 
                 p_nec = 0.09,
                 d_s = d_s)

# Run mcda
results <- run_smaa(ps_array = p_s,
         ranks = ranks,
          w = w)
N = 30000
np <- simplex.sample(4, N)[1] %>% as.data.frame()

smaa_np <- smaa(p_s, np)

values_np <- smaa.values(p_s, np)
ranks_smaa_np <- smaa.ranks(values_np)
cw_np <- smaa.cw(ranks_smaa_np, np)
cf <- smaa.cf(p_s, cw_np)

cf$cf*100


class(results$smaa$ra) <- "matrix"

out <- results$smaa$ra %>% as.data.frame() %>% rownames_to_column() %>% purrr::set_names(c("var",paste0("r",1:10))) %>% arrange(-r1) %>%
  mutate_at(vars(r1:r10), ~ . * 100)

nice_names <- data.frame(data = c("INDOIV", "IBUIV", "IBUPO", "PARAPO", "IBUIVHIGHDOSE",
                                  "IBUPOHIGHDOSE", "IBUIVCONT", "INDOIVCONT", "INDOTHERS",
                                  "PLAC_NORX"),
                           nice = c("Indomethacin (IV)", "Ibuprofen (IV)", "Ibuprofen (PO)",
                "Paracetamol (PO)", "Ibuprofen (IV, high dose)", "Ibuprofen (PO, high dose)",
                "Ibuprofen (Cont. IV)", "Indomethacin (Cont. IV)", "Indomethacin (Others)",
                "Placebo/No Rx"))
out$var <- nice_names[match(out$var, nice_names$data), 2]

highchart() %>%
  hc_xAxis(categories = out$var, title = list(text = "Treatment")) %>%
  hc_yAxis(max = 100, labels = list(format = '{value} %')) %>%
  hc_add_series(name = "Rank 10", data = out$r10, type = "column")%>%
  hc_add_series(name = "Rank 9", data = out$r9, type = "column")%>%
  hc_add_series(name = "Rank 8", data = out$r8, type = "column")%>%
  hc_add_series(name = "Rank 7", data = out$r7, type = "column")%>%
  hc_add_series(name = "Rank 6", data = out$r6, type = "column")%>%
  hc_add_series(name = "Rank 5", data = out$r5, type = "column")%>%
  hc_add_series(name = "Rank 4", data = out$r4, type = "column")%>%
  hc_add_series(name = "Rank 3", data = out$r3, type = "column")%>%
  hc_add_series(name = "Rank 2", data = out$r2, type = "column")%>%
  hc_add_series(name = "Rank 1", data = out$r1, type = "column")%>%
  hc_tooltip(valueDecimals = 0,
             valueSuffix = "%") %>%
  hc_plotOptions(series=list(stacking='normal')) %>%
  hc_exporting(enabled = TRUE)


p_bar <- map(p_bar_dat, ~t(.) %>% as.data.frame %>% rownames_to_column %>% purrr::set_names(c("trt", "low", "med", "high")))

for(i in seq_along(p_bar)){
  p_bar[[i]] <- mutate_at(p_bar[[i]], vars(low:high), funs(. * 100))
}

p_bar$closure$trt <- nice_names[match(p_bar$closure$trt, nice_names$data), 2]

highchart() %>%
  hc_xAxis(categories = p_bar$closure$trt, title = list(text = "Treatment")) %>%
  hc_yAxis(max = 100, title = list(text = "Probability of event"), labels = list(format = '{value} %')) %>%
  
  hc_add_series(data = p_bar$closure$med, type = "column", name = "PDA Closure") %>%
  hc_add_series(data = list_parse(p_bar$closure[c(2,4)]), 
                type = "errorbar") %>%
  
  hc_add_series(data = p_bar$nec$med, type = "column", name = "NEC") %>%
  hc_add_series(data = list_parse(p_bar$nec[c(2,4)]), 
                type = "errorbar") %>%
  
  hc_add_series(data = p_bar$mort$med, type = "column", name = "Mortality") %>%
  hc_add_series(data = list_parse(p_bar$mort[c(2,4)]), 
                type = "errorbar") %>%
  
  hc_add_series(data = p_bar$sx$med, type = "column", name = "Surgical ligation") %>%
  hc_add_series(data = list_parse(p_bar$sx[c(2,4)]), 
                type = "errorbar") %>%
  hc_tooltip(valueDecimals = 0,
             valueSuffix = "%") %>% 
  hc_exporting(enabled = TRUE)




