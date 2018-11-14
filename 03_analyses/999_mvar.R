library(tidyverse)
library(readxl)
source("./03_analyses/01_clean.R")
source("./04_functions/nma_cont_gemtc.R")
library(beepr)
all_outs$closure
# Trial of 2014 Efthimiou code to use MVNMA to impute missing BPD
out1 <- all_outs$closure %>% filter(r_1 != n_1) %>% mutate(n_1 = ifelse(r_1 == n_1, n_1 + 1, n_1),
                                    r_1 = ifelse(r_1 == 0, r_1 + 0.5, r_1),
                                    y_2 = qlogis(r_2/n_2) - qlogis(r_1/n_1),
                                    var = 1/r_1 + 1/(n_1 - r_1) + 1/r_2 + 1/(n_2 - r_2),
                                    se = sqrt(var),
                                    outcome = "close",
                                    study_name = case_when(
                                      study_name == "INDIVOTHERS" ~ "69",
                                      study_name == "PLACOTHERS" ~ "70",
                                      TRUE ~ study_name
                                    ))

out1$t_1 <- all_codes[match(out1$t_1, all_codes$closure), 1][[1]]
out1$t_2 <- all_codes[match(out1$t_2, all_codes$closure), 1][[1]]


out2 <- all_outs$bpd %>% filter(r_1 != 0) %>% mutate(n_1 = ifelse(r_1 == 0, n_1 + 1, n_1),
                                r_1 = ifelse(r_1 == 0, r_1 + 0.5, r_1),
                                y_2 = qlogis(r_2/n_2) - qlogis(r_1/n_1),
                                var = 1/r_1 + 1/(n_1 - r_1) + 1/r_2 + 1/(n_2 - r_2),
                                study_name = as.character(study_name),
                                study_name = case_when(
                                  study_name == "INDIVOTHERS" ~ "69",
                                  study_name == "PLACOTHERS" ~ "70",
                                  TRUE ~ study_name
                                ),
                                outcome = "bpd")


out2$t_1 <- all_codes[match(out2$t_1, all_codes$bpd), 1][[1]]
out2$t_2 <- all_codes[match(out2$t_2, all_codes$bpd), 1][[1]]


comb <- full_join(out1, out2) %>% arrange(as.numeric(study_name))

treats <- comb %>% distinct(study_name, t_1, t_2)
#Prepare data for model. Odds are closure, evens are BPD.
comb2 <- data.frame(study_name = rep(unique(comb$study_name),2),
           outcome = c(rep("close", length(unique(comb$study_name))),
                       rep("bpd", length(unique(comb$study_name))))) %>%
  left_join(treats) %>% left_join(comb) %>% arrange(as.numeric(study_name, outcome)) %>% select(study_name,
                                                                 t_1, t_2,
                                                                 y_2, var, outcome) %>%
  mutate(var = ifelse(is.na(var), 10000, var))

comb2$T1 <- all_codes[match(comb2$t_1, all_codes$trt),2][[1]]
comb2$T2 <- all_codes[match(comb2$t_2, all_codes$trt),2][[1]]


data_list <- list(
  y = as.numeric(comb2$y_2),
  varr = comb2 %>% filter(outcome == "close") %>% .$var,
  vard = comb2 %>% filter(outcome == "bpd") %>% .$var,
  T1 = as.numeric(comb2$T1),
  T2 = as.numeric(comb2$T2),
  Ns = comb2 %>% distinct(study_name) %>% nrow,
  N2h = comb2 %>% distinct(study_name) %>% nrow,
  ref = 1,
  NT = 10
)



params <- c("EffR", "EffD", "psiR.sq", "psiD.sq")
model <- "./05_models/mvar_2out_re.txt"
fit1 <- BRugsFit(here(model), data_list,
                 parametersToSave = params, nIter = 40000, nBurnin = 40000, coda = TRUE)
plot(fit1$Stats)

fit1
library(here)
all_codes

library(netmeta)


temp_test <- pairwise(list(t_1, t_2),
         n = list(n_1, n_2),
         event = list(r_1, r_2),
  data = out2, studlab = study_name, sm = "OR")


test <- netmeta(TE, seTE, t_1, t_2, study_name, data = temp_test, sm = "OR")

forest(test, ref = "PLAC_NORX", backtransf = TRUE)

