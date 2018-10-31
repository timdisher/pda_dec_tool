#User input
library(tidyverse)
library(readxl)
or_raw <- read_excel("./02_data/pda.xlsx", na = "NA", sheet = 1)
ranks <- read_excel("./02_data/pda.xlsx", sheet = 3)

#----------------------------------------------------------------------------- -
#----------------------------------------------------------------------------- -
# Missing Data
#----------------------------------------------------------------------------- -
#----------------------------------------------------------------------------- -
                                                             
# lor---------------------------------------------------------------------------                                                                  

# For missing outcomes (2 on BPD and 1 on repeat Rx) we assume the missing 
# value is equal to the median treatment
#    - Known drawbacks: Should be MI or multivariate but that's too much work
# For outcomes where published paper had OR of 0, we take log of 0.000001
# 
# Oliguria odds ratios are too crazy to bother including
pda_or = or_raw %>% group_by(outcome) %>% 
  
  #Impute missing
  mutate_at(vars(median:upper), 
            funs(ifelse(is.na(.), 
                        median(., na.rm = TRUE), .))) %>% 
  ungroup() %>% 
  
  #Convert ORs to log odds
  #Assume zeroes actually had estimates that were just small
  mutate_at(vars(median:upper),
            funs(ifelse(. == 0, log(0.000001), log(.)))) %>%
  mutate(se = (upper - lower) / 3.92)  %>%
  
  #Drop oligo
  filter(outcome != "oliguria")
  



#----------------------------------------------------------------------------- -
#----------------------------------------------------------------------------- -
# Baseline rates
#----------------------------------------------------------------------------- -
#----------------------------------------------------------------------------- -

p_samp <- function(event, n, iter = 10000){
  a <- event
  b <- n - event
  rbeta(iter, a, b)
}
# Placebo (for all but oliguria)
base <- NULL
base[["closure"]] <- p_samp(227, 597)
base[["rpt"]] <- p_samp(69, 121) 
base[["lig"]] <- p_samp(60, 321)
base[["mort"]] <- p_samp(79, 454)
base[["nec"]] <- p_samp(24, 367)
base[["bpd"]] <- p_samp(147, 333)
base[["ivh"]] <- p_samp(70, 211)


#----------------------------------------------------------------------------- -
#----------------------------------------------------------------------------- -
# Log odds
#----------------------------------------------------------------------------- -
#----------------------------------------------------------------------------- -
lor_samp <- function(iter = 10000,data = pda_or, out_name){
df <- tibble(closure = rep(NA, iter),
             rpt = NA,
             lig = NA,
             mort = NA,
             nec = NA,
             bpd = NA,
             ivh = NA)
temp <- data %>% select(basic_param, outcome, median, se) %>%
  filter(basic_param == out_name)

for(i in seq_along(df)){
  point <- temp[i,3][[1]]
  se <- temp[i,4][[1]]
  var <- se^2
  
  # Parameterize to make sure you get correct mean OR as per Briggs Decision
  # Modeling PSA chapter p.107
  df[[i]] <- rnorm(iter, point - var/2, se)
  
  
}
df
}

trts <- unique(pda_or$basic_param)
ors <- NULL

for(i in seq_along(trts)){
  ors[[trts[[i]]]] <- lor_samp(out_name = trts[[i]])
}

#Confirm everything worked, take the exp of every row and then the mean, compare
#against data sheet
map(ors, exp) %>% map(., colMeans)


#----------------------------------------------------------------------------- -
#----------------------------------------------------------------------------- -
# Baseline to absolute rate conversion
#----------------------------------------------------------------------------- -
#----------------------------------------------------------------------------- -

#Bind simulated base rates together
#
p_base <- do.call(cbind, base)
#Convert to log odds

log_p_base <- log(p_base/(1-p_base))

#For each outcome, convert each row to a sampled probability
abs <- ors
trts <- unique(pda_or[[1]])


#Calculate absolute rates

temp_or <- abs[["IBUPOHIGHDOSE"]]

t <- as.tibble(log(p_base/(1-p_base)))

tempest <- t$mort + temp_or$mort
plot(t$mort)
plot(temp_or$mort)
z <- t$mort + temp_or$mort

quantile(exp(temp_or$mort), probs = c(0.025, 0.5, 0.975))
tibble(mort = temp_or$mort,
           base = t$mort,
           tot = mort + base)
mean(1/(1+exp(-z)))
#Inverse logit
inv_logit <- function(x, y = log_p_base){
  z <- x + y
  1/(1+exp(-z))
}


#For each outcome in each treatment, combine each estimated OR with each
#baseline rate and then run them through inverse logit to get absolute rate
#for that treatment/outcome combo

quantile(exp(temp_or$mort), probs = c(0.025, 0.5, 0.975))
abs_trt <- map(abs, inv_logit)

# Make sure overall averages make sense

map(abs_trt, ~summarise_all(. , mean))
colMeans(p_base)


z <- log_p_base + temp_or
temp <- 1/(1+exp(-z))
colMeans(temp)
#----------------------------------------------------------------------------- -
#----------------------------------------------------------------------------- -
# Express as beta(a, b) for use in JSMAA
#----------------------------------------------------------------------------- -
#----------------------------------------------------------------------------- -   



#----------------------------------------------------------------------------- -
#----------------------------------------------------------------------------- -
# SMAA in R
#----------------------------------------------------------------------------- -
#----------------------------------------------------------------------------- -                                                        