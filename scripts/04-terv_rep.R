library(tidyverse)
sims <- 1000

# Definitions
# 
# Criteria = Outcomes of interest
# Alternatives = treatments
# 


# Criteria ---------------------------------------------------------------------
# These would be estimated from NMA

prefs <- read_csv("./data/terv_prefs.csv") %>% 
  mutate(lower = qbeta(0.025, a,b),
         median = qbeta(0.5, a, b),
         upper = qbeta(0.975, a, b))

# Confidence interval hull------------------------------------------------------

# The Tervonen paper uses the 95% probability Hull to scale criteria

hulls <- prefs %>% group_by(Criterion) %>% summarize(min = round(min(lower), 2),
                                                     max = round(max(upper), 2))

# Crit low and Crit high are made to be the 95% hull of the least and most preferable
# values respectively

table1 <- tibble(Name = c("Efficacy", "Nausea ADRs", "Insomnia ADRs", "Anxiety ADRs"),
       direction = c("higher", rep("lower",3))) %>% 
  left_join(hulls, by = c("Name" = "Criterion")) %>%
  mutate(crit_low = ifelse(direction == "higher", min, max),
         crit_high = ifelse(direction == "higher", max, min)) %>% select(-c(min, max))




#Definitions
#
# m_j = Number of criteria (outcomes)
# alt_j = number of alternatives (treatments) 
#
#------ Ordinal criteria
# If criteria are ordinal, this places them on cardinal (0,1) interval

m_j <- 3
ord_c <- function(m_j){

  q <- runif(m_j - 2, 0, 1) %>% sort(., decreasing = TRUE)
q <- c(1, q, 0)

q
}

#------- Empty weights
# Tervonen 2007 p. 507
empty_w <- function(n_crit){

  q <- runif(n_crit - 1, 0, 1) %>% sort(.)
  q <- c(0, q, 1)
  
  w <- rep(NA, length(q))
  for(i in 2:length(q)){
    w[[i]] <- q[[i]] - q[[i - 1]]
  }
  w <- w[-1]
  
  w
}

#------- Ordinal weights
#
# May be easier to implement using 



ord_w <- function(n_crit){
  w <- empty_w(n_crit)
  
sort(w, decreasing = T)
  
}
ord_w(3)
#------- rand_w

rand_w <- function(n_crit, ord = TRUE){

if(ord == TRUE) out <- ord_w(n_crit)

if(ord == FALSE) out <- empty_w(n_crit)

out
}

# rand_x function goes here
# 
# Creates an m x n random criterion matrix. Each row of the matrix contains
# criteria measurements of a certain alternative.
# 
# This would just be the posterior of each outcome from an NMA done using Bayesian
# methods
n_alt = 3
n_crit = 3
ord = TRUE
sims = 10000

ven <- tibble(Efficacy = rbeta(sims, 52, 46),
              Nausea = rbeta(sims, 41, 61),
              Insomnia = rbeta(sims, 23, 79),
              Anxiety = rbeta(sims, 11, 91))

flu <- tibble(Efficacy = rbeta(sims, 46, 56),
              Nausea = rbeta(sims, 23, 81),
              Insomnia = rbeta(sims, 16, 88),
              Anxiety = rbeta(sims, 8, 96))

plac <- tibble(Efficacy = rbeta(sims, 38, 65),
               Nausea = rbeta(sims, 9, 95),
               Insomnia = rbeta(sims, 15, 89),
               Anxiety = rbeta(sims, 2, 102))

crit_mat <- list(ven = ven, flu = flu, plac = plac)



#------ Rank acceptability and central weight
#
# Definitions
# 
# b_r = Rank acceptability indices
# w_c = Central weight vectors
# 
# h_ij = Number of times alternative i is evaluated into rank j in Monte Carlo 
#        simulations of phase 1.
# 
# k_w  = Number of iterations in phase 1
# k_c = number of iterations in phase 2
# r = vector of ranks of the alternatives
# t = vector of value function values of the alternatives
# 
# 
n_alt <- 3 
m_j <- 3

# initialization of w_c and hit count

w_c <- h_j <-matrix(rep(0, n_alt), rep(0, m_j),
               nrow = n_alt, ncol = m_j)

# Main loop
k_w = 10000

weights <- matrix(data = NA, nrow = k_w, ncol = n_crit)

for(i in 1:k_w){
  weights[i,] <- rand_w(n_crit, ord)
}
alt_mat <- 

for(i in 1:k_w){
  
  w <- weights[i,]
  
  for(j in 1:n_alt){
    
  }

}   




library(smaa)

N <- 1E4; m <- 2; n <- 3
meas <- dget(system.file("extdata/thrombo-meas.txt.gz", package="smaa"))
library(hitandrun)
pref <- simplex.sample(n, N, sort = TRUE)$samples


# Calculate SMAA metrics
result <- smaa(meas, pref)
print(result)
plot(result)
result <- smaa(meas, c(0.5, 0.2, 0.3))
print(result)

smaa.cf(meas, )



