library(tidyverse)
library(readxl)
library(here)
library(R2WinBUGS)

m2_2 <- read_excel(here("./02_data/sutton_mvnma.xlsx"), sheet = 1, na = "NA")
m2_3 <- read_excel(here("./02_data/sutton_mvnma.xlsx"), sheet = 2, na = "NA")

data2 <- list(

#Data 1  
  N1=45,              #no of datapoints
  N2=66,             # no of studies x no of outcomes (22x3=66) 
  ns=22,              # no of studies
  no=3,               #no of outcomes
  nt.total =c(7,7,7),                                      # no of interventions for outcomes 1, 2 and 3
  mn.rhoW =c(0.184,-0.052,0.051),      #mean of within-study correlations from IPD
  se.rhoW = c(0.118,0.064,0.059,1),    #se of within-study correlations from IPD
  na = c(2,2,2,2,2,  2,2,2,2,3,  2,2,2,2,2,  2,2,2,2,2,   2,2), # no. of arms in each study
  #R = structure(.Data = c(1,0,0,  0,1,0, 0,0,1),.Dim = c(3,3)) # needed for model 2a

#Data 2

studyid = m2_2$`studyid[]`,
study = m2_2$`study[]`,
arm = m2_2$`arm[]`,
y = m2_2 %>% select(starts_with("y")) %>% as.matrix,
se = m2_2 %>% select(starts_with("se")) %>% as.matrix,

#Data 3

studyid1 = m2_3$`studyid1[]`,
s = m2_3$`s[]`,
t = m2_3 %>% select(starts_with("t")) %>% as.matrix,
o = m2_3$`o[]`,
out = m2_3$`out[]`,
na2 = m2_3$`na[]`
)


sutton2 <- bugs(model.file = here("./05_models/sutton_m2.txt"), inits = NULL,parameters.to.save = "or", data = data2,
     bugs.directory = "C:/Users/TheTimbot/Desktop/WinBUGS14", DIC = FALSE, n.iter = 400000, n.burnin = 200000, n.thin = 1)


