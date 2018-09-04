#User input
library(tidyverse)
source("./functions/functions.R")
df_raw <- read_csv("./data/sucras.csv")


#Processing
if(any(is.na(df_raw))) {
  
print(heatplot(df_raw))

  warning("Some outcomes are NAs, please use heatplot to decide
       how you would like to handle them. Please note that none of the
          available options are optimal. If possible, consider fitting
          a multivariate model.")
  
}


if(any(is.na(df_raw))) {missing <- menu(c("Use overall mean", 
                                      "Use treatment specific mean",
                                      "Drop treatments with missing values"))}


if(missing == 0) missing <- 1


df_raw <- df_raw %>% gather("outcome", "sucra", -TREATMENT)

df <- switch(missing,
       "1"  = df_raw %>% mutate(sucra = ifelse(is.na(sucra), mean(sucra, na.rm = TRUE), sucra)),
       
       "2" = {df_raw %>% group_by(TREATMENT) %>%
         mutate(sucra = ifelse(is.na(sucra), mean(sucra, na.rm = TRUE), sucra)) %>%
           ungroup()},
       
       "3" = {df_raw %>% spread("outcome", "sucra") %>% drop_na() %>% 
         gather("outcome", "sucra", -TREATMENT)}
       )



weights <- df %>% distinct(outcome) %>% mutate(weights = rep(0.5, length(outcome)))


weights <- weights %>% mutate(normalized = weights/sum(weights))

df <- left_join(df, weights, by = c("outcome")) %>% 
  mutate(utility = sucra * normalized) %>%
  group_by(TREATMENT) %>% summarise(utility = sum(utility)) %>% arrange(-utility)

                                                             
                                                                     
                                                                    