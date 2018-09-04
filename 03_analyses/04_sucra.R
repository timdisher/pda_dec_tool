# Sucra ------------------------------------------------------------------------
#Missing outcome data
# 1 = Replace with overall mean
# 2 = Replace with group specific mean
# 3 = Drop the treatment

missing <- 2


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
