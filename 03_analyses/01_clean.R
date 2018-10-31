#============================================================================= =
#
#
#
#
#
#============================================================================= =

library(tidyverse)
library(readxl)
library(janitor)
read_excel_allsheets <- function(filename) {

  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}

all_outs <- read_excel_allsheets("./02_data/pda_outcomes.xlsx") %>% 
  map(., ~ clean_names(., "snake") %>% 
        select(study_name, ends_with("1"), ends_with("2")))


all_codes <- read_excel_allsheets("./02_data/pda_trt_codes.xlsx") 
out_names <- names(all_codes)
all_codes <- reduce(all_codes, left_join, by = "Drug") %>% 
  `colnames<-`(c("trt",out_names))

missing_outs <- all_codes %>% select_if(., ~anyNA(.)) %>% colnames()

for(i in seq_along(missing_outs)){

all_outs[[missing_outs[[i]]]]$t_1 <- all_codes[match(all_outs[[missing_outs[[i]]]]$t_1, all_codes[[missing_outs[[i]]]]),2] %>% .$closure

all_outs[[missing_outs[[i]]]]$t_2 <- all_codes[match(all_outs[[missing_outs[[i]]]]$t_2, all_codes[[missing_outs[[i]]]]),2] %>% .$closure

}

# Simple contigency correction adding 0.5 to studies with all zero event cells.
# Using this because original analysis was in NetmetaXL and the Sweeney correction
# was used to make a connected network.


for(i in seq_along(all_outs)){

all_outs[[i]] = all_outs[[i]] %>% mutate(r_1 = ifelse(r_1 == 0 & r_2 == 0, r_1 + 0.5, r_1),
                                         r_2 = ifelse(r_1 == 0.5 & r_2 == 0, r_2 + 0.5, r_2),
                                         n_1 = ifelse(r_1 == 0.5 & r_2 == 0.5, n_1 + 0.5, n_1),
                                         n_2 = ifelse(r_1 == 0.5 & r_2 == 0.5, n_2 + 0.5, n_2))

}
