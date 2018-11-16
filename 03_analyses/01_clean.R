#============================================================================= =
#
#
#
#
#
#============================================================================= =
library(memisc)
library(tidyverse)
library(readxl)
library(janitor)
read_excel_allsheets <- function(filename) {

  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}


# Please note one correction: In the BPD NetmetaXL, reference 2 was entered
# as INDOIV vs Placebo but should have been IBUIV vs placebo. Data use correct
# treatment code.

all_outs <- read_excel_allsheets("./02_data/pda_outcomes.xlsx") %>% 
  map(., ~ clean_names(., "snake") %>% 
        select(study_name, ends_with("1"), ends_with("2")))


all_codes <- read_excel_allsheets("./02_data/pda_trt_codes.xlsx") 
out_names <- names(all_codes)
all_codes <- reduce(all_codes, left_join, by = "Drug") %>% 
  `colnames<-`(c("trt",out_names))

missing_outs <- all_codes %>% select_if(., ~anyNA(.)) %>% colnames()


ranks <- read_excel("./02_data/pda.xlsx", sheet = "rankings")
ranks2 <- read_excel("./02_data/pda.xlsx", sheet = "rankings_td")

