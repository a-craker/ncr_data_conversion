library(readxl)
library(writexl)
library(dplyr)
library(glue)
library(janitor)
library(tidyverse)


mfsa_read_transform <- function(sheet_name) {
  
  df <- read_xlsx("data/mfsa_credit_product_by_income.xlsx", sheet = sheet_name)
  
  df %>% 
    rename(income_category = `Income Category`) %>% 
    pivot_longer(
      -income_category, 
      names_to = "quarter", 
      values_to = "value"
    ) %>% 
    mutate(year = substr(quarter, 1, 4)) %>% 
    select(income_category, year, quarter, value) %>% 
    mutate(
      value_category = sheet_name
    )
  
}

# read in CPI and add to function for transform

mfsa_allproduct_income <- excel_sheets("data/mfsa_credit_product_by_income.xlsx") %>% 
  .[2:length(.)] %>% 
  map_dfr(
    .,
    mfsa_read_transform
  ) %>% left_join(cpi, by = "year") 


write_xlsx(mfsa_allproduct_income, "output/mfsa_allproduct_income.xlsx")
