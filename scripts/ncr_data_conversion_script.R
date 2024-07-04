library(readxl)
library(writexl)
library(dplyr)
library(glue)
library(janitor)
library(tidyverse)


# FUNCTION SPEC ---------------------------------------------------------------

#> "mfsa_read_transform" reads in each sheet of the data (different credit product types), combines the products and 
#> transforms the data to a long format


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


# READ DATA ---------------------------------------------------------------

#> The following reads the data, as well as joins in the CPI sheet that is necessary for inflation adjusting

mfsa_allproduct_income <- excel_sheets("data/mfsa_credit_product_by_income.xlsx") %>% 
  .[2:length(.)] %>% 
  map_dfr(
    .,
    mfsa_read_transform
  ) %>%  
  left_join(
    
    (read_xlsx("data/mfsa_credit_product_by_income.xlsx", sheet = "CPI") %>% 
       rename(year = 1, 
              average_cpi = Average) %>%
       filter(year >= 2007 & year <= 2023) %>% 
       select(year, average_cpi) %>% 
       mutate(year = as.character(year))
    ), 
    by = "year") %>%  
  write_xlsx(mfsa_allproduct_income, "output/mfsa_allproduct_income.xlsx")
