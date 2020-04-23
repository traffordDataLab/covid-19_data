# Confirmed cases by UTLA
# Source: Public Health England
# URL: https://coronavirus.data.gov.uk

library(tidyverse)

df <- read_csv("https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv",
                  col_types = cols(
                    `Area name` = col_character(),
                    `Area code` = col_character(),
                    `Area type` = col_character(),
                    `Specimen date` = col_date(format = "%Y-%m-%d"),
                    `Daily lab-confirmed cases` = col_integer(),
                    `Cumulative lab-confirmed cases` = col_integer())) %>% 
  select(Date = `Specimen date`,
         AreaName = `Area name`,
         AreaCode = `Area code`,
         AreaType = `Area type`,
         NewCases = `Daily lab-confirmed cases`,
         CumCases = `Cumulative lab-confirmed cases`) %>% 
  filter(AreaType == "Upper tier local authority")

write_csv(filter(df, AreaName == "Trafford"), "coronavirus_cases.csv")
