# Confirmed cases by UTLA
# Source: Public Health England
# URL: https://coronavirus.data.gov.uk/#local-authorities

library(tidyverse)

df <- read_csv("coronavirus-cases.csv") %>% 
  filter(`Area name` == "Trafford") %>% 
  select(area_name = `Area name`,
         area_code = `Area code`,
         date = `Specimen date`,
         new_cases = `Daily lab-confirmed cases`) %>% 
  arrange(date) %>% 
  mutate(cum_cases = cumsum(new_cases))

write_csv(df, "coronavirus_cases.csv")
