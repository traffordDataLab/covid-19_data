# Mid-2018 population estimates, Over 70 extract

# Source: ONS
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/annualmidyearpopulationestimates/mid2018
# Licence: Open Government Licence v3.0

library(tidyverse)

over70<- read_csv("https://github.com/traffordDataLab/population_picker/raw/master/data/mid-2018_population_estimates_all_geographies.csv") %>%
  filter(age>69,geography=="LSOA",gender=="Persons") %>%
  select(-age) %>%
  mutate(measure="Over 70") %>%
  group_by(period,area_code,area_name,gender,geography,measure) %>%
  summarise(value=sum(n)) %>%
  rename(unit=gender)

write_csv(over70,"over_70.csv")
