# Smoking prevalence at age 15 - regular smokers (modelled estimates)
# Source: NHS Digital ; Public Health England
# URL: https://fingertips.phe.org.uk/profile/local-health

library(tidyverse); library(fingertipsR)

df <- fingertips_data(IndicatorID = 93488, AreaTypeID = 3) %>%
  filter(ParentName=="Trafford") %>%
  mutate(measure="percentage", unit="persons") %>%
  select(area_code=AreaCode, area_name=AreaName, indicator=IndicatorName, period=Timeperiod, measure, unit, value= Value)

write_csv(df, "smoking_15yrs_prevalence.csv") 
