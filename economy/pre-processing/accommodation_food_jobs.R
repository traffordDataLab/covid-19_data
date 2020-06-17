# Employee jobs in accommodation & food services

# Source: Business Register and Employment Survey
# URL: https://www.ons.gov.uk/surveys/informationforbusinesses/businesssurveys/businessregisterandemploymentsurvey
# Licence: OGL 3.0

# 55 : Accommodation
# 56 : Food and beverage service activities
df <- read_csv('https://www.nomisweb.co.uk/api/v01/dataset/NM_189_1.data.csv?geography=1245709510...1245709537&date=latest&industry=146800695,146800696&employment_status=1&measure=1,2&measures=20100') %>%
  mutate(indicator = "Employee jobs in accommodation and food services", unit = "Persons") %>%
  select(area_code = GEOGRAPHY_CODE, area_name = GEOGRAPHY_NAME, indicator, period = DATE, measure = MEASURE_NAME, unit, value = OBS_VALUE) %>%
  group_by_at(setdiff(names(df), "value")) %>%
  summarise(value=sum(value)) %>%
  ungroup()

#House of Commons Library MSOA Names
# URL: https://visual.parliament.uk/msoanames

lookup <- read_csv("https://visual.parliament.uk/msoanames/static/MSOA-Names-v1.1.0.csv") %>%
  filter(Laname=="Trafford")

df_names <- df %>%
  left_join(lookup%>%select(msoa11cd,msoa11hclnm), by = c("area_code" = "msoa11cd")) %>%
  mutate(area_name=msoa11hclnm) %>%
  select(-msoa11hclnm)

write_csv(df_names, "accommodation_food_jobs.csv")
