# COVID-19 combined risk scores #
# Method adapted from @jburnmurdoch (https://twitter.com/jburnmurdoch/status/1332233679704383488)

library(tidyverse) ; library(httr) ; library(readODS) ; library(readxl) 
library(lubridate) ; library(janitor) ; library(scales)

### Check week-ending date in data - used to filter subsequent indicators
end_date <- as.Date("2020-11-25")

# Local restriction tiers ------------------------------------------------------
# Source: Department of Health and Social Care
# URL: https://www.gov.uk/government/collections/coronavirus-cases-by-local-authority-epidemiological-data
url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/941144/20201203_Watchlist_for_publication_gov_uk.ods"
GET(url, write_disk(tmp <- tempfile(fileext = ".ods")))
tiers <- read_ods(tmp) %>% 
  select(area_name = `Lower Tier Local Authority`, tier = `Local COVID Alert Level`) %>% 
  mutate(area_name = case_when(
    area_name %in% c("Cornwall", "Isles of Scilly") ~ "Cornwall and Isles of Scilly",
    area_name %in% c("City of London", "Hackney") ~ "Hackney and City of London",
    TRUE ~ area_name),
    tier = case_when(
      tier == "Medium" ~ "Tier 1",
      tier == "High" ~ "Tier 2",
      tier == "Very High" ~ "Tier 3")
    ) %>% 
  distinct(area_name, .keep_all = TRUE)

# Mid-2019 population estimates ------------------------------------------------
# Source: Nomis / ONS
# URL: https://www.nomisweb.co.uk/datasets/pestsyoala
population <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1811939329...1811939332,1811939334...1811939336,1811939338...1811939497,1811939499...1811939501,1811939503,1811939505...1811939507,1811939509...1811939517,1811939519,1811939520,1811939524...1811939570,1811939575...1811939599,1811939601...1811939628,1811939630...1811939634,1811939636...1811939647,1811939649,1811939655...1811939664,1811939667...1811939680,1811939682,1811939683,1811939685,1811939687...1811939704,1811939707,1811939708,1811939710,1811939712...1811939717,1811939719,1811939720,1811939722...1811939730,1811939757...1811939767&date=latest&gender=0&c_age=200,14...18,210&measures=20100&select=date_name,geography_name,geography_code,gender_name,c_age_name,measures_name,obs_value,obs_status_name") %>% 
  rename(area_code = GEOGRAPHY_CODE, ageband = C_AGE_NAME, population = OBS_VALUE) %>% 
  filter(str_detect(area_code, "^E")) %>% 
  # combine those aged 60 years and over
  mutate(ageband = case_when(ageband == "All Ages" ~ "All Ages", TRUE ~ "Aged 60 and over")) %>% 
  group_by(area_code, ageband) %>% 
  summarise(population = sum(population)) %>% 
  pivot_wider(names_from = ageband, values_from = population) %>% 
  # combine population estimates for Hackney and City of London / Cornwall and Isles of Scilly 
  mutate(area_code = case_when(
    as.character(area_code) %in% c("E09000012", "E09000001") ~ "E09000012", 
    as.character(area_code) %in% c("E06000052", "E06000053") ~ "E06000052", 
    TRUE ~ area_code)) %>% 
  group_by(area_code) %>% 
  summarise(`All Ages` = sum(`All Ages`),
            `Aged 60 and over` = sum(`Aged 60 and over`)) 

# Pillar 1 and 2 tests ---------------------------------------------------------------
# Source: Demographic and regional information for people tested and testing positive, NHS Test & Trace
# URL: https://www.gov.uk/government/collections/nhs-test-and-trace-statistics-england-weekly-reports
url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/940822/Demographic_LA_tables_w26.ods"
GET(url, write_disk(tmp <- tempfile(fileext = ".ods")))
tests <- read_ods(tmp, sheet = 8, skip = 2) %>% 
  select(area_code = LTLA, tests = last_col(1)) %>% 
  filter(!area_code == "Unknown")

# Coronavirus cases ------------------------------------------------------------
# Source: PHE Coronavirus Dashboard
# URL: https://coronavirus.data.gov.uk/details/download
cases_all_groups <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDate&format=csv") %>% 
  # filter by ltla in England
  filter(str_detect(areaCode, "^E")) %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>% 
  select(date,
         area_code = areaCode,
         area_name = areaName,
         new_cases = newCasesBySpecimenDate) %>% 
  arrange(date) %>% 
  group_by(area_code, area_name) %>%
  complete(date = seq.Date(min(date), max(date), by = "day")) %>% 
  mutate(new_cases = replace_na(new_cases, 0)) %>% 
  ungroup() %>% 
  fill(area_name) %>% 
  filter(date <= end_date) %>% 
  mutate(period = case_when(
    date >= max(date)-days(6) & date <= max(date) ~ "current_week",
    date >= max(date)-days(13) & date <= max(date)-days(7) ~ "previous_week"
  )) %>% 
  filter(!is.na(period)) %>% 
  select(-date) %>% 
  group_by(area_code, area_name, period) %>% 
  summarise(total_cases = sum(new_cases)) %>% 
  pivot_wider(names_from = period, values_from = total_cases) %>% 
  select(area_code, area_name, previous_week, current_week) %>% 
  left_join(population, by = "area_code") %>% 
  mutate(previous_week_rate = round(previous_week/`All Ages`*100000,1),
         current_week_rate = round(current_week/`All Ages`*100000,1),
         growth_rate = round(current_week_rate/previous_week_rate,1)) %>% 
  ungroup() %>%
  select(area_code, area_name, cases = current_week, case_rate = current_week_rate, growth_rate)

cases_over_60s <- read_csv("https://coronavirus.data.gov.uk/downloads/demographic/cases/specimenDate_ageDemographic-unstacked.csv") %>% 
  filter(areaType == "ltla") %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>% 
  select(date, area_code = areaCode, area_name = areaName, starts_with("newCasesBySpecimenDate-")) %>% 
  pivot_longer(cols = -c(date, area_code, area_name), names_to = "ageband", values_to = "new_cases") %>% 
  filter(ageband == "newCasesBySpecimenDate-60+") %>% 
  select(date, area_code, area_name, new_cases) %>% 
  arrange(date) %>% 
  group_by(area_code, area_name) %>%
  complete(date = seq.Date(min(date), max(date), by = "day")) %>% 
  mutate(new_cases = replace_na(new_cases, 0)) %>% 
  ungroup() %>% 
  fill(area_name) %>% 
  filter(date >= end_date-days(6) & date <= end_date) %>% 
  group_by(area_code, area_name) %>% 
  summarise(current_week = sum(new_cases)) %>% 
  left_join(population, by = "area_code") %>% 
  mutate(case_rate_over_60s = round(current_week/`Aged 60 and over`*100000,1)) %>% 
  ungroup() %>%
  select(area_code, case_rate_over_60s)

# Apportioning Hospital Trusts to MSOA to LTLA ---------------------------------

# MSOA to LTLA lookup
# Source: ONS Open Geography Portal 
# URL: https://geoportal.statistics.gov.uk/datasets/middle-layer-super-output-area-2011-to-ward-to-lad-december-2019-lookup-in-england-and-wales
msoa <- read_csv("https://opendata.arcgis.com/datasets/0b3c76d1eb5e4ffd98a3679ab8dea605_0.csv") %>% 
  select(msoa11cd = MSOA11CD, area_code = LAD19CD, area_name = LAD19NM) %>% 
  distinct(msoa11cd, .keep_all = TRUE) %>% 
  # combine Hackney and City of London / Cornwall and Isles of Scilly 
  mutate(
    area_code = case_when(
      area_name == "Cornwall and Isles of Scilly" ~ "E06000052",
      area_name == "Hackney and City of London" ~ "E09000012",
      TRUE ~ area_code),
    area_name = case_when(
      area_name %in% c("Cornwall", "Isles of Scilly") ~ "Cornwall and Isles of Scilly",
      area_name %in% c("City of London", "Hackney") ~ "Hackney and City of London", 
      TRUE ~ area_name)
  )

# Hospital Trust Catchment Populations
# Source: PHE (https://app.powerbi.com/view?r=eyJrIjoiODZmNGQ0YzItZDAwZi00MzFiLWE4NzAtMzVmNTUwMThmMTVlIiwidCI6ImVlNGUxNDk5LTRhMzUtNGIyZS1hZDQ3LTVmM2NmOWRlODY2NiIsImMiOjh9)
# URL: https://app.box.com/s/qh8gzpzeo1firv1ezfxx2e6c4tgtrudl
catchment <- read_xlsx("2020 Trust Catchment Populations_Supplementary MSOA Analysis.xlsx", sheet = 2) %>% 
  filter(CatchmentYear == "2018") %>% 
  select(msoa11cd = msoa, trust_code = TrustCode, trust_name = TrustName, msoa_total_catchment)

# Aggregate MSOA catchment populations for each provider by local authority district
ltla_catchment <- left_join(msoa, catchment, by = "msoa11cd") %>% 
  group_by(trust_code, area_code) %>% 
  summarise(la_total_catchment = sum(msoa_total_catchment)) %>% 
  mutate(proportion = la_total_catchment/sum(la_total_catchment)) %>% 
  select(trust_code, area_code, proportion)

# Weekly hospital admissions and beds ---------------------------------------------------
# Source: NHS COVID-19 Hospital Activity
# URL: https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity
tmp <- tempfile(fileext = ".xlsx")
GET(url = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/12/Weekly-covid-admissions-and-beds-publication-201203.xlsx",
    write_disk(tmp))

# Hospital bed occupancy
occupancy <- read_xlsx(tmp, sheet = "All beds COVID", skip = 14) %>% 
  filter(!is.na(Code), Code != "-") %>% 
  select(trust_code = Code, trust_name = Name, starts_with("4")) %>% 
  pivot_longer(cols = -c(trust_code, trust_name), names_to = "date", values_to = "occupancy") %>% 
  mutate(trust_name = str_to_title(trust_name),
         date = excel_numeric_to_date(as.numeric(as.character(date)), date_system = "modern"),
         occupancy = replace_na(occupancy, 0)) %>% 
  filter(date == end_date)

prop_occupancy <- left_join(ltla_catchment, occupancy, by = "trust_code") %>% 
  mutate(prop_occupancy = occupancy*proportion,
         prop_occupancy = replace_na(prop_occupancy, 0)) %>% 
  group_by(area_code) %>% 
  summarise(estimated_occupancy = sum(prop_occupancy)) %>% 
  mutate(estimated_occupancy = replace_na(estimated_occupancy, 0)) %>% 
  left_join(population, by = "area_code") %>% 
  mutate(estimated_occupancy_rate = round(estimated_occupancy/`All Ages`*100000,1)) %>% 
  select(area_code, estimated_occupancy_rate)

# Hospital admissions
admissions <- read_xlsx(tmp, sheet = "Hosp ads & diag", skip = 14) %>% 
  filter(!is.na(Code), Code != "-") %>% 
  select(trust_code = Code, trust_name = Name, starts_with("4")) %>% 
  pivot_longer(cols = -c(trust_code, trust_name), names_to = "date", values_to = "admissions") %>% 
  mutate(trust_name = str_to_title(trust_name),
         date = excel_numeric_to_date(as.numeric(as.character(date)), date_system = "modern"),
         admissions = replace_na(admissions, 0)) %>% 
  filter(date == end_date)

prop_admissions <- left_join(ltla_catchment, admissions, by = "trust_code") %>% 
  mutate(prop_admissions = admissions*proportion,
         prop_admissions = replace_na(prop_admissions, 0)) %>% 
  group_by(area_code) %>% 
  summarise(estimated_admissions = sum(prop_admissions)) %>% 
  mutate(estimated_admissions = replace_na(estimated_admissions, 0)) %>% 
  left_join(population, by = "area_code") %>% 
  mutate(estimated_admissions_rate = round(estimated_admissions/`All Ages`*100000,1)) %>% 
  select(area_code, estimated_admissions_rate)

# Combining indicators ---------------------------------------------------------
indicators <- left_join(cases_all_groups, cases_over_60s, by = "area_code") %>% 
  left_join(tests, by = "area_code") %>% 
  left_join(prop_occupancy, by = "area_code") %>% 
  left_join(prop_admissions, by = "area_code") %>% 
  left_join(tiers, by = "area_name") %>% 
  mutate(positivity_rate = round(cases/tests*100,1)) %>% 
  select(area_code, area_name, tier, case_rate, case_rate_over_60s, growth_rate, positivity_rate, estimated_admissions_rate, estimated_occupancy_rate)

# Create a combined COVID-19 risk score ----------------------------------------
scores <- indicators %>% 
  mutate(
    # standardise each input variable to a z-score
    case_rate = (case_rate - mean(case_rate, na.rm = TRUE))/sd(case_rate, na.rm = TRUE),
    case_rate_over_60s = (case_rate_over_60s - mean(case_rate_over_60s, na.rm = TRUE))/sd(case_rate_over_60s, na.rm = TRUE),
    growth_rate = (growth_rate - mean(growth_rate, na.rm = TRUE))/sd(growth_rate, na.rm = TRUE),
    positivity_rate = (positivity_rate - mean(positivity_rate, na.rm = TRUE))/sd(positivity_rate, na.rm = TRUE),
    estimated_admissions_rate = (estimated_admissions_rate - mean(estimated_admissions_rate, na.rm = TRUE))/sd(estimated_admissions_rate, na.rm = TRUE),
    estimated_occupancy_rate = (estimated_occupancy_rate - mean(estimated_occupancy_rate, na.rm = TRUE))/sd(estimated_occupancy_rate, na.rm = TRUE),
    # average the standardised scores in each area
    mean = rowMeans(.[,4:9]),
    # rescale the result from 0 to 100
    score = rescale(mean, to = c(0,100))
    ) %>% 
  filter(!is.na(score))

# Write data -------------------------------------------------------------------
left_join(indicators, select(scores, area_code, score), by = "area_code") %>%
  filter(!is.na(score)) %>%
  write_csv("covid19_combined_risk_scores.csv")


