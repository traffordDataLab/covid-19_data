# COVID-19 mortality risk by LSOA #
# Adapted from https://github.com/VictimOfMaths/COVID-19/blob/master/COVIDExposures.R

library(tidyverse) ; library(sf) ; library(janitor) ; library(readxl) 
library(biscale) ; library(cowplot)

# -------------------------------------------
# Retrieve data
# ------------------------------------------- 

id <- "Trafford"

# LSOA boundaries
# Source: ONS Open Geography Portal
# Publisher URL: https://geoportal.statistics.gov.uk/datasets/lower-layer-super-output-areas-december-2011-boundaries-ew-bgc

lsoa <- st_read(paste0("https://ons-inspire.esriuk.com/arcgis/rest/services/Census_Boundaries/Lower_Super_Output_Areas_December_2011_Boundaries/MapServer/2/query?where=UPPER(lsoa11nm)%20like%20'%25", URLencode(toupper(id), reserved = TRUE), "%25'&outFields=lsoa11cd,lsoa11nm&outSR=4326&f=geojson")) %>% 
  select(area_code = lsoa11cd)

# 2019 Indices of Multiple Deprivation
# Source: Ministry of Housing, Communities and Local Government
# Publisher URL: https://www.gov.uk/government/statistics/announcements/english-indices-of-deprivation-2019

imd19 <- read_csv("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833982/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators.csv") %>% 
  clean_names() %>% 
  filter(str_detect(lsoa_name_2011, id)) %>% 
  select(area_code = 1, 17:19) %>% 
  pivot_longer(-area_code, names_to = "domain", values_to = "value") %>% 
  mutate(measure = case_when(str_detect(domain, "score") ~ "score", 
                             str_detect(domain, "decile") ~ "decile", 
                             str_detect(domain, "rank") ~ "rank")) %>% 
  select(area_code, measure, value) %>% 
  pivot_wider(names_from = measure, values_from = value) %>% 
  mutate(imd_decile = factor(decile, levels = c(1:10), ordered = TRUE),
         imd_rank = as.integer(rank)) %>% 
  select(area_code, imd_decile, imd_rank)

# Mid-2018 population estimates
# Source: Office for National Statistics
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/annualmidyearpopulationestimates/mid2018

url <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2flowersuperoutputareamidyearpopulationestimates%2fmid2018sape21dt1a/sape21dt1amid2018on2019lalsoasyoaestimatesformatted.zip"
download.file(url, dest = "sape21dt1amid2018on2019lalsoasyoaestimatesformatted.zip")
unzip("sape21dt1amid2018on2019lalsoasyoaestimatesformatted.zip", exdir = ".")
file.remove("sape21dt1amid2018on2019lalsoasyoaestimatesformatted.zip")

females <- read_excel("SAPE21DT1a-mid-2018-on-2019-LA-lsoa-syoa-estimates-formatted.xlsx", 
                      sheet = "Mid-2018 Females", range = "A5:CQ35097", col_names = TRUE) %>% 
  mutate(sex = "Female")

males <- read_excel("SAPE21DT1a-mid-2018-on-2019-LA-lsoa-syoa-estimates-formatted.xlsx", 
                     sheet = "Mid-2018 Males", range = "A5:CQ35097", col_names = TRUE) %>% 
  mutate(sex = "Male")

population <- bind_rows(females, males) %>% 
  filter(str_detect(LSOA, id)) %>% 
  mutate(`0-9` = rowSums(select(., `0`:`9`)),
         `10-19` = rowSums(select(., `10`:`19`)),
         `20-29` = rowSums(select(., `20`:`29`)),
         `30-39` = rowSums(select(., `30`:`39`)),
         `40-49` = rowSums(select(., `40`:`49`)),
         `50-59` = rowSums(select(., `50`:`59`)),
         `60-69` = rowSums(select(., `60`:`69`)),
         `70-79` = rowSums(select(., `70`:`79`)),
         `80+` = rowSums(select(., `80`:`90+`))) %>% 
  select(area_code = `Area Codes`, area_name = LSOA, sex,
         `0-9`, `10-19`, `20-29`, `30-39`, `40-49`, `50-59`, `60-69`, `70-79`, `80+`) %>% 
  gather(age, population, c(4:12))

# Case Fatality Ratio (CFR)
# Source: Istituto Superiore di Sanità
# URL: https://www.epicentro.iss.it/coronavirus/bollettino/Bollettino-sorveglianza-integrata-COVID-19_23-aprile-2020.pdf

# Infection Fatality Ratio (IFR)
# Source: Imperial College
# URL: https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-NPI-modelling-16-03-2020.pdf

cfr <-  tribble(
  ~age, ~b, ~m, ~f, ~ifr,
  "0-9",      0.2,      0.1,      0.2,      0.002,
  "10-19",    0.000001, 0.000001, 0.000001, 0.006,
  "20-29",    0.1,      0.1,      0.000001, 0.03,
  "30-39",    0.4,      0.5,      0.3,      0.08,
  "40-49",    0.9,      1.6,      0.4,      0.15,
  "50-59",    2.6,      4.3,      1.1,      0.6,
  "60-69",   10.0,     12.6,      5.9,      2.2,
  "70-79",   24.9,     30.2,     17.2,      5.1,
  "80+",     30.8,     42.0,     22.0,      9.3
  ) %>% 
  # Calculate age-specific sex:population Case Fatality Ratios
  mutate(mtobratio = m/b,
         ftobratio = f/b,
  # Apply population estimates of Infection Fatality Ratio         
         mifr = ifr*mtobratio,
         fifr = ifr*ftobratio)

# -------------------------------------------
# Merge and summarise data
# ------------------------------------------- 

df <- left_join(population, cfr, by = "age") %>% 
  # Calculate expected deaths with 100% inflection by age group
  mutate(expected_deaths = case_when(
    sex == "Male" ~ population*mifr/100,
    sex == "Female" ~ population*fifr/100)) %>% 
  group_by(area_code) %>% 
  summarise(area_name = unique(area_name), 
            population = sum(population), 
            expected_deaths = sum(expected_deaths)) %>% 
  # Calculate mortality rate per 100,000 population
  mutate(mortality_rate = expected_deaths*100000/population) %>% 
  ungroup() %>% 
  # Join Health Deprivation and Disability domain
  left_join(imd19, by = "area_code")

write_csv(df, "data.csv")

# -------------------------------------------
# Plot data
# ------------------------------------------- 

# Bivariate choropleth map

sf <- left_join(lsoa, df, by = "area_code") %>% 
  mutate(imd_rank = -imd_rank)

data <- bi_class(sf, x = mortality_rate, y = imd_rank, style = "quantile", dim = 3)

map <- ggplot() +
  geom_sf(data = data, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  labs(title = paste0("Mapping potential COVID-19 risk across ", id),
       subtitle = "LSOA-level health deprivation and potential COVID-19 mortality risk based on age-sex structure of population",
       caption = "Source: ONS, Istituto Superiore di Sanità & Imperial College\nCode adapted from https://github.com/VictimOfMaths/COVID-19") +
  coord_sf(crs = st_crs(4326), datum = NA) +
  theme_void() +
  theme(plot.margin = unit(c(1,1,1,1), "cm"),
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(margin = margin(b = 25)), 
        plot.caption = element_text(size = 10, color = "grey50", margin = margin(t = 20)))

legend <- bi_legend(pal = "DkBlue", dim = 3, 
                    xlab = "Greater age-based COVID-19 risk",
                    ylab = "Greater health deprivation", size = 8)

plot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.1, 0.1, 0.25, 0.25)

plot

save_plot("plot.png", plot, base_height = 8, base_aspect_ratio = 1.8)
