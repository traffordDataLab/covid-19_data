# Shielded Patient List

# Source: NHS Digital
# URL: https://digital.nhs.uk/data-and-information/publications/statistical/mi-english-coronavirus-covid-19-shielded-patient-list-summary-totals/latest
# Licence: OGL 3.0

library(tidyverse) ; library(scales) ; library(nord) ; library(tidytext)

# load data ---------------------------
patients <- read_csv("https://files.digital.nhs.uk/96/69FFAA/Coronavirus%20%28COVID-19%29%20Shielded%20Patient%20List%2C%20England%20-%20Open%20Data%20-%20LA%20-%202020-05-15.csv") %>% 
  filter(`LA Name` %in% c("Bolton", "Bury", "Manchester", "Oldham", "Rochdale",
                          "Salford", "Stockport", "Tameside", "Trafford", "Wigan"),
         `Breakdown Field` %in% c("ALL", "Age")) %>% 
  select(area_code = `LA Code`, area_name = `LA Name`, period = `Extract Date`, group = `Breakdown Value`, value = `Patient Count`) %>% 
  mutate(group = case_when(group == "ALL" ~ "All ages", TRUE ~ group),
         indicator = "Residents on the Shielded Patient List", 
         measure = "Count", unit = "Persons") %>% 
  select(area_code, area_name, indicator, period, measure, unit, group, value)

# Mid-2018 population estimates
# Source: Nomis / ONS
# URL: https://www.nomisweb.co.uk/datasets/pestsyoala

population <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1820327961...1820327970&date=latest&gender=0&c_age=101...191&measures=20100&select=date_name,geography_name,geography_code,gender_name,c_age_name,measures_name,obs_value,obs_status_name") %>% 
  select(area_code = GEOGRAPHY_CODE, area_name = GEOGRAPHY_NAME, age = C_AGE_NAME, count = OBS_VALUE) %>%
  mutate(age = as.integer(str_trim(str_replace_all(age, "Age.|\\+", "")))) %>% 
  pivot_wider(names_from = age, values_from = count) %>% 
  mutate(`0-18` = rowSums(select(., `0`:`18`)),
         `19-69` = rowSums(select(., `19`:`69`)),
         `70+` = rowSums(select(., `70`:`90`)),
         `All ages` = rowSums(select(., `0`:`90`))) %>% 
  select(area_code, `0-18`, `19-69`, `70+`, `All ages`) %>% 
  pivot_longer(-area_code, names_to = "group", values_to = "population")

# join data ---------------------------
df <- left_join(patients, population, by = c("area_code", "group")) %>% 
  mutate(percent = value/population)

# plot data ---------------------------
ggplot(df, aes(reorder_within(area_name, percent, group), percent, fill = group)) +
  geom_col() + 
  geom_label(aes(reorder_within(area_name, percent, group), percent, label = paste0(round(percent*100,0), "%")),
             hjust = ifelse(df$group == "70+", 1, 0), vjust = 0.5, 
             colour = ifelse(df$group == "70+", "#FFFFFF", "#212121"),
             fill = NA, label.size = NA, size = 4) +
  scale_fill_nord("algoma_forest") +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~group, scales = "free_y", ncol = 4) + 
  coord_flip() +
  labs(x = NULL, y = NULL,
       title = "Residents on the Shielded Patient List (SPL) by age group",
       subtitle = "Greater Manchester, 15 May 2020",
       caption = "Source: NHS Digital") +
  theme_minimal(base_size = 12) +
  theme(plot.margin = unit(rep(0.5, 4), "cm"),
        panel.spacing = unit(0.3, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, margin = margin(b = 30)),
        plot.caption = element_text(colour = "grey60", margin = margin(t = 20, b = -10)),
        strip.text = element_text(size = 12, face = "bold", hjust = 0),
        axis.text.x = element_blank(),
        legend.position = "none")

# write data ---------------------------
write_csv(patients, "../shielded_patients.csv")
ggsave("shielded_patients.png", width = 8, height = 6, dpi = 300)         

