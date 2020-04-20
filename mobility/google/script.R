# Mobility trends in Greater Manchester #
# Source: Google COVID-19 Community Mobility Trends
# URL: https://www.google.com/covid19/mobility/

library(tidyverse) ; library(scales) ; library(ggrepel)

df <- read_csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv") %>% 
  filter(country_region == "United Kingdom",
         sub_region_1 == "Greater Manchester") %>% 
  select(-c(1:4)) %>% 
  pivot_longer(-date, names_to = "place", values_to = "percent") %>% 
  mutate(percent = percent/100,
         place = case_when(
           place == "retail_and_recreation_percent_change_from_baseline" ~ "Retail & recreation",
           place == "grocery_and_pharmacy_percent_change_from_baseline" ~ "Grocery & pharmacy",
           place == "parks_percent_change_from_baseline" ~ "Parks",
           place == "transit_stations_percent_change_from_baseline" ~ "Transit stations",
           place == "workplaces_percent_change_from_baseline" ~ "Workplaces",
           place == "residential_percent_change_from_baseline" ~ "Residential"
  )) %>% 
  group_by(place) %>% 
  mutate(label = if_else(date == max(date), percent, NA_real_)) %>% 
  ungroup()

ggplot(df, aes(x = date, y = percent)) +
  geom_hline(yintercept = 0, size = 0.5, colour = "#212121") +
  geom_line(size = 0.6, colour = "#90BB39") +
  geom_label_repel(aes(label = percent(label, accuracy = 1)), nudge_x = 1, na.rm = TRUE) +
  scale_y_continuous(labels = percent) +
  facet_wrap(~place, ncol = 2) +
  labs(x = NULL, y = NULL,
       title = "Mobility trends in Greater Manchester",
       subtitle = paste(format(min(df$date), '%d %B'), "to", format(max(df$date), '%d %B %Y')),
       caption = "Source: Google COVID-19 Community Mobility Reports") +
  theme_minimal() +
  theme(
    plot.margin = unit(rep(0.5, 4), "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title.position = "plot",
    plot.caption.position =  "plot",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 25)), 
    plot.caption = element_text(size = 10, color = "grey50", margin = margin(t = 20)),
    strip.text = element_text(size = 12, face = "bold")
  )

ggsave("google_mobility_trends_in_GM.pdf", device = cairo_pdf, scale = 1.5, width = 6, height = 6)

