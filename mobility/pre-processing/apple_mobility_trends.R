# COVID‑19 mobility trends
# Source: Apple Mobility Trends Reports
# URL: https://www.apple.com/covid19/mobility

library(tidyverse) ; library(ggsci) ; library(scales) ; library(ggrepel)

df <- read_csv("applemobilitytrends-2020-04-18.csv") %>% 
  filter(region == "Manchester") %>% 
  select(-c(1:2)) %>% 
  pivot_longer(-transportation_type, names_to = "date", values_to = "percent") %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         transportation_type = str_to_title(transportation_type),
         percent = percent/100) %>% 
  group_by(transportation_type) %>% 
  mutate(area_name = "Manchester",
         label = if_else(date == max(date), percent, NA_real_)) %>% 
  ungroup() %>% 
  select(date, area_name, everything())

write_csv(select(df, -label), "../apple_mobility_trends.csv")

ggplot(df, aes(x = date, y = percent, colour = transportation_type)) +
  geom_hline(yintercept = 0, size = 0.5, colour = "#212121") +
  geom_line(size = 0.6) +
  geom_label_repel(aes(label = percent(label, accuracy = 1)), nudge_x = 1, na.rm = TRUE, show.legend = FALSE) +
  scale_color_startrek() +
  scale_y_continuous(expand = c(0.005, 0.005), labels = percent) +
  labs(x = NULL, y = NULL,
       title = "Mobility trends in Manchester",
       subtitle = paste("Change in routing requests since", format(min(df$date), '%d %B %Y')),
       caption = "Source: Apple Mobility Trends Reports") +
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
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "top",
    legend.title = element_blank()
  )

ggsave("../outputs/apple_mobility_trends.pdf", device = cairo_pdf, scale = 1.5, width = 6, height = 6)
