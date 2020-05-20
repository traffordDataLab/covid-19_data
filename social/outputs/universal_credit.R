library(tidyverse) ; library(scales)

df <- read_csv("../universal_credit.csv") %>% 
  pivot_longer(-Month, names_to = "JobcentrePlus", values_to = "n")

ggplot(df, aes(x = Month, y = n, colour = JobcentrePlus)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_colour_manual(values = c("#FAAB18", "#1380A1")) +
  scale_x_date(expand = c(0.005, 0.005), date_labels = "%b-%Y") +
  scale_y_continuous(expand = c(0.005, 0.005), labels = comma, position = "right") +
  labs(x = NULL, y = NULL,
       title = "People claiming Universal Credit",
       subtitle = paste0("Trafford, ", format(min(df$Month), "%B %Y"), " to ", format(max(df$Month), "%B %Y")),
       caption = "Source: Department for Work and Pensions",
       colour = NULL) +
    theme_minimal() +
    theme(plot.margin = unit(rep(0.5, 4), "cm"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title.position = "plot",
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 12, margin = margin(b = 20)),
          plot.caption = element_text(colour = "grey60", margin = margin(t = 20, b = -10)),
          legend.position = "top")

ggsave("universal_credit.png", width = 8, height = 6, dpi = 300)
