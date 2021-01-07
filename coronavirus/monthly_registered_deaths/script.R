# Monthly registered deaths #
# Source: Office for National Statistics
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/monthlyfiguresondeathsregisteredbyareaofusualresidence

library(tidyverse) ; library(httr) ; library(readxl) ; library(lubridate) ; library(ggtext)

id <- "Trafford"

# load data --------------------------------------------------------------------

# 2020 (provisional)
tmp <- tempfile(fileext = ".xls")
GET(url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fmonthlyfiguresondeathsregisteredbyareaofusualresidence%2f2020/publishedoutputnovember2020new.xls",
    write_disk(tmp))
deaths_2020 <- read_xls(tmp, sheet = 4, skip = 3, col_types = "text") %>% 
  filter(`...2` == id) %>% 
  select(-`Area of usual residence`) %>% 
  rename(area_name = `...2`) %>% 
  pivot_longer(-area_name, names_to = "period", values_to = "n") %>% 
  mutate(period = str_remove_all(period, "3"),
         period = parse_date_time(period, "%b-%y"),
         month = month(period))

# 2019
tmp <- tempfile(fileext = ".xls")
GET(url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fmonthlyfiguresondeathsregisteredbyareaofusualresidence%2f2019/annual2019publishedoutputrefresh.xls",
    write_disk(tmp))
deaths_2019 <- read_xls(tmp, sheet = 4, skip = 3, col_types = "text") %>% 
  filter(`...2` == id) %>% 
  select(-`Area of usual residence`) %>% 
  rename(area_name = `...2`) %>% 
  pivot_longer(-area_name, names_to = "period", values_to = "n") %>% 
  mutate(period = str_remove_all(period, "3"),
         period = parse_date_time(period, "%b-%y"))
  
# 2018
tmp <- tempfile(fileext = ".xls")
GET(url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fmonthlyfiguresondeathsregisteredbyareaofusualresidence%2f2018/publishedannual2018.xls",
    write_disk(tmp))
deaths_2018 <- read_xls(tmp, sheet = 4, skip = 3, col_types = "text") %>% 
  filter(`...2` == id) %>% 
  select(-`Area of usual residence`) %>% 
  rename(area_name = `...2`) %>% 
  pivot_longer(-area_name, names_to = "period", values_to = "n") %>% 
  mutate(period = str_remove_all(period, "3"),
         period = parse_date_time(period, "%b-%y"))

# 2017
tmp <- tempfile(fileext = ".xls")
GET(url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fmonthlyfiguresondeathsregisteredbyareaofusualresidence%2f2017/publishedoutputannual2017final.xls",
    write_disk(tmp))
deaths_2017 <- read_xls(tmp, sheet = 4, skip = 3, col_types = "text") %>% 
  filter(`...2` == id) %>% 
  select(-`Area of usual residence`) %>% 
  rename(area_name = `...2`) %>% 
  pivot_longer(-area_name, names_to = "period", values_to = "n") %>% 
  mutate(period = str_remove_all(period, "3"),
         period = parse_date_time(period, "%b-%y"))

# 2016
tmp <- tempfile(fileext = ".xls")
GET(url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fmonthlyfiguresondeathsregisteredbyareaofusualresidence%2f2016/publishedoutput2016final.xls",
    write_disk(tmp))
deaths_2016 <- read_xls(tmp, sheet = 4, skip = 3, col_types = "text") %>% 
  filter(`...2` == id) %>% 
  select(-`Area of usual residence`) %>% 
  rename(area_name = `...2`) %>% 
  pivot_longer(-area_name, names_to = "period", values_to = "n") %>% 
  mutate(period = str_remove_all(period, "3"),
         period = parse_date_time(period, "%b-%y"))

# 2015
tmp <- tempfile(fileext = ".xls")
GET(url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fmonthlyfiguresondeathsregisteredbyareaofusualresidence%2f2015/publishedoutput2015final.xls",
    write_disk(tmp))
deaths_2015 <- read_xls(tmp, sheet = 4, skip = 3, col_types = "text") %>% 
  filter(`...2` == id) %>% 
  select(-`Area of usual residence`) %>% 
  rename(area_name = `...2`) %>% 
  pivot_longer(-area_name, names_to = "period", values_to = "n") %>% 
  mutate(period = str_remove_all(period, "3"),
         period = parse_date_time(period, "%b-%y"))

# transform data ---------------------------------------------------------------

# calculate 5-year average (2015-19)
five_year_average <- bind_rows(deaths_2019, deaths_2018, deaths_2017, deaths_2016, deaths_2015) %>% 
  mutate(month = month(period),
         year = year(period),
         n = as.integer(n)) %>% 
  group_by(month) %>% 
  summarise(average = mean(n),
            min = min(n),
            max = max(n))

# left join to 2020 data
deaths <- left_join(deaths_2020, five_year_average, by = "month") %>% 
  select(-month) %>% 
  mutate(period = as.Date(period),
         n = as.integer(n))

# plot data --------------------------------------------------------------------

ggplot(data = deaths) +
  geom_hline(yintercept = 0, size = 0.3, colour = "#333333") +
  geom_smooth(aes(x = period, y = average, ymin = min, ymax = max, colour = "average"), size = 0.8, fill = "#bdbdbd", stat = "identity") +
  geom_line(aes(x = period, y = n, group = 1), size = 0.8, colour = "#a50f15") +
  scale_colour_manual(values = c("average" = "#757575"), name = NULL, labels = "2015-2019 average with min / max range") +
  scale_x_date(date_labels = "%b '%y") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  labs(x = NULL, y = NULL,
       title = "Registered deaths from all causes",
       subtitle = paste0("<span style = 'color:#757575;'>Provisional monthly count, ", id, ", 2020</span>"),
       caption = "Source: ONS") +
  theme_minimal(base_size = 14) +
  theme(text = element_text(family = "Source Sans Pro"),
        plot.margin = unit(rep(1, 4), "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour = "#bdbdbd"),
        plot.title.position = "plot",
        plot.title = element_text(family = "Merriweather", face = "bold", size = 22, lineheight = 26),
        plot.subtitle = element_markdown(margin = margin(b = 20)),
        plot.caption = element_text(colour = "grey60", margin = margin(t = 20, b = -10)),
        legend.position = "top", 
        legend.justification = "left",
        legend.direction = "horizontal",
        legend.text = element_text(size = 12),
        axis.ticks.x = element_line(colour = 'black', size = 0.5))

ggsave("plot.png", width = 8, height = 6, dpi = 300)


