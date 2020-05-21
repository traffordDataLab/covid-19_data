#Personal Independence Payment (PIP)

# Source: DWP
# URL: https://stat-xplore.dwp.gov.uk/webapi/metadata/PIP_Monthly/PIP_Monthly.html
# Licence: Open Government Licence

# load the necessary R packages
library(tidyverse) ; library(httr) ; library(jsonlite) ; library(zoo)

#House of Commons Library MSOA Names
# URL: https://visual.parliament.uk/msoanames

lookup <- read_csv("https://visual.parliament.uk/msoanames/static/MSOA-Names-v1.1.0.csv") %>%
  filter(Laname=="Trafford")

# add your API key
api_key <- ""

# API endpoint
path <- "https://stat-xplore.dwp.gov.uk/webapi/rest/v1/table"

query <- list(database = unbox("str:database:PIP_Monthly"),
              measures = "str:count:PIP_Monthly:V_F_PIP_MONTHLY",
              dimensions = c("str:field:PIP_Monthly:V_F_PIP_MONTHLY:COA_2011",
                             "str:field:PIP_Monthly:F_PIP_DATE:DATE2") %>% matrix(),
              recodes = list(
                `str:field:PIP_Monthly:V_F_PIP_MONTHLY:COA_2011` = list(
                  map = as.list(paste0("str:value:PIP_Monthly:V_F_PIP_MONTHLY:COA_2011:V_C_MASTERGEOG11_MSOA_TO_LA:E0", seq(2001259, 2001286, 1)))),
                `str:field:PIP_Monthly:F_PIP_DATE:DATE2` = list(
                  map = as.list(paste0("str:value:PIP_Monthly:F_PIP_DATE:DATE2:C_PIP_DATE:",c(201901,201902,201903,201904,201905,201906,201907,201908,201909,201910,201911,201912,202001))))
              )) %>% toJSON()

request <- POST(
  url = path,
  body = query,
  config = add_headers(APIKey = api_key),
  encode = "json")

response <- fromJSON(content(request, as = "text"), flatten = TRUE)

# extract list items and convert to a dataframe
tabnames <- response$fields$items %>% map(~.$labels %>% unlist)
values <- response$cubes[[1]]$values
dimnames(values) <- tabnames

df <- as.data.frame.table(values, stringsAsFactors = FALSE) %>%
  as_tibble() %>% 
  set_names(c(response$fields$label,"value")) %>%
  left_join(lookup%>%select(msoa11cd,msoa11nm,msoa11hclnm), by = c("National - Regional - LA - OAs" = "msoa11nm")) %>%
  mutate(indicator="Personal Independence Payment", 
         measure="count", unit="persons", 
         period=format(as.yearmon(sub(".*(?:\\((.*)\\)).*|.*", "\\1",df$Month), "%b-%y"),"%Y-%m")) %>%
  select(area_code=msoa11cd, area_name = msoa11hclnm, indicator, period, measure, unit, value)

write_csv(df, "personal_independence.csv")
