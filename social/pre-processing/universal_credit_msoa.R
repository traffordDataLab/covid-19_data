# Universal Credit by MSOA

# Source: DWP
# URL: https://stat-xplore.dwp.gov.uk/webapi/metadata/UC_Monthly/UC_Monthly.html
# Licence: Open Government Licence

# load the necessary R packages
library(tidyverse) ; library(httr) ; library(jsonlite) ; library(zoo)

# House of Commons Library MSOA Names
# URL: https://visual.parliament.uk/msoanames

lookup <- read_csv("https://visual.parliament.uk/msoanames/static/MSOA-Names-Latest.csv") %>%
  filter(Laname == "Trafford")

# API key
api_key <- "65794a30655841694f694a4b563151694c434a68624763694f694a49557a49314e694a392e65794a7063334d694f694a7a644849756333526c6247786863694973496e4e3159694936496d6c7961584d755a32467959326c68636d6c7663304230636d466d5a6d39795a43356e62335975645773694c434a70595851694f6a45314f544d304e4449794e7a5973496d46315a434936496e4e30636935765a47456966512e6b39504530737535384a356a335838363072394e67394e694b4c5f685a76463056697963722d6561714859"

# API endpoint
path <- "https://stat-xplore.dwp.gov.uk/webapi/rest/v1/table"

query <- list(database = unbox("str:database:UC_Monthly"),
              measures = "str:count:UC_Monthly:V_F_UC_CASELOAD_FULL",
              dimensions = c("str:field:UC_Monthly:V_F_UC_CASELOAD_FULL:COA_CODE",
                             "str:field:UC_Monthly:F_UC_DATE:DATE_NAME") %>% matrix(),
              recodes = list(
                `str:field:UC_Monthly:V_F_UC_CASELOAD_FULL:COA_CODE` = list(
                  map = as.list(paste0("str:value:UC_Monthly:V_F_UC_CASELOAD_FULL:COA_CODE:V_C_MASTERGEOG11_MSOA_TO_LA:E0", seq(2001259, 2001286, 1)))),
                `str:field:UC_Monthly:F_UC_DATE:DATE_NAME` = list(
                  map = as.list(paste0("str:value:UC_Monthly:F_UC_DATE:DATE_NAME:C_UC_DATE:",c(201901,201902,201903,201904,201905,201906,201907,201908,201909,201910,201911,201912,202001,202002,202003,202004,202005,202006,202007,202008,202009,202010,202011,202012,202101))))
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
  mutate(indicator="People on Universal Credit", 
         measure="count", unit="persons", 
         period=format(as.yearmon(`Month`, "%B %Y"),"%Y-%m")) %>%
  select(area_code=msoa11cd, area_name = msoa11hclnm, indicator, period, measure, unit, value)

write_csv(df, "../universal_credit_msoa.csv")
