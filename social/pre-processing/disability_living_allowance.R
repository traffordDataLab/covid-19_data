#Disability Living Allowance (cases in payment)

# Source: DWP, Stat-Explore
# URL: https://stat-xplore.dwp.gov.uk/webapi/metadata/DLA_In_Payment_New/DLA_In_Payment_New.html
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

query <- list(database = unbox("str:database:DLA_In_Payment_New"),
              measures = "str:count:DLA_In_Payment_New:V_F_DLA_In_Payment_New",
              dimensions = c("str:field:DLA_In_Payment_New:V_F_DLA_In_Payment_New:COA_CODE",
                             "str:field:DLA_In_Payment_New:F_DLA_QTR_New:DATE_NAME") %>% matrix(),
              recodes = list(
                `str:field:DLA_In_Payment_New:V_F_DLA_In_Payment_New:COA_CODE` = list(
                  map = as.list(paste0("str:value:DLA_In_Payment_New:V_F_DLA_In_Payment_New:COA_CODE:V_C_MASTERGEOG11_MSOA_TO_LA:E0", seq(2001259, 2001286, 1)))),
                `str:field:DLA_In_Payment_New:F_DLA_QTR_New:DATE_NAME` = list(
                  map = as.list(paste0("str:value:DLA_In_Payment_New:F_DLA_QTR_New:DATE_NAME:C_DLA_QTR_New:",c(201805,201808,201811,201902,201905,201908,201911))))
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
  mutate(indicator="Disability Living Allowance (cases in payment)", 
         measure="count", unit="persons", 
         period=format(as.yearmon(`Quarter`, "%b-%y"),"%Y-%m")) %>%
  select(area_code=msoa11cd, area_name = msoa11hclnm, indicator, period, measure, unit, value)

write_csv(df, "disability_living_allowance.csv")
