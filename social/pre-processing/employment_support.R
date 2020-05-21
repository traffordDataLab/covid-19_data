#Employment and Support Allowance (ESA) Caseload. Subset Medical Condition Diseases of the Circulatory System and Diseases of the Respiratory System


# Source: DWP, Stat-Explore
# URL: https://stat-xplore.dwp.gov.uk/webapi/metadata/ESA_Caseload_new/ESA_Caseload_new.html
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

query <- list(database = unbox("str:database:ESA_Caseload_new"),
              measures = "str:count:ESA_Caseload_new:V_F_ESA_NEW",
              dimensions = c("str:field:ESA_Caseload_new:V_F_ESA_NEW:COA_CODE",
                             "str:field:ESA_Caseload_new:F_ESA_QTR_NEW:DATE_NAME",
                             "str:field:ESA_Caseload_new:V_F_ESA_NEW:ICDGP") %>% matrix(),
              recodes = list(
                `str:field:ESA_Caseload_new:V_F_ESA_NEW:COA_CODE` = list(
                  map = as.list(paste0("str:value:ESA_Caseload_new:V_F_ESA_NEW:COA_CODE:V_C_MASTERGEOG11_MSOA_TO_LA:E0", seq(2001259, 2001286, 1)))),
                `str:field:ESA_Caseload_new:F_ESA_QTR_NEW:DATE_NAME` = list(
                  map = as.list(paste0("str:value:ESA_Caseload_new:F_ESA_QTR_NEW:DATE_NAME:C_ESA_QTR_NEW:",c(201805,201808,201811,201902,201905,201908,201911)))),
                `str:field:ESA_Caseload_new:V_F_ESA_NEW:ICDGP` = list(
                  map = list(paste0("str:value:ESA_Caseload_new:V_F_ESA_NEW:ICDGP:C_ESA_ICDGP:",c(9:10))))
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
dimnames(values) <- tabnames[1:2]

df <- as.data.frame.table(values, stringsAsFactors = FALSE) %>%
  as_tibble() %>% 
  set_names(c(response$fields$label,"value")) %>%
  left_join(lookup%>%select(msoa11cd,msoa11nm,msoa11hclnm), by = c("National - Regional - LA - OAs" = "msoa11nm")) %>%
  mutate(indicator="Employment and Support Allowance (respiratory or circulatory medical conditions)",
         measure="count", unit="persons", 
         period=format(as.yearmon(`Quarter`, "%b-%y"),"%Y-%m")) %>%
  select(area_code=msoa11cd, area_name = msoa11hclnm, indicator, period, measure, unit, value)

write_csv(df, "employment_support.csv")
