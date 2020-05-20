# People on Universal Credit
# Source: Department for Work and Pensions ; Stat-Xplore
# URL: https://stat-xplore.dwp.gov.uk/webapi/metadata/UC_Monthly/UC_Monthly.html

library(tidyverse) ; library(httr) ; library(jsonlite) ; library(zoo)

# API key
api_key <- read_lines("stat-xplore_key.txt")

# API endpoint
path <- "https://stat-xplore.dwp.gov.uk/webapi/rest/v1/table"

# JSON query
query <- '{
  "database" : "str:database:UC_Monthly",
  "measures" : [ "str:count:UC_Monthly:V_F_UC_CASELOAD_FULL" ],
  "recodes" : {
    "str:field:UC_Monthly:F_UC_DATE:DATE_NAME" : {
      "map" : [ [ "str:value:UC_Monthly:F_UC_DATE:DATE_NAME:C_UC_DATE:201901" ], [ "str:value:UC_Monthly:F_UC_DATE:DATE_NAME:C_UC_DATE:201902" ], [ "str:value:UC_Monthly:F_UC_DATE:DATE_NAME:C_UC_DATE:201903" ], [ "str:value:UC_Monthly:F_UC_DATE:DATE_NAME:C_UC_DATE:201904" ], [ "str:value:UC_Monthly:F_UC_DATE:DATE_NAME:C_UC_DATE:201905" ], [ "str:value:UC_Monthly:F_UC_DATE:DATE_NAME:C_UC_DATE:201906" ], [ "str:value:UC_Monthly:F_UC_DATE:DATE_NAME:C_UC_DATE:201907" ], [ "str:value:UC_Monthly:F_UC_DATE:DATE_NAME:C_UC_DATE:201908" ], [ "str:value:UC_Monthly:F_UC_DATE:DATE_NAME:C_UC_DATE:201909" ], [ "str:value:UC_Monthly:F_UC_DATE:DATE_NAME:C_UC_DATE:201910" ], [ "str:value:UC_Monthly:F_UC_DATE:DATE_NAME:C_UC_DATE:201911" ], [ "str:value:UC_Monthly:F_UC_DATE:DATE_NAME:C_UC_DATE:201912" ], [ "str:value:UC_Monthly:F_UC_DATE:DATE_NAME:C_UC_DATE:202001" ], [ "str:value:UC_Monthly:F_UC_DATE:DATE_NAME:C_UC_DATE:202002" ], [ "str:value:UC_Monthly:F_UC_DATE:DATE_NAME:C_UC_DATE:202003" ], [ "str:value:UC_Monthly:F_UC_DATE:DATE_NAME:C_UC_DATE:202004" ] ],
      "total" : false
    },
    "str:field:UC_Monthly:V_F_UC_CASELOAD_FULL:JCP_OFFICE_CODE" : {
      "map" : [ [ "str:value:UC_Monthly:V_F_UC_CASELOAD_FULL:JCP_OFFICE_CODE:C_UC_JCP_OFFICE_FULL:ALU" ], [ "str:value:UC_Monthly:V_F_UC_CASELOAD_FULL:JCP_OFFICE_CODE:C_UC_JCP_OFFICE_FULL:STY" ] ],
      "total" : false
    }
  },
  "dimensions" : [ [ "str:field:UC_Monthly:V_F_UC_CASELOAD_FULL:JCP_OFFICE_CODE" ], [ "str:field:UC_Monthly:F_UC_DATE:DATE_NAME" ] ]
}'

# submit the API request
request <- POST(
  url = path,
  body = query,
  config = add_headers(APIKey = api_key),
  encode = "json")

# check for any server error
request$status_code

# parse the response
response <- fromJSON(content(request, as = "text"), flatten = TRUE)

# extract list items and convert to a dataframe
dimnames <- response$fields$items %>% map(~.$labels %>% unlist)
values <- response$cubes[[1]]$values
dimnames(values) <- dimnames
df <- as.data.frame.table(values, stringsAsFactors = FALSE) %>%
  as_tibble() %>% 
  set_names(c(response$fields$label,"value")) %>% 
  mutate(Month = as.Date(as.yearmon(Month))) %>% 
  pivot_wider(names_from = `Jobcentre Plus`, values_from = value)

# write results as CSV
write_csv(df, "../univeral_credit.csv")