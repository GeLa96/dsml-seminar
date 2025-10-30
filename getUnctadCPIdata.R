
  # Synchronous request

  # UNCTADstat report: Consumer price indices, annual (analytical)
  # Version: 18 Jul. 2023

  # Source: 
  # https://unctadstat.unctad.org/datacentre/dataviewer/US.Cpi_A

# -------------------------------------------------------------------
# getUnctadCPIdata()
# Downloads UNCTADstat CPI Data
#
# Input:
# - years e.g: 2000:2022
# - economies: e.g. c('Austria','Germany','Asia') or country codes, NULL: All Countries
# - clientId: from UNCTAD website
# - clientSecret: from UNCTAD website
# - 
#
# Output:
# - data.table with the selected inputs

# -------------------------------------------------------------------
getUnctadCPIdata <- function(years, economies = NULL, clientId, clientSecret) {

  # Package management
  if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
  pacman::p_load(data.table, utils, curl, countrycode)
  
  # Define the path of the csv file that stores the data returned by the API
  tempFile <- tempfile(fileext = ".csv.gz")

  # Year filter
  filterString <- sprintf("Year in (%s)", paste(years, collapse = ","))

  # Economy matching country names and create filter
  if (!is.null(economies) && length(economies) > 0){
    country_code <- countrycode::countrycode(economies, origin = 'country.name', destination = 'iso3n', warn = TRUE)
  }

  if (!is.null(economies) && length(economies) > 0) {
    filterString <-  paste(sprintf("Economy/Code in ('%s')", paste(unique((sprintf('%03d', country_code))), collapse = "','")), filterString , sep = ' and ')
  }

  #Define the data center user info
  ClientId <- "8f073f8d-9ba2-4f3c-b53d-818e5f518c5d"
  ClientSecret <- "jXx1stdbj1nxdCpOcYgMR/1UgSLDZQlBfYjTaE25TCQ="


  #Download data as csv and store it as a file
  curlHandle <- curl::new_handle() |>
    curl::handle_setform(
      "$select" = "Economy/ Label ,Year, Annual_average_growth_rate_Value, Annual_average_growth_rate_Footnote, Annual_average_growth_rate_MissingValue",
      "$filter" = filterString,
      "$orderby" = "Economy/Order asc ,Year asc",
      "$compute" = "round(M4017/Value div 1, 2) as Annual_average_growth_rate_Value, M4017/Footnote/Text as Annual_average_growth_rate_Footnote, M4017/MissingValue/Label as Annual_average_growth_rate_MissingValue",
      "$format" = "csv",
      "compress" = "gz"
    ) |>
    curl::handle_setheaders(
      "ClientId"=ClientId,
      "ClientSecret"=ClientSecret)
  
  url <- "https://unctadstat-user-api.unctad.org/US.Cpi_A/cur/Facts?culture=en"

  curl::curl_download(url, tempFile, handle = curlHandle)
  
  #Load downloaded data in a dataframe
  data1 <- utils::read.csv(
    gzfile(tempFile),
    header = TRUE,
    na.strings = "",
    encoding = "UTF-8",
    colClasses = c("character","integer","double","character","character")
  )

  # return output as data.table
  as.data.table(data1)
}

data <- getUnctadCPIdata(years = 2010:2015, "Germany", clientId = ClientId,clientSecret = ClientSecret)
