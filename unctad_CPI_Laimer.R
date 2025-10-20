# getUnctadCpiData()
# Downloads UNCTADstat CPI (analytical) Data
#
# Input:
# - years: e.g. 2000:2024
# - economies: UNCTAD Economy *codes* (e.g. '0000' = World), NULL: All
# - clientId: from UNCTAD Data Centre
# - clientSecret: from UNCTAD Data Centre
# - growthRate: TRUE = Annual avg. growth rate (M4017), FALSE = Index base 2010=100 (M6510)
#
# Output:
# - data.table with the selected inputs
#
# Sign in: https://unctadstat.unctad.org/datacentre/
# -------------------------------------------------------------------
# Load API credentials (local only, ignored by Git)
if (file.exists("secrets.R")) source("secrets.R") # used such that clientId and clientSecret are availabe but not hard-coded

getUnctadCpiData <- function(years = 2000:2024,
                             economies = NULL, # Selects all economies by default
                             clientId, clientSecret, # sign in to UNCTAD Data Centre to get these
                             growthRate = TRUE) { # selects Annual average growth rate (M4017) not Index base 2010=100 (M6510) by default
  # temp csv gz file
  tmpFile <- tempfile(fileext = ".csv.gz") # UCTAD API supports gz compression
  
  # build filter String
  filterString <- sprintf(
    "Year in (%s)", # inserts string in template ; %s place holder for string
    paste(years, collapse = ",") # combines slected years in a single string with commas
  )
  # Economy filter if not all (NULL)
  if (!is.null(economies) && length(economies) > 0) { # filters if not NULL and length > 0
    filterString <- paste(
      filterString,
      sprintf(
        "Economy/Code in ('%s')",
        paste(unique(as.character(economies)), # only unique as characters
              collapse = "','" # seperates country codes.
        )
      ),
      sep = " and "
    )
  }
  
  # Select and compute strings based on growthRate
  if (growthRate) {
    # Annual average growth rate (M4017)
    selectString <- "Economy/Label ,Year ,Annual_average_growth_rate_Value ,Annual_average_growth_rate_Footnote ,Annual_average_growth_rate_MissingValue"
    computeString <- "round(M4017/Value div 1, 2) as Annual_average_growth_rate_Value, M4017/Footnote/Text as Annual_average_growth_rate_Footnote, M4017/MissingValue/Label as Annual_average_growth_rate_MissingValue"
  } else {
    # Index, base 2010 = 100 (M6510)
    selectString <- "Economy/Label ,Year ,Index_Base_2010_Value ,Index_Base_2010_Footnote ,Index_Base_2010_MissingValue"
    computeString <- "round(M6510/Value div 1, 2) as Index_Base_2010_Value, M6510/Footnote/Text as Index_Base_2010_Footnote, M6510/MissingValue/Label as Index_Base_2010_MissingValue"
  }
  
  # url String for CPI
  urlString <- "https://unctadstat-user-api.unctad.org/US.Cpi_A/cur/Facts?culture=en"
  
  # save results to gz-csv ; curl:: for making internt requests
  handleString <- curl::new_handle() |> # create a HTTP (API) request
    curl::handle_setform( # Attach data fiels to be sent via HTTP
      "$select"  = selectString, # select columns
      "$filter"  = filterString, # apply conditions
      "$orderby" = "Economy/Order asc ,Year asc", # order by economy and year ascending
      "$compute" = computeString, # create processed output columns (computation on the server)
      "$format"  = "csv", # output format
      "compress" = "gz" # compress output
    ) |>
    curl::handle_setheaders( # without it, API returns 401 Unauthorized
      "ClientId"     = clientId,
      "ClientSecret" = clientSecret
    )
  # send request and download file
  curl::curl_download(urlString, tmpFile, handle = handleString)
  
  # Read back into R
  data <- utils::read.csv( # reads csv into R
    gzfile(tmpFile), # opens copressed file
    header     = TRUE, # first row as header
    na.strings = "", # empty strings as NA
    encoding   = "UTF-8", # UTF-8 encoding (special characters in country names)
    colClasses = c("character", "integer", "double", "character", "character") # specify column types
  )
  
  # return output as data.table (GDP template returns data.table)
  data.table::as.data.table(data) # converts output to data.table
}
# "0000" = World
# "5400" = Europe

clientId <- "..."
clientSecret <- "..."
cpi_index <- getUnctadCpiData(2000:2022, "5400", clientId, clientSecret, growthRate = TRUE)
head(cpi_index)
