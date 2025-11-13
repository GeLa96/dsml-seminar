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
if (file.exists("secrets.R")) source("secrets.R") # used such that clientId and clientSecret are available but not hard-coded

getUnctadCpiData <- function(years = NULL,
                             economies = NULL, # Selects all economies by default
                             clientId, clientSecret, # sign in to UNCTAD Data Centre to get these
                             growthRate = TRUE) { # selects Annual average growth rate (M4017) not Index base 2010=100 (M6510) by default
  # temp csv gz file
  tmpFile <- tempfile(fileext = ".csv.gz") # UCTAD API supports gz compression

  # build filter String
  filterString <- sprintf(
    "Year in (%s)", # inserts string in template ; %s place holder for string
    paste(years, collapse = ",") # combines selected years in a single string with commas
  )
  # creates something like this "Year in (2000, ... ,2024)"

  # Economy filter if not all (NULL)
  if (!is.null(economies) && length(economies) > 0) { # filters if not NULL and length > 0
    filterString <- paste(
      filterString,
      sprintf(
        "Economy/Code in ('%s')",
        paste(unique(as.character(economies)), # only unique as characters
          collapse = "','" # seperats country codes.
        )
      ),
      sep = " and "
    )
  }
  # creates something like "Year in (2000, ... ,2024) and Economy/Code in ("World")

  # Select and compute strings based on growth-rate
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

  # save results to gz-csv ; curl:: for making internet requests
  handleString <- curl::new_handle() |> # create a HTTP (API) request
    curl::handle_setform( # Attach data files to be sent via HTTP
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
  curl::curl_download(urlString, # where to download from
    tmpFile, # where to save the file locally
    handle = handleString
  ) # connection setting

  # Read back into R
  data <- utils::read.csv( # reads csv into R
    gzfile(tmpFile), # opens compressed file
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

clientId <- "70e80d59-00eb-4e26-b51e-c459d12aa6d8"
clientSecret <- "uFieZejhm91ftgBfktJdgZObAy79BdQeINHAt/h49zY="
cpi_index <- getUnctadCpiData(1950:2040, NULL, clientId, clientSecret, growthRate = TRUE)
head(cpi_index)
View(cpi_index)

# --- variance band (mean ± sd) + lines for World & Europe ----
library(dplyr)
library(ggplot2)

# 1) per-year mean & sd across all economies
stats <- cpi_index %>%
  group_by(Year) %>%
  summarise(
    mean = mean(Annual_average_growth_rate_Value, na.rm = TRUE),
    sd = sd(Annual_average_growth_rate_Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(lo = mean - sd, hi = mean + sd)

# 2) highlight series (World, Europe) — uses labels already returned
hl_df <- cpi_index %>%
  filter(`Economy_Label` %in% c("World", "Europe", "Americas", "Asia", "Oceania")) %>%
  transmute(
    Economy = `Economy_Label`,
    Year,
    value = Annual_average_growth_rate_Value
  )

# 3) plot
p <- ggplot() +
   geom_ribbon(
    data = stats, aes(x = Year, ymin = lo, ymax = hi),
    fill = "grey70", alpha = 0.35
   ) +
  geom_line(
    data = hl_df, aes(x = Year, y = value, color = Economy),
    linewidth = 0.9
  ) +
  labs(x = NULL, y = "CPI YoY (%)", color = NULL) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())

print(p)
# If you want it interactive: plotly::ggplotly(p)
