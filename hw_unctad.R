# getUnctadCPIData()
# Downloads UNCTADstat CPI Data
#
# Input:
# - years e.g: 2000:2024
# - economies: c('Austria','Germany','Asia'), NULL: All Countries
# - clientId: from UNCAD website
# - clientSecret: from UNCAD website
# - direction: 1:inward, 1:outward
# - flowStock: "08": Flow, "09": Stock
# Output:
# - data.table with the selected inputs

# -------------------------------------------------------------------
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
if (file.exists("secrets.R")) source("secrets.R")

getUnctadCpiData <- function(years = 2000:2024,
                             economies = NULL,
                             clientId, clientSecret,
                             growthRate = TRUE) {
    # temp csv gz file
    tmpFile <- tempfile(fileext = ".csv.gz")

    # build filter String
    filterString <- sprintf("Year in (%s)", paste(years, collapse = ","))

    # Economy if not all (NULL)
    if (!is.null(economies) && length(economies) > 0) {
        filterString <- paste(
            filterString,
            sprintf(
                "Economy/Code in ('%s')",
                paste(unique(as.character(economies)), collapse = "','")
            ),
            sep = " and "
        )
    }

    # select/compute (match template style; 2 decimals as requested)
    if (growthRate) {
        # Annual average growth rate (M4017)
        selectString <- "Economy/Label ,Year ,Annual_average_growth_rate_Value ,Annual_average_growth_rate_Footnote ,Annual_average_growth_rate_MissingValue"
        computeString <- "round(M4017/Value div 1, 2) as Annual_average_growth_rate_Value, M4017/Footnote/Text as Annual_average_growth_rate_Footnote, M4017/MissingValue/Label as Annual_average_growth_rate_MissingValue"
    } else {
        # Index, base 2010 = 100 (M6510)
        selectString <- "Economy/Label ,Year ,Index_Base_2010_Value ,Index_Base_2010_Footnote ,Index_Base_2010_MissingValue"
        computeString <- "round(M6510/Value div 1, 2) as Index_Base_2010_Value, M6510/Footnote/Text as Index_Base_2010_Footnote, M6510/MissingValue/Label as Index_Base_2010_MissingValue"
    }

    # url String
    urlString <- "https://unctadstat-user-api.unctad.org/US.Cpi_A/cur/Facts?culture=en"

    # save results to gz-csv
    handleString <- curl::new_handle() |>
        curl::handle_setform(
            "$select"  = selectString,
            "$filter"  = filterString,
            "$orderby" = "Economy/Order asc ,Year asc",
            "$compute" = computeString,
            "$format"  = "csv",
            "compress" = "gz"
        ) |>
        curl::handle_setheaders(
            "ClientId"     = clientId,
            "ClientSecret" = clientSecret
        )

    curl::curl_download(urlString, tmpFile, handle = handleString)

    # Read back into R
    data <- utils::read.csv(
        gzfile(tmpFile),
        header     = TRUE,
        na.strings = "",
        encoding   = "UTF-8",
        colClasses = c("character", "integer", "double", "character", "character")
    )

    # return output as data.table (GDP template returns data.table)
    data.table::as.data.table(data)
}
# "0000" = World
# "5400" = Europe

clientId <- "70e80d59-00eb-4e26-b51e-c459d12aa6d8"
clientSecret <- "uFieZejhm91ftgBfktJdgZObAy79BdQeINHAt/h49zY="
cpi_index <- getUnctadCpiData(2000:2022, "5400", clientId, clientSecret, growthRate = TRUE)
head(cpi_index)
