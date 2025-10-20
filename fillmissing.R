# fillmiss()
# Interpolation
# Extrapolation for beginning/end

library(data.table)

fillmiss <- function(data) {
    data <- as.data.table(data) # ensure data.table
    data <- data[!is.na(Year)] # drops rows with missing Year
    setorder(data, Economy.Code, Year) # order by Economy.Code and Year

    data[, Value := { # runs by group (Economy.Code)
        yea <- Year # Column Year into variable yea
        val <- Value # Column Value into variable val
        obs <- !is.na(y) # logical vector, O if NA, 1 else

        if (sum(obs) < 2) { # not enough data to interpolate
            val # return original values
        } else { # interpolation between points and extrapolation at beg/end
            approx(yea = yea[obs], )
            # Interpolation
            val <- approx(yea[obs], val[obs], xout = yea, rule = 2)$y

            val # return filled values
        }
    }]
}
