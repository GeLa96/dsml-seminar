# fillmiss()
# Interpolation
# Extrapolation for beginning/end

library(data.table)

fillmiss <- function(data) {
    data <- as.data.table(data) # ensure data.table
    data <- data[!is.na(Year)] # drops rows with missing Year
    data[, Year := as.integer(Year)]
    data[, Value := as.numeric(Value)]
    setorder(data, Economy.Code, Year) # order by Economy.Code and Year

    data[, Value := { # runs by group (Economy.Code)
        yea <- Year # Column Year into variable yea
        val <- Value # Column Value into variable val
        obs <- !is.na(val) # logical vector, O if NA, 1 else

        if (sum(obs) < 2) { # not enough data to interpolate
            val # return original values
        } else { # interpolation between points and extrapolation at beg/end
            val <- approx(
                x    = yea[obs], # years with observations
                y    = val[obs], # values with observations
                xout = yea, # defines that we want values for all years
                rule = 2, # 2 = extrapolate at both ends ; 1 = NA at both ends
                ties = "ordered" # if duplicatess for years exists, keeps order(no aggregation)
            )$y # since approx() returns a list, we extract only y component
        }
    }, by = .(Economy.Code)] # applies everything seperatly by Economy.Code groups

    return(data)
}
