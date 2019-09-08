# Converts a CANSIM dataset I downloaded to monthly
yields_to_monthly <- function(yield_data) {
    
    # impute 0.00 with NA because a yield of 0.00 doesn't make sense
    yield_data[,2][which(yield_data[,2]==0.00)] <- NA
    names(yield_data)[1] <- "DATE"
    yield_data$DATE <- as.Date(yield_data$DATE)
    
    # remove NA rows
    yield_data <- yield_data[!is.na(yield_data[,2]),]
    row.names(yield_data) <- 1:(length(yield_data[,2]))
    months <- unique(format(yield_data$DATE, "%Y%m"))
    dates <- character(0)
    means <- numeric(0)
    for(month in months) {
        index <- which(format(yield_data$DATE, "%Y%m") == month)
        rowData <- yield_data[,2][index]
        means <- c(means, mean(rowData))
        dates <- c(dates, month)
    }    
    yield_data_monthly <- data.frame(DATE=dates, YIELD=means)
    yield_data_monthly$YIELD <- yield_data_monthly$YIELD/100 # decimal
    yield_data_monthly
}

Treasury_Bills_1_month <- read.csv("Canada_Treasury_Bills_1_month.csv")
Treasury_Bills_1_year <- read.csv("Canada_Treasury_Bills_1_year.csv")
Bond_Yields_2_year <- read.csv("Canada_Benchmark_Bond_Yields_2_year.csv")
Bond_Yields_3_year <- read.csv("Canada_Benchmark_Bond_Yields_3_year.csv")
Bond_Yields_5_year <- read.csv("Canada_Benchmark_Bond_Yields_5_year.csv")
Bond_Yields_7_year <- read.csv("Canada_Benchmark_Bond_Yields_7_year.csv")
Bond_Yields_10_year <- read.csv("Canada_Benchmark_Bond_Yields_10_year.csv")

Treasury_Bills_1_month_monthly <- yields_to_monthly(Treasury_Bills_1_month)
Treasury_Bills_1_year_monthly <- yields_to_monthly(Treasury_Bills_1_year)
Bond_Yields_2_year_monthly <- yields_to_monthly(Bond_Yields_2_year)
Bond_Yields_3_year_monthly <- yields_to_monthly(Bond_Yields_3_year)
Bond_Yields_5_year_monthly <- yields_to_monthly(Bond_Yields_5_year)
Bond_Yields_7_year_monthly <- yields_to_monthly(Bond_Yields_7_year)
Bond_Yields_10_year_monthly <- yields_to_monthly(Bond_Yields_10_year)

rm(Treasury_Bills_1_month, Treasury_Bills_1_year, Bond_Yields_2_year,
   Bond_Yields_3_year, Bond_Yields_5_year, Bond_Yields_10_year)

yields_monthly <- data.frame(DATE=Treasury_Bills_1_month_monthly$DATE,
    ONEMONTH=Treasury_Bills_1_month_monthly$YIELD,
    ONEYEAR=Treasury_Bills_1_year_monthly$YIELD,
    TWOYEAR=Bond_Yields_2_year_monthly$YIELD,
    THREEYEAR=Bond_Yields_3_year_monthly$YIELD,
    FIVEYEAR=Bond_Yields_5_year_monthly$YIELD,
    SEVENYEAR=Bond_Yields_7_year_monthly$YIELD,
    TENYEAR=Bond_Yields_10_year_monthly$YIELD)
                        
par(mfrow=c(3,2))
yaxis_titles <- names(yields_monthly)[2:7]
for(i in 1:6) {
    plot(ts(yields_monthly[,i+1], start = 1995, end = 2015, frequency = 12),
        ylab=yaxis_titles[i], ylim=c(0,0.10))
}

write.csv(yields_monthly, file="~/Desktop/Bond_Yields/yields_monthly.csv", row.names=F)
