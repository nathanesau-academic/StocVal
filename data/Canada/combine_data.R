# Combine financial data
setwd("~/Dropbox/Research/StocVal/data/Canada")
stocks <- read.csv("stocks.csv")
yields <- read.csv("yields.csv")
inflation <- read.csv("inflation.csv")

varinput <- data.frame(
    date=inflation$Date,
    onemonth=yields$Cont_one_month,
    oneyear=yields$Cont_one_year,
    twoyear=yields$Cont_two_year,
    threeyear=yields$Cont_three_year,
    fiveyear=yields$Cont_five_year,
    sevenyear=yields$Cont_seven_year,
    tenyear=yields$Cont_ten_year,
    stock=stocks$Cont_monthly,
    inflation=inflation$Annual_inflation)

# Write to varinput_canada.Rda
saveRDS(varinput, file="~/Dropbox/Research/StocVal/data/Canada/varinput_canada.Rda")
