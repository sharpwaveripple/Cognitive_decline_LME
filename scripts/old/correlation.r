library(psych)

variables <- c("log_WMH_2006","log_WMH_2006")

datafile <- "../data/RUNDMC_datasheet_long.csv"
df <- read.csv(datafile)
print(names(df))
