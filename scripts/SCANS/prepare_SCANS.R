library(tidyverse)
library(reshape2)

df <- read.csv("../data/SCANS_hv_wide.csv")
names(df)[2:5] <- c(0, 1, 2, 3)
dfLong <- melt(df, id.vars="ID", variable.name = "TP", value.name = "hv")
dfLong$TP <- as.integer(dfLong$TP)
scans <- read.csv("../data/scans_covariates.csv")

full <- merge(dfLong, scans, c("ID", "TP"))
full <- full[order(full$ID),]
full$age <- full$BSL_age + full$time_scan

write.csv(full, "../data/scans_full.csv", row.names=F)
