library(psych)

datafile <- "path_to_file_here"

df <- read.csv(datafile)
corr.mat <- corr.test(df)
summary(corr.mat)
