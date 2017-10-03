library(mice)
library(lavaan)

wd <- getwd()
datafile <- paste(wd, "data_for_R.csv", sep="/")
modelfile <- paste(wd, "model.lav", sep="/")

df <- read.csv(datafile)
model <- readLines(modelfile)

fit <- sem(model,
           data = df,
           missing = "ML",
           meanstructure = T)
fitMeasures(fit)

