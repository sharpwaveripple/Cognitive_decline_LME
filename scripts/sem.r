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

#nImps <- 5
#imp_method <- "pmm"
#nIter <- 5
#imp_data = mice(df, m = nImps, method = imp_method, seed = 1105, maxit = nIter, printFlag = FALSE)
#imp_data_means = 0
#for(i in 1:nImps) {
#  imp_data_means = imp_data_means + complete(imp_data,i)
#}
#imp_data_means = imp_data_means/nImps
#
#pca <- prcomp(imp_data_means,
#              center = T,
#              scale = T)
#print(pca)
