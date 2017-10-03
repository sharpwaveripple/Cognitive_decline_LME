### sem.r
install.packages("mice")
install.packages("semTools")
library(mice)
library(lavaan)
library(semTools)

wd <- getwd()
datafile <- paste(wd, "RUNDMC_datasheet_long.csv", sep="/")
df <- read.csv(datafile, sep=";", dec=",")

modelfile <- paste(wd, "model.lav", sep="/")
model <- readLines(modelfile)

variables06 <- c("wmh06", "hv06", "cognitiveindex06_all", "Inclusion")
df06 <- df[variables06]                 

fit <- sem(model,
           data = df06,
           missing = "ML",
           meanstructure = T, group  = "Inclusion")
fitMeasures(fit)
summary(fit)
standardizedSolution(fit)


fit <- measurementInvariance(model,
           data = df06,
           missing = "ML",
           meanstructure = T, group  = "Inclusion")
fitMeasures(fit)
summary(fit)
standardizedSolution(fit)


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