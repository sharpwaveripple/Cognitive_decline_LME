#sem.r
library(mice)
library(lavaan)

wd <- getwd()
datafile <- paste(wd, "RUNDMC_datasheet_long.csv", sep="/")
df <- read.csv(datafile, sep=";", dec=",")

modelfile <- paste(wd, "model.lav", sep="/")
model <- readLines(modelfile)

variables06 <- c("wmh06", "HV_06_ml_norm", "cognitiveindex06_all", 
                 "verbalmemory06_all",	"visuospatialmemory06_all",	
                 "immediatememory06_all",	"delayedmemory06_all",	"workingmemory06_all",	
                 "psychomotorspeed06_all",	"fluency06_all",	"responseinhibition06_all",	
                 "attention06_all",	"executivefunction06_all")
df06 <- df[variables06]

fit <- sem(model,
           data = df06,
           missing = "ML",
           meanstructure = T)
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
