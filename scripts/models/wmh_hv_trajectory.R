library(lavaan)


MakeLatentStrings <- function(latVar, latVarNames) {

  latSlope <- paste(latVar, "slope", sep="_")
  latInt <- paste(latVar, "int", sep="_")
  timeVec <- character()

  for (times in range(0, length(latVarNames)-1)) {
    timeVec <- c(timeVec, times)
  }

  latSlopeForm <- paste(latSlope, "=~",
                        paste("1*", latVarNames, sep="", collapse=" + "))
  latIntForm <- paste(latInt, "=~",
                        paste("1*", latVarNames, sep="", collapse=" + "))

}


TwoFactorLGM <- function(var1, var1names, var2, var2names) {

  lat1slope <- paste(var1, "slope", sep="_")
  lat1int <- paste(var1, "int", sep="_")
  lat2slope <- paste(var2, "slope", sep="_")
  lat2int <- paste(var2, "int", sep="_")
  lat1form <- paste(lat1slope, "=~", paste("1*", var1names, sep="", collapse=" + "))

  test <- file("../../temp/model.lav")
  writeLines(c("# lat variable definitions",
               lat1form),
             test)
  close(test) 
}

#MTL vols?
volVars <- c("logwmh", "tbv", "gmv", "wmv", "nawm")
hpcVars <- c("lhv", "rhv", "hv")
timepoints <- c("06", "11", "15")
datafile <- "../../data/RUNDMC_datasheet_long.csv"
df <- read.csv(datafile, header=T)

for (i in hpcVars) {
  for (j in volVars) {
    variables <- c(i, j)
    varNames <- c(as.vector(outer(variables, timepoints, paste, sep="")))
    vecLen <- length(varNames)
    hpcNames <- varNames[seq(1, vecLen, 2)]
    volNames <- varNames[seq(2, vecLen, 2)]
    print(hpcNames)
    df.subset <- df[varNames]
    df.subset <- df.subset[complete.cases(df.subset), ]
    TwoFactorLGM(i,hpcNames, j, volNames)
  
  }

}

#df.subset[wmh.names] <- log(df.subset[wmh.names])
#
#model <- readLines("wmh_hv_trajectory.lav")
#fit <- sem(model,
#           data=df.subset)
#summary(fit, standardized=T)
