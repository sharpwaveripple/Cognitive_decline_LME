
#This script is created by Esther van Leijsen dd 08-09-2017

####INDEX####
# Packages & Reading in data
# Correlations
# Longitudinal analyses ## Repeated measures ANOVA


###############################
# Packages & Reading in data
###############################

# Instdeltaing and loading packages and functions
instdelta.packages("ggplot2")
instdelta.packages("lme4")
instdelta.packages("mediation")
instdelta.packages("lavaan")
instdelta.packages("psych")

library(ggplot2)
library(lme4)
library(mediation)
library(lavaan)
library(psych)

# Setting up working directory
#setwd("D:/Esther/Documenten/Werk/Neurologie/LGM_Cambridge")
#getwd()

#Loading table 
#data_rundmc = read.csv("RUNDMC_datasheet.csv", sep=";", dec=",")
#data_rundmc = read.csv("RUNDMC_datasheet.csv", sep=",", dec=".")


datafile <- "D:/Esther/Documenten/Werk/Neurologie/LGM_Cambridge/RUNDMC_datasheet_long.csv"
df <- read.csv(datafile, sep=";", dec=",")


###############################
# Cross-sectional correlations
###############################

variables <- c("wmh06", "hv06", "Age_2006", "Sex", "educationyears", 
               "Depressive_symptoms_2006", "cognitiveindex06_delta",	"verbalmemory06_delta",	
               "visuospatialmemory06_delta",	"immediatememory06_delta",	"delayedmemory06_delta",	
               "workingmemory06_delta",	"psychomotorspeed06_delta",	"fluency06_delta",	
               "responseinhibition06_delta",	"attention06_delta",	"executivefunction06_delta")
df <- df[variables]

# 2006
variables06 <- c("wmh06", "hv06", "Age_2006", "Sex", "educationyears", 
                 "Depressive_symptoms_2006", "cognitiveindex06_delta",	"verbalmemory06_delta",	
                 "visuospatialmemory06_delta",	"immediatememory06_delta",	"delayedmemory06_delta",
                 "workingmemory06_delta",	"psychomotorspeed06_delta",	"fluency06_delta",	
                 "responseinhibition06_delta",	"attention06_delta",	"executivefunction06_delta")
df06 <- df[variables06]

corr06.mat <- corr.test(df06)
summary(corr06.mat)

# 2011
variables11 <- c("wmh11", "hv11", "Age_2011", "Sex", 
                 "educationyears", "Depressive_symptoms_2006", "cognitiveindex11_delta",	
                 "verbalmemory11_delta",	"visuospatialmemory11_delta",	"immediatememory11_delta",	
                 "delayedmemory11_delta",	"workingmemory11_delta",	"psychomotorspeed11_delta",	
                 "fluency11_delta",	"responseinhibition11_delta",	"attention11_delta",	
                 "executivefunction11_delta")
df11 <- df[variables11]

corr11.mat <- corr.test(df11)
summary(corr11.mat)

# 2015
variables15 <- c("wmh15", "hv15", "Age_2015", "Sex", 
                 "educationyears", "Depressive_symptoms_2006", "cognitiveindex15_delta",	
                 "verbalmemory15_delta",	"visuospatialmemory15_delta",	"immediatememory15_delta",
                 "delayedmemory15_delta",	"workingmemory15_delta",	"psychomotorspeed15_delta",	
                 "fluency15_delta",	"responseinhibition15_delta",	"attention15_delta",	
                 "executivefunction15_delta")
df15 <- df[variables15]

corr15.mat <- corr.test(df15)
summary(corr15.mat)

corr06.mat$r


### Tables

df.cross <- as.data.frame(matrix(0, ncol=6, nrow=17))

df.cross[1:17,1] <- round(corr06.mat$r[1:17,1], digits=3)
df.cross[1:17,2] <- round(corr11.mat$r[1:17,1], digits=3)
df.cross[1:17,3] <- round(corr15.mat$r[1:17,1], digits=3)
df.cross[1:17,4] <- round(corr06.mat$r[1:17,2], digits=3)
df.cross[1:17,5] <- round(corr11.mat$r[1:17,2], digits=3)
df.cross[1:17,6] <- round(corr15.mat$r[1:17,2], digits=3)
rownames(df.cross) <- variables
colnames(df.cross) <- c("wmh06", "wmh11", "wmh15", "hv06", "hv11", "hv15")



