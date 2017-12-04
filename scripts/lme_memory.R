library(stargazer)
library(lmerTest)

df = read.csv("../data/RUNDMC_data_long.csv", sep=";", dec=",")

# apply data transformations
df$age06s <- scale(df$age06)
df$sex <- factor(df$sex)
df$rundmcs <- factor(df$rundmcs)
df$timesq <- df$time^2
df$lnwmh <- log(df$wmh)
df$gmvnohv <- df$gmv - df$hv

## STEP 1: 
# First, determine the relationships between WMH, HV, and age-related memory
# decline

wmh
