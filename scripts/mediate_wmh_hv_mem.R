library(lme4)
library(ggplot2)
library(mediation)
library(lmerTest)


setwd("../temp/")
df = read.csv("../data/RUNDMC_data_long.csv", sep=";", dec=",")
df$age06mc <- df$age06 - mean(df$age06)


###############################
# Mediation analyses
###############################

# 1 Fit models for the mediator and outcome variable and store these models.
# > m <- lm(Mediator ~ Treat + X,data=Data)
# > y <- lm(Outcome ~ Treat + Mediator + X,data=Data)
# 2 Mediation analysis: Feed model objects into the mediate()function. Call a summary of results.
# > m.out<-mediate(m, y, treat = "Treat", mediator = "Mediator")
# > summary(m.out)
# 3 Sensitivity analysis: Feed the output into the medsens() function. Summarize and plot.
# > s.out <- medsens(m.out)
# > summary(s.out)
# > plot(s.out, "rho")
# > plot(s.out, "R2")
# 4 Experimental designs and analysis now also available



# Mediation analyses WMH -> Hippo -> cognitive index
## Note: Preliminary analyses - Have to be checked & improved!!
variables.cognind <- c("wmh", "hv", "cognitiveindex")
df.incl.cognind <- df[complete.cases(df[variables.cognind]),]

model.m1 <- lmer(hv ~ log(wmh) + time + timesqrt + age06c + sex + (1+time|rundmcs), 
                 data=df.incl.cognind, REML=FALSE, na.action=na.exclude)
model.y1 <- lmer(cognitiveindex ~ log(wmh) + hv + time + timesqrt + age06c + sex + (1+time|rundmcs), 
                 data=df.incl.cognind, REML=FALSE, na.action=na.exclude)
mediate1.out <- mediate(model.m1, model.y1, treat = "log(wmh)", mediator = "hv")
summary(model.m1)
summary(model.y1)
summary(mediate1.out)

# Memory
variables.mem <- c("wmh", "hv", "memory")
df.incl.mem <- df[complete.cases(df[variables.mem]),]

model.m2 <- lmer(hv ~ log(wmh) + time + timesqrt + age06c + sex + (1+time|rundmcs), 
                 data=df.incl.mem, REML=FALSE, na.action=na.exclude)
model.y2 <- lmer(memory ~ log(wmh) + hv + time + timesqrt + age06c + sex + (1+time|rundmcs), 
                 data=df.incl.mem, REML=FALSE, na.action=na.exclude)
mediate2.out <- mediate(model.m2, model.y2, treat = "log(wmh)", mediator = "hv")
summary(model.m2)
summary(model.y2)
summary(mediate2.out)