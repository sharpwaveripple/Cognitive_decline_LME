library(lme4)
library(ggplot2)
library(mediation)



setwd("../temp/")
df = read.csv("../data/RUNDMC_data_long.csv", sep=";", dec=",")
df.wide = read.csv("../data/RUNDMC_data_wide.csv", sep=";", dec=",")
df$age06mc <- df$age06 - mean(df$age06)
df$agesq <- df$age06mc^2
df$sex <- factor(df$sex)
df$rundmcs <- factor(df$rundmcs)
df$gmvnohv <- (df$gmv - df$hv)/100


#### Mediation analyses ####


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

model.m2 <- lmer(hv ~ log(wmh) + time + timesqrt 
                 + age06mc + sex + (1+time|rundmcs), 
                 data=df.incl.mem, REML=FALSE, na.action=na.exclude)
model.y2 <- lmer(memory ~ log(wmh) + hv + time + timesqrt 
                 + age06mc + sex + (1+time|rundmcs), 
                 data=df.incl.mem, REML=FALSE, na.action=na.exclude)
mediate2.out <- mediate(model.m2, model.y2, treat = "log(wmh)", mediator = "hv")
summary(model.m2)
summary(model.y2)
summary(mediate2.out)


# Memory - Time effect
variables.mem <- c("wmh", "hv", "memory")
df.incl.mem <- df[complete.cases(df[variables.mem]),]

model.m3 <- lmer(hv ~ log(wmh) + time + timesqrt + log(wmh):time + log(wmh):timesqrt
                 + age06mc + sex + (1+time|rundmcs), 
                 data=df.incl.mem, REML=FALSE, na.action=na.exclude)
model.y3 <- lmer(memory ~ log(wmh) + hv + time + timesqrt + log(wmh):time + log(wmh):timesqrt
                 + age06mc + sex + (1+time|rundmcs), 
                 data=df.incl.mem, REML=FALSE, na.action=na.exclude)
mediate3.out <- mediate(model.m3, model.y3, treat = "log(wmh)", mediator = "hv")
summary(model.m3)
summary(model.y3)
summary(mediate3.out)





### WMH -> HA -> Memory decline ####

# Define individual hippocampal atrophy slopes (ha)
model.ha <- lmer(hv ~  time + age06mc + sex + log(wmh) + (1+time|rundmcs), 
                 data=df, REML=FALSE, na.action=na.exclude)
summary(model.ha)
coef(summary(model.ha))[ , "Estimate"]
ha.slopes <- coef(model.ha)$rundmcs
ha <- ha.slopes$time
df.wide$ha <- ha

# Mediation analysis on cross-sectional data
# Memory - wmh -> ha -> memory
variables.mem <- c("wmh06", "ha", "memory0615")
df.wide.incl.mem <- df.wide[complete.cases(df.wide[variables.mem]),]


model.m10 <- lm(ha ~ log(wmh06) + Age_2006 + Sex, 
                 data=df.wide.incl.mem, na.action=na.exclude)
model.y10 <- lm(memory0615 ~ log(wmh06) + ha + Age_2006 + Sex, 
                 data=df.wide.incl.mem, na.action=na.exclude)
mediate10.out <- mediate(model.m10, model.y10, treat = "log(wmh06)", mediator = "ha")
summary(model.m10)
summary(model.y10)
summary(mediate10.out)
# Klopt niet zo. Geen significant mediation-effect. 







