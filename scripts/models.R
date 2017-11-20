dfLong <- read.csv("../data/RUNDMC_data_long.csv", sep=";", dec=",")
dfLong$age06 <- scale(dfLong$age06, center = T)
dfLong$subject <- as.factor(dfLong$rundmcs)
dfLong$timesq <- dfLong$time^2
dfLong$lnwmh <- log(dfLong$wmh)
dfWide <- read.csv("../data/RUNDMC_data_wide.csv", sep=";", dec=",")

library(lmerTest)
library(sjPlot)
fit <- lmer(cognitiveindex ~ 1 + age06 + time + timesq + (1 + time|subject), data = dfLong)
sjp.lmer(fit, vars = "time", type = "rs.ri")
sjp.lmer(fit, vars = "time", type = "rs.ri", sample.n=15, sort.est = "sort.all")
fit <- lmer(cognitiveindex ~ 1 + age06 + time + timesq + (1 + time|subject), data = dfLong)
summary(fit)
fit1 <- lmer(cognitiveindex ~ 1 + age06 + time + timesq + (1 + time|subject) +
             lnwmh + lnwmh*time, data = dfLong)
fit2 <- lmer(cognitiveindex ~ 1 + age06 + time + timesq + (1 + time|subject) + 
             lnwmh + lnwmh*timesq, data = dfLong)
summary(fit1)
summary(fit2)
anova(fit1, fit2)
fit1 <- lmer(cognitiveindex ~ 1 + age06 + time + timesq + (1 + time|subject) +
             lnwmh + lnwmh*timesq + hv + hv*time, data = dfLong)
fit2 <- lmer(cognitiveindex ~ 1 + age06 + time + timesq + (1 + time|subject) + 
             lnwmh + lnwmh*timesq + hv + hv*time + lnwmh*hv, data = dfLong)

library(mice)
dfTemp <- mice(dfLong)
fit1 <- with(dfTemp, lmer(cognitiveindex ~ 1 + age06 + time + timesq + (1 + time|subject) +
             lnwmh + lnwmh*timesq + hv + hv*time))
fit2 <- with(dfTemp, lmer(cognitiveindex ~ 1 + age06 + time + timesq + (1 + time|subject) + 
             lnwmh + lnwmh*timesq + hv + hv*time + lnwmh*hv))
fit3 <- with(dfTemp, lmer(cognitiveindex ~ 1 + age06 + poly(time, 2) + (1 + time|subject) +
             lnwmh + lnwmh*timesq + hv + hv*time))

mod1 <- with(dfTemp, fit1 <<- lmer(cognitiveindex ~ 1 + age06 + time + timesq + (1 + time|subject) +
                                   lnwmh + lnwmh*timesq + hv + hv*time))
mod2 <- with(dfTemp, fit2 <<- lmer(cognitiveindex ~ 1 + age06 + time + timesq + (1 + time|subject) +
                                   lnwmh + lnwmh*timesq + hv + hv*time + lnwmh*hv))

summary(pool(mod1))
aov(fit1, fit2)
summary(pool(fit1))
summary(pool(fit2))
summary(pool(fit3))
