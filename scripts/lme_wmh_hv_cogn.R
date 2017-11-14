library(lme4)
library(ggplot2)
library(mediation)

setwd("G:/Documenten_Esther/OiO/Cambridge/RUNDMC_LGM/RUNDMC_LGM/data")

df.long = read.csv("RUNDMC_data_long.csv", sep=";", dec=",")

###############################
# Spaghetti plots
###############################

wmh.hv.plot <- ggplot(df.long, aes(x=log(wmh), y=hv, group=rundmcs)) +
  geom_line() +
  geom_point() +
  stat_smooth(aes(group=1), method=lm, colour="#cc0000", fill="#c3c3c3", alpha=0.6) +
  theme_bw(base_size=22)
plot(wmh.hv.plot)
lm.wmh.hv = lm(hv ~ log(wmh), data=df.long)
lm.wmh.hv.corr = lm(hv ~ log(wmh) + age06c + sex, data=df.long)
summary(lm.wmh.hv)
summary(lm.wmh.hv.corr)


time.cognind.plot <- ggplot(df.long, aes(x=time, y=cognitiveindex, group=rundmcs)) +
  geom_line() +
  geom_point() +
  stat_smooth(aes(group=1), method=lm, colour="#cc0000", fill="#c3c3c3", alpha=0.6) +
  theme_bw(base_size=22)
plot(time.cognind.plot)

age.cognind.plot <- ggplot(df.long, aes(x=age, y=cognitiveindex, group=rundmcs)) +
  geom_line() +
  geom_point() +
  stat_smooth(aes(group=1), method=lm, colour="#cc0000", fill="#c3c3c3", alpha=0.6) +
  theme_bw(base_size=22)
plot(age.cognind.plot)

###############################
# LME - WMH*HV
###############################

### WMH ###

# Does the WMH-model improve when timesqrt is added?
m0.wmh = lmer(log(wmh) ~ time + age06c + sex + (1 + time|rundmcs),
                      data=df.long, REML=FALSE, na.action=na.exclude)
m1.wmh = lmer(log(wmh) ~ time + timesqrt + age06c + sex + (1 + time|rundmcs),
              data=df.long, REML=FALSE, na.action=na.exclude)
summary(m0.wmh)
summary(m1.wmh)
anova(m1.wmh, m0.wmh)
## The WMH-model improves significantly when timesqrt is added
## WMH progression occurs quadraticly over time


### HV ###

# Does the HV-model improve when timesqrt is added?
m0.hv = lmer(hv ~ time + age06c + sex + (1 + time|rundmcs),
              data=df.long, REML=FALSE, na.action=na.exclude)
m1.hv = lmer(hv ~ time + timesqrt + age06c + sex + (1 + time|rundmcs),
              data=df.long, REML=FALSE, na.action=na.exclude)
summary(m0.hv)
summary(m1.hv)
anova(m1.hv, m0.hv)
## The HV-model does NOT significantly improve when timesqrt is added to the model
## HV atrophy occurs linearly


### COGNITIVE INDEX ###

# Does the CognIndex-model improve when timesqrt is added?
m0.cognind = lmer(cognitiveindex ~ time + age06c + sex + (1 + time|rundmcs),
             data=df.long, REML=FALSE, na.action=na.exclude)
m1.cognind = lmer(cognitiveindex ~ time + timesqrt + age06c + sex + (1 + time|rundmcs),
             data=df.long, REML=FALSE, na.action=na.exclude)
summary(m0.cognind)
summary(m1.cognind)
anova(m1.cognind, m0.cognind)
## The CognIndex-model does NOT significantly improve when timesqrt is added to the model
## Decline of Cognitive Index does not occur linearly


### COGNITIVE INDEX & WMH ###

# Random intercept & slope model
m0.wmh.cognind = lmer(cognitiveindex ~ log(wmh) + time + timesqrt + age06c + sex + (1 + time|rundmcs), 
                    data=df.long, REML=FALSE, na.action=na.exclude)
summary(m0.wmh.cognind)
confint(m0.wmh.cognind)
# WMH is significantly associated with cognitive index

# Model with interaction term of WMH*time
m1.wmh.cognind = lmer(cognitiveindex ~ log(wmh)*time + log(wmh)*timesqrt + age06c + sex + (1 + time|rundmcs),
                      data=df.long, REML=FALSE, na.action=na.exclude)
summary(m1.wmh.cognind)
confint(m1.wmh.cognind)
anova(m1.wmh.cognind, m0.wmh.cognind)
## The CognIndex-Model significantly improved when interaction term of WMH*time was added to the model
# However, the slope of WMH is not significantly associated with cognitive index (95%CI of wmh+wmh:time includes 0)


### COGNITIVE INDEX & HV ###

# Model HV
m0.hv.cognind = lmer(cognitiveindex ~  hv + time + timesqrt + age06c + sex + (1 + time|rundmcs), 
                     data=df.long, REML=FALSE, na.action=na.exclude)
summary(m0.hv.cognind)

m1.hv.cognind = lmer(cognitiveindex ~  hv*time + timesqrt + age06c + sex + (1 + time|rundmcs), 
                     data=df.long, REML=FALSE, na.action=na.exclude)
summary(m1.hv.cognind)
confint(m1.hv.cognind)
anova(m1.hv.cognind, m0.hv.cognind)
## The CognIndex-Model significantly improved when interaction term of HV*time was added to the model
# The slope of HV is significantly associated with cognitive index (95%CI of hv+hv:time does not include 0)


### COGNITIVE INDEX & WMH*HV ###

# Random intercept & slope model wmh + hv
m0.wmh.hv.cognind = lmer(cognitiveindex ~ log(wmh)*time + log(wmh)*timesqrt + hv*time
                         + age06c + sex + (1 + time|rundmcs),
                         data=df.long, REML=FALSE, na.action=na.exclude)
summary(m0.wmh.hv.cognind)

# Random intercept & slope model with interaction wmh*hv
m1.wmh.hv.cognind = lmer(cognitiveindex ~ log(wmh)*time + log(wmh)*timesqrt + hv*time + log(wmh)*hv
                         + age06c + sex + (1 + time|rundmcs),
                         data=df.long, REML=FALSE, na.action=na.exclude)
summary(m1.wmh.hv.cognind)
confint(m1.wmh.hv.cognind)
anova(m1.wmh.hv.cognind, m0.wmh.hv.cognind)
## The CognIndex-Model significantly improved when the interaction term of WMH*HV was added to the model (p=0.005786)


### Interaction WMH*HV & Adjustments for age, sex, tbv ###
# Random intercept & slope model with interaction wmh*hv
m0.wmh.hv.cognind.unadj = lmer(cognitiveindex ~ log(wmh)*time + log(wmh)*timesqrt + hv*time
                         + (1 + time|rundmcs),
                         data=df.long, REML=FALSE, na.action=na.exclude)
m1.wmh.hv.cognind.unadj = lmer(cognitiveindex ~ log(wmh)*time + log(wmh)*timesqrt + hv*time + log(wmh)*hv
                         + (1 + time|rundmcs),
                         data=df.long, REML=FALSE, na.action=na.exclude)

m0.wmh.hv.cognind.adj1 = lmer(cognitiveindex ~ log(wmh)*time + log(wmh)*timesqrt + hv*time
                               + age06c + sex + (1 + time|rundmcs),
                               data=df.long, REML=FALSE, na.action=na.exclude)
m1.wmh.hv.cognind.adj1 = lmer(cognitiveindex ~ log(wmh)*time + log(wmh)*timesqrt + hv*time + log(wmh)*hv
                               + age06c + sex + (1 + time|rundmcs),
                               data=df.long, REML=FALSE, na.action=na.exclude)

m0.wmh.hv.cognind.adj2 = lmer(cognitiveindex ~ log(wmh)*time + log(wmh)*timesqrt + hv*time
                               + age06c + sex + tbv + (1 + time|rundmcs),
                               data=df.long, REML=FALSE, na.action=na.exclude)
m1.wmh.hv.cognind.adj2 = lmer(cognitiveindex ~ log(wmh)*time + log(wmh)*timesqrt + hv*time + log(wmh)*hv
                               + age06c + sex + tbv + (1 + time|rundmcs),
                               data=df.long, REML=FALSE, na.action=na.exclude)

anova(m1.wmh.hv.cognind.unadj, m0.wmh.hv.cognind.unadj)
anova(m1.wmh.hv.cognind.adj1, m0.wmh.hv.cognind.adj1)
anova(m1.wmh.hv.cognind.adj2, m0.wmh.hv.cognind.adj2)

## The CognIndex-Model significantly improved when the interaction term of WMH*HV was added to the model 
# Unadjusted: p=0.0009965
# Adjusted for age & sex: p=0.005786
# Adjusted for age & sex & tbv: p=0.01514



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
df.long.incl.cognind <- df.long[complete.cases(df.long[variables.cognind]),]

model.m1 <- lmer(hv ~ log(wmh) + time + age06c + sex + (1+time|rundmcs), 
                 data=df.long.incl.cognind, REML=FALSE, na.action=na.exclude)
model.y1 <- lmer(cognitiveindex ~ log(wmh) + hv + time + age06c + sex + (1+time|rundmcs), 
                 data=df.long.incl.cognind, REML=FALSE, na.action=na.exclude)
mediate1.out <- mediate(model.m1, model.y1, treat = "log(wmh)", mediator = "hv")
summary(model.m1)
summary(model.y1)
summary(mediate1.out)


# WMH -> Hippo -> memory
variables.mem <- c("wmh06", "hv", "memory")
df.long.incl.mem <- df.long[complete.cases(df.long[variables.mem]),]

model.m2 <- lmer(hv ~ log(wmh06) + time + age06c + sex + (1+time|rundmcs), 
                 data=df.long.incl.mem, REML=FALSE, na.action=na.exclude)
model.y2 <- lmer(memory ~ log(wmh06) + hv + time + age06c + sex + (1+time|rundmcs), 
                 data=df.long.incl.mem, REML=FALSE, na.action=na.exclude)
mediate2.out <- mediate(model.m2, model.y2, treat = "log(wmh06)", mediator = "hv")
summary(mediate2.out)

