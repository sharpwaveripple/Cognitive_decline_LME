library(lme4)
library(ggplot2)
library(mediation)
library(lmerTest)


setwd("../temp/")
df = read.csv("../data/RUNDMC_data_long.csv", sep=";", dec=",")
df$age06mc <- df$age06 - mean(df$age06)
df$agesq <- df$age06mc^2
df$sex <- factor(df$sex)
df$rundmcs <- factor(df$rundmcs)
df$gmvnohv <- (df$gmv - df$hv)/100


###############################
# Spaghetti plots
###############################

age.mem.plot <- ggplot(df, aes(x=age, y=memory, group=rundmcs)) +
  geom_line() +
  geom_point() +
  stat_smooth(aes(group=1), method=lm, colour="#cc0000", fill="#c3c3c3", alpha=0.6) +
  theme_bw(base_size=22)
plot(age.mem.plot)

wmh.mem.plot <- ggplot(df, aes(x=log(wmh), y=memory, group=rundmcs)) +
  geom_line() +
  geom_point() +
  stat_smooth(aes(group=1), method=lm, colour="#cc0000", fill="#c3c3c3", alpha=0.6)+ 
  theme_bw(base_size=22)
plot(wmh.mem.plot)
lm.wmh.mem = lm(memory ~ log(wmh), data=df)
lm.wmh.mem.corr1 = lm(memory ~ log(wmh) + df$age06mc + sex, data=df)
summary(lm.wmh.mem)
summary(lm.wmh.mem.corr1)

hv.mem.plot <- ggplot(df, aes(x=hv, y=memory, group=rundmcs)) +
  geom_line() +
  geom_point() +
  stat_smooth(aes(group=1), method=lm, colour="#cc0000", fill="#c3c3c3", alpha=0.6)+ 
  theme_bw(base_size=22)
plot(hv.mem.plot)
lm.hv.mem = lm(memory ~ hv, data=df)
lm.hv.mem.corr1 = lm(memory ~ hv + df$age06mc + sex, data=df)
summary(lm.hv.mem)
summary(lm.hv.mem.corr1)



###############################
# LME - WMH*HV - Memory
###############################

### MEMORY ###

### Memory & WMH ###

m0.wmh.mem = lmer(memory ~ log(wmh) + time + timesqrt + df$age06mc + sex + (1 + time|rundmcs), 
                      data=df, REML=FALSE, na.action=na.exclude)
m1.wmh.mem = lmer(memory ~ log(wmh)*time + log(wmh)*timesqrt + df$age06mc + sex + (1 + time|rundmcs),
                      data=df, REML=FALSE, na.action=na.exclude)
summary(m0.wmh.mem)
summary(m1.wmh.mem)
anova(m1.wmh.mem, m0.wmh.mem)
## The Memory-Model significantly improved when interaction term of WMH*time was added to the model
# However, the slope of WMH is not significantly associated with memory


### MEMORY & HV ###

m0.hv.mem = lmer(memory ~  hv + time + timesqrt + df$age06mc + sex + (1 + time|rundmcs), 
                     data=df, REML=FALSE, na.action=na.exclude)
m1.hv.mem = lmer(memory ~  hv*time + timesqrt + df$age06mc + sex + (1 + time|rundmcs), 
                     data=df, REML=FALSE, na.action=na.exclude)
summary(m0.hv.mem)
summary(m1.hv.mem)
anova(m1.hv.mem, m0.hv.mem)
## The Memory-Model significantly improved when interaction term of HV*time was added to the model
# The slope of HV is significantly associated with memory


### MEMORY & WMH*HV ###

### Interaction WMH*HV & Adjustments for age, sex ###
# Random intercept & slope model with interaction wmh*hv
m0.wmh.hv.mem.unadj = lmer(memory ~ log(wmh)*time + log(wmh)*timesqrt + hv*time
                               + (1 + time|rundmcs),
                               data=df, REML=FALSE, na.action=na.exclude)
m1.wmh.hv.mem.unadj = lmer(memory ~ log(wmh)*time + log(wmh)*timesqrt + hv*time + log(wmh)*hv
                               + (1 + time|rundmcs),
                               data=df, REML=FALSE, na.action=na.exclude)

m0.wmh.hv.mem.adj1 = lmer(memory ~ log(wmh)*time + log(wmh)*timesqrt + hv*time
                              + df$age06mc + sex + (1 + time|rundmcs),
                              data=df, REML=FALSE, na.action=na.exclude)
m1.wmh.hv.mem.adj1 = lmer(memory ~ log(wmh)*time + log(wmh)*timesqrt + hv*time + log(wmh)*hv
                              + df$age06mc + sex + (1 + time|rundmcs),
                              data=df, REML=FALSE, na.action=na.exclude)

summary(m0.wmh.hv.mem.unadj)
summary(m1.wmh.hv.mem.unadj)
summary(m0.wmh.hv.mem.adj1)
summary(m1.wmh.hv.mem.adj1)
anova(m1.wmh.hv.mem.unadj, m0.wmh.hv.mem.unadj)
anova(m1.wmh.hv.mem.adj1, m0.wmh.hv.mem.adj1)

## The Memory-Model significantly improved when the interaction term of WMH*HV was added to the model 



##########################################
# LME - WMH*GMV - Memory
# Test specificity of hv -> global gmv
##########################################


### MEMORY & WMH*GMV ###

### Interaction WMH*GMV & Adjustments for age, sex ###
# Random intercept & slope model with interaction wmh*gmv
m0.wmh.gmv.mem.unadj = lmer(memory ~ log(wmh)*time + log(wmh)*timesqrt + df$gmvnohv*time
                           + (1 + time|rundmcs),
                           data=df, REML=FALSE, na.action=na.exclude)
m1.wmh.gmv.mem.unadj = lmer(memory ~ log(wmh)*time + log(wmh)*timesqrt + df$gmvnohv*time + log(wmh)*df$gmvnohv
                           + (1 + time|rundmcs),
                           data=df, REML=FALSE, na.action=na.exclude)

m0.wmh.gmv.mem.adj1 = lmer(memory ~ log(wmh)*time + log(wmh)*timesqrt + df$gmvnohv*time
                          + df$age06mc + sex + (1 + time|rundmcs),
                          data=df, REML=FALSE, na.action=na.exclude)
m1.wmh.gmv.mem.adj1 = lmer(memory ~ log(wmh)*time + log(wmh)*timesqrt + df$gmvnohv*time + log(wmh)*df$gmvnohv
                          + df$age06mc + sex + (1 + time|rundmcs),
                          data=df, REML=FALSE, na.action=na.exclude)

summary(m0.wmh.gmv.mem.unadj)
summary(m1.wmh.gmv.mem.unadj)
summary(m0.wmh.gmv.mem.adj1)
summary(m1.wmh.gmv.mem.adj1)
anova(m1.wmh.gmv.mem.unadj, m0.wmh.gmv.mem.unadj)
anova(m1.wmh.gmv.mem.adj1, m0.wmh.gmv.mem.adj1)

## The interaction term of WMH*GMV did not improve the model

