library(lme4)
library(ggplot2)
library(mediation)
library(lavaan)


setwd("../temp/")
df = read.csv("../data/RUNDMC_data_long.csv", sep=";", dec=",")
df.wide = read.csv("../data/RUNDMC_data_wide.csv", sep=";", dec=",")
df$age06mc <- df$age06 - mean(df$age06)
df$agesq <- df$age06mc^2
df$sex <- factor(df$sex)
df$rundmcs <- factor(df$rundmcs)
df$gmvnohv <- (df$gmv - df$hv)/100
df.wide$episodicmemory0615 <- (df.wide$immediatememory0615 + df.wide$delayedmemory0615)/2

#### Mediation analyses - WMH -> HA -> MEMORY DECLINE ####

# Define individual hippocampal atrophy slopes (ha)
model.ha <- lmer(hv ~  time + age06mc + sex + log(wmh) + (1+time|rundmcs), 
                 data=df, REML=FALSE, na.action=na.exclude)
summary(model.ha)
coef(summary(model.ha))[ , "Estimate"]
ha.slopes <- coef(model.ha)$rundmcs
ha <- ha.slopes$time
df.wide$ha <- ha

# Mediation analysis on cross-sectional dataset using Lavaan
# Memory - wmh -> ha -> memory
variables.mem <- c("wmh06", "ha", "memory0615")
df.wide.incl.mem <- df.wide[complete.cases(df.wide[variables.mem]),]

model <- '
# Regression models
memory0615 ~ hv06 + b1*ha + Age_2006 + Sex + educationyears + c1*wmh06 + memory06
ha ~ a1*wmh06

# Covariances
ha ~~ hv06

# Defined parameters
mem.wmh := c1
mem.ha := b1
ha.wmh := a1

direct := c1
indirect := b1*a1
totaal := c1 + (b1*a1)'

mediation.model <- sem(model, data=df.wide.incl.mem, fixed.x=FALSE)
summary(mediation.model)

# The association between WMH & memory decline is NOT mediated via hippocampal atrophy


### Working memory
# Memory - wmh -> ha -> memory
variables.wm <- c("wmh06", "ha", "workingmemory0615")
df.wide.incl.wm <- df.wide[complete.cases(df.wide[variables.wm]),]

model.wm <- '
# Regression models
memory0615 ~ hv06 + b1*ha + Age_2006 + Sex + educationyears + c1*wmh06 + memory06
ha ~ a1*wmh06

# Covariances
ha ~~ hv06

# Defined parameters
mem.wmh := c1
mem.ha := b1
ha.wmh := a1

direct := c1
indirect := b1*a1
totaal := c1 + (b1*a1)'

mediation.model.wm <- sem(model.wm, data=df.wide.incl.wm, fixed.x=FALSE)
summary(mediation.model.wm)


### Episodic memory
# Memory - wmh -> ha -> memory
variables.em <- c("wmh06", "ha", "episodicmemory0615")
df.wide.incl.em <- df.wide[complete.cases(df.wide[variables.em]),]

model.em <- '
# Regression models
memory0615 ~ hv06 + b1*ha + Age_2006 + Sex + educationyears + c1*wmh06 + memory06
ha ~ a1*wmh06

# Covariances
ha ~~ hv06

# Defined parameters
mem.wmh := c1
mem.ha := b1
ha.wmh := a1

direct := c1
indirect := b1*a1
totaal := c1 + (b1*a1)'

mediation.model.em <- sem(model.em, data=df.wide.incl.em, fixed.x=FALSE)
summary(mediation.model.em)
