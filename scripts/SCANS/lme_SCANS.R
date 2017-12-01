library(tidyverse)
library(lme4)

df <- read.csv("../data/scans_full.csv")
df$logwmh <- log(df$WMH_volume_ml)
df$timesq <- log(df$time_scan^2)


ggplot(df, aes(age, PS, group=ID)) +
  geom_point() + geom_line() +
  scale_x_continuous(limits=c(50, 90), expand=c(0,0)) + 
  theme_classic()

library(lmerTest)
library(sjPlot)
fit <- lmer(Global ~ 1 + BSL_age + time_scan + timesq + (1 + time_scan|ID), data = df)
sjp.lmer(fit, vars = "time", type = "rs.ri")
