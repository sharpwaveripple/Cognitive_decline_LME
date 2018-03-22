library(ggplot2)
library(sjPlot)
library(lmerTest)
library(mice)

scatterPlot <- function(x, y, m, xLab, yLab, mLab) {
  ggplot(df, aes(x, y)) + geom_point(aes(color=m), size=2) +
  scale_color_gradient(low="cyan", high="blue") +
  labs(x=xLab, y=yLab, colour=mLab) +
  theme_classic()
}

df <- read.csv("../data/RUNDMC_data_long.csv", sep=";", dec=",")
df$subject <- factor(df$rundmcs)
df$logwmh <- log(df$wmh)
df$em <- (df$immediatememory + df$delayedmemory)/2

#### Memory ####
scatterPlot(df$logwmh, df$memory, df$age,
            "WMH volume (log ml)", "Memory (z-score)", "Age (years)")

scatterPlot(df$hv, df$memory, df$age,
            "Hippocampal volume (ml)", "Memory (z-score)", "Age (years)")

#### Working Memory ####
scatterPlot(df$logwmh, df$workingmemory, df$age,
            "WMH volume (log ml)", "Working Memory (z-score)", "Age (years)")

scatterPlot(df$hv, df$workingmemory, df$age,
            "Hippocampal volume (ml)", "Working Memory (z-score)", "Age (years)")

#### Episodic Memory ####
scatterPlot(df$logwmh, df$em, df$age,
            "WMH volume (log ml)", "Episodic Memory (z-score)", "Age (years)")

scatterPlot(df$hv, df$em, df$age,
            "Hippocampal volume (ml)", "Episodic Memory (z-score)", "Age (years)")