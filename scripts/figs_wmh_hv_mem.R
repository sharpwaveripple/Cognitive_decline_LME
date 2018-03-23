library(ggplot2)
library(sjPlot)
library(lmerTest)
library(mice)

WorkingDir<-"\\\\umcfs034/NEUROuser$/Z515179/Documenten_Esther/OiO/Cambridge/RUNDMC_LGM/cognitive_decline_LME/plots/"

scatterPlot <- function(x, y, m, xLab, yLab, mLab) {
  ggplot(df, aes(x, y)) + geom_point(aes(color=m), size=2) +
  scale_color_gradient(low="cyan", high="blue") +
  labs(x=xLab, y=yLab, colour=mLab) +
  coord_cartesian(ylim=c(-3,3)) +
  theme_classic()
}

df <- read.csv("../data/RUNDMC_data_long.csv", sep=";", dec=",")
df$subject <- factor(df$rundmcs)
df$logwmh <- log(df$wmh)
df$em <- (df$immediatememory + df$delayedmemory)/2

mywidth<-10
myheight<-9
myres<-500

#### Memory ####
png(filename=paste(WorkingDir, "scatter_wmh_mem", ".png", sep=""), units="in", width=mywidth, height=myheight, res=myres)
scatterPlot(df$logwmh, df$memory, df$age,
            "WMH volume (log ml)", "Memory (z-score)", "Age (years)")
dev.off()

png(filename=paste(WorkingDir, "scatter_hv_mem", ".png", sep=""), units="in", width=mywidth, height=myheight, res=myres)
scatterPlot(df$hv, df$memory, df$age,
            "Hippocampal volume (ml)", "Memory (z-score)", "Age (years)")
dev.off()

#### Working Memory ####
png(filename=paste(WorkingDir, "scatter_wmh_wm", ".png", sep=""), units="in", width=mywidth, height=myheight, res=myres)
scatterPlot(df$logwmh, df$workingmemory, df$age,
            "WMH volume (log ml)", "Working Memory (z-score)", "Age (years)")
dev.off()

png(filename=paste(WorkingDir, "scatter_hv_wm", ".png", sep=""), units="in", width=mywidth, height=myheight, res=myres)
scatterPlot(df$hv, df$workingmemory, df$age,
            "Hippocampal volume (ml)", "Working Memory (z-score)", "Age (years)")
dev.off()

#### Episodic Memory ####
png(filename=paste(WorkingDir, "scatter_wmh_em", ".png", sep=""), units="in", width=mywidth, height=myheight, res=myres)
scatterPlot(df$logwmh, df$em, df$age,
            "WMH volume (log ml)", "Episodic Memory (z-score)", "Age (years)")
dev.off()

png(filename=paste(WorkingDir, "scatter_hv_em", ".png", sep=""), units="in", width=mywidth, height=myheight, res=myres)
scatterPlot(df$hv, df$em, df$age,
            "Hippocampal volume (ml)", "Episodic Memory (z-score)", "Age (years)")
dev.off()