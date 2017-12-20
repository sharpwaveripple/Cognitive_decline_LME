library(ggplot2)
library(sjPlot)
library(lmerTest)
library(mice)

spaghettiPlot <- function(x, y, group, xLab, yLab) {
    ggplot(df, aes(x, y, group=group)) +
    geom_line() + geom_point() +
    stat_smooth(aes(group=1), method=lm, colour="#cc0000", fill="#c3c3c3", alpha=0.6) +
    theme_bw(base_size=22) +
    labs(x=xLab, y=yLab)
}

scatterPlot <- function(x, y, m, xLab, yLab, mLab) {
  ggplot(df, aes(x, y)) + geom_point(aes(color=m), size=2) +
  scale_color_gradient(low="cyan", high="blue") +
  labs(x=xLab, y=yLab, colour=mLab) +
  theme_classic()
}

df <- read.csv("../data/RUNDMC_data_long.csv", sep=";", dec=",")
df$subject <- factor(df$rundmcs)
df$logwmh <- log(df$wmh)
df$age06 <- (df$age06) #scale
df$agesq <- df$age06^2
df$timesq <- df$time^2
df$times <- as.factor(df$times)


df2 <- read.csv("../data/RUNDMC_data_wide.csv", sep=";", dec=",")

spaghettiPlot(df$age, df$cognitiveindex, df$subject,
              "Age", "Cognitive index")

spaghettiPlot(df$logwmh, df$cognitiveindex, df$subject,
              "White matter hyperintensities", "Cognitive index")

spaghettiPlot(df$hv, df$cognitiveindex, df$rundmcs,
              "Hippocampal volume", "Cognitive index")

scatterPlot(df$logwmh, df$cognitiveindex, df$age,
            "White matter hyperintensities", "Cognitive index", "Age")

scatterPlot(df$logwmh, df$cognitiveindex, df$age,
            "White matter hyperintensities", "Cognitive index", "Age")

scatterPlot(df$hv, df$cognitiveindex, df$age,
            "Hippocampal volumes", "Cognitive index", "Age")

scatterPlot(df$logwmh, df$memory, df$age,
            "White matter hyperintensities", "Memory", "Age")

scatterPlot(df$hv, df$memory, df$age,
            "Hippocampal volumes", "Memory", "Age")


# here, each line segment represents a subject. 
ggplot(df, aes(age, cognitiveindex, group=subject)) +
  geom_point() + geom_line() +
  geom_smooth(aes(group=1), method="lm", colour="red") +
  geom_smooth(aes(group=1), method="lm", formula = y~poly(x, 2), colour="orange") +
  scale_x_continuous(limits=c(50, 90), expand=c(0,0)) + 
  theme_classic()

# fixed effect of baseline age represents 
with(x, ggplot(aes(age, cognitiveindex, group=subject)) + geom_point)
ggplot(df2, aes(Age_2006, cognitiveindex06)) + geom_point() + geom_smooth(method="lm")
ggplot(df, aes(age, cognitiveindex)) + geom_point()
x <- mice(df)
fit <- with(x, lmer(cognitiveindex ~ 1 + age06 + poly(time,2) + (1 + time|subject)))
fixef(fit)
y <- (pool.r.squared(fit))
poly(df$age06, 2, raw=T)
fit <- lmer(cognitiveindex ~ 1 + age06 + poly(time,2, na.action=omit) + (1 + time|subject), data = df)
plot_model(fit)
 
lm(cognitiveindex ~ poly(age06, 2), data=df)
