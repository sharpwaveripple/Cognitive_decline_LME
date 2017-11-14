### model.r
library(lavaan)

datafile <- "D:/Esther/Documenten/Werk/Neurologie/LGM_Cambridge/RUNDMC_datasheet_long.csv"
df <- read.csv(datafile, sep=";", dec=",")
df.incl = df[df$Inclusion==1,]

# wmh
variables.wmh061115 <- c("wmh06", "wmh11", "wmh15")
df.wmh.incl <- df.incl[variables.wmh061115] 

model_wmh <- ' # set time
i_wmh =~ 1*wmh06 + 1*wmh11 + 1*wmh15
s_wmh =~ 0*wmh06 + 1*wmh11 + 2*wmh15 
i_wmh ~~ s_wmh
'
fit.wmh <- growth(model_wmh, data=df.wmh.incl)
summary(fit.wmh)


# wmh & cogn0615
variables.wmhcogn0615 <- c("wmh06", "wmh11", "wmh15",
                        "cognitiveindex0615_delta")
df.wmhcogn0615.incl <- df.incl[variables.wmhcogn0615] 

model_wmhcogn0615 <- ' # set time
i_wmh =~ 1*wmh06 + 1*wmh11 + 1*wmh15
s_wmh =~ 0*wmh06 + 1*wmh11 + 2*wmh15
i_wmh ~~ s_wmh
cognitiveindex0615_delta ~ i_wmh + s_wmh
'
fit.wmhcogn0615 <- growth(model_wmhcogn0615, data=df.wmh.incl)
summary(fit.wmhcogn0615)


# Cognition
variables.cogn061115 <- c("cognitiveindex06_delta", "cognitiveindex0611_delta", "cognitiveindex0615_delta")
df.cogn.incl <- df.incl[variables.cogn061115] 

model_cogn <- ' # set time
i_cogn =~ 1*cognitiveindex06_delta + 1*cognitiveindex0611_delta + 1*cognitiveindex0615_delta
s_cogn =~ 0*cognitiveindex06_delta + 1*cognitiveindex0611_delta + 2*cognitiveindex0615_delta 
i_cogn ~~ s_cogn
'
fit.cogn <- growth(model_cogn, data=df.cogn.incl)
summary(fit.cogn)


# wmh & Cognition
variables.wmhcogn061115 <- c("wmh06", "wmh11", "wmh15",
                             "cognitiveindex06_delta", "cognitiveindex0611_delta", "cognitiveindex0615_delta")
df.wmhcogn.incl <- df.incl[variables.wmhcogn061115] 

model_wmhcogn <- ' # set time
i_wmh =~ 1*wmh06 + 1*wmh11 + 1*wmh15
s_wmh =~ 0*wmh06 + 1*wmh11 + 2*wmh15 
i_cogn =~ 1*cognitiveindex06_delta + 1*cognitiveindex0611_delta + 1*cognitiveindex0615_delta
s_cogn =~ 0*cognitiveindex06_delta + 1*cognitiveindex0611_delta + 2*cognitiveindex0615_delta
i_wmh ~~ s_wmh
i_cogn ~~ s_cogn
i_wmh ~~ i_cogn + s_cogn
s_wmh ~~ i_cogn + s_cogn
'
fit.wmhcogn <- growth(model_wmhcogn, data=df.wmhcogn.incl)
summary(fit.wmhcogn)
standardizedSolution(fit.wmhcogn)
fitMeasures(fit.wmhcogn)


# wmh & mmse
variables.wmhmmse <- c("wmh06", "wmh11", "wmh15",
                       "MMSE_2006",	"MMSE_2011",	"MMSE_2015")
df.wmhmmse.incl <- df.incl[variables.wmhmmse] 

model_wmhmmse <- ' # set time
i_wmh =~ 1*wmh06 + 1*wmh11 + 1*wmh15
s_wmh =~ 0*wmh06 + 1*wmh11 + 2*wmh15 
i_mmse =~ 1*MMSE_2006 + 1*MMSE_2011 + 1*MMSE_2015
s_mmse =~ 0*MMSE_2006 + 1*MMSE_2011 + 2*MMSE_2015
i_wmh ~~ s_wmh
i_mmse ~~ s_mmse
i_wmh ~~ i_mmse + s_mmse
s_wmh ~~ i_mmse + s_mmse
'
fit.wmhmmse <- growth(model_wmhmmse, data=df.wmhmmse.incl)
summary(fit.wmhmmse)
standardizedSolution(fit.wmhmmse)
fitMeasures(fit.wmhmmse)