spaghettiPlot <- function(df, x, y, group, xLab, yLab) {
	library(ggplot2) 
	pasta <- ggplot(df, aes(x, y, group=group)) +
		geom_line() + geom_point() +
		stat_smooth(aes(group=1), method=lm, colour="#cc0000", fill="#c3c3c3", alpha=0.6) +
		theme_bw(base_size=22) +
		# labs(x=expression(WMH["Log10"])) # for later...
		scale_x_discrete(name=xLab) + scale_y_discrete(name=yLab)
	return(pasta)
}

df <- read.csv("../data/RUNDMC_data_long.csv", sep=";", dec=",")


### recreate original plots ###

wmhXhv <- spaghettiPlot(df, log(df$wmh), df$hv, df$rundmcs,
												"White matter hyperintensities",
												"Hippocampal volume")

wmhXage <- spaghettiPlot(df, df$age, (df$wmh)**2, df$rundmcs,
                        "Age",
                        "WMH")

timeXcogind <- spaghettiPlot(df, log(df$wmh), df$cognitiveindex, df$rundmcs,
                             "WMH",
                             "Cognitive Index")

timeXcogind

timeXcogind <- spaghettiPlot(df, df$time, df$cognitiveindex, df$rundmcs,
   													"Timepoint",
   													"Cognitive Index")

ageXcogind <- spaghettiPlot(df, df$age, df$cognitiveindex, df$rundmcs,
   													"Age",
   													"Cognitive Index")


### example of usage - examining plots for each cognitive domain ###

cognitiveVars <- c("cognitiveindex", "executivefunction", "psychomotorspeed")
for (domain in cognitiveVars) {
	sauce <- spaghettiPlot(df, log(df$wmh), df[[domain]], df$rundmcs,
												 "White matter hyperintensities",
												 domain) #make fancier labels later
	plot(sauce)
}

