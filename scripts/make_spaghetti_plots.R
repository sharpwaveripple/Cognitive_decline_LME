spaghettiPlot <- function(df, x, y, group, xLab, yLab) {
	library(ggplot2) 
	pasta <- ggplot(df, aes(x, y, group=group)) +
		geom_line() +
		geom_point() +
		stat_smooth(aes(group=1), method=lm, colour="#cc0000", fill="#c3c3c3", alpha=0.6) +
		theme_bw(base_size=22) +
		# labs(x=expression(WMH["Log10"]))
		scale_x_discrete(name=xLab) + scale_y_discrete(name=yLab)
	return(pasta)
}

df <- read.csv("../data/RUNDMC_data_long.csv", sep=";", dec=",")
cognitiveVars <- c("cognitiveindex", "executivefunction", "psychomotorspeed")

for (domain in cognitiveVars) {
	sauce <- spaghettiPlot(df, log(df$wmh), df[[domain]], df$rundmcs,
												 "White matter hyperintensities",
												 "Cognition")
	plot(sauce)
}
