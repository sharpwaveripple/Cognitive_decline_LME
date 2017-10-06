library(ggplot2)

datafile_ggplot <- "../data/ggplot_dem_wmh_hv.csv"
dem = read.csv(datafile_ggplot, sep=";", dec=",")
attach(dem)



dem.wmh.age.plot <- ggplot(dem, aes(x=age, y=wmh, group=rundmcs)) +
  geom_line() +
  geom_point() +
  stat_smooth(aes(group=1), method=lm, colour="#cc0000", fill="#c3c3c3", alpha=0.6) +
  facet_grid(.~dementiai) +
  theme_bw(base_size=22)
print(dem.wmh.age.plot)


dem.hv.age.plot <- ggplot(dem, aes(x=age, y=hv, group=rundmcs)) +
  geom_line() +
  geom_point() +
  stat_smooth(aes(group=1), method=lm, colour="#cc0000", fill="#c3c3c3", alpha=0.6) +
  facet_grid(.~dementiai) +
  theme_bw(base_size=22)
print(dem.hv.age.plot)


wmh.age.plot <- ggplot(dem, aes(x=age, y=wmh, group=rundmcs)) +
  geom_line() +
  geom_point() +
  stat_smooth(aes(group=1), method=lm, colour="#cc0000", fill="#c3c3c3", alpha=0.6) +
  theme_bw(base_size=22)
print(wmh.age.plot)

hv.age.plot <- ggplot(dem, aes(x=age, y=hv, group=rundmcs)) +
  geom_line() +
  geom_point() +
  stat_smooth(aes(group=1), method=lm, colour="#cc0000", fill="#c3c3c3", alpha=0.6) +
  theme_bw(base_size=22)
print(hv.age.plot)

