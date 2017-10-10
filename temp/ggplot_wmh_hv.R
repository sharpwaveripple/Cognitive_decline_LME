library(ggplot2)

setwd("D:/Esther/Documenten/Werk/Neurologie/LGM_Cambridge/RUNDMC_LGM")

datafile <- "data/ggplot_dem_wmh_hv.csv"
dem <- read.csv(datafile, sep=";", dec=",")

dem.incl <- dem[dem$inclusion==1,]

# plots by dementia status
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

# wmh & hv plots
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

# wmh & hv plots of those with 3 scans
wmh.age.plot <- ggplot(dem.incl, aes(x=age, y=wmh, group=rundmcs)) +
  geom_line() +
  geom_point() +
  stat_smooth(aes(group=1), method=lm, colour="#cc0000", fill="#c3c3c3", alpha=0.6) +
  theme_bw(base_size=22)
print(wmh.age.plot)

hv.age.plot <- ggplot(dem.incl, aes(x=age, y=hv, group=rundmcs)) +
  geom_line() +
  geom_point() +
  stat_smooth(aes(group=1), method=lm, colour="#cc0000", fill="#c3c3c3", alpha=0.6) +
  theme_bw(base_size=18)
print(hv.age.plot)

# proportions of tbv
wmhprop.age.plot <- ggplot(dem.incl, aes(x=age, y=wmhprop, group=rundmcs)) +
  geom_line() +
  geom_point() +
  theme_bw(base_size=18)
print(wmhprop.age.plot)

hvprop.age.plot <- ggplot(dem.incl, aes(x=age, y=hvprop, group=rundmcs)) +
  geom_line() +
  geom_point() +
  theme_bw(base_size=18)
print(hvprop.age.plot)

# proportions of tbv by dementia status
dem.wmhprop.age.plot <- ggplot(dem, aes(x=age, y=wmhprop, group=rundmcs)) +
  geom_line() +
  geom_point() +
  facet_grid(.~dementiai) +
  stat_smooth(aes(group=1), method=lm, colour="#cc0000", fill="#c3c3c3", alpha=0.6) +
  theme_bw(base_size=18)
print(dem.wmhprop.age.plot)

dem.hvprop.age.plot <- ggplot(dem, aes(x=age, y=hvprop, group=rundmcs)) +
  geom_line() +
  geom_point() +
  facet_grid(.~dementiai) +
  stat_smooth(aes(group=1), method=lm, colour="#cc0000", fill="#c3c3c3", alpha=0.6) +
  theme_bw(base_size=18)
print(dem.hvprop.age.plot)
