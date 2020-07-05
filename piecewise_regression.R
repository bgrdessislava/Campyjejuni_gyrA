library(segmented)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(minpack.lm)
library(segmented)

#Code just for piece-wise that shows we do actually have a change in slope on the 2006 showing
#That the slope really did change at 2006


setwd("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data")
#Using the data that uses 200 isolates, 10000 iteration
raw_data <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/200-_iteration_10000.txt",sep="")
raw_data = filter(raw_data, raw_data$base == 'I')
raw_data = filter(raw_data, raw_data$source == 'human_stool')

year = raw_data$year
total_count = raw_data$ID
plot(year, total_count)

#This is a cool piecewise that shows me the actual piecewise occurs on the 2006!

lin.mod <- lm(total_count ~ year)
segmented.mod <- segmented(lin.mod, seg.Z = ~year, psi=2012)
plot(year, total_count, pch=16, ylim= c(0,120))
plot(segmented.mod, add=T)
