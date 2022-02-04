library(segmented)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(minpack.lm)
library(segmented)
library(ggpubr)

#Code just for piece-wise that shows we do actually have a change in slope on the 2006 showing
#That the slope really did change at 2006


setwd("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data")
#Using the data that uses 200 isolates, 1000 iteration
raw_data <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/200_iteration_1000.txt",sep="")
raw_data = filter(raw_data, raw_data$base == 'I')
raw_data = filter(raw_data, raw_data$source == 'human_stool')

year = raw_data$year
total_count = raw_data$ID
plot(year, total_count)

#This is a cool piecewise that shows me the actual piecewise occurs on the 2006!

lin.mod <- lm(total_count ~ year)
segmented.mod2 <- segmented(lin.mod, seg.Z = ~year, psi=2012)
plot(year, total_count, pch=16, ylim= c(0,120),col = "red")
plot(segmented.mod2, add=T)

summary(segmented.mod2)
par(mfrow=c(1,1))
plot(segmented.mod2)

res <- cor.test(raw_data$base, raw_data$year) 

#Using the data that uses 100 isolates, 1000 iteration
raw_data <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/100_iteration_1000.txt",sep="")
raw_data = filter(raw_data, raw_data$base == 'I')
raw_data = filter(raw_data, raw_data$source == 'human_stool')

year = raw_data$year
total_count = raw_data$ID
plot(year, total_count)

#This is a cool piecewise that shows me the actual piecewise occurs on the 2006!

lin.mod <- lm(total_count ~ year)
segmented.mod <- segmented(lin.mod, seg.Z = ~year, psi=2006)
plot(year, total_count, pch=16, ylim= c(0,65), col = "red")
plot(segmented.mod, add=T)

summary(segmented.mod)

#Using the data that uses 50 isolates, 1000 iteration
raw_data <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/50_iteration_1000.txt",sep="")
raw_data = filter(raw_data, raw_data$base == 'I')
raw_data = filter(raw_data, raw_data$source == 'human_stool')

year = raw_data$year
total_count = raw_data$ID
plot(year, total_count)

#This is a cool piecewise that shows me the actual piecewise occurs on the 2006!

lin.mod <- lm(total_count ~ year)
segmented.mod <- segmented(lin.mod, seg.Z = ~year, psi=2006)
plot(year, total_count, pch=16, ylim= c(0,40),col = "red")
plot(segmented.mod, add=T)

summary(segmented.mod)

#res <- cor.test(raw_data$base, raw_data$year) 

#Using the data that uses 10 isolates, 1000 iteration
raw_data <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/10_iteration_1000.txt",sep="")
raw_data = filter(raw_data, raw_data$base == 'I')
raw_data = filter(raw_data, raw_data$source == 'human_stool')

year = raw_data$year
total_count = raw_data$ID
plot(year, total_count)

#This is a cool piecewise that shows me the actual piecewise occurs on the 2006!

lin.mod <- lm(total_count ~ year)
segmented.mod <- segmented(lin.mod, seg.Z = ~year, psi=2006)
plot(year, total_count, pch=16, ylim= c(0,20))
plot(segmented.mod, add=T)

summary(segmented.mod)


