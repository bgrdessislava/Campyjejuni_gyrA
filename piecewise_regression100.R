library(segmented)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(minpack.lm)
library(segmented)
library(ggpubr)

#100 iterations
#Code just for piece-wise that shows we do actually have a change in slope on the 2006 showing

setwd("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data")
#Using the data that uses 200 isolates, 100 iteration
raw_data <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/200_iteration_100.txt",sep="")
raw_data = filter(raw_data, raw_data$base == 'I')
raw_data = filter(raw_data, raw_data$source == 'human_stool')

year = raw_data$year
total_count = raw_data$ID
plot(year, total_count)

#This is a cool piecewise that shows me the actual piecewise occurs on the 2006!

lin.mod <- lm(total_count ~ year)
segmented.mod1 <- segmented(lin.mod, seg.Z = ~year, psi=2012)
plot(year, total_count, pch=16, ylim= c(0,150))
plot(segmented.mod1, add=T)

summary(segmented.mod1)

par(mfrow=c(1,1))


#Testing models with AIC and model checking
AIC(lin.mod,segmented.mod1)
par(mfrow=c(2,2))
plot(segmented(lin.mod, seg.Z = ~year, psi=2012))
dev.off()

segmented.mod2

#Using the data that uses 100 isolates, 100 iteration
raw_data <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/100_iteration_100.txt",sep="")
raw_data = filter(raw_data, raw_data$base == 'I')
raw_data = filter(raw_data, raw_data$source == 'human_stool')

year = raw_data$year
total_count = raw_data$ID
plot(year, total_count)

#This is a cool piecewise that shows me the actual piecewise occurs on the 2006!
lin.mod <- lm(total_count ~ year)
segmented.mod <- segmented(lin.mod, seg.Z = ~year, psi=2006)
plot(year, total_count, pch=16, ylim= c(0,80))
plot(segmented.mod, add=T)

summary(segmented.mod)

#Using the data that uses 50 isolates, 100 iteration
raw_data <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/50_iteration_100.txt",sep="")
raw_data = filter(raw_data, raw_data$base == 'I')
raw_data = filter(raw_data, raw_data$source == 'human_stool')

year = raw_data$year
total_count = raw_data$ID
plot(year, total_count)

#This is a cool piecewise that shows me the actual piecewise occurs on the 2006!

lin.mod <- lm(total_count ~ year)
segmented.mod <- segmented(lin.mod, seg.Z = ~year, psi=2006)
plot(year, total_count, pch=16, ylim= c(0,40))
plot(segmented.mod, add=T)

summary(segmented.mod)


#Using the data that uses 10 isolates, 100 iteration
raw_data <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/10_iteration_100.txt",sep="")
raw_data = filter(raw_data, raw_data$base == 'I')
raw_data = filter(raw_data, raw_data$source == 'human_stool')

year = raw_data$year
total_count = raw_data$ID
plot(year, total_count)

#This is a cool piecewise that shows me the actual piecewise occurs on the 2006!

lin.mod <- lm(total_count ~ year)
segmented.mod <- segmented(lm(total_count ~ year), seg.Z = ~year, psi=2006)
plot(year, total_count, pch=16, ylim= c(0,15))
plot(segmented.mod, add=T)

par(mfrow=c(2,2))
summary(segmented.mod)

summary.aov(lin.mod)

#res <- cor.test(raw_data$base, raw_data$year) 
par(mfrow=c(2,2))
plot(lin.mod)
plot(segmented.mod)
qqnorm(segmented.mod)

par(mfrow=c(1,1))
