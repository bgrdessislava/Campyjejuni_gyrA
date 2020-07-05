library(dplyr)
library(ggplot2)
library(ggpubr)
library(minpack.lm)
library(segmented)
#Golden script that allowed me to choose the data from 1990-2018 for only human 
#Made piece-wise and nls regression data

setwd("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data")
#Using the data that uses 200 isolates, 10000 iteration
raw_data <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/200-_iteration_10000.txt",sep="")
raw_data = filter(raw_data, raw_data$base == 'I')
raw_data = filter(raw_data, raw_data$source == 'human_stool')

# Plot your time series of times(x) against measurements (y)
plot(raw_data$year, raw_data$ID)

# Print out the sumamry of the model fit
summary(raw_data)

# Create a data frame with your Times (x) and Observations (y)
pseudo <- data.frame(cbind(raw_data$year,raw_data$ID))

#Changing the column names
colnames(pseudo) = c("x","y") #It works when it is c("y,"x)
plot(pseudo$x,pseudo$y)
#linear model
res_linear <- lm(pseudo$x~pseudo$y)

#Linear regression data
plot(pseudo$x, pseudo$y, pch = 16, cex = 1.3, col = "red", main = "Year vs resistant humanfaeces 200 isolates ", font.main = 1,cex.main = 1, xlab = "Year", ylab = "Isolates Count")

abline(1993.6175,0.2459)
abline(lm(pseudo$y~pseudo$x))


lin.mod <- lm(y~x, data=pseudo)
segmented.mod <- segmented(lin.mod, seg.Z = ~x, psi=2015)
plot(pseudo$x,pseudo$y,pch=16, ylim=c(0,120))
plot(segmented.mod, add=T)



# Sse nls - non linear least squares - to fit the non-linear model y = 1-exp(-a*x) to the data
#res <- nlsLM(y~b-exp(-a*x), data = pseudo, start = list(a = 0.01,b=100),control = list(maxiter=1000))
res <- nlsLM(y~b*(1-exp(-a*x)), data = pseudo, start = list(a = 0.01,b=100),control = list(maxiter=1000))

#res = nls(y~(a*x)/(b+x), data = pseudo, start = list(a=100, b=10))

# Print out the sumamry of the model fit
summary(res)


# Store the parameter estimate for a (in the y=1-exp(-a*x) equation):
temp <- summary(res)[[11]]
a_est <- summary(res)[[11]][1]
b_est <- summary(res)[[11]][2,1]


#genfitted <- b_est-exp(-a_est*pseudo$x)
genfitted <- b_est*(1-exp(-a_est*pseudo$x))

#nls(ID~1-exp(-a*year), data = pseudo, start = list(a = 100))

o <- order(pseudo$x)
lines(pseudo$x[o],genfitted[o], col="blue", lwd=2)

