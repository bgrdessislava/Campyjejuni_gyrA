library(ggplot2)
library(stats)
library(gplots)
library(d3heatmap)
library(reshape2)
library(pheatmap)
library(dplyr)
library(tidyverse)
library(viridis)
library(lme4)

setwd("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data")
#Getting the raw big database with all metadata
year_df<- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/CleanMeta_for_Tableau_resistantcleaned_10362.csv")

#Taking the first column which is not needed
#year_df = year_df[,-1]
#Only picking human stool
year_df = year_df %>%
  filter(!is.na(year))

 # filter(source == 'human_stool')%>% 
 # filter(base == 'I' | base == 'T') %>% 
 # filter(year >= 1997)


#adding binary column
year_df$binary = ifelse(year_df$Gyrase_DessyPipeline == 'I', 1, 0)

#basic lm 
res=glm(year_df$binary~year_df$year,family=binomial)
#it does show that year do have significant relationship with resistance change
summary(res)

#ordered the year and add a line across the year and binary 
o=order(year_df$year)
plot(year_df$year,year_df$binary,ylab="Resistance",xlab="Year",bty="l")
lines(year_df$year[o],res$fitted[o],lwd=2,col="red")

#residual plot do show linear relation
plot(res, which = 1) 
#qq plot showing it is not forming normal distribution which is true
plot(res, which = 2)

#Tried to do boxplot but too many clonal-complex maybe should try to work on the main 18 clonal complex
boxplot(year_df$binary ~ year_df$clonal_complex, data = year_df) 

# Smoothing function with different behaviour in the different plot panels
mysmooth <- function(formula,data,...){
  meth <- eval(parse(text=meths[unique(data$PANEL)]))
  x <- match.call()
  x[[1]] <- meth
  eval.parent(x)
}

#CC overtime
split_plot <- ggplot(aes(year_df$year, year_df$binary), data = year_df) + 
    geom_point() + 
    geom_smooth(method = glm, method.args = c(family=binomial)) +
    facet_wrap(~ year_df$clonal_complex..MLST.) + # create a facet for each mountain range
    xlab("length") + 
    ylab("test score")

split_plot

ggsave("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Figures/Scatterplot/binary_CC_year.png",plot=split_plot, width = 50, height = 30, units = "cm",dpi = 600)

#ST overtime
split_plot_ST <- ggplot(aes(year_df$year, year_df$binary), data = year_df) + 
  geom_point() + 
  geom_smooth(method = glm, method.args = c(family=binomial)) +
  facet_wrap(~ year_df$ST..MLST.) + # create a facet for each mountain range
  xlab("length") + 
  ylab("test score")

#split_plot_ST dont run too many


#clonal_complex each of them shown as whether it is significant or not CC
clonal.lm <- lm(year_df$binary ~ year_df$year + year_df$clonal_complex..MLST., data = year_df)
summary(clonal.lm)

#ST each of them shown as whether it is significant or not 
clonal.lm <- lm(year_df$binary ~ year_df$year + year_df$ST..MLST., data = year_df)
summary(clonal.lm)


#Mix model inluding the clonal complex
mixed.lmer <- lmer(year_df$binary ~ year_df$year + (1|year_df$clonal_complex..MLST.), data = year_df)

#Mix model inluding the ST
mixed.lmer_ST <- lmer(year_df$binary ~ year_df$year + (1|year_df$ST..MLST.), data = year_df)


summary(mixed.lmer)
summary(mixed.lmer_ST) #55% of the variance comees from ST  type
#Differences between clonal complex ranges explain ~30% of
#the variance that’s “left over” after the variance explained by our fixed effects.



plot(mixed.lmer)  # not sure what residue plot is doing here..
qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))  # The qq plot looks better than the normal lm?



#Experimenting
(split_plot <- ggplot(aes(year_df$year, year_df$binary), data = year_df) + 
    geom_point() + 
    geom_smooth(method = lmer, method.args = c(family=binomial)) +
    facet_wrap(~ year_df$clonal_complex) + # create a facet for each mountain range
    xlab("length") + 
    ylab("test score"))


