library(ggplot2)
library(ggpubr)
library(stats)
library(gplots)
library(d3heatmap)
library(reshape2)
library(pheatmap)
library(dplyr)
library(tidyverse)
library(viridis)

#Useful: Logistic regression overtime for the all the isolates overtime

setwd("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data")
#Getting the raw big database with all metadata
year_df<- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/year_df_humanfecesONLY_ID.csv")

  
year_df$binary <- ifelse(year_df$base == 'I',1,0)

  
attach(year_df)
detach(year_df)
  
modela <- glm(year_df$binary ~ year_df$year,family = binomial)
summary(modela)
  

modelb <- glm(year_df$binary ~ , family = binomial)
  
  xv <-seq(1970, 2020, 1)
  yv<- predict(modela,data.frame(year=xv),type="response")
  
  s <- summary(modela)
  summaryFile = paste("logic_regression-model-group", g,".txt", sep="")
  capture.output(s, file = summaryFile)
  
  modelData = data.frame('x' = xv, 'y' = yv)
  
  logistic <- glm(binary ~ sex, data = year_df, family = "binomial")
  
  
  
  
  
  
  #For jittering  
  group1 = c('ST-353_complex','ST-354_complex','ST-464_complex')
  group2 = c('ST-48_complex','ST-45_complex',
             'ST-61_complex','ST-42_complex','ST-22_complex',
             'ST-283_complex')
  group3 = c('ST-21_complex','ST-257_complex','ST-206_complex',
             'ST-443_complex','ST-658_complex','ST-574_complex',
             'ST-52_complex','ST-403_complex')
  year_df$group = ifelse(year_df$clonal_complex %in% group1, 1, 
                         ifelse(year_df$clonal_complex %in% group2, 2, 
                                ifelse(year_df$clonal_complex %in% group3, 3, 0)))
  
  
  for (g in c(1,2,3)) {
    year_df_g <- year_df %>% filter(group == g)
    year_df_g <- year_df_g %>%
      select(year,base,clonal_complex)
#jitter graph that lets me show where my data points are at
  j = ggplot(data = year_df_g, aes(year,binary, alpha=0.5, colour = as.factor(clonal_complex)))+
    geom_point(position = "jitter") +
    geom_line(data=modelData,aes(x=xv, y=yv, colour='Red')) +
    xlim(2004, 2020) +
    theme_pubr() +
    guides(alpha = FALSE)
  detach(year_df_g)
  plotname = paste("logic_regression-group", g,".png", sep="")
  ggsave(plotname, j) 
} 

plot(year,binary,pch=21,bg="yellow") 
lines(xv,yv,col="blue")

plot(j)

