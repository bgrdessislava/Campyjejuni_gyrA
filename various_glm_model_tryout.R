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
library(lme4)

#Useful: Logistic regression overtime for the all the isolates overtime

setwd("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data")
#Getting the raw big database with all metadata
year_df<- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/year_df_humanfecesONLY_ID.csv")

  
year_df$binary <- ifelse(year_df$base == 'I',1,0)

modela <- glm(year_df$binary ~ year_df$year,family = binomial)
summary(modela)
  
  
  xv <-seq(1970, 2020, 1)
  yv<- predict(modela,data.frame(year=xv),type="response")

  

#Cleaning all the data so that they are in factors
year_df$year <- as.factor(year_df$year)
year_df$ST <- as.factor(year_df$ST)
year_df$clonal_complex <- as.factor(year_df$clonal_complex)
year_df$cgMLST.coli_jejuni. <- as.factor(year_df$cgMLST.coli_jejuni.)
year_df$rMLST <- as.factor(year_df$rMLST)
year_df$region <- as.factor(year_df$region)
year_df$age_yr <- as.factor(year_df$age_yr)
year_df$sex <- as.factor(year_df$sex)
year_df$binary <- as.factor(year_df$binary)
year_df$month <- as.factor(year_df$month)


#removing all the columns that either have one value or not needed
year_df_analysis <- year_df
year_df_analysis <- subset(year_df_analysis, select = -X.2  )
year_df_analysis <- subset(year_df_analysis, select = -X.1  )
year_df_analysis <- subset(year_df_analysis, select = -X  )
year_df_analysis <- subset(year_df_analysis, select = -ID  )
year_df_analysis <- subset(year_df_analysis, select = -source  )
year_df_analysis <- subset(year_df_analysis, select = -base  )
year_df_analysis <- subset(year_df_analysis, select = -position  )
year_df_analysis <- subset(year_df_analysis, select = -cgMLST.coli_jejuni.  )
year_df_analysis <- subset(year_df_analysis, select = -rMLST)

#Making sure we do not have any empty strings
year_df_analysis[year_df_analysis == "?"] <- NA
year_df_analysis[year_df_analysis == ""] <- NA

logistic <- glm(binary ~ clonal_complex, data = year_df_analysis, family = "binomial")
logistic_CC_summary_table <- summary.glm(logistic)$coefficient
write.csv(logistic_CC_summary_table,"Summary_table_CC.csv")


logistic_all <- glm(binary ~ ., data = year_df_analysis, family = "binomial")
logistic_all_summary <- summary.glm(logistic_all)$coefficient
write.csv(logistic_all_summary,"Summary_tabl_all.csv")

  
  
year_df <- subset( year_df, select = -position )
  
  
  
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

