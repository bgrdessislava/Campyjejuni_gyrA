#!/bin/bash

#SBATCH --job-name=fasttree_%j
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --time=0-2:00:00
#SBATCH --chdir=/data/zool-microbe-genomics/spet4850/campy_analysis/data/CC353/
#SBATCH --output=/data/zool-microbe-genomics/spet4850/campy_analysis/logs/fasttree_%j.out
#SBATCH --error=/data/zool-microbe-genomics/spet4850/campy_analysis/logs/fasttree_%j.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=spet4850@spc.ox.ac.uk





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

modela <- glm(binary ~ clonal_complex, data = year_df, family = binomial)
summary(modela)
  

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

str(year_df)


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
year_df_analysis <- subset(year_df_analysis, select = -age_yr)
year_df_analysis <- subset(year_df_analysis, select = -region)
year_df_analysis <- subset(year_df_analysis, select = -ST)

#Making sure we do not have any empty strings
year_df_analysis[year_df_analysis == "?"] <- NA
year_df_analysis[year_df_analysis == ""] <- NA

logistic <- glm(binary ~ clonal_complex, data = year_df_analysis, family = "binomial")
logistic_CC_summary_table <- summary.glm(logistic)$coefficient
summary(logistic)
write.csv(logistic_CC_summary_table,"Summary_table_CC.csv")


logistic_all <- glm(binary ~ ., data = year_df_analysis, family = "binomial")
logistic_all_summary <- summary.glm(logistic_all)$coefficient
summary(logistic_all)

write.csv(logistic_all_summary,"Summary_tabl_all.csv")

#Generalized Linear Mixed-Effects Models - random intercept
GLMixed_randomintercept <- glmer(binary ~ year + 1 | sex, data = year_df_analysis, family = "binomial")

#Generalized linear miex effect model - random slope
GLMixed_randomintercept <- glmer(binary ~ year + year | sex, data = year_df_analysis, family = "binomial")
