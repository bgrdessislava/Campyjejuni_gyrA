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

class(year_df$clonal_complex)
  
year_df$binary <- ifelse(year_df$base == 'I',1,0)

modela <- glm(binary ~ clonal_complex, data = year_df, family = binomial)

summary(modela)
  
year_df <- year_df %>% filter(year >= 1950) %>% mutate(year_scaled = year - min(year,na.rm = TRUE))


#Cleaning all the data so that they are in factors - all the categorical values are 'factors'
#year_df$year <- as.factor(year_df$year) --> Year is not categorical 
year_df$ST <- as.factor(year_df$ST)
year_df$clonal_complex <- as.factor(year_df$clonal_complex)
year_df$cgMLST.coli_jejuni. <- as.factor(year_df$cgMLST.coli_jejuni.)
year_df$rMLST <- as.factor(year_df$rMLST)
year_df$region <- as.factor(year_df$region)
#year_df$age_yr <- as.factor(year_df$age_yr)
year_df$sex <- as.factor(year_df$sex)
year_df$binary <- as.factor(year_df$binary)
year_df$month <- as.factor(year_df$month)

str(year_df)


#removing all the columns that either have one value or not needed
year_df_analysis <- year_df %>% select(year, clonal_complex,month,sex,binary,year_scaled)
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

year_df_analysis$clonal_complex_ref = relevel(year_df_analysis$clonal_complex, ref="ST-21_complex")
logistic <- glm(binary ~ clonal_complex_ref, data = year_df_analysis, family = "binomial")
logistic_CC_summary_table <- summary.glm(logistic)$coefficient
summary(logistic)
write.csv(logistic_CC_summary_table,"Summary_table_CC.csv")


logistic_all <- glm(binary ~ ., data = year_df_analysis, family = "binomial")
logistic_all_summary <- summary.glm(logistic_all)$coefficient
summary(logistic_all)

write.csv(logistic_all_summary,"Summary_tabl_all.csv")


include_strain <- c("ST-353_complex","ST-464_complex")
year_df_analysis_subset <- year_df_analysis %>% filter(clonal_complex %in% include_strain)
logist_include <- glm(binary ~ year_scaled, data = year_df_analysis_subset, family = binomial)
summary(logist_include)

#exp(0.29201) - this is the estimate for year_scaled 
logist_include_cc <- glm(binary ~ year_scaled + clonal_complex, data = year_df_analysis_subset, family = binomial)
#exp(0.29554) - strain is involved to create this and gives you the odds of being resistant year + 1 - when looking  at 
#same strain
summary(logist_include_cc)


logist_include_cc_interaction <- glm(binary ~ year_scaled * clonal_complex, data = year_df_analysis_subset, family = binomial)
summary(logist_include_cc_interaction)
#Generalized Linear Mixed-Effects Models - random intercept
GLMixed_randomintercept <- glmer(binary ~ year + 1 | sex, data = year_df_analysis, family = "binomial")
GLMixed_randomintercept_CC <- glmer(binary ~ year + 1 | clonal_complex, data = year_df_analysis, family = "binomial")
GLMixed_randomintercept_CC_sex <- glmer(binary ~ clonal_complex + 1 | sex, data = year_df_analysis, family = "binomial")



factor(year_df_analysis$clonal_complex)
summary(GLMixed_randomintercept)
summary(GLMixed_randomintercept_CC)
summary(GLMixed_randomintercept_CC_sex)
#Generalized linear miex effect model - random slope
GLMixed_slope <- glmer(binary ~ year + year | sex, data = year_df_analysis, family = "binomial")




