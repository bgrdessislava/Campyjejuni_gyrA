library(ggplot2)
library(stats)
library(gplots)
library(d3heatmap)
library(reshape2)
library(pheatmap)
library(dplyr)
library(tidyverse)
library(viridis)

setwd("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data")

year_df<- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/long_list_allmeta.csv")
#Taking the first column which is not needed
year_df = year_df[,-1]
#Only picking human stool
year_df = year_df %>%
  filter(source == 'human_stool') %>%
  filter(base == 'I' | base == 'T') %>%
  filter(year == 2012)
year_df$resistant = ifelse(year_df$base == 'I', TRUE, FALSE)
G1 <- c('ST-354_complex','ST-464_complex','ST-353_complex','')

G2 <- c('ST-48_complex','ST-22_complex','ST-61_complex',
        'ST-42_complex','ST-283_complex','ST-45_complex')

year_df$cluster <- ifelse(year_df$clonal_complex %in% G1, 'Group1',
                      ifelse(year_df$clonal_complex %in% G2, 'Group2', 'Group3'))

prop.table(table(year_df$cluster, year_df$resistant))*100

chisq.test(year_df$cluster, year_df$resistant)
