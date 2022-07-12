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
year_df_catboost<- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/Catboost_group_df.csv")
year_df_resistance<- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/CleanMeta_for_Tableau_resistantcleaned_10362.csv")

year_df_resistance = year_df_resistance %>% 
  rename(ID = id)

combined_df <- inner_join(year_df_catboost, year_df_resistance, by = "ID")

#adding binary column
combined_df$binary = ifelse(combined_df$Gyrase_DessyPipeline == 'I', 1, 0)

counts = combined_df %>% count(Group)
group_higher_than30isolate = c('1','2','3','8','9','10','16',
                              '17','18','23','24','26','27','29','30','31')
combined_df <- combined_df[combined_df$Group %in% group_higher_than30isolate,] 


#Groups overtime
split_plot <- ggplot(aes(combined_df$year, combined_df$binary), data = combined_df) + 
  geom_point() + 
  geom_smooth(method = glm, method.args = c(family=binomial)) +
  facet_wrap(~ combined_df$Group) + # create a facet for each mountain range +
  geom_vline(aes(xintercept = 2006),colour="red", linetype = "longdash") +
  xlab("length") + 
  ylab("test score") +
  ggtitle('NeighbourGroups and its fluoroquinolone resistance across time, at least 30 isolates per group ')+
  ylab('Fluoroquinolone resistance') +
  xlab('Year') +
  scale_x_continuous(breaks=seq(1997, 2018, 5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 15),panel.grid.major = element_blank(), 
        axis.text.x = element_text(size = 8),
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),text = element_text(size = 12),
        legend.key.size = unit(2, 'mm')) 

split_plot
ggsave("../Figures/NeighbourGroups_fluoroquinoloneRes_atleast30isolate.png",dpi = 300)


