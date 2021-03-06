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
#Getting the raw big database with all metadata
year_df<- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/long_list_allmeta.csv")

#Taking the first column which is not needed
year_df = year_df[,-1]
#Only picking human stool
year_df = year_df %>%
  filter(source == 'human_stool')

#Only picking up the bases with T or I, creating total I and T columns and making the proportion
a = year_df %>% 
  filter(base == 'I' | base == 'T') %>%
  group_by(year, rMLST) %>%
  summarise('total_I' = sum(base=='I'),
            'total_T'= sum(base=='T'),
            'total' = length(base),
            'proportion_I'= total_I / total)


#Choosing samples which the total is 10 
a = filter(a, total >= 5)
#Choosing clonal complex which has at least 5 
rMLST_count = a %>% 
  group_by(rMLST) %>%
  summarise('total_rMLST' = length(total)) %>%
  filter(total_rMLST > 5) 

#layering the ST_count with at least 5 in the initial filter of a with total equalling 10
a = filter(a, rMLST %in% rMLST_count$rMLST)

#ggploting the graphs but using log scale with 10 base, facet wrap allowing all of them to show
plot_rMLST <- 
  ggplot(a, aes(x=year,y=proportion_I, colour=log10(total))) +
  geom_point() +
  geom_smooth(color = "black",size = 0.2) +
  facet_wrap(~rMLST) +
  xlim(1997,2018) +
  ylim(0,1) + 
  scale_color_viridis() +
  geom_vline(xintercept = 2006,colour="red", linetype = "longdash") +
  labs(title="Resistance isolates (Isoleucine) changes in human stool samples overtime in rMLST Scheme (53 locis).\nRed line shows when the growth promoting antibiotic were banned in 2006 in the UK",
       x ="Year", y = "Log scale proportion") +
  theme(plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5))

plot_rMLST

ggsave("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Figures/Scatterplot/plot_rMLST.png",plot=plot_rMLST, width = 50, height = 30, units = "cm",dpi = 600)

