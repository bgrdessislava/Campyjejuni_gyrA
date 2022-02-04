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
year_df<- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/long_list_allmeta.csv",
                   na.strings = c(""," "))
#year_df[is.na(year_df)] <-  "Unknown"



#Taking the first column which is not needed
year_df = year_df[,-1]
#Only picking human stool
year_df = year_df %>%
  filter(source == 'human_stool')

write.csv(year_df,file = "/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/loglist_humanonly_allmeta.csv")

#Only picking up the bases with T or I, creating total I and T columns and making the proportion
a = year_df %>% 
  filter(base == 'I' | base == 'T') %>%
  group_by(year, age_yr) %>%
  summarise('total_I' = sum(base=='I'),
            'total_T'= sum(base=='T'),
            'total' = length(base),
            'proportion_I'= total_I / total)


#Choosing samples which the total is 10 
a = filter(a, total >5)
#Choosing clonal complex which has at least 5 per year
age_yr_count = a %>% 
  group_by(age_yr) %>%
  summarise('total_age_yr' = length(total)) %>%
  filter(total_age_yr >5) 



#layering the ST_count with at least 5 in the initial filter of a with total equalling 10
a = filter(a, age_yr %in% age_yr_count$age_yr) 

#ggploting the graphs but using log scale with 10 base, facet wrap allowing all of them to show
plot_age_yr <- 
  ggplot(a, aes(x=year,y=proportion_I, colour=log10(total))) +
  geom_point() +
  geom_smooth(color = "black",size = 0.2) +
  facet_wrap(~age_yr) +
  xlim(1997,2018) +
  ylim(0,1) + 
  scale_color_viridis() +
  geom_vline(xintercept = 2006,colour="red", linetype = "longdash") +
  labs(title="Resistance isolates (Isoleucine) changes in human stool samples overtime in different age \nRed line shows when the growth promoting antibiotic were banned in 2006 in the UK",
       x ="Year", y = "Log scale proportion") +
  theme(plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5))

plot_age_yr

ggsave("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Figures/Scatterplot/plots_age.png",plot=plot_age, width = 50, height = 30, units = "cm",dpi = 600)

#Heatmaps
data_wide <- dcast(a, age_yr ~ year, value.var="proportion_I")
rownames(data_wide) = data_wide$age_yr
data_wide$age_yr <- NULL
heatmap_age_yr <- pheatmap(data_wide, scale = "none", cluster_rows = TRUE, cluster_cols = FALSE,
                       color = viridis(10),
                       main = "Isoleucine resistance across time in age_yr",
                       fontsize=15,na_col = "white")

heatmap_age_yr

ggsave("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Figures/Heatmaps/heatmap_CC.png",plot=heatmap_CC, width = 50, height = 30, units = "cm",dpi = 600)


