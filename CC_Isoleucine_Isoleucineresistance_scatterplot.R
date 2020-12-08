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
  group_by(year, clonal_complex) %>%
  summarise('total_I' = sum(base=='I'),
            'total_T'= sum(base=='T'),
            'total' = length(base),
            'proportion_I'= total_I / total)


#Choosing samples which the total is 10 
a = filter(a, total >= 10)
#Choosing clonal complex which has at least 5 
CC_count = a %>% 
  group_by(clonal_complex) %>%
  summarise('total_CC' = length(total)) %>%
  filter(total_CC > 5) 

#layering the ST_count with at least 5 in the initial filter of a with total equalling 10
a = filter(a, clonal_complex %in% CC_count$clonal_complex)

write.csv(a, file = "/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/clonal_year_proportion_df.csv")


#Just choosing ST-353
a_353 <- filter(a, clonal_complex == "ST-353_complex")
a_year_2012 <- filter(a, year == "2012")
write.csv(a_year_2012, file = "/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/2012_year_proportion_df.csv")
write.csv(a_353, file = "/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/cc353_year_proportion_df.csv")

#ggploting the graphs but using log scale with 10 base, facet wrap allowing all of them to show
plot_CC <- 
  ggplot(a, aes(x=year,y=proportion_I, colour=log10(total))) +
  geom_point() +
  geom_smooth(color = "black",size = 0.2) +
  facet_wrap(~clonal_complex) +
  xlim(1997,2018) +
  ylim(0,1) + 
  scale_color_viridis() +
  geom_vline(xintercept = 2006,colour="red", linetype = "longdash") +
  labs(title="Resistance isolates (Isoleucine) changes in human stool samples overtime in CC Scheme (4 locis out of 7 MLST same).\nRed line shows when the growth promoting antibiotic were banned in 2006 in the UK",
       x ="Year", y = "Log scale proportion") +
  theme(plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5))

plot_CC

ggsave("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Figures/Scatterplot/plot_CC.png",plot=plot_CC, width = 50, height = 30, units = "cm",dpi = 600)

#Heatmaps
data_wide <- dcast(a, clonal_complex ~ year, value.var="proportion_I")
rownames(data_wide) = data_wide$clonal_complex
data_wide$clonal_complex <- NULL
data_wide_2012Only <- filter(data_wide, year == "2012")

heatmap_CC <- pheatmap(data_wide, scale = "none", cluster_rows = TRUE, cluster_cols = FALSE,
         color = viridis(10),
         main = "Isoleucine resistance across time in clonal-complex (4 loci same in 7 MLST scheme)",
         fontsize=15,na_col = "white")

heatmap_CC

ggsave("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Figures/Heatmaps/heatmap_CC.png",plot=heatmap_CC, width = 50, height = 30, units = "cm",dpi = 600)


