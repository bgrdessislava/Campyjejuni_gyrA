library(ggplot2)
library(stats)
library(gplots)
library(d3heatmap)
library(reshape2)
library(pheatmap)
library(dplyr)
library(tidyverse)
library(viridis)

#Heatmaps of 3 clusters illustraiting the overall resistance
#geom smooth method illustrating the trend in resistance across time with log scale of change as well.

setwd("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data")
#Getting the raw big database with all metadata
year_df<- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/CleanMeta_for_Tableau_resistantcleaned_10362.csv")


#adding binary column
year_df$binary_fluro = ifelse(year_df$Gyrase_DessyPipeline == 'I', 1, 0)
year_df$tetO =  ifelse(year_df$tetracycline_genotypes_2_AlisonPipeline == 'tetO', 1, 0)
year_df$multidrug = year_df$binary_fluro + year_df$tetO
year_df$random = sample(year_df$binary_fluro)

#Only picking up the bases with T or I, creating total I and T columns and making the proportion
a = year_df %>% 
  group_by(year, clonal_complex..MLST.) %>%
  summarise('total_I' = sum(multidrug=='2'),
            'total_T'= sum(multidrug=='0'),
            'total' = length(tetO),
            'proportion_I'= total_I / total)


#Choosing samples which the total is 10 
a = filter(a, total >= 10)
#Choosing clonal complex which has at least 5 
CC_count = a %>% 
  group_by(clonal_complex..MLST.) %>%
  summarise('total_CC' = length(total)) %>%
  filter(total_CC > 5) 

#layering the ST_count with at least 5 in the initial filter of a with total equalling 10
a = filter(a, clonal_complex..MLST. %in% CC_count$clonal_complex..MLST.)



#write.csv(a, file = "/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/clonal_year_tetracycline_proportion_df.csv")


#Just choosing ST-353
a_353 <- filter(a, clonal_complex == "ST-353_complex")
a_353_464_354 <- filter(a, clonal_complex == "ST-353_complex" | clonal_complex == "ST-354_complex" | clonal_complex == "ST-464_complex")

a_year_2012 <- filter(a, year == "2012")
write.csv(a_year_2012, file = "/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/2012_year_proportion_df.csv")
write.csv(a_353, file = "/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/cc353_year_proportion_df.csv")
write.csv(a_353_464_354, file = "/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/a_353_464_354_year_proportion_df.csv")

#ggploting the graphs but using log scale with 10 base, facet wrap allowing all of them to show
plot_CC <- 
  ggplot(a, aes(x=year,y=proportion_I, colour=log10(total))) +
  geom_point() +
  geom_smooth(color = "black",size = 0.2) +
  facet_wrap(~clonal_complex..MLST.) +
  xlim(1997,2018) +
  ylim(0,1) + 
  scale_color_viridis() +
  geom_vline(xintercept = 2006,colour="red", linetype = "longdash") +
  labs(title="Multidrug resistance changes in human stool samples overtime in CC Scheme (4 locis out of 7 MLST same).\nRed line shows when the growth promoting antibiotic were banned in 2006 in the UK",
       x ="Year", y = "Log scale proportion") +
  theme(plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5))

plot_CC

ggsave("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Figures/Scatterplot/tetO_plot_CC_10417.png",plot=plot_CC, width = 50, height = 30, units = "cm",dpi = 600)


#Heatmaps
data_wide <- dcast(a, clonal_complex..MLST. ~ year, value.var="proportion_I")
rownames(data_wide) = data_wide$clonal_complex..MLST.
data_wide$clonal_complex..MLST. <- NULL
#data_wide_2012Only <- filter(data_wide, year == "2012")

heatmap_CC <- pheatmap(data_wide, scale = "none", cluster_rows = TRUE, cluster_cols = FALSE,
                       color = viridis(10),
                       main = "Multidrug resistance across time in clonal-complex (4 loci same in 7 MLST scheme)",
                       fontsize=15,na_col = "white")

heatmap_CC

ggsave("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Figures/Heatmaps/multidrug_res_heatmap_CC.png",plot=heatmap_CC, width = 50, height = 30, units = "cm",dpi = 600)


#Trying out to put linear regression
plot_CC <- 
  ggplot(a, aes(x=year,y=proportion_I, colour=log10(total))) +
  geom_point() +
  facet_wrap(~clonal_complex..MLST.) +
  xlim(1997,2018) +
  ylim(0,1) + 
  scale_color_viridis() +
  geom_smooth(aes(group = clonal_complex..MLST.), method = lm, 
              se=FALSE, color="black", formula = y ~ x) +
  geom_text(x = 25, y = 300, label = lm_eq(df), parse = TRUE) +
  labs(title="Resistance isolates (Tetracycline) changes in human stool samples overtime in CC Scheme (4 locis out of 7 MLST same).\nRed line shows when the growth promoting antibiotic were banned in 2006 in the UK",
       x ="Year", y = "Log scale proportion") +
  theme(plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5))

plot_CC

ggsave("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Figures/Scatterplot/plot_CC_linearregression.png",plot=plot_CC, width = 50, height = 30, units = "cm",dpi = 600)


