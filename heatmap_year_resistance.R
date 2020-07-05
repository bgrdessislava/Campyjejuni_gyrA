library(dplyr)
library(ggplot2)
library(ggpubr)

#This code allows us to get geom smooth line with multiple level of isolates that have been boot strapped 
#It uses the text that we have made from another code
all_data = read.table('/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/500-_iteration_10000.txt', header = TRUE)
all_data2 = all_data[all_data$base == 'I' | all_data$base == 'T',]
  # Sample year_source groups to same size
chicken_human_graph_threshold_500_iteration_100000 <- ggplot(all_data2, aes(x = year, y = ID, fill = base)) + 
  geom_point(aes(alpha = 0.0001)) +
  geom_tile()
  
chicken_human_graph_threshold_500_iteration_100000
  
all_data = read.table('/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/10-_iteration_10000.txt', header = TRUE)
all_data2 = all_data[all_data$base == 'I' | all_data$base == 'T',]
# Sample year_source groups to same size
chicken_human_graph_threshold_10_iteration_100000 <- ggplot(all_data2, aes(x = year, y = ID, colour = base, fill = base)) + 
  geom_point(alpha = 0.005) +
  geom_smooth() +
  facet_wrap(~source, nrow = 2) +
  theme_pubr(legend = 'bottom')

chicken_human_graph_threshold_10_iteration_100000

ggsave(paste(threshold,'_iteration_10000.pdf',sep = '-'))

