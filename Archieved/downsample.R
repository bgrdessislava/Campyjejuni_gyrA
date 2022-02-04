library(dplyr)
library(ggplot2)
library(ggpubr)

#Archieved

setwd('/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Code')
data = read.table('/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Code/Alighnment_codeby_stevo/BIGSdb_072307_1585245099_65469-aligned.fasta', 
                  sep ='\t', header = TRUE)



data = data[data$position == 86, ]

data$year_source = paste(data$year, data$source)

# Get sample size grouped by year and source
counts = aggregate(ID ~ year_source, data, length)

# Set year_source groups to keep
keep = counts[counts$ID > 10,]

# Only keep year_source within threshold
data_subset = subset(data, data$year_source %in% keep$year_source)

# This is slow - better to initiliase a dataframe with the correct number of rows
nreps = 200
all_data = NULL
for (rep in c(1:nreps)) {
  sampled_data = data_subset %>% group_by(year, source) %>% sample_n(min(keep$ID))
  pos86_counts = aggregate(ID ~ year + source + base, sampled_data, length)
  all_data = rbind.data.frame(all_data, pos86_counts)
}

all_data2 = all_data[all_data$base == 'I' | all_data$base == 'T',]
# Sample year_source groups to same size
ggplot(all_data2, aes(x = year, y = ID, colour = base)) + 
  geom_point(aes(alpha = 0.0001)) +
  geom_smooth() +
  facet_wrap(~source, nrow = 2) +
  theme_pubr(legend = 'bottom')

