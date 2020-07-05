library(dplyr)
library(ggplot2)
library(ggpubr)

#Same as the fulllist_binary_list so have been archieved
long_list <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/long_list.csv")
long_list <- long_list[long_list$position == '86',]
year_86 <- long_list[,c("year","base")]
s <- summary(year_86)
capture.output(s, file = "year_86.txt")

long_list$year_source = paste(long_list$year, long_list$source)
#Making a binary column which only has T or I and this to equal to 0 or 1 
long_list$binary[long_list$base == 'T'] <- 0
long_list$binary[long_list$base =='I'] <- 1
long_list$binary[long_list$base !='I' & long_list$base !='T'] <- 'else'

#Creating a binary list which has only 0 or 1 
binary_list <- long_list %>% select(year,binary) %>%
  filter(binary == 0 | binary == 1) %>% filter(year > 1990)

write.csv(binary_list,"binary_list.csv",row.names = FALSE)
