library(dplyr)
library(ggplot2)
library(ggpubr)
#This code does the bootstrapping that allowed me to choose same amount of samples across the years
#Uses the binary list code to get the 1 and 0 binary regression but not neccesary worked.
#It showed us that it is not binary

long_list <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/long_list.csv")
long_list <- long_list[long_list$position == '86',]
long_list$year_source = paste(long_list$year, long_list$source)
# Get sample size grouped by year and source
counts = aggregate(ID ~ year_source, long_list, length)

for (threshold in c(5,10,25,50,75,100,200,300,500)){
  
  # Set year_source groups to keep
  keep = counts[counts$ID >= threshold,]
  
  # Only keep year_source within threshold
  data_subset = subset(long_list, long_list$year_source %in% keep$year_source)
  nreps = 100
  for (toreplace in c(TRUE,FALSE)) {
    print(toreplace)
    for (rep in c(1:nreps)) {
      sampled_data = data_subset %>% group_by(year, source) %>% sample_n(threshold,replace = toreplace)
      pos86_counts = aggregate(ID ~ year + source + base, sampled_data, length)
      pos86_counts$rep = rep
      pos86_counts$replacement_condition = toreplace
      if (rep == 1 & toreplace == TRUE) {
        to_append = FALSE
        colnames = TRUE
      } else {
        to_append = TRUE
        colnames = FALSE
      }
      write.table(pos86_counts, paste(threshold,'_iteration_100.txt',sep = ''), append = to_append, col.names = colnames, row.names = FALSE)
    }
  }
  
  all_data = read.table(paste(threshold,'_iteration_100.txt',sep = ''),header = TRUE)
  all_data2 = all_data[all_data$base == 'I' | all_data$base == 'T',]
  # Sample year_source groups to same size
  chicken_human_graph_threshold_5iteration_100 <- ggplot(all_data2, aes(x = year, y = ID, colour = base)) + 
    geom_point(aes(alpha = 0.0001)) +
    geom_smooth() +
    facet_wrap(replacement_condition~source, nrow = 2)
  #theme_pubr(legend = 'bottom')
  
  chicken_human_graph_threshold_5iteration_100
}
#ggsave(paste(threshold,'_iteration_100.png',sep = '-'))
#}



########################################
#Add a new column where if it is I make the value to be 1 and if it is T to be 0 
#long_list


#This code makes iteration of different amount of isolates and does iterations of it.
binary_list <- read.csv(file = '/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/binary_list.csv', header = TRUE)

#binary regression
plot(binary_list$year,binary_list$binary)
#shows how much samples I have for each values
table(binary_list$binary)


binary_list$binary = as.numeric(binary_list$binary)

#model1 <- glm(binary_list$binary ~ binary_list$year*binary_list$binary,binomial)
#model2 <- glm(binary_list$binary ~ binary_list$year + binary_list$binary,binomial)

attach(binary_list)
#detach(binary_list)
modela <- glm(binary~year,binomial)
xv <-seq(1970,2020,10)
yv<- predict(modela,list(year=xv),type="response")
plot(year,binary,pch=21,bg="yellow")
lines(xv,yv,col="blue")

summary(modela)
