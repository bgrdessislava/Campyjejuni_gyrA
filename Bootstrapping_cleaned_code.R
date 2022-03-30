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

#without bootsrapping
long_list_revisited <- long_list %>% 
  filter(source == 'human_stool') %>%
  filter(base == 'I') %>% 
  filter(year >= 1996) %>% 
  group_by(year) %>%
  summarise(resistance_count = n())

without_bootstrap = ggplot(aes(year, resistance_count), data = long_list_revisited) +
         geom_point(size = 5) +
  ggtitle('A) Resistant isolates across time without bootstrapping') +
  labs(y = "Resistant isolate count", x = "") +
  theme(plot.title = element_text(hjust = 0.5),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),text = element_text(size = 15)) +
        scale_x_continuous(breaks=seq(1997, 2018, 3))

without_bootstrap

ggsave("../Figures/withoutbootsrap.png",dpi = 300)

tmp <- long_list %>% 
  filter(source == 'human_stool') %>%
  filter(year >= 1997) %>%
  filter(base == 'I' | base == 'T') %>% 
  group_by(year, base) %>%
  summarise(count = n())

proportion_resistance = ggplot(tmp, aes(fill=base, y=count, x=year)) +
  geom_bar(position='stack', stat='identity') +
  ggtitle('B) Resistant and Suscpetible Isolate Count') +
  labs(y = "Count", x = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "Fluoroquinolone Resistance", labels = c("Resistant", "Susceptible")) +
  theme(legend.position="top", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black"),text = element_text(size = 15)) +
  scale_x_continuous(breaks=seq(1997, 2018, 3))

proportion_resistance
ggsave("../Figures/proportion_resistance.png",dpi = 300)

#bootsrapping starting

for (threshold in c(50,100)){
  
  # Set year_source groups to keep
  keep = counts[counts$ID >= threshold,]
  
  # Only keep year_source within threshold
  data_subset = subset(long_list, long_list$year_source %in% keep$year_source)
  nreps = 100
  toreplace = TRUE


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

  
  all_data = read.table(paste(threshold,'_iteration_100.txt',sep = ''),header = TRUE)
  all_data2 = all_data[all_data$base == 'I',]
  all_data2 = all_data2[all_data2$source == 'human_stool',]
  # Sample year_source groups to same size
  human_graph <- ggplot(all_data2, aes(x = year, y = ID, alpha = 0.1)) + 
    geom_point() +
    geom_smooth(method='lm') +
    labs(title="C) Percentage gyrA-ThR86Ile across time",
         x= "", y = "Percentage ") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),text = element_text(size = 15)) +
          scale_y_continuous(name="Percentage", limits=c(0, 70),breaks = seq(0,70,10)) +
          scale_x_continuous(name="", limits=c(1997, 2018),breaks = seq(1997,2018,3)) +
          theme(plot.title = element_text(hjust = 0.4, size = 18),
                legend.key.size = unit(2, 'mm'),legend.position = "none")
  human_graph 
}
human_graph 

#ggsave(paste(threshold,'_iteration_100.png',sep = '-'))
#}

linear_bootstrap <- lm(ID ~ year, data = all_data2)
summary(linear_bootstrap)
anova(linear_bootstrap, test = 'Chi')

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
