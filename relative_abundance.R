library(ggplot2)
library(forcats)
library(magrittr)
library(ggpubr)
library(scales)
library(pracma)
library(ggpmisc)
setwd("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Code")

#This is a good code that looks at relative abundance and tries to put linear regression line
#It is a great documented code that needs more ways to check what kind of linear regression it is

caecal_CC <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/clonal_complex_caecal.csv")
caecal_ST<- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/ST_caecal.csv")
carcass_CC <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/clonal_complex_carcass.csv")
carcass_ST <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/ST_carcass.csv")
food_CC <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/clonal_complex_caecal.csv")
food_ST <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/ST_food.csv")
human_CC <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/clonal_complex_human_newcastle_oxford.csv")
human_ST <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/ST_human_newcastle_oxford.csv")
####Procedure for data cleaning ######
#1. Ordering the frequency value by putting frequency to be levels and '-' makes the order to be from high-low
#2. Creating a column called rank that allows us to make the categorical value the same order from 1 to the end 
# and also formula for ggpmisc to work
#3. ggplot the graph
#4. Save the image onto directory (manually)
#5. linear model equation
#6. ggplot LOG
#7.save the image 
# linear model equation LOG

####################Caecal CC #######################
#1. Ordering the clonal complex , ST
#Grouping values that lets the categorical value to be ordered # By putting - we can reorder it 
caecal_CC$clonal.complex <- factor(caecal_CC$clonal.complex, 
                                       levels = caecal_CC$clonal.complex[order(-caecal_CC$Frequency)])
#2. Add one column called rank and formula
#as.numeric makes the string to become numeric
caecal_CC$rank = as.numeric(rownames(caecal_CC))
caecal_CCformula <-caecal_CC$Frequency ~ I(caecal_CC$rank^2)

#3. ggplot the caecal graph 
#hjust 0.5 makes it so that it is centered angle = 90 makes the x axis to rotate
#caecal_CCformula is a function used in stat_poly_eq 

caecal_CC_graph= ggplot(caecal_CC, 
                            aes(rank, Frequency)) + 
  labs(y = "Frequency",
       x = "Rank") +  
  ggtitle("Relative abundance caecal CC(4 allele)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(linetype = "dashed")+
  geom_point(color="blue") +
  theme(plot.background = element_rect(fill = "white"))
  #scale_y_continuous(trans = log10_trans(),
                    # breaks = trans_breaks("log10", function(x) 10^x),
                    # labels = trans_format("log10", math_format(10^.x)))+
  #stat_poly_eq(formula = caecal_CCformula, 
        #       aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
        #       parse = TRUE)+
 # geom_smooth(method = "lm", se = FALSE,color='red')
  
caecal_CC_graph

#4. Save the image onto directory
#5. linear model graph
mod <- lm(caecal_CC$Frequency ~ I((caecal_CC$rank)^2))

#####Log Scale caecal CC####### Makes Y-axis in log format and put line of regression

#6. ggplot the caecal graph LOG
caecal_CC_graph_log= ggplot(caecal_CC, 
                            aes((rank^2), Frequency, group = 1)) + 
  labs(y = "Log",
       x = "Rank") +  
  ggtitle("Relative abundance caecal CC (4 allele) LOG") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(linetype = "dashed")+
  geom_point(color="blue") +
  theme(plot.background = element_rect(fill = "white"))+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)))+
  geom_smooth(method = "lm", se = FALSE,color='red')+
  stat_poly_eq(formula = caecal_CCformula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)+
  geom_smooth(method = "lm", se = FALSE,color='red')

caecal_CC_graph_log
#7. Save the image onto directory
#8. linear model graph
lm_caecal_CC <- lm(caecal_CC$Frequency ~ caecal_CC$rank)

#Another function that lets me find the slope and the y intercept (will not be used for other examples)
polyfit(1:24,caecal_CC$Frequency,1)

#####################caecal ST###########################
#1. Ordering the clonal complex , ST
#Grouping values that lets the categorical value to be ordered # By putting - we can reorder it 
caecal_ST$ST <- factor(caecal_ST$ST, 
                                       levels = caecal_ST$ST[order(-caecal_ST$Frequency)])
#2. Add one column called rank and formula
#as.numeric makes the string to become numeric
caecal_ST$rank = as.numeric(rownames(caecal_ST))
#caecal_CCformula is a function used in stat_poly_eq 
caecal_STformula <-caecal_ST$Frequency ~ I(caecal_ST$rank^2)


#3. ggplot the caecal graph 
#hjust 0.5 makes it so that it is centered angle = 90 makes the x axis to rotate
caecal_ST_graph= ggplot(caecal_ST, 
                        aes(rank, Frequency)) + 
  labs(y = "Frequency",
       x = "Rank") +  
  ggtitle("Relative abundance caecal ST (7 allele)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(linetype = "dashed")+
  geom_point(color="blue") +
  theme(plot.background = element_rect(fill = "white"))
#+ scale_y_continuous(trans = log10_trans(),
#                   breaks = trans_breaks("log10", function(x) 10^x),
#                   labels = trans_format("log10", math_format(10^.x)))+
#  geom_smooth(method = "lm", se = FALSE,color='red')+
#  stat_poly_eq(formula = caecal_STformula, 
#               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#               parse = TRUE)+
#  geom_smooth(method = "lm", se = FALSE,color='red')

caecal_ST_graph

#4. Save the image onto directory

#5. linear model graph
lm_caecal_ST <- lm(caecal_ST$Frequency ~ caecal_ST$rank)

#####z##################Log Scale caecal ST#####################
#6. ggplot the caecal graph LOG 
caecal_ST_graph_log= ggplot(caecal_ST, 
                            aes((rank^2), Frequency, group = 1)) + 
  labs(y = "Log",
       x = "Rank") +  
  ggtitle("Relative abundance caecal ST (7 allele) LOG") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(linetype = "dashed")+
  geom_point(color="blue") +
  theme(plot.background = element_rect(fill = "white"))+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)))+
  geom_smooth(method = "lm", se = FALSE,color='red')+
  stat_poly_eq(formula = caecal_STformula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)+
  geom_smooth(method = "lm", se = FALSE,color='red')

caecal_ST_graph_log

#7. Save the log images
##################################carcass CC#################################
#1. Ordering the clonal complex , ST
#Grouping values that lets the categorical value to be ordered # By putting - we can reorder it 
carcass_CC$clonal.complex <- factor(carcass_CC$clonal.complex, 
                                   levels = carcass_CC$clonal.complex[order(-carcass_CC$Frequency)])
#2. Add one column called rank and formula
#as.numeric makes the string to become numeric
carcass_CC$rank = as.numeric(rownames(carcass_CC))
carcass_CCformula <-carcass_CC$Frequency ~ carcass_CC$(rank^2)


#3. ggplot the caecal graph 
#hjust 0.5 makes it so that it is centered angle = 90 makes the x axis to rotate
#caecal_CCformula is a function used in stat_poly_eq 

carcass_CC_graph= ggplot(caecal_CC, 
                        aes(rank, Frequency)) + 
  labs(y = "Frequency",
       x = "Rank") +  
  ggtitle("Relative abundance carcass CC") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(linetype = "dashed")+
  geom_point(color="blue") +
  theme(plot.background = element_rect(fill = "wheat"))
#scale_y_continuous(trans = log10_trans(),
# breaks = trans_breaks("log10", function(x) 10^x),
# labels = trans_format("log10", math_format(10^.x)))+
#stat_poly_eq(formula = caecal_CCformula, 
#       aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#       parse = TRUE)+
# geom_smooth(method = "lm", se = FALSE,color='red')

carcass_CC_graph

#4. Save the image onto directory
#5. linear model graph
mod <- lm(carcass_CC$Frequency ~ carcass_CC$rank)

#####Log Scale caecal CC####### Makes Y-axis in log format and put line of regression

#6. ggplot the caecal graph LOG
carcass_CC_graph_log= ggplot(carcass_CC, 
                            aes((rank^2), Frequency, group = 1)) + 
  geom_point(stat = "identity") + 
  labs(y = "Frequency",
       x = "Rank") +  
  ggtitle("Relative abundance carcass CC (4 allele)LOG") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(linetype = "dashed")+
  geom_point(color="blue") +
  theme(plot.background = element_rect(fill = "wheat"))+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)))+
  stat_poly_eq(formula = caecal_CCformula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)+
  geom_smooth(method = "lm", se = FALSE,color='red')

carcass_CC_graph_log
#7. Save the image onto directory
#8. linear model graph
mod_carcass_cc <- lm(carcass_CC$Frequency ~ carcass_CC$rank)

##################################carcass ST#################################
#1. Ordering the clonal complex , ST
#Grouping values that lets the categorical value to be ordered # By putting - we can reorder it 
carcass_ST$ST <- factor(carcass_ST$ST, 
                       levels = carcass_ST$ST[order(-carcass_ST$Frequency)])
#2. Add one column called rank and formula
#as.numeric makes the string to become numeric
carcass_ST$rank = as.numeric(rownames(carcass_ST))
#caecal_CCformula is a function used in stat_poly_eq 
carcass_STformula <-carcass_ST$Frequency ~ I(carcass_ST$rank^2)


#3. ggplot the caecal graph 
#hjust 0.5 makes it so that it is centered angle = 90 makes the x axis to rotate
carcass_ST_graph= ggplot(carcass_ST, 
                        aes(rank, Frequency)) + 
  labs(y = "Frequency",
       x = "Rank") +  
  ggtitle("Relative abundance carcass ST (7 allele)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(linetype = "dashed")+
  geom_point(color="blue") +
  theme(plot.background = element_rect(fill = "wheat"))
#+ scale_y_continuous(trans = log10_trans(),
#                   breaks = trans_breaks("log10", function(x) 10^x),
#                   labels = trans_format("log10", math_format(10^.x)))+
#  geom_smooth(method = "lm", se = FALSE,color='red')+
#  stat_poly_eq(formula = caecal_STformula, 
#               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#               parse = TRUE)+
#  geom_smooth(method = "lm", se = FALSE,color='red')

carcass_ST_graph

#4. Save the image onto directory

#5. linear model graph
lm_carcass_ST <- lm(carcass_ST$Frequency ~ carcass_ST$rank)

#####z##################Log Scale caecal ST#####################
#6. ggplot the caecal graph LOG 
carcass_ST_graph_log= ggplot(carcass_ST, 
                            aes((rank^2), Frequency, group = 1)) + 
  labs(y = "Log",
       x = "Rank") +  
  ggtitle("Relative abundance carcass ST (7 allele) LOG") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(linetype = "dashed")+
  geom_point(color="blue") +
  theme(plot.background = element_rect(fill = "wheat"))+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)))+
  geom_smooth(method = "lm", se = FALSE,color='red')+
  stat_poly_eq(formula = carcass_STformula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)

carcass_ST_graph_log

#7. Save the log images
#################################Food CC#####################################
#1. Ordering the clonal complex , ST
#Grouping values that lets the categorical value to be ordered # By putting - we can reorder it 
food_CC$clonal.complex <- factor(food_CC$clonal.complex, 
                                    levels = food_CC$clonal.complex[order(-food_CC$Frequency)])
#2. Add one column called rank and formula
#as.numeric makes the string to become numeric
food_CC$rank = as.numeric(rownames(food_CC))
food_CCformula <-food_CC$Frequency ~ I(food_CC$rank^2)


#3. ggplot the caecal graph 
#hjust 0.5 makes it so that it is centered angle = 90 makes the x axis to rotate
#caecal_CCformula is a function used in stat_poly_eq 

food_CC_graph= ggplot(food_CC, 
                         aes(rank, Frequency)) + 
  labs(y = "Frequency",
       x = "Rank") +  
  ggtitle("Relative abundance food CC (4 allele)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(linetype = "dashed")+
  geom_point(color="blue") +
  theme(plot.background = element_rect(fill = "wheat1"))
#scale_y_continuous(trans = log10_trans(),
# breaks = trans_breaks("log10", function(x) 10^x),
# labels = trans_format("log10", math_format(10^.x)))+
#stat_poly_eq(formula = caecal_CCformula, 
#       aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#       parse = TRUE)+
# geom_smooth(method = "lm", se = FALSE,color='red')

food_CC_graph

#4. Save the image onto directory
#5. linear model graph
mod <- lm(food_CC$Frequency ~ food_CC$rank)

#####Log Scale caecal CC####### Makes Y-axis in log format and put line of regression

#6. ggplot the caecal graph LOG
food_CC_graph_log= ggplot(food_CC, 
                             aes((rank^2), Frequency, group = 1)) + 
  labs(y = "Log",
       x = "Rank") +  
  ggtitle("Relative abundance food CC (4 allele) LOG") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(linetype = "dashed")+
  geom_point(color="blue") +
  theme(plot.background = element_rect(fill = "wheat2"))+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)))+
  geom_smooth(method = "lm", se = FALSE,color='red')+
  stat_poly_eq(formula = food_CCformula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)+
  geom_smooth(method = "lm", se = FALSE,color='red')

food_CC_graph_log
#7. Save the image onto directory
#8. linear model graph
mod <- lm(food_CC$Frequency ~ food_CC$rank)

##################################food ST#################################
#1. Ordering the clonal complex , ST
#Grouping values that lets the categorical value to be ordered # By putting - we can reorder it 
food_ST$ST <- factor(food_ST$ST, 
                        levels = food_ST$ST[order(-food_ST$Frequency)])
#2. Add one column called rank and formula
#as.numeric makes the string to become numeric
food_ST$rank = as.numeric(rownames(food_ST))
#caecal_CCformula is a function used in stat_poly_eq 
food_STformula <-food_ST$Frequency ~ I(food_ST$rank^2)


#3. ggplot the caecal graph 
#hjust 0.5 makes it so that it is centered angle = 90 makes the x axis to rotate
food_ST_graph= ggplot(food_ST, 
                         aes(rank, Frequency)) + 
  labs(y = "Frequency",
       x = "Rank") +  
  ggtitle("Relative abundance food ST (7 allele)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(linetype = "dashed")+
  geom_point(color="blue") +
  theme(plot.background = element_rect(fill = "wheat2"))
#+ scale_y_continuous(trans = log10_trans(),
#                   breaks = trans_breaks("log10", function(x) 10^x),
#                   labels = trans_format("log10", math_format(10^.x)))+
#  geom_smooth(method = "lm", se = FALSE,color='red')+
#  stat_poly_eq(formula = caecal_STformula, 
#               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#               parse = TRUE)+
#  geom_smooth(method = "lm", se = FALSE,color='red')

food_ST_graph

#4. Save the image onto directory

#5. linear model graph
lm_food_ST <- lm(food_ST$Frequency ~ food_ST$rank)

#####z##################Log Scale food ST#####################
#6. ggplot the caecal graph LOG 
food_ST_graph_log= ggplot(food_ST, 
                             aes((rank^2), Frequency, group = 1)) + 
  labs(y = "Log",
       x = "Rank") +  
  ggtitle("Relative abundance food ST (7 allele) LOG") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(linetype = "dashed")+
  geom_point(color="blue") +
  theme(plot.background = element_rect(fill = "wheat2"))+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)))+
  geom_smooth(method = "lm", se = FALSE,color='red')+
  stat_poly_eq(formula = food_STformula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)+
  geom_smooth(method = "lm", se = FALSE,color='red')

food_ST_graph_log

#7. Save the log images


##################################HumanFeces CC#################################
#1. Ordering the clonal complex , ST
#Grouping values that lets the categorical value to be ordered # By putting - we can reorder it 
human_CC$clonal.complex <- factor(human_CC$clonal.complex, 
                                 levels = human_CC$clonal.complex[order(-human_CC$Frequency)])
#2. Add one column called rank and formula
#as.numeric makes the string to become numeric
human_CC$rank = as.numeric(rownames(human_CC))
human_CCformula <-human_CC$Frequency ~ I(human_CC$rank^2)

#3. ggplot the caecal graph 
#hjust 0.5 makes it so that it is centered angle = 90 makes the x axis to rotate
#caecal_CCformula is a function used in stat_poly_eq 

human_CC_graph= ggplot(human_CC, 
                      aes(rank, Frequency)) + 
  labs(y = "Frequency",
       x = "Rank") +  
  ggtitle("Relative abundance humanfeces CC (4 allele)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(linetype = "dashed")+
  geom_point(color="blue") +
  theme(plot.background = element_rect(fill = "wheat2"))
#scale_y_continuous(trans = log10_trans(),
# breaks = trans_breaks("log10", function(x) 10^x),
# labels = trans_format("log10", math_format(10^.x)))+
#stat_poly_eq(formula = caecal_CCformula, 
#       aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#       parse = TRUE)+
# geom_smooth(method = "lm", se = FALSE,color='red')

human_CC_graph

#4. Save the image onto directory
#5. linear model graph
mod <- lm(human_CC$Frequency ~ human_CC$rank)

#####Log Scale caecal CC####### Makes Y-axis in log format and put line of regression

#6. ggplot the caecal graph LOG
human_CC_graph_log= ggplot(human_CC, 
                          aes((rank^2), Frequency, group = 1)) + 
  labs(y = "Log",
       x = "Rank") +  
  ggtitle("Relative abundance humanfeces CC (4 allele) LOG") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(linetype = "dashed")+
  geom_point(color="blue") +
  theme(plot.background = element_rect(fill = "wheat4"))+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)))+
  geom_smooth(method = "lm", se = FALSE,color='red')+
  stat_poly_eq(formula = human_CCformula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)+
  geom_smooth(method = "lm", se = FALSE,color='red')

human_CC_graph_log
#7. Save the image onto directory
#8. linear model graph
mod <- lm(human_CC$Frequency ~ human_CC$rank)

##################################human ST#################################
#1. Ordering the clonal complex , ST
#Grouping values that lets the categorical value to be ordered # By putting - we can reorder it 
human_ST$ST <- factor(human_ST$ST, 
                     levels = human_ST$ST[order(-human_ST$Frequency)])
#2. Add one column called rank and formula
#as.numeric makes the string to become numeric
human_ST$rank = as.numeric(rownames(human_ST))
#caecal_CCformula is a function used in stat_poly_eq 
human_STformula <-human_ST$Frequency ~ I(human_ST$rank^2)

#3. ggplot the caecal graph 
#hjust 0.5 makes it so that it is centered angle = 90 makes the x axis to rotate
human_ST_graph= ggplot(human_ST, 
                      aes(rank, Frequency)) + 
  labs(y = "Frequency",
       x = "Rank") +  
  ggtitle("Relative abundance humanfeces ST (7 allele)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(linetype = "dashed")+
  geom_point(color="blue") +
  theme(plot.background = element_rect(fill = "wheat4"))
#+ scale_y_continuous(trans = log10_trans(),
#                   breaks = trans_breaks("log10", function(x) 10^x),
#                   labels = trans_format("log10", math_format(10^.x)))+
#  geom_smooth(method = "lm", se = FALSE,color='red')+
#  stat_poly_eq(formula = caecal_STformula, 
#               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#               parse = TRUE)+
#  geom_smooth(method = "lm", se = FALSE,color='red')

human_ST_graph

#4. Save the image onto directory

#5. linear model graph
lm_human_ST <- lm(human_ST$Frequency ~ human_ST$rank)

#####z##################Log Scale food ST#####################
#6. ggplot the caecal graph LOG 
human_ST_graph_log= ggplot(human_ST, 
                          aes((rank^2), Frequency, group = 1)) + 
  labs(y = "Log",
       x = "Rank") +  
  ggtitle("Relative abundance humanfeces ST (7 allele) LOG") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(linetype = "dashed")+
  geom_point(color="blue") +
  theme(plot.background = element_rect(fill = "wheat4"))+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)))+
  geom_smooth(method = "lm", se = FALSE,color='red')+
  stat_poly_eq(formula = human_STformula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)+
  geom_smooth(method = "lm", se = FALSE,color='red')

human_ST_graph_log

#7. Save the log images


################Merging data into one image #######

merge_caecal_carcass = ggarrange(caecal_CC_graph_log, caecal_ST_graph_log,carcass_CC_graph,carcass_ST_graph + 
                                   rremove("x.text"), 
                                 labels = c("A", "B", "C","D"),
                                 ncol = 2, nrow = 2)
merge_caecal_carcass


merge_food_human = ggarrange(food_CC_graph, food_ST_graph,human_CC_graph,human_ST_graph + 
                               rremove("x.text"), 
                             labels = c("A", "B", "C","D"),
                             ncol = 2, nrow = 2)
merge_food_human

#Log scale 
merge_caecal_log = ggarrange(caecal_CC_graph_log, caecal_ST_graph_log + 
                                   rremove("x.text"), 
                                 labels = c("A", "B"),
                                 ncol = 2, nrow = 1)
merge_caecal_log


merge_carcass_log = ggarrange(carcass_CC_graph_log, carcass_ST_graph_log+ 
                               rremove("x.text"), 
                             labels = c("A", "B"),
                             ncol = 2, nrow = 1)
merge_carcass_log

####ST
merge_st_log_caecal_carcass = ggarrange(caecal_ST_graph_log,carcass_ST_graph_log + 
                           rremove("x.text"), 
                         labels = c("A", "B"),
                         ncol = 2, nrow = 1)
merge_st_log_caecal_carcass

merge_st_log_food_human = ggarrange(food_ST_graph_log,human_ST_graph_log + 
                           rremove("x.text"), 
                         labels = c("C", "D"),
                         ncol = 2, nrow = 1)
merge_st_log_food_human

####### CC 
merge_cc_log_caecal_carcass = ggarrange(caecal_CC_graph_log,carcass_CC_graph_log + 
                                          rremove("x.text"), 
                                        labels = c("A", "B"),
                                        ncol = 2, nrow = 1)
merge_cc_log_caecal_carcass

merge_cc_log_food_human = ggarrange(food_CC_graph_log,human_CC_graph_log + 
                                      rremove("x.text"), 
                                    labels = c("C", "D"),
                                    ncol = 2, nrow = 1)
merge_cc_log_food_human



