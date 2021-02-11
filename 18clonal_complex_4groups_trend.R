library(ggplot2)
library(stats)
library(gplots)
library(d3heatmap)
library(reshape2)
library(pheatmap)
library(dplyr)
library(tidyverse)
library(janitor)
library(RColorBrewer)
library(wesanderson)

setwd("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data")
#Getting the raw big database with all metadata, Skip allows you to skip the raws not needed
year_df<- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/humanstool1990_2018_clonalcomplex_proportion_year_percentage_year100.csv")
#Making the header equal to 4th raw
names(year_df) <- year_df[4,]

#Removing raw using anti_join
year_df <- anti_join(year_df,year_df[1:4,])

names(year_df)[names(year_df) == "1"] <- "clonal_complex"
names(year_df)[names(year_df) == "10"] <- "Unknown_year"
names(year_df)[names(year_df) == "26"] <- "Total percentage"

year_df_melt <- melt(year_df,id.vars = c('clonal_complex'),
                     variable.name = 'year',
                     value.name = 'isolate_percentage')

year_df_melt <- year_df_melt[year_df_melt$clonal_complex != 'Total', ]   
year_df_melt <- year_df_melt[year_df_melt$year != 'Total percentage', ]   
year_df_melt <- year_df_melt[year_df_melt$year != '1900', ]   
year_df_melt <- year_df_melt[year_df_melt$year != '1991', ]   
year_df_melt <- year_df_melt[year_df_melt$year != '1977', ]   
year_df_melt <- year_df_melt[year_df_melt$year != '1981', ]   
year_df_melt <- year_df_melt[year_df_melt$year != 'Unknown_year', ]   
year_df_melt <- year_df_melt[year_df_melt$year != '1999', ]   
year_df_melt <- year_df_melt[year_df_melt$year != '2008', ]   



year_df_melt[is.na(year_df_melt)] = 0


#Adding the three groups all the other will be group 4
group1 = c('ST-354 complex','ST-464 complex')
group2 = c('ST-48 complex','ST-45 complex',
           'ST-61 complex','ST-42 complex','ST-22 complex',
           'ST-283 complex')
group3 = c('ST-353 complex')
group4 = c('ST-21 complex','ST-257 complex','ST-206 complex',
              'ST-443 complex','ST-658 complex','ST-574 complex',
              'ST-52 complex','ST-403 complex')

year_df_melt$group = ifelse(year_df_melt$clonal_complex %in% group1, 1, 
                            ifelse(year_df_melt$clonal_complex %in% group2, 2, 
                                   ifelse(year_df_melt$clonal_complex %in% group3, 3,
                                          ifelse(year_df_melt$clonal_complex %in% group4, 4, 0))))


#First plot illustrates the stack bar chart
ggplot(year_df_melt, 
       aes(fill=as.factor(group), 
           y=as.numeric(isolate_percentage), 
           x=as.factor(year))) + 
  geom_bar(stat = 'identity') +
  scale_fill_brewer(type = "qual",palette = "Dark2",
                    name="Clustering Groups",
                    labels=c("Others", "Resistant","Suscpetible","CC353","Mix")) +
                    ggtitle("3 Clonal-complex clusters formed by resistance level over time") +
                    labs (x = "year",  y = "percentage per year")

#with all clonal complex
ggplot(year_df_melt, 
       aes(fill=as.factor(clonal_complex), 
           y=as.numeric(isolate_percentage), 
           x=as.factor(year))) + 
  geom_bar(stat = 'identity') 
#geom_text(aes(label = isolate_percentage),
#size = 3, hjust = 0.5, vjust = 3,position = "stack")

#shows the line graph 
ggplot(year_df_melt, 
       aes(y=as.numeric(isolate_percentage), 
           x=as.factor(year),
           color = as.factor(group))) +
  geom_point() +
  geom_line(aes(group = clonal_complex)) +
  scale_color_brewer(type = "qual",palette = "Dark2", 
                     name="Clustering Groups",
                    labels=c("Others", "Resistant","Suscpetible","CC353","Mix")) +
  ggtitle("3 Clonal-complex clusters formed by resistance level over time") +
  labs (x = "year",  y = "percentage per year")



#shows the line graph  removing 0 group - colored by group

year_df_melt_G123 <- year_df_melt[year_df_melt$group != 0, ]    
ggplot(year_df_melt_G123, 
       aes(y=as.numeric(isolate_percentage), 
           x=as.factor(year),
           color = as.factor(group))) +
  geom_point() +
  geom_line(aes(group = clonal_complex)) +
  scale_color_brewer(type = "qual",palette = "Dark2",name="Clustering Groups",
                     labels=c("Resistant", "Suceptible","CC353","Mix"))

#just resistant 
year_df_melt_G123 <- year_df_melt[year_df_melt$group == 1, ]    
ggplot(year_df_melt_G123, 
       aes(y=as.numeric(isolate_percentage), 
           x=as.factor(year),
           color = as.factor(group))) +
  geom_smooth(aes(group = clonal_complex), method = lm) +
  geom_point(color = "firebrick") +
  geom_line(aes(group = clonal_complex)) + 
  ggtitle("Resistant clusters formed by resistance level over time")



#just suscpetible

year_df_melt_G123 <- year_df_melt[year_df_melt$group == 2, ]    
ggplot(year_df_melt_G123, 
       aes(y=as.numeric(isolate_percentage), 
           x=as.factor(year),
           color = as.factor(group))) +
  geom_smooth(aes(group = clonal_complex), method = lm) +
  geom_point(color = "blue") +
  geom_line(aes(group = clonal_complex),color = "blue") + 
  ggtitle("Suscpetible clusters formed by resistance level over time")

#mix 

year_df_melt_G123 <- year_df_melt[year_df_melt$group == 4, ]    
ggplot(year_df_melt_G123, 
       aes(y=as.numeric(isolate_percentage), 
           x=as.factor(year),
           color = as.factor(group))) +
  geom_smooth(aes(group = clonal_complex), method = lm) +
  geom_point(color = "seagreen") +
  geom_line(aes(group = clonal_complex),color = "seagreen") + 
  ggtitle("Mix clusters formed by resistance level over time") +
  geom_smooth()

#cC353

year_df_melt_G123 <- year_df_melt[year_df_melt$group == 3, ]    
ggplot(year_df_melt_G123, 
       aes(y=as.numeric(isolate_percentage), 
           x=as.factor(year),
           color = as.factor(group))) +
  geom_smooth(aes(group = clonal_complex), method = lm) +
  geom_point(color = "deeppink") +
  geom_line(aes(group = clonal_complex),color = "deeppink") + 
  ggtitle("Clonal complex 353 formed by resistance level over time") 
  

#shows the line graph  removing 0 group - colored by clonal-complex
year_df_melt_G123 <- year_df_melt[year_df_melt$group != 0, ]  

ggplot(data = year_df_melt, 
       aes(x=as.factor(year),y=as.numeric(isolate_percentage)) + 
             geom_point() +
             geom_line(aes(group = clonal_complex)) +
             scale_color_brewer(type = "qual",palette = "Set1")) 
       
       #shows the line graph  only 0 group --> non significant
       year_df_melt_G0 <- year_df_melt[year_df_melt$group == 0,]    
       ggplot(year_df_melt_G0, 
              aes(y=as.numeric(isolate_percentage), 
                  x=as.factor(year),
                  color = as.factor(group))) +
         geom_point() +
         geom_line(aes(group = clonal_complex)) +
         scale_color_brewer(type = "qual",palette = "Dark2")
       
       
       