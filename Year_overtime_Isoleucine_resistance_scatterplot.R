library(ggplot2)
library(stats)
library(gplots)
library(d3heatmap)
library(reshape2)
library(pheatmap)
library(dplyr)
library(tidyverse)
setwd("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data")

year_df <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/year_resistance_df.csv")
year_df = year_df[,-1]


names(year_df)[names(year_df) == "Var1"] <- "Year"

year_df['sum'] = year_df['I'] + year_df['T']


year_df <- year_df %>% 
  mutate(I_normalised = I/sum)

year_df <- year_df %>% 
  mutate(T_normalised = T/sum)

year_df <- year_df %>% 
  mutate(normalised_Total = I_normalised + T_normalised )


#loess method
ggplot(year_df, aes(x=Year, y=I_normalised)) + 
  geom_point()+
  geom_smooth() +
  labs(title="Isoleucine point mutation changes over time using loess method",
       x="Year", y = "Isoleucine porportion")

# Add the regression line
ggplot(year_df, aes(x=Year,y=I_normalised)) + 
  geom_point()+
  geom_smooth(method=lm) +
  labs(title="Isoleucine point mutation changes over time using regression line",
     x="Year", y = "Isoleucine porportion") 

