library(ggplot2)
library(stats)
library(gplots)
library(d3heatmap)
library(reshape2)
library(pheatmap)
library(dplyr)
library(tidyverse)
library(viridis)
library(lme4)

#Golden code - GLM model of fluoroquinolone resistance across time and statistical results
setwd("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data")
#Getting the raw big database with all metadata
year_df<- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/CleanMeta_for_Tableau_resistantcleaned_10362.csv")

#removing all na
year_df = year_df %>%
  filter(!is.na(year))

#adding binary column
year_df$binary = ifelse(year_df$Gyrase_DessyPipeline == 'I', 1, 0)

#Taking the first column which is not needed
#year_df = year_df[,-1]
#Only picking human stool
year_df = year_df %>%
  filter(!is.na(year)) %>%
  filter(year >= 1996)

#cleaning up 
#year_df <- year_df %>% 
#  group_by(year) %>%
#  summarise(resistance_count = n())

#ggplot(aes(year, resistance_count), data = year_df) +
 # geom_point() +
 # ggtitle('Resistant isolates across time without bootstrapping') +
 # labs(y = "Resistant isolate count", x = "Year") +
 # theme(plot.title = element_text(hjust = 0.5))


#making year into from 0
year_df$year_normalised = year_df$year - min(year_df$year)

#Making prediction for future
#Creating a data frame
variable_years<-data.frame(year_normalised=seq(0,40,1))


#trend for all of them
res_total=glm(binary~year_normalised,family=binomial,data = year_df)
summary(res_total)
anova(res_total, test = 'Chi')
exp(confint(res_total))

preds = predict(res_total, newdata=variable_years, type = "link", se.fit = TRUE)
critval <- 1.96 ## approx 95% CI
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit
fit2 <- res_total$family$linkinv(fit)
upr2 <- res_total$family$linkinv(upr)
lwr2 <- res_total$family$linkinv(lwr)
variable_years$lwr <- lwr2 
variable_years$upr <- upr2

predict_res = as.data.frame(predict(res_total,newdata = variable_years, type = "response", interval = 'confidence')) %>% rename(predicted = 1)
predicted_res = cbind(predict_res,variable_years)
predicted_res$year = predicted_res$year_normalised + min(year_df$year)
trend_total = ggplot(data = year_df)  + 
  geom_point(aes(x = year, y = binary)) +
  geom_line(data = predicted_res, aes(x = year, y = predicted)) +
  geom_line(data=predicted_res, mapping=aes(x=year, y=upr), col="red") + 
  geom_line(data=predicted_res, mapping=aes(x=year, y=lwr), col="red") +
  ggtitle('D) Prediction of all isolates in the future')+
  ylab('Proportion of resistance') + xlab('') +
  theme(plot.title = element_text(hjust = 0.5),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),text = element_text(size = 15)) +
  scale_x_continuous(breaks=seq(1997, 2040, 5))

trend_total

ggsave("../Figures/prediction_allisolates.png",dpi = 300)

