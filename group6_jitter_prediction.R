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

#Taking the first column which is not needed
#year_df = year_df[,-1]
#Only picking human stool
year_df = year_df %>%
  filter(!is.na(year))

#adding binary column
year_df$binary = ifelse(year_df$Gyrase_DessyPipeline == 'I', 1, 0)


trend_group_1 = c('ST-464 complex')
trend_group_2 = c('ST-354 complex','ST-353 complex')
trend_group_3 = c('ST-443 complex','ST-52 complex')
trend_group_4 = c('ST-21 complex','ST-574 complex',
                  'ST-658 complex','ST-257 complex')
trend_group_5 = c('ST-45 complex','ST-61 complex',
                  'ST-42 complex','ST-22 complex',
                  'ST-283 complex')
trend_group_6 = c('ST-206 complex','ST-48 complex')



year_df$trend_group = ifelse(year_df$clonal_complex %in% trend_group_1, 1, 
                             ifelse(year_df$clonal_complex %in% trend_group_2, 2, 
                                    ifelse(year_df$clonal_complex %in% trend_group_3, 3,
                                           ifelse(year_df$clonal_complex %in% trend_group_4, 4,
                                                  ifelse(year_df$clonal_complex %in% trend_group_5, 5,
                                                         ifelse(year_df$clonal_complex %in% trend_group_6, 6, 0)))
                                           
                                    )))

year_df$trend_group = relevel(factor(year_df$trend_group), ref=3)
table(year_df$trend_group)

#making year into from 0
year_df$year_normalised = year_df$year - min(year_df$year)

# The exp(intercept) means the group2's intercept is for 0th year (1997) is the odds of being resistant in 1997 - exp(-1.83578) --> 0.159
# The odds of being resistant increases by 1.21 (exp( 0.19049)) every year for group 2 
#for 1998 the odds of being resistant in 1998 is 0.159 * 1.21 =  
#1999  0.159 * 1.21 * 1.21


#Making prediction for future
#Creating a data frame
variable_years<-data.frame(year_normalised=seq(0,43,1))


#trend for group 1
res_1=glm(binary~year_normalised,family=binomial,data = year_df%>% filter(trend_group == 1))
summary(res_1)
anova(res_1, test = 'Chi')
exp(confint(res_1))

preds = predict(res_1, newdata=variable_years, type = "link", se.fit = TRUE)
critval <- 1.96 ## approx 95% CI
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit
fit2 <- res_1$family$linkinv(fit)
upr2 <- res_1$family$linkinv(upr)
lwr2 <- res_1$family$linkinv(lwr)
variable_years$lwr <- lwr2 
variable_years$upr <- upr2

predict_res = as.data.frame(predict(res_1,newdata = variable_years, type = "response", interval = 'confidence')) %>% rename(predicted = 1)
predicted_res = cbind(predict_res,variable_years)
predicted_res$year = predicted_res$year_normalised + min(year_df$year)
trend_1 = ggplot(data = year_df%>% filter(trend_group == 1))  + 
  geom_point(aes(x = year, y = binary,color = factor(binary)),position = "jitter",alpha = 0.3) +
  geom_line(data = predicted_res, aes(x = year, y = predicted)) +
  geom_line(data=predicted_res, mapping=aes(x=year, y=upr), col="red",linetype = "dashed") + 
  geom_line(data=predicted_res, mapping=aes(x=year, y=lwr), col="red",linetype = "dashed") +
  scale_color_manual(values = c("#4E84C4", "#D16103")) +
  ggtitle('Group1 CC464')+
  ylab('') +
  xlab('') +
  scale_x_continuous(breaks=seq(1997, 2040, 5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 18),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),text = element_text(size = 15),
        legend.key.size = unit(2, 'mm'),legend.position = "none")
  

trend_1 
ggsave("../Figures/trend_1_jitter.png",dpi = 300)

#I can also try to see the trend for each of the groups - group 2
res_2=glm(binary~year_normalised,family=binomial,data = year_df%>% filter(trend_group == 2))
summary(res_2)
anova(res_2, test = 'Chi')
exp(confint(res_2))
#The odds of being resistance increases by odds 21% each year, p-value <2e-16 and 
#confidence interval between 0.663(2.5%) and 1.165(97.5%)

preds = predict(res_2, newdata=variable_years, type = "link", se.fit = TRUE)
critval <- 1.96 ## approx 95% CI
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit
fit2 <- res_2$family$linkinv(fit)
upr2 <- res_2$family$linkinv(upr)
lwr2 <- res_2$family$linkinv(lwr)
variable_years$lwr <- lwr2 
variable_years$upr <- upr2

predict_res = as.data.frame(predict(res_2,newdata = variable_years, type = "response", interval = 'confidence')) %>% rename(predicted = 1)
predicted_res = cbind(predict_res,variable_years)
predicted_res$year = predicted_res$year_normalised + min(year_df$year)
trend_2 = ggplot(data = year_df%>% filter(trend_group == 2))  + 
  geom_point(aes(x = year, y = binary,color = factor(binary)),position = "jitter",alpha = 0.3) +
  geom_line(data = predicted_res, aes(x = year, y = predicted)) +
  geom_line(data=predicted_res, mapping=aes(x=year, y=upr), col="red",linetype = "dashed") + 
  geom_line(data=predicted_res, mapping=aes(x=year, y=lwr), col="red",linetype = "dashed") +
  ggtitle('Group2 CC354,353')+
  scale_color_manual(values = c("#4E84C4", "#D16103")) +
  ylab('') +
  xlab('') +
  scale_x_continuous(breaks=seq(1997, 2040, 5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 18),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),text = element_text(size = 15),
        legend.key.size = unit(2, 'mm'),legend.position = "none")

trend_2
ggsave("../Figures/trend_2_jitter.png",dpi = 300)

exp(confint(res_2))
exp(confint(res_1))
#trend for group 4
res_4=glm(binary~year_normalised,family=binomial,data = year_df%>% filter(trend_group == 4))
summary(res_4)
anova(res_4, test = 'Chi')
exp(confint(res_4))

#predicts the future values
preds = predict(res_4, newdata=variable_years, type = "link", se.fit = TRUE)
critval <- 1.96 ## approx 95% CI
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit
fit2 <- res_4$family$linkinv(fit)
upr2 <- res_4$family$linkinv(upr)
lwr2 <- res_4$family$linkinv(lwr)
variable_years$lwr <- lwr2 
variable_years$upr <- upr2

predict_res = as.data.frame(predict(res_4,newdata = variable_years, type = "response", interval = 'confidence')) %>% rename(predicted = 1)
predicted_res = cbind(predict_res,variable_years)
predicted_res$year = predicted_res$year_normalised + min(year_df$year)
trend_4 = ggplot(data = year_df%>% filter(trend_group == 4))  + 
  geom_point(aes(x = year, y = binary,color = factor(binary)),position = "jitter",alpha = 0.3) +
  geom_line(data = predicted_res, aes(x = year, y = predicted)) +
  geom_line(data=predicted_res, mapping=aes(x=year, y=upr), col="red",linetype = "dashed") + 
  geom_line(data=predicted_res, mapping=aes(x=year, y=lwr), col="red",linetype = "dashed") +
  scale_color_manual(values = c("#4E84C4", "#D16103")) +
  ggtitle('Group4 CC21,574,658,257')+
  ylab('') +
  xlab('') +
  scale_x_continuous(breaks=seq(1997, 2040, 5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 18),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),text = element_text(size = 15),
        legend.key.size = unit(2, 'mm'),legend.position = "none")

trend_4 
ggsave("../Figures/trend_4_jitter.png",dpi = 300)


#trend for group 3
res_3=glm(binary~year_normalised,family=binomial,data = year_df%>% filter(trend_group == 3))
summary(res_3)
anova(res_3, test = 'Chi')
exp(confint(res_3))

#predicts the future values
preds = predict(res_3, newdata=variable_years, type = "link", se.fit = TRUE)
critval <- 1.96 ## approx 95% CI
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit
fit2 <- res_3$family$linkinv(fit)
upr2 <- res_3$family$linkinv(upr)
lwr2 <- res_3$family$linkinv(lwr)
variable_years$lwr <- lwr2 
variable_years$upr <- upr2

predict_res = as.data.frame(predict(res_3,newdata = variable_years, type = "response", interval = 'confidence')) %>% rename(predicted = 1)
predicted_res = cbind(predict_res,variable_years)
predicted_res$year = predicted_res$year_normalised + min(year_df$year)
trend_3 = ggplot(data = year_df%>% filter(trend_group == 3))  + 
  geom_point(aes(x = year, y = binary,color = factor(binary)),position = "jitter",alpha = 0.3) +
  geom_line(data = predicted_res, aes(x = year, y = predicted)) +
  geom_line(data=predicted_res, mapping=aes(x=year, y=upr), col="red",linetype = "dashed") + 
  geom_line(data=predicted_res, mapping=aes(x=year, y=lwr), col="red",linetype = "dashed") +
  scale_color_manual(values = c("#4E84C4", "#D16103")) +
  ggtitle('Group3 CC443,52')+
  ylab('0-Suscpetible 1-Resistant') +
  xlab('') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=seq(1997, 2040, 5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 18),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),text = element_text(size = 15),
        legend.key.size = unit(2, 'mm'),legend.position = "none") 
  

trend_3
ggsave("../Figures/trend_3_jitter.png",dpi = 300)

#trend for group 5
res_5=glm(binary~year_normalised,family=binomial,data = year_df%>% filter(trend_group == 5))
summary(res_5)
anova(res_5, test = 'Chi')
exp(confint(res_5))

preds = predict(res_5, newdata=variable_years, type = "link", se.fit = TRUE)
critval <- 1.96 ## approx 95% CI
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit
fit2 <- res_5$family$linkinv(fit)
upr2 <- res_5$family$linkinv(upr)
lwr2 <- res_5$family$linkinv(lwr)
variable_years$lwr <- lwr2 
variable_years$upr <- upr2

predict_res = as.data.frame(predict(res_5,newdata = variable_years, type = "response", interval = 'confidence')) %>% rename(predicted = 1)
predicted_res = cbind(predict_res,variable_years)
predicted_res$year = predicted_res$year_normalised + min(year_df$year)
trend_5 = ggplot(data = year_df%>% filter(trend_group == 5))  + 
  geom_point(aes(x = year, y = binary,color = factor(binary)),position = "jitter",alpha = 0.3) +
  geom_line(data = predicted_res, aes(x = year, y = predicted)) +
  geom_line(data=predicted_res, mapping=aes(x=year, y=upr), col="red",linetype = "dashed") + 
  geom_line(data=predicted_res, mapping=aes(x=year, y=lwr), col="red",linetype = "dashed") +
  scale_color_manual(values = c("#4E84C4", "#D16103")) +
  ggtitle('Group5 CC45,61,42,22,283')+
  ylab('') +
  xlab('') +
  scale_x_continuous(breaks=seq(1997, 2040, 5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 18),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),text = element_text(size = 15),
        legend.key.size = unit(2, 'mm'),legend.position = "none")

trend_5
ggsave("../Figures/trend_5_jitter.png",dpi = 300)

#trend for group 6
res_6=glm(binary~year_normalised,family=binomial,data = year_df%>% filter(trend_group == 6))
summary(res_6)
anova(res_6,test = 'Chi')
exp(confint(res_6))


preds = predict(res_6, newdata=variable_years, type = "link", se.fit = TRUE)
critval <- 1.96 ## approx 95% CI
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit
fit2 <- res_6$family$linkinv(fit)
upr2 <- res_6$family$linkinv(upr)
lwr2 <- res_6$family$linkinv(lwr)
variable_years$lwr <- lwr2 
variable_years$upr <- upr2

predict_res = as.data.frame(predict(res_6,newdata = variable_years, type = "response", interval = 'confidence')) %>% rename(predicted = 1)
predicted_res = cbind(predict_res,variable_years)
predicted_res$year = predicted_res$year_normalised + min(year_df$year)
trend_6 = ggplot(data = year_df%>% filter(trend_group == 6))  + 
  geom_point(aes(x = year, y = binary,color = factor(binary)), position = "jitter",alpha = 0.3) +
  geom_line(data = predicted_res, aes(x = year, y = predicted)) +
  geom_line(data=predicted_res, mapping=aes(x=year, y=upr), col="red",linetype = "dashed") + 
  geom_line(data=predicted_res, mapping=aes(x=year, y=lwr), col="red",linetype = "dashed") +
  scale_color_manual(values = c("#4E84C4", "#D16103")) +
  ggtitle('Group6 CC206,48')+
  ylab('') +
  xlab('') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=seq(1997, 2040, 5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 18),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),text = element_text(size = 15),
        legend.key.size = unit(2, 'mm'),legend.position = "none")


trend_6
ggsave("../Figures/trend_6_jitter.png",dpi = 300)
#writing up
# As year increases by  1, the odds of being resistant increases by exp(0.0727)
#Time was significantly associated with being resistant, (estimate = exp(0.0727),95% confidence interval = 
# CHECK THE LINK)
#For the groups I can try to make a plot - 
#Table 
# Reference group - Group 1
# Write the estimate and calculate the standard error 
#All throups are different from the reference (group 1)
#However this will be different so I could try Tukey Post hoc Test