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

 # filter(source == 'human_stool')%>% 
 # filter(base == 'I' | base == 'T') %>% 
 # filter(year >= 1997)


#adding binary column
year_df$binary = ifelse(year_df$Gyrase_DessyPipeline == 'I', 1, 0)

#Adding the three groups into these three groups
#Adding the three groups all the other will be group 4
group1 = c('ST-354 complex','ST-464 complex','ST-353 complex')
group2 = c('ST-48 complex','ST-45 complex',
           'ST-61 complex','ST-42 complex','ST-22 complex',
           'ST-283 complex')
group3 = c('ST-443 complex','ST-21 complex','ST-574 complex',
           'ST-206 complex','ST-658 complex','ST-257 complex',
           'ST-52 complex')

year_df$group = ifelse(year_df$clonal_complex %in% group1, 1, 
                            ifelse(year_df$clonal_complex %in% group2, 2, 
                                   ifelse(year_df$clonal_complex %in% group3, 3, 0)))
table(year_df$group, year_df$binary)
#basic lm with group times 
res=glm(binary~year,family=binomial,data = year_df)
summary(res)

res=glm(binary~factor(group),family=binomial,data = year_df)
summary(res)

res=glm(binary~year * factor(group),family=binomial,data = year_df) #Too big and it is not the right model
#it does show that year do have significant relationship with resistance change - The model is not able to estimate the ruslts
summary(res)
#boxplot(factor(year_df$group)) <- try to check this

year_df$group = relevel(factor(year_df$group), ref=3)
table(year_df$group)

#basic lm with group plused  
res=glm(binary~year + factor(group),family=binomial,data = year_df)
#it does show that year do have significant relationship with resistance change
summary(res)

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
#

#basic lm with group times 
res=glm(binary~year_normalised * trend_group,family=binomial,data = year_df)
#it does show that year do have significant relationship with resistance change
summary(res)
anova(res, test = 'Chi')


#basic lm with group plus 
res=glm(binary~year + factor(trend_group),family=binomial,data = year_df)
#it does show that year do have significant relationship with resistance change
summary(res)

res=glm(binary~year + factor(trend_group),family=binomial,data = year_df)
summary(res)

#Making prediction for future
#Creating a data frame
variable_years<-data.frame(year_normalised=seq(0,40,1))


#trend for group 1
res_1=glm(binary~year_normalised,family=binomial,data = year_df%>% filter(trend_group == 1))
summary(res_1)
anova(res_1, test = 'Chi')

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
  geom_point(aes(x = year, y = binary)) +
  geom_line(data = predicted_res, aes(x = year, y = predicted)) +
  geom_line(data=predicted_res, mapping=aes(x=year, y=upr), col="red") + 
  geom_line(data=predicted_res, mapping=aes(x=year, y=lwr), col="red") +
  ggtitle('Prediction of group1, ST-464 complex')+
  ylab('Resistance') +
  xlab('Year') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=seq(1997, 2040, 3))

trend_1 

#I can also try to see the trend for each of the groups - group 2
res_2=glm(binary~year_normalised,family=binomial,data = year_df%>% filter(trend_group == 2))
summary(res_2)
anova(res_2, test = 'Chi')

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
  geom_point(aes(x = year, y = binary)) +
  geom_line(data = predicted_res, aes(x = year, y = predicted)) +
  geom_line(data=predicted_res, mapping=aes(x=year, y=upr), col="red") + 
  geom_line(data=predicted_res, mapping=aes(x=year, y=lwr), col="red") +
  ggtitle('Prediction of group2, ST-354 complex, ST-353 complex')+
  ylab('Resistance') +
  xlab('Year') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=seq(1997, 2040, 3))

trend_2


#trend for group 4
res_4=glm(binary~year_normalised,family=binomial,data = year_df%>% filter(trend_group == 4))
summary(res_4)
anova(res_4, test = 'Chi')
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
  geom_point(aes(x = year, y = binary)) +
  geom_line(data = predicted_res, aes(x = year, y = predicted)) +
  geom_line(data=predicted_res, mapping=aes(x=year, y=upr), col="red") + 
  geom_line(data=predicted_res, mapping=aes(x=year, y=lwr), col="red") +
  ggtitle('Prediction of group 4, ST-21 complex, ST-574 complex,ST-658 complex, ST-257 complex')+
  ylab('Resistance') +
  xlab('Year') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=seq(1997, 2040, 3))

trend_4 


#trend for group 3
res_3=glm(binary~year_normalised,family=binomial,data = year_df%>% filter(trend_group == 3))
summary(res_3)
anova(res_3, test = 'Chi')

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
  geom_point(aes(x = year, y = binary)) +
  geom_line(data = predicted_res, aes(x = year, y = predicted)) +
  geom_line(data=predicted_res, mapping=aes(x=year, y=upr), col="red") + 
  geom_line(data=predicted_res, mapping=aes(x=year, y=lwr), col="red") +
  ggtitle('Prediction of group 3, ST-443 complex,ST-52 complex')+
  ylab('Resistance') +
  xlab('Year') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=seq(1997, 2040, 3))

trend_3

#trend for group 5
res_5=glm(binary~year_normalised,family=binomial,data = year_df%>% filter(trend_group == 5))
summary(res_5)
anova(res_5, test = 'Chi')

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
  geom_point(aes(x = year, y = binary)) +
  geom_line(data = predicted_res, aes(x = year, y = predicted)) +
  geom_line(data=predicted_res, mapping=aes(x=year, y=upr), col="red") + 
  geom_line(data=predicted_res, mapping=aes(x=year, y=lwr), col="red") +
  ggtitle('Prediction of group 5, ST-45 complex , ST-61 complex, ST-42 complex, ST-22 complex,ST-283 complex')+
  ylab('Resistance') +
  xlab('Year') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=seq(1997, 2040, 3))

trend_5

#trend for group 6
res_6=glm(binary~year_normalised,family=binomial,data = year_df%>% filter(trend_group == 6))
summary(res_6)
anova(res_6,test = 'Chi')


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
  geom_point(aes(x = year, y = binary)) +
  geom_line(data = predicted_res, aes(x = year, y = predicted)) +
  geom_line(data=predicted_res, mapping=aes(x=year, y=upr), col="red") + 
  geom_line(data=predicted_res, mapping=aes(x=year, y=lwr), col="red") +
  ggtitle('Prediction of group 6, ST-206 complex, ST-48 complex')+
  ylab('Resistance') +
  xlab('Year') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=seq(1997, 2040, 3))

trend_6
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

#ordered the year and add a line across the year and binary 
o=order(year_df$year)
plot(year_df$year,year_df$binary,ylab="Resistance",xlab="Year",bty="l")
lines(year_df$year[o],res$fitted[o],lwd=2,col="red")

#residual plot do show linear relation
plot(res, which = 1) 
#qq plot showing it is not forming normal distribution which is true
plot(res, which = 2)

#Tried to do boxplot but too many clonal-complex maybe should try to work on the main 18 clonal complex
boxplot(year_df$binary ~ year_df$clonal_complex, data = year_df) 

# Smoothing function with different behaviour in the different plot panels
mysmooth <- function(formula,data,...){
  meth <- eval(parse(text=meths[unique(data$PANEL)]))
  x <- match.call()
  x[[1]] <- meth
  eval.parent(x)
}

unique_cc_heatmaps = c('ST-206 complex','ST-21 complex', 'ST-22 complex','','ST-257 complex','ST-283 complex',
                       'ST-353 complex', 'ST-354 complex', 'ST-42 complex','ST-443 complex','ST-45 complex',
                       'ST-464 complex', 'ST-48 complex','ST-52 complex', 'ST-574 complex','ST-61 complex','ST-658 complex')
year_df <- year_df %>% group_by(clonal_complex..MLST.) %>% filter(n() >= 10) 
#a = filter(a, total >= 10)

year_df = filter(year_df, clonal_complex..MLST. %in% unique_cc_heatmaps)
#CC overtime
split_plot <- ggplot(aes(year_df$year, year_df$binary), data = year_df) + 
    geom_point(alpha = 0.4) + 
    geom_smooth(method = glm, method.args = c(family=binomial)) +
    facet_wrap(~ year_df$clonal_complex..MLST.) + # create a facet for each mountain range
    xlab("Year") + 
    geom_vline(aes(xintercept = 2006),colour="red", linetype = "longdash") +
    ylab("Resistance") +
    xlim(1997,2018) +
    ylim(0,1) +
    ggtitle("Fluoroquinolone Resistance over Clonal-complex") +
    theme(plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5))

split_plot

ggsave("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Figures/Scatterplot/fluroquinoloneRes_CC_year_17graphs.png",plot=split_plot, width = 50, height = 30, units = "cm",dpi = 600)

#ST overtime
split_plot_ST <- ggplot(aes(year_df$year, year_df$binary), data = year_df) + 
  geom_point() + 
  geom_smooth(method = glm, method.args = c(family=binomial)) +
  facet_wrap(~ year_df$ST..MLST.) + # create a facet for each mountain range +
  geom_vline(aes(xintercept = 2006),colour="red", linetype = "longdash") +
  xlab("length") + 
  ylab("test score")

split_plot_ST 
#split_plot_ST dont run too many


#clonal_complex each of them shown as whether it is significant or not CC
clonal.lm <- lm(year_df$binary ~ year_df$year + year_df$clonal_complex..MLST., data = year_df)
summary(clonal.lm)

#ST each of them shown as whether it is significant or not 
clonal.lm <- lm(year_df$binary ~ year_df$year + year_df$ST..MLST., data = year_df)
summary(clonal.lm)


#Mix model inluding the clonal complex
mixed.lmer <- lmer(year_df$binary ~ year_df$year + (1|year_df$clonal_complex..MLST.), data = year_df)

#Mix model inluding the ST
mixed.lmer_ST <- lmer(year_df$binary ~ year_df$year + (1|year_df$ST..MLST.), data = year_df)


summary(mixed.lmer)
summary(mixed.lmer_ST) #55% of the variance comees from ST  type
#Differences between clonal complex ranges explain ~30% of
#the variance that’s “left over” after the variance explained by our fixed effects.



plot(mixed.lmer)  # not sure what residue plot is doing here..
qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))  # The qq plot looks better than the normal lm?



#Experimenting
(split_plot <- ggplot(aes(year_df$year, year_df$binary), data = year_df) + 
    geom_point() + 
    geom_smooth(method = lmer, method.args = c(family=binomial)) +
    facet_wrap(~ year_df$clonal_complex) + # create a facet for each mountain range
    xlab("length") + 
    ylab("test score"))


