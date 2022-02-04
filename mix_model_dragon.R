library(ggplot2)  # load the package
library(lme4)
setwd("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Code")

hist(dragons$testScore)  # seems close to a normal distribution - good!
#It is good practice to scale it so we can standarlise the variables
dragons$bodyLength2 <- scale(dragons$bodyLength, center = TRUE, scale = TRUE)

#Plot the graph without thinking about the site and the mountain
basic.lm <- lm(testScore ~ bodyLength2, data = dragons)
summary(basic.lm)


(prelim_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore)) +
    geom_point() +
    geom_smooth(method = "lm"))

plot(basic.lm, which = 1)  # not perfect... 
## but since this is a fictional example we will go with it
## for your own data be careful:
## the bigger the sample size, the less of a trend you'd expect to see

plot(basic.lm, which = 2)  # a bit off at the extremes, but that's often the case; again doesn't look too bad

#box plot should allow me to see whether the data are different according to mountain range
boxplot(testScore ~ mountainRange, data = dragons)  
# certainly looks like something is going on here
#Bodylength and test score
(colour_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = mountainRange)) +
    geom_point(size = 2) +
    theme_classic() +
    theme(legend.position = "none"))

#Splitting all the different mountains and test score
(split_plot <- ggplot(aes(bodyLength, testScore), data = dragons) + 
    geom_point() + 
    facet_wrap(~ mountainRange) + # create a facet for each mountain range
    xlab("length") + 
    ylab("test score"))

#Linear model using the body length and mountain range shows that the body size does not matter
mountain.lm <- lm(testScore ~ bodyLength2 + mountainRange, data = dragons)
summary(mountain.lm)

#1stMix model trying to understand whether the body and mountain rainge both has an effect
mixed.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange), data = dragons)
summary(mixed.lmer)

plot(mixed.lmer)  # looks alright, no patterns evident
qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))  # points fall nicely onto the line - good!



head(dragons)  # we have site and mountainRange
str(dragons)  # we took samples from three sites per mountain range and eight mountain ranges in total

dragons <- within(dragons, sample <- factor(mountainRange:site))
#2nd mix model
mixed.lmer2 <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|sample), data = dragons)  # the syntax stays the same, but now the nesting is taken into account
summary(mixed.lmer2)

(mm_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
    facet_wrap(~mountainRange, nrow=2) +   # a panel for each mountain range
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(dragons, pred = predict(mixed.lmer2)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)
