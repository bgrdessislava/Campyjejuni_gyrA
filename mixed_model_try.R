library(lme4)

head(dragons)
hist(dragons$testScore)
dragons$bodyLength2 <- scale(dragons$bodyLength, center = TRUE, scale = TRUE)

basic.lm <- lm(testScore ~ bodyLength2, data = dragons)
summary(basic.lm)

library(ggplot2)  # load the package

(prelim_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore)) +
    geom_point() +
    geom_smooth(method = "lm"))

plot(basic.lm, which = 1)  # not perfect... 
## but since this is a fictional example we will go with it
## for your own data be careful:
## the bigger the sample size, the less of a trend you'd expect to see

plot(basic.lm, which = 2)  # a bit off at the extremes, but that's often the case; again doesn't look too bad

boxplot(testScore ~ mountainRange, data = dragons)  # certainly looks like something is going on here


(colour_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = mountainRange)) +
    geom_point(size = 2) +
    theme_classic() +
    theme(legend.position = "none"))

(split_plot <- ggplot(aes(bodyLength, testScore), data = dragons) + 
    geom_point() + 
    facet_wrap(~ mountainRange) + # create a facet for each mountain range
    xlab("length") + 
    ylab("test score"))

mountain.lm <- lm(testScore ~ bodyLength2 + mountainRange, data = dragons)
summary(mountain.lm)

library(lme4)

mixed.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange), data = dragons)
summary(mixed.lmer)

plot(mixed.lmer)  # looks alright, no patterns evident

qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))  # points fall nicely onto the line - good!
