library(ggplot2)

#Code that shows the amount of different base pairs for each years
setwd("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data")
long_list <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/long_list.csv")
long_list <- long_list[long_list$position == '86',]
year_86 <- long_list[,c("year","base")]
s <- summary(year_86)
capture.output(s, file = "year_86.txt")
attach(year_86)

write.csv(long_list,"long_list_86_position.csv")

year_base <- aggregate(year_86,list(Years=year,Base=base),length)
table(year_86)
summary(year_86)


barplot(year_base$base,year_base$Years)
ggplot(data=year_base, aes(x=Years, y=base, fill=Base)) +
  geom_bar(stat="identity", position=position_dodge())

head(year_86)
year_86_antibioticera <- year_base[year_base$Years >= 1997 & year_base$Years < 2007,]
year_86_postantibioticera <- year_base[year_base$Years>2006,]

ggplot(data=year_86_antibioticera, aes(x=Years, y=base, fill=Base)) +
  geom_bar(stat="identity", position=position_dodge())

ggplot(data=year_86_postantibioticera, aes(x=Years, y=base, fill=Base)) +
  geom_bar(stat="identity", position=position_dodge())

