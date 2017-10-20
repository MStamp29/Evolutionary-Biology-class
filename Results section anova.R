#Task for Tom Little's class
#Running anova on data and creating graphs 

library(dplyr)
library(ggplot2)
rm(list=ls())

expt1<- read.csv("/Users/meganstamp/Documents/Experiment 1.csv", sep=",")
glimpse(expt1)

#change from integer to factor
expt1[,'PH.during.Assay'] <- as.factor(expt1[,'PH.during.Assay'])
glimpse(expt1)

#calculate mean and sd of pop growth for all combinations for pop growth
sum_expt1<- expt1 %>%
  group_by(Evolution.Treatment, PH.during.Assay)%>%
  summarise(meanpopgrowth= mean(Growth.Rate), sdpopgrowth= sd(Growth.Rate))

#create a bar plot
ggplot(sum_expt1, aes(x= Evolution.Treatment, y= meanpopgrowth, fill = PH.during.Assay, colour=PH.during.Assay, group=PH.during.Assay))+
  geom_bar(stat="identity", position="dodge")+ #fill = change colour of bars, colour = what the colour change is based on, position = dodge puts bars next to each other
  geom_errorbar(aes(ymin=meanpopgrowth-sdpopgrowth, ymax=meanpopgrowth+sdpopgrowth), #add error bars, make them thinner, and position= makes them in line with the bars
                width=.3,
                size=.3,
                colour="black",
                position=position_dodge(0.9))+
  xlab("Evolution pH")+
  ylab("Mean Population Growth")+
  labs(fill='pH during assay')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) #remove gridlines and background colour
#this graph has two figure legends because I tried to alter the title and I can't figure out how to get rid of one of them 
 
#calculate mean and sd of pop growth for all combinations for oxygen production
sum_expt1_O2<- expt1 %>%
  group_by(Evolution.Treatment, PH.during.Assay)%>%
  summarise(meanO2prod= mean(O2.Production), sdO2prod= sd(O2.Production))

#create a bar plot
ggplot(sum_expt1_O2, aes(x= Evolution.Treatment, y= meanO2prod, fill = PH.during.Assay, colour=PH.during.Assay, group=PH.during.Assay))+
  geom_bar(stat="identity", position="dodge")+ #fill = change colour of bars, colour = what the colour change is based on, position = dodge puts bars next to each other
  geom_errorbar(aes(ymin=meanO2prod-sdO2prod, ymax=meanO2prod+sdO2prod), #add error bars, make them thinner, and position= makes them in line with the bars
                width=.3,
                size=.3,
                colour="black",
                position=position_dodge(0.9))+
  xlab("Evolution pH")+
  ylab("Mean O2 Production")+
  labs(fill='pH during assay')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) #remove gridlines and background colour
#this graph has two figure legends because I tried to alter the title and I can't figure out how to get rid of one of them 
#maybe just change the column title in the initial data set

#find F, df and p for model looking at the interaction between ev pH and assay pH for pop growth
model_exp1_pg<- lm(Growth.Rate ~ Evolution.Treatment * PH.during.Assay, data=expt1)

anova (model_exp1_pg)

#same but for O2 production
model_exp1_op<- lm(O2.Production ~ Evolution.Treatment * PH.during.Assay, data=expt1)

anova (model_exp1_op)