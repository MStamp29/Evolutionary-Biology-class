#chi sq test for Tom Little's class

library(dplyr)
library(ggplot2)
rm(list=ls())

exp2<- read.csv("/Users/meganstamp/Documents/4th year courses/Research project class/EB_research_class/Experiment 2 with number.csv", sep=",")
#change PH to a factor from an integer
exp2[,'pH'] <- as.factor(exp2[,'pH'])
glimpse(exp2) #check it worked 

#calculate total no in each group (pH 7 - extinct or not, pH 3 - extinct or not)
totals <- exp2 %>%
  group_by(pH, Extinct.) %>%
  summarise(total.number=sum(Number))

ggplot(totals, aes(x=pH, y=total.number, fill=Extinct.))+
  geom_bar(stat='identity', position='dodge')+
  scale_fill_manual(values = c(No="cadetblue3", Yes="seagreen4"))+
  ylab("Total number")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#create a matrix in order to do chi sq test 
matrix <- xtabs(Number ~ pH + Extinct., data=exp2)

chisq.test(matrix)


  