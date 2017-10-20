library(dplyr)
library(ggplot2)
rm(list=ls())

expt1<- read.csv("/Users/meganstamp/Documents/Experiment 1.csv", sep=",")
glimpse(expt1)

#change from integer to factor
expt1[,'PH.during.Assay'] <- as.factor(expt1[,'PH.during.Assay'])
glimpse(expt1)

sum_expt1<- expt1 %>%
  group_by(Evolution.Treatment, PH.during.Assay)%>%
  summarise(meanpopgrowth= mean(Growth.Rate), sdpopgrowth= sd(Growth.Rate))
