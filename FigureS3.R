# last edited 16 Feb 2024
# last run 16 Feb 2024
# Objective: get figures for survey-weighted and stratified results

rm(list=ls())

########################################################### Load Libraries
library(ggplot2)
library(survey) #allows for design based analysis
library(RColorBrewer)
library(forcats)
library(tidyverse)
library(scales)
library(stringr)
library(cowplot)
library(gridGraphics)
library(gridExtra)
library(reshape2)
'%!in%' <- function(x,y)!('%in%'(x,y))

# date = substr(date(),5,10)
date = "Jun 26"
# svy.date <- "Jul 21" 
# svy.strat.date <- "Jul 21" 
location <- "/Users/EWilson/Desktop/DAC/Delivery"
setwd(location)

########################################################### GET DATA FILES - survey-weighted estimates
svydata <- read_csv(paste0(location,"/Results/",date,"_data.svy.csv"))

# svydata <- svydata[!(svydata$country=="SaoTomeandPrincipe"),]

head(svydata)
sort(unique(svydata$country))
n <- table(svydata$country, svydata$indicator)
dim(n)





# LINEAR ASSOCIATION
# plot delivery and PNC score by MMR and compare to facility delivery and score
# read in obstetric transition values
MMR <- read.csv(paste0(location,"/ReadyforAnalysis_2022-10-26.csv"))
MMR <- MMR[,c("country","mmr")]
head(MMR)

# score
svydata <- read.csv(paste0(location,"/Results/",date,"_data.svy.csv"), stringsAsFactors = FALSE)
data <- subset(svydata,indicator=="score")
dim(data)
data <- merge(data,MMR,by=c("country"))
dim(data)

data <- subset(data, country!="Palestine")

table(data$mmr, exclude = NULL)
data$mmr <- as.numeric(data$mmr)

model_plot <- lm(mmr ~ value, data=data)
fit <- predict(model_plot, interval = "confidence")
data_plot <- cbind(data,fit)
summary(model_plot)
coef(lm(data$mmr~data$value))
coef(model_plot)

# test <- paste0("y=",round(coef(lm(data$mmr~data$value))[1],3),"x",round(coef(lm(data$mmr~data$value))[2],3))
test <- paste0(round(coef(lm(data$mmr~data$value))[1],3),"x",round(coef(lm(data$mmr~data$value))[2],3))
test

RSqAdj <-round(summary(model_plot)$adj.r.squared,3)
RSqAdj

head(data)
data$value <- data$value*20

graph_association <- ggplot(data,aes(x=value,y=mmr)) +
  geom_point(size=1, alpha=.5) +
  geom_smooth(method='lm', se=FALSE, formula= y~x, color="black") +
  ylab("maternal mortality ratio") +
  xlab("delivery care coverage %") +
  theme_bw() + 
  theme(axis.title = element_text(size=15),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12)) +
  # annotate("text",x=.6,y=875,label=(paste0("y==",round(coef(lm(data$mmr~data$value))[1],3),"test")),parse=TRUE)+
  # annotate("text",x=.6,y=875,label=(test))+
  annotate("text",x=55.00,y=875,label=(paste0("y=",test)),size = 5)+
  annotate("text",x=55.00,y=800,label=(paste0("Rsquared==",RSqAdj)),parse=TRUE,size = 5)
  
graph_association

ggsave(plot=graph_association, height = 5 , width = 5 , "/Users/EWilson/Desktop/DAC/Delivery/Results/FigureS3.png")




