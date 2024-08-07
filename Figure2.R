# last edited 3 Jul 2024
# last run 3 Jul 2024
# Objective: get Figure2

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
date = "May 13"
# svy.date <- "Jul 21" 
# svy.strat.date <- "Jul 21" 
location <- "/Users/EWilson/Desktop/DAC/Delivery"
setwd(location)

########################################################### GET DATA FILES - survey-weighted estimates
svydata <- read_csv(paste0(location,"/Results/",date,"_data.svy.csv"))


########################################################### FIGURE 2 - Average score by country
### Horizontal version
svydata <- svydata[!(svydata$country=="Palestine"),]

head(svydata)
sort(unique(svydata$country))
n <- table(svydata$country, svydata$indicator)
dim(n)

data <- subset(svydata,indicator=="score")
dim(data)
head(data)

# data$new.var = factor(data$country, levels=data[order(data$value, decreasing = TRUE), "country"])
data$new.var <- factor(data$country, levels = data$country[order(data$value, decreasing = TRUE)])

data$value <- round(data$value,3)
head(data)

data$value <- 20*data$value
data$CIL <- 20*data$CIL
data$CIU <- 20*data$CIU

mean(data$value)

plot_partA <- ggplot(data=data, aes(x=new.var,y=value)) + 
  geom_point(shape=1) +
  geom_errorbar(aes(ymin=CIL, ymax=CIU),size=0.5) +
#  geom_hline(yintercept=mean(data$value),colour="grey25") +
  labs(tag = "(A)") +
  theme_bw() +
  theme(text = element_text(size=17),
        plot.tag.position = c(0.01,.99),
        panel.grid.major.x = element_blank() ,
        axis.ticks.x=element_blank(),
        axis.text.x = element_text(size=10, angle=90),
        axis.text.y = element_text(size=12)) +
  scale_y_continuous(breaks = c(0,20,40,60,80,100))+
  xlab("") +
  ylab("delivery care coverage %")
plot_partA
# ggsave(plot=plot_partA, height = 7 , width = 10 , "/Users/EWilson/Desktop/DAC/Delivery/Results/Figure2a.png")

wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

data$boxplot <- 1
plot_partB <-  ggplot(data=data,aes(y=value,x=1)) +
#  geom_point(size=.75, show.legend = FALSE) +
  geom_jitter(size=1, position=position_jitter(width=0.05, height=0.05)) +
  geom_boxplot(alpha=.20) +
  scale_y_continuous(breaks=c(0,20,40,60,80,100)) +
  labs(tag = "(B)") +
  xlab("") +
  ylab("") +
  labs(col="") +
  theme_bw() +
  theme(text = element_text(size=15),
        plot.tag.position = c(0.10,.99),
        axis.ticks.x=element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y = element_blank())
plot_partB 

quantile(data$value)
IQR(data$value)

# Fig2 <- ggdraw(plot_grid(plot_partA, plot_partB, nrow=1, rel_widths = c(4,1)))
Fig2 <- plot_grid(plot_partA, plot_partB, align="hv", rel_widths=c(5,1))
Fig2
ggsave(plot=Fig2, height = 7, width = 14, dpi = 300, "/Users/EWilson/Desktop/DAC/Delivery/Results/Figure2.jpeg")




