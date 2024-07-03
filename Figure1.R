# last edited 3 Jul 2024
# last run 3 Jul 2024
# Objective: get Figure1

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
svydata_pooled <- read_csv(paste0(location,"/Results/May 13_pooled_standard_data.svy.csv"))
names(svydata_pooled)[names(svydata_pooled)=="level"] <- "country"
svydata <- rbind(svydata,svydata_pooled)

svydata <- svydata[!(svydata$country=="Palestine"),] # no MMR estimate from 2017

head(svydata)
sort(unique(svydata$country))
n <- table(svydata$country, svydata$indicator)
dim(n)
########################################################### FIGURE 1 - multi-country with median value bars
# data <- svydata[svydata$indicator %!in% c("faclevel","score"),] # remove summary measures not being used here
data <- svydata[svydata$indicator %!in% c("faclevel","qcov"),] # remove summary measures not being used here
unique(data$indicator)

data$use.this[data$indicator=='sba'] <-1
data$use.this[data$indicator=='faclevel0'] <-2
data$use.this[data$indicator=='faclevel1'] <-3
data$use.this[data$indicator=='faclevel2'] <-4
data$use.this[data$indicator=='ideliv'] <-5
data$use.this[data$indicator=='sbaORideliv'] <-6
data$use.this[data$indicator=='ideliv24hr'] <-7
data$use.this[data$indicator=='pncwm'] <-8
data$use.this[data$indicator=='score0'] <-9
data$use.this[data$indicator=='score1'] <-10
data$use.this[data$indicator=='score2'] <-11
data$use.this[data$indicator=='score3'] <-12
data$use.this[data$indicator=='score4'] <-13
data$use.this[data$indicator=='score5'] <-14
data$use.this[data$indicator=='score'] <-15

data$indicator <- factor(data$indicator, levels = unique(data$indicator[order(data$use.this)]))

df1 <- data %>% group_by(indicator) %>% mutate(med = median(value))
df2 <- df1 %>% group_by(indicator) %>%  summarise(n=n())
df1$value <- ifelse(df1$indicator == "score", df1$value*20, df1$value*100)
df1$med <- ifelse(df1$indicator == "score", df1$med*20, df1$med*100)

# make separate plots and then grid arrange
wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

my.title <- c("Place of delivery\n")
mycolors = c("lightblue","cornflowerblue","midnightblue","plum1")

df1_1 <- df1[df1$indicator %in% c("faclevel0","faclevel1","faclevel2","ideliv") & df1$country!="pooled standard",]
df1_2 <- df1[df1$indicator %in% c("faclevel0","faclevel1","faclevel2","ideliv") & df1$country=="pooled standard",]

graph_facility <- ggplot() +
  geom_point(data=df1_1,aes(x=indicator,y=value),position=position_jitter(width=0.1, height=0.1)) +
  geom_bar(data=df1_2,aes(x=indicator,y=value,fill=indicator),
           position="dodge", stat="identity", alpha=0.65) +
  labs(tag = "(A)") +
  scale_y_continuous(limits=c(0,100),breaks=c(0,20,40,60,80,100)) +
  xlab("") +
  ylab("coverage estimate %") +
#  ggtitle(wrapper("Place of delivery", width=15)) +
  ggtitle(my.title) +
  labs(col="") +
  theme_bw() +
  theme( # axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of y ticks/text
          text = element_text(size=12),     
          plot.tag.position = c(0.17,.99),
          axis.title.y=element_text(size=16),
          axis.text.y = element_text(size=14),
          axis.text.x = element_text(size=10, vjust=.5),
          plot.title = element_text(hjust = 0.5, size = 14)) +
  scale_x_discrete(labels=str_wrap(c("home","lower-level","hospital","facility delivery"),width=11))  +
  scale_fill_manual(values=mycolors) +
  scale_colour_manual(values=mycolors) +
  theme(legend.position = "none") 
graph_facility 


df1_1 <- df1[df1$indicator %in% c("sba","sbaORideliv","ideliv24hr","pncwm") & df1$country!="pooled standard",]
df1_2 <- df1[df1$indicator %in% c("sba","sbaORideliv","ideliv24hr","pncwm") & df1$country=="pooled standard",]

my.title <- c("Contact indicators\n")

my.labels <- c("skilled \nattendent", "skilled attendent \nOR facility delivery", "24hr+ \nstay","health check 2d")

mycolors = c("purple2","purple4","lightgreen","darkgreen")
graph_indicator <- ggplot() +
  geom_point(data=df1_1,aes(x=indicator,y=value),position=position_jitter(width=0.1, height=0.1)) +
  geom_bar(data=df1_2,aes(x=indicator,y=value,fill=indicator),
           position="dodge", stat="identity", alpha=0.65) +
  labs(tag = "(B)") +
  scale_y_continuous(limits=c(0,100),breaks=c(0,20,40,60,80,100)) +
  xlab("") +
  ylab("coverage estimate %") +
#  ggtitle(wrapper("Contact Indicators", width=18)) +
  ggtitle(my.title) +
  labs(col="") +
  theme_bw() +
  theme(text = element_text(size=12),
        plot.tag.position = c(0.05,.99),
        axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of y ticks/text
        axis.title.y=element_blank(), # remove x axis label
        axis.text.x = element_text(size=10, vjust=.5),
        plot.title = element_text(hjust = 0.5, size = 14)) +
#  scale_x_discrete(labels = str_wrap(c("skilled attendent", "skilled attendent OR facility delivery", "24hr+ stay","health check 2d"), width = 21)) +
  scale_x_discrete(labels= my.labels) +
  scale_fill_manual(values=mycolors) +
  scale_colour_manual(values=mycolors) +
  theme(legend.position = "none")
graph_indicator


df1_1 <- df1[df1$indicator %in% c("score0","score1","score2","score3","score4","score5") & df1$country!="pooled standard",]
df1_2 <- df1[df1$indicator %in% c("score0","score1","score2","score3","score4","score5") & df1$country=="pooled standard",]

my.labels <- c("\n0", "\n1","\n2","\n3","\n4","\n5")
my.title <- c("Delivery care score\n")
mycolors = c(brewer.pal(name="YlOrRd", n = 7))
graph_score <- ggplot() +
  geom_point(data=df1_1,aes(x=indicator,y=value),position=position_jitter(width=0.1, height=0.1)) +
  geom_bar(data=df1_2,aes(x=indicator,y=value,fill=indicator),
           position="dodge", stat="identity", alpha=0.65) +
  labs(tag = "(C)") +
  scale_y_continuous(limits=c(0,100),breaks=c(0,20,40,60,80,100)) +
  xlab("") +
  ylab("coverage estimate %") +
#  ggtitle(wrapper("Quality delivery score", width=30)) +
  ggtitle(my.title) +
  labs(col="") +
  theme_bw() +
  theme(text = element_text(size=12),
        plot.tag.position = c(0.05,.99),
        axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of y ticks/text
        axis.title.y=element_blank(), # remove x axis label
        axis.text.x = element_text(size=10, vjust=.5),
        plot.title = element_text(hjust = 0.5, size = 14)) +
#  scale_x_discrete(labels = str_wrap(c("0","1","2","3","4","5"),width=0)) +
  scale_x_discrete(labels= my.labels) +
  scale_fill_manual(values=rev(mycolors)) +
  scale_colour_manual(values=rev(mycolors)) +
  theme(legend.position = "none")
graph_score 


df1_1 <- df1[df1$indicator %in% c("score") & df1$country!="pooled standard",]
df1_2 <- df1[df1$indicator %in% c("score") & df1$country=="pooled standard",]

my.labels <- c("\nco-coverage")
mycolors = c("grey")
head(df1_1)
head(df1_2)
graph_wt_scaled_score <- ggplot() +
  geom_point(data=df1_1,aes(x=indicator,y=value),position=position_jitter(width=0.1, height=0.1)) +
  geom_bar(data=df1_2,aes(x=indicator,y=value,fill=indicator),
           position="dodge", stat="identity", alpha=0.65) +
  labs(tag = "(D)") +
  scale_y_continuous(limits=c(0,100),breaks=c(0,20,40,60,80,100)) +
  xlab("") +
  ylab("") +
  ggtitle(wrapper("Delivery care", width=6)) +
  labs(col="") +
  theme_bw() +
  theme(text = element_text(size=12),
        plot.tag.position = c(0.05,.99),
        axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of y ticks/text
        axis.title.y=element_blank(), # remove x axis label
        axis.text.x = element_text(size=10, vjust=.5),
        plot.title = element_text(hjust = 0.5, size = 14)) +
  # scale_x_discrete(labels = str_wrap(c("co-coverage"), width = 12)) +
  scale_x_discrete(labels= my.labels) +
  scale_fill_manual(values=mycolors) +
  scale_colour_manual(values=mycolors) +
  theme(legend.position = "none")
graph_wt_scaled_score




Fig1 <- ggdraw(plot_grid(plot_grid(graph_facility, graph_indicator, graph_score, graph_wt_scaled_score, nrow=1, rel_widths = c(1,1,1,.4))))
ggsave(plot=Fig1, height = 7 , width = 14, dpi = 300, "/Users/EWilson/Desktop/DAC/Delivery/Results/Figure1.jpeg")



