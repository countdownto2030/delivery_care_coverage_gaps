# last edited 19 Jul 2023
# last run 19 Jul 2023
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





function(){
########################################################### FIGURE 1 - multi-country with median value bars
data <- svydata[svydata$indicator %!in% c("faclevel","score"),] # remove summary measures not being used here
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

data$indicator <- factor(data$indicator, levels = unique(data$indicator[order(data$use.this)]))

df1 <- data %>% group_by(indicator) %>% mutate(med = median(value))
df2 <- df1 %>% group_by(indicator) %>%  summarise(n=n())
df1$value <- df1$value*100
df1$med <- df1$med*100

# make separate plots and then grid arrange
wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

mycolors = c("lightblue","cornflowerblue","midnightblue","plum1")
graph_facility <- ggplot(data=df1[df1$indicator %in% c("faclevel0","faclevel1","faclevel2","ideliv"),],aes(x=indicator,y=value)) +
  geom_bar(aes(x=indicator,y=med,fill=indicator),
           position="dodge", stat="identity", alpha=.02) +
  geom_jitter(size=1, position=position_jitter(width=0.1, height=0.1)) +#
  labs(tag = "(A)") +
  scale_y_continuous(limits=c(0,100),breaks=c(0,20,40,60,80,100)) +
  xlab("") +
  ylab("coverage estimate %") +
  ggtitle(wrapper("Place of delivery", width=20)) +
  labs(col="") +
  theme_bw() +
  theme( # axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of y ticks/text
          text = element_text(size=15),     
          plot.tag.position = c(0.17,.99),
          axis.title.y=element_text(size=15),
          axis.text.y = element_text(size=12),
          axis.text.x = element_text(size=9, vjust=.5),
          plot.title = element_text(hjust = 0.5, size = 15)) +
  scale_x_discrete(labels=str_wrap(c("0-Home","1-Basic & Primary","2-Secondary","facility delivery"),width=11))  +
  scale_fill_manual(values=mycolors) +
  scale_colour_manual(values=mycolors) +
  theme(legend.position = "none")
graph_facility 


mycolors = c("purple2","purple4","lightgreen","darkgreen")
graph_indicator <- ggplot(data=df1[df1$indicator %in% c("sba","sbaORideliv","ideliv24hr","pncwm"),],aes(x=indicator,y=value)) +
  geom_bar(aes(x=indicator,y=med,fill=indicator),
           position="dodge", stat="identity", alpha=0.02) +
  geom_jitter(size=1, position=position_jitter(width=0.1, height=0.1)) +#
  labs(tag = "(B)") +
  scale_y_continuous(limits=c(0,100),breaks=c(0,20,40,60,80,100)) +
  xlab("") +
  ylab("coverage estimate %") +
  ggtitle(wrapper("Contact Indicators", width=20)) +
  labs(col="") +
  theme_bw() +
  theme(text = element_text(size=15),
        plot.tag.position = c(0.05,.99),
        axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of y ticks/text
        axis.title.y=element_blank(), # remove x axis label
        axis.text.x = element_text(size=9, vjust=.5),
        plot.title = element_text(hjust = 0.5, size = 15)) +
  scale_x_discrete(labels = str_wrap(c("skilled attendent", "skilled attendent OR facility delivery", "24+hr stay in facility","health check within 2d"), width = 20)) +
  scale_fill_manual(values=mycolors) +
  scale_colour_manual(values=mycolors) +
  theme(legend.position = "none")
graph_indicator


my.labels <- c("\n0", "\n1","\n2","\n3","\n4","\n5")

mycolors = c(brewer.pal(name="YlOrRd", n = 7))
graph_score <- ggplot(data=df1[df1$indicator %in% c("score0","score1","score2","score3","score4","score5"),],aes(x=indicator,y=value)) +
  geom_bar(aes(x=indicator,y=med,fill=indicator),
           position="dodge", stat="identity", alpha=0.02) +
  geom_jitter(size=1, position=position_jitter(width=0.1, height=0.1)) +#
  labs(tag = "(C)") +
  scale_y_continuous(limits=c(0,100),breaks=c(0,20,40,60,80,100)) +
  xlab("") +
  ylab("coverage estimate %") +
  ggtitle(wrapper("Composite score", width=20)) +
  labs(col="") +
  theme_bw() +
  theme(text = element_text(size=15),
        plot.tag.position = c(0.05,.99),
        axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of y ticks/text
        axis.title.y=element_blank(), # remove x axis label
        axis.text.x = element_text(size=9, vjust=.5),
        plot.title = element_text(hjust = 0.5, size = 15)) +
#  scale_x_discrete(labels = str_wrap(c("0","1","2","3","4","5"),width=0)) +
  scale_x_discrete(labels= my.labels) +
   scale_fill_manual(values=rev(mycolors)) +
  scale_colour_manual(values=rev(mycolors)) +
  theme(legend.position = "none")
graph_score 


Fig1a <- ggdraw(plot_grid(plot_grid(graph_facility,graph_indicator, graph_score, nrow=1, rel_widths = c(1,1,1))))
ggsave(plot=Fig1a, height = 7 , width = 14 , "/Users/EWilson/Desktop/DAC/Delivery/Results/Figure1.png")




########################################################### FIGURE 2 - Average score by country
# svydata <- read.csv(paste0(location,"/Results/",date,"_data.svy.csv"), stringsAsFactors = FALSE)
# data <- subset(svydata,indicator=="score")
# dim(data)
# head(data)

# ### Vertical
# data$new.var = factor(data$country, levels=data[order(data$value, decreasing = TRUE), "country"])
# # View(data)
# data$value <- round(data$value,3)
# 
# plot <- ggplot(data=data, aes(x=value,y=new.var)) + 
#   geom_point(shape=1) +
#   geom_errorbarh(aes(xmin=CIL, xmax=CIU),height=0.5,size=0.5) +
#   geom_vline(xintercept=c(3.3),colour="grey25") +
#   theme(text = element_text(size=17),
#         panel.grid.major.x = element_blank() ,
#         axis.text.x = element_text(size=9),
#         axis.text.y = element_text(size=9)) +
#   scale_x_continuous(breaks = c(1,2,3,4,5,6))+
#   xlab("svy-weighted score") +
#   ylab("country")
# plot
# ggsave(plot=plot, height = 11 , width = 6 , "/Users/EWilson/Desktop/DAC/Delivery/Results/Figure2a.png")


### Horizontal
svydata <- read.csv(paste0(location,"/Results/",date,"_data.svy.csv"), stringsAsFactors = FALSE)
data <- subset(svydata,indicator=="score")
dim(data)
head(data)

data$new.var = factor(data$country, levels=data[order(data$value, decreasing = TRUE), "country"])
# View(data)
data$value <- round(data$value,3)

plot <- ggplot(data=data, aes(x=new.var,y=value)) + 
  geom_point(shape=1) +
  geom_errorbar(aes(ymin=CIL, ymax=CIU),height=0.5,size=0.5) +
  # expand_limits(x=c(-.35,.35)) +
  geom_hline(yintercept=mean(data$value),colour="grey25") +
  theme_bw() +
  theme(text = element_text(size=17),
        panel.grid.major.x = element_blank() ,
        axis.ticks.x=element_blank(),
        axis.text.x = element_text(size=9, angle=90),
        axis.text.y = element_text(size=9)) +
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6))+
  xlab("") +
  ylab("composite care score")
plot
ggsave(plot=plot, height = 7 , width = 10 , "/Users/EWilson/Desktop/DAC/Delivery/Results/Figure2.png")
}





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

# add stage of obstetric transition
# data$mmr <- as.numeric(data$mmr)
# data$stage[data$mmr>=700]<-1
# data$stage[data$mmr>=300 & data$mmr<700]<-2
# data$stage[data$mmr>=100 & data$mmr<300]<-3
# data$stage[data$mmr>=20 & data$mmr<100]<-4
# data$stage[data$mmr<20]<-5

# table(data$mmr, exclude = NULL)
# plot(data$mmr,data$value)
data <- subset(data, country!="Palestine")

# data$mmr <- as.numeric(data$mmr)

# graph_association_1 <- ggplot(data,aes(x=value,y=mmr)) +
# graph_association_1 <- ggplot(data,aes(x=indicator,y=mmr)) +
#   geom_point(size=1, alpha=.5) +
#   geom_smooth(method='lm', se=FALSE, formula= y~x, color="black") +
#   ylab("maternal mortality ratio") +
#   xlab("composite care score") +
#   theme_bw()
# graph_association_1
# 
# model_plot_1 <- lm(mmr ~ value, data=data)
# fit_1 <- predict(model_plot_1, interval = "confidence")
# data_plot_1 <- cbind(data,fit_1)
# summary(model_plot_1)
# 
# 
# 
# 
# 
# # facility delivery
# data <- subset(svydata,indicator=="ideliv")
# dim(data)
# data <- merge(data,MMR,by=c("country"))
# dim(data)
# table(data$mmr, exclude = NULL)
# plot(data$mmr,data$value)
# data <- subset(data, country!="Palestine")
# 
# data$mmr <- as.numeric(data$mmr)
# 
# graph_association_2 <- ggplot(data,aes(x=value,y=mmr)) +
#   geom_point(size=1, alpha=.5) +
#   geom_smooth(method='lm', se=FALSE, formula= y~x, color="black") +
#   ylab("maternal mortality ratio") +
#   xlab("facility delivery coverage") +
#   theme_bw()
# graph_association_2
# 
# model_plot_2 <- lm(mmr ~ value, data=data)
# fit_2 <- predict(model_plot_2, interval = "confidence")
# data_plot_2 <- cbind(data,fit_2)
# summary(model_plot_2)


# quality coverage
svydata <- read.csv(paste0(location,"/Results/",date,"_data.svy.csv"), stringsAsFactors = FALSE)
data <- subset(svydata,indicator=="qcov")
dim(data)
head(data)
data <- merge(data,MMR,by=c("country"))
dim(data)
head(data)

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


graph_association <- ggplot(data,aes(x=value,y=mmr)) +
  geom_point(size=1, alpha=.5) +
  geom_smooth(method='lm', se=FALSE, formula= y~x, color="black") +
  ylab("maternal mortality ratio") +
  xlab("quality delivery coverage") +
  theme_bw() + 
  theme(axis.title = element_text(size=15),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12)) +
  # annotate("text",x=.6,y=875,label=(paste0("y==",round(coef(lm(data$mmr~data$value))[1],3),"test")),parse=TRUE)+
  # annotate("text",x=.6,y=875,label=(test))+
  annotate("text",x=.62,y=875,label=(paste0("y=",test)),size = 5)+
  annotate("text",x=.6,y=800,label=(paste0("Rsquared==",RSqAdj)),parse=TRUE,size = 5)
  
graph_association

ggsave(plot=graph_association, height = 5 , width = 5 , "/Users/EWilson/Desktop/DAC/Delivery/Results/Figure7.png")




