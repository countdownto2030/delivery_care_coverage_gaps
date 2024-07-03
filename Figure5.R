# last edited 3 Jul 2024
# last run 3 Jul 2024
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
date = "May 13"
# svy.date <- "Jul 21" 
# svy.strat.date <- "Jul 21" 
location <- "/Users/EWilson/Desktop/DAC/Delivery"
setwd(location)

########################################################### FIGURE 3 - Equiplots - MMR
# read in obstetric transition values
MMR <- read.csv(paste0(location,"/ReadyforAnalysis_2022-10-26.csv"))
MMR <- MMR[,c("country","mmr")]
# head(data_master)
head(MMR)

svydatastrata <- read_csv(paste0(location,"/Results/",date,"_data.strat.svy.csv"))
svydatastrata$level[svydatastrata$covariate=="education" & svydatastrata$level=="none"] <- "pre-primary or none"

dim(svydatastrata)
svydatastrata <- merge(svydatastrata,MMR,by=c("country"))
dim(svydatastrata)

# add stage of obstetric transition
svydatastrata$mmr <- as.numeric(svydatastrata$mmr)

svydatastrata$stage[svydatastrata$mmr>=700]<-1
svydatastrata$stage[svydatastrata$mmr>=300 & svydatastrata$mmr<700]<-2
svydatastrata$stage[svydatastrata$mmr>=100 & svydatastrata$mmr<300]<-3
svydatastrata$stage[svydatastrata$mmr>=20 & svydatastrata$mmr<100]<-4
svydatastrata$stage[svydatastrata$mmr<20]<-5

data <- subset(svydatastrata, indicator %in% c("score"))
data <- data %>% filter(covariate!="wealth" | (covariate=="wealth" & level %in% c("1","5")))
data$level[data$covariate=="wealth" & data$level=="1"] <- "lowest"
data$level[data$covariate=="wealth" & data$level=="5"] <- "highest"

head(data)

# scale score to a proportion
data$value <- data$value*20

data2 <- data %>%
  group_by(country,indicator,covariate) %>%
  summarise(
    max = max(value, na.rm = T),
    min = min(value, na.rm = T)
  ) %>%
  arrange(country)
data2$diff <- data2$max - data2$min
head(data2)

dim(data)
data <- merge(data,data2,by=c("country","indicator","covariate"))
dim(data)
unique(data$covariate)


# average gap by MMR for discussion of how they compare
data_for_gap <- data[,c("country","level","value","mmr","stage")]

data_for_gap_wide = data_for_gap %>% spread(level,value)
data_for_gap_wide$diff <- abs(data_for_gap_wide$`lowest` - data_for_gap_wide$`highest`)

data_for_gap_avg <- data_for_gap_wide %>% group_by(stage) %>% 
  summarize(Mean = mean(diff, na.rm=TRUE))

data_for_gap_avg



getPalette = colorRampPalette(brewer.pal(11, "RdYlGn"))

wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

# reformat names so same scale
data$country[data$country %in% "CentralAfricanRepublic"] <- "CAR"
data$country[data$country %in% "DominicanRepublic"] <- "DR"



j = 1
subj <- subset(data, data$covariate == 'wealth')

my.title <- c("MMR: >=700\n mean gap: 44pp")

  sub <- subset(subj, subj$stage == 1)
  mmr1 <- ggplot(sub, aes(x=reorder(country,-diff), y=value)) +
    geom_line(aes(group=country))+
    geom_point(size=4, aes(color = level)) +
    labs(tag = "(A)",x= "", y = "delivery care coverage %") +
    ggtitle(my.title) +
    ylim(0,100) +
    scale_colour_manual(values=c('darkblue','gold')) +
    theme_bw() +
    theme(text = element_text(size=12), 
          plot.tag.position = c(0.18,.99),
          axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust=1),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(hjust = .14, size = 11),
          axis.title.y=element_text(size = 16),
          legend.position = "none") 
  mmr1
  
  
my.title <- c("MMR: 300-699\n mean gap: 34pp")
  
  sub <- subset(subj, subj$stage == 2)
  mmr2 <- ggplot(sub, aes(x=reorder(country,-diff), y=value)) +
    geom_line(aes(group=country))+
    geom_point(size=4, aes(color = level)) +
    labs(tag = "(B)",x= "", y = "") +
    ggtitle(my.title) +
    ylim(0,100) +
    scale_colour_manual(values=c('darkblue','gold')) +
    theme_bw() +
    theme(plot.tag.position = c(0.15,.99),
          axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust=1),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.title = element_text(hjust =.25, size = 11),
          legend.position = "none") +
    theme(plot.margin=margin(l=-0.4,unit="cm"))
  mmr2

  
my.title <- c("MMR: 100-299\n mean gap: 33pp")
  
  sub <- subset(subj, subj$stage == 3)
  mmr3 <- ggplot(sub, aes(x=reorder(country,-diff), y=value)) +
    geom_line(aes(group=country))+
    geom_point(size=4, aes(color = level)) +
    labs(tag = "(C)",x= "", y = "") +
    ggtitle(my.title) +
    ylim(0,100) +
    scale_colour_manual(values=c('darkblue','gold')) +
    theme_bw() +
    theme(plot.tag.position = c(0.15,.99),
          axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust=1),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.title = element_text(hjust =.25, size = 11),
          legend.position = "none") +
    theme(plot.margin=margin(l=-0.3,unit="cm"))
  mmr3
  
  
my.title <- c("MMR: 20-99\n  mean gap: 11pp")

  sub <- subset(subj, subj$stage == 4)
  mmr4 <- ggplot(sub, aes(x=reorder(country,-diff), y=value)) +
    geom_line(aes(group=country))+
    geom_point(size=4, aes(color = level)) +
    labs(tag = "(D)",x= "", y = "") +
    ggtitle(my.title) +
    ylim(0,100) +
    scale_colour_manual(values=c('darkblue','gold')) +
    theme_bw() +
    theme(plot.tag.position = c(0.15,.99),
          axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust=1),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.title = element_text(hjust = .25, size = 11),
          legend.position = "none") +
    theme(plot.margin=margin(l=-0.3,unit="cm"))
  mmr4

  # sub$covariate <- "wealth quintile"
  
my.title <- c("MMR: <20\n      mean gap: 5pp")
  
  sub <- subset(subj, subj$stage == 5)
  mmr5 <- ggplot(sub, aes(x=reorder(country,-diff), y=value)) +
    geom_line(aes(group=country))+
    geom_point(size=4, aes(color = level)) +
    labs(tag = "(E)",x= "", y = "") +
    ggtitle(my.title) +
    ylim(0,100) +
    scale_colour_manual(values=c('darkblue','gold')) +
    theme_bw() +
    theme(plot.tag.position = c(0.12,.99),
          axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust=1),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.title = element_text(hjust = .45, size = 11)) +
    theme(plot.margin=margin(l=-0.3,unit="cm")) +
    guides(color = guide_legend(title = "wealth quintile",labels=c("highest quintile","lowest quintile")))
  
  mmr5

  

  mmr <- plot_grid(mmr1,mmr2,mmr3,mmr4,mmr5,ncol=5, align='h',rel_widths = c(.7,1.2,1.1,1,1)) +
  theme(plot.background = element_rect(fill = "white", colour = NA))
  ggsave(plot=mmr, height = 7.5, width = 13, dpi = 300, paste0("/Users/EWilson/Desktop/DAC/Delivery/Results/Figure5.jpeg"))
