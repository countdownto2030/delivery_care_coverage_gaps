# last edited 26 Jun 2023
# last run 26 Jun 2023
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
# get high/low value for region
# regiondata <- subset(data, covariate %in% c("region"))
# data1 <- regiondata %>% group_by(country) %>% filter(value %in% min(value))
# data1$level <- "min"
# data2 <- regiondata %>% group_by(country) %>% filter(value %in% max(value))
# data2$level <- "max"
# data_region <- rbind(data1,data2)

# data <- subset(data, covariate %!in% c("region"))
# data <- rbind(data_region,data)

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
covariates <- c('wealth','urban.rural','age','education','marital.status','first.time.mother','region')

wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

# reformat names so same scale
data$country[data$country %in% "CentralAfricanRepublic"] <- "CAR"
data$country[data$country %in% "DominicanRepublic"] <- "DR"


# for(j in 1:length(unique(data$covariate))){
j = 1
subj <- subset(data, data$covariate == covariates[j])
#   print(subj$level)
  sub <- subset(subj, subj$stage == 1)
  colourCount = length(unique(sub$level))
  mmr1 <- ggplot(sub, aes(x=reorder(country,-diff), y=value)) +
    geom_line(aes(group=country))+
    geom_point(size=4, aes(color = level)) +
    labs(tag = "(A)",x= "", y = "quality delivery score") +
    ggtitle(wrapper(paste0("mean diff: 2.2"), width=30)) +
    ylim(0,5) +
    scale_colour_discrete(name=covariates[j]) +
    theme_bw() +
    theme(text = element_text(size=12), 
          plot.tag.position = c(0.25,.99),
          axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust=1),
          plot.title = element_text(hjust = .8, size = 12),
          legend.position = "none") 
  mmr1
  
  sub <- subset(subj, subj$stage == 2)
  colourCount = length(unique(sub$level))
  mmr2 <- ggplot(sub, aes(x=reorder(country,-diff), y=value)) +
    geom_line(aes(group=country))+
    geom_point(size=4, aes(color = level)) +
    labs(tag = "(B)",x= "", y = "") +
    ggtitle(wrapper(paste0("mean diff: 1.7"), width=45)) +
    ylim(0,5) +
    scale_colour_discrete(name=covariates[j]) +
    theme_bw() +
    theme(plot.tag.position = c(0.1,.99),
          axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust=1),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.title = element_text(hjust =.15, size = 12),
          legend.position = "none") +
    theme(plot.margin=margin(l=-0.4,unit="cm"))
  mmr2

  sub <- subset(subj, subj$stage == 3)
  colourCount = length(unique(sub$level))
  mmr3 <- ggplot(sub, aes(x=reorder(country,-diff), y=value)) +
    geom_line(aes(group=country))+
    geom_point(size=4, aes(color = level)) +
    labs(tag = "(C)",x= "", y = "") +
    ggtitle(wrapper(paste0("mean diff: 1.7"), width=45)) +
    ylim(0,5) +
    scale_colour_discrete(name=covariates[j]) +
    theme_bw() +
    theme(plot.tag.position = c(0.1,.99),
          axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust=1),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.title = element_text(hjust =.15, size = 12),
          legend.position = "none") +
    theme(plot.margin=margin(l=-0.3,unit="cm"))
  mmr3
  
  sub <- subset(subj, subj$stage == 4)
  colourCount = length(unique(sub$level))
  mmr4 <- ggplot(sub, aes(x=reorder(country,-diff), y=value)) +
    geom_line(aes(group=country))+
    geom_point(size=4, aes(color = level)) +
    labs(tag = "(D)",x= "", y = "") +
    ggtitle(wrapper(paste0("mean diff: 0.5"), width=45)) +
    ylim(0,5) +
    scale_colour_discrete(name=covariates[j]) +
    theme_bw() +
    theme(plot.tag.position = c(0.1,.99),
          axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust=1),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.title = element_text(hjust = .15, size = 12),
          legend.position = "none") +
    theme(plot.margin=margin(l=-0.3,unit="cm"))
  mmr4

  sub$covariate <- "wealth quintile"
  
  sub <- subset(subj, subj$stage == 5)
  colourCount = length(unique(sub$level))
  mmr5 <- ggplot(sub, aes(x=reorder(country,-diff), y=value)) +
    geom_line(aes(group=country))+
    geom_point(size=4, aes(color = level)) +
    labs(tag = "(E)",x= "", y = "") +
    ggtitle(wrapper(paste0("mean diff: 0.3"), width=45)) +
    ylim(0,5) +
    scale_colour_discrete(name=covariates[j]) +
    theme_bw() +
    theme(plot.tag.position = c(0.15,.99),
          axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust=1),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.title = element_text(hjust = .75, size = 12)) +
    theme(plot.margin=margin(l=-0.3,unit="cm")) +
    guides(color = guide_legend(title = wrapper(paste0("Wealth quintile"), width=10)))
  
  mmr5

  
  # legend <- get_legend(mmr1)
  
  # and replot suppressing the legend
  # mmr1 <- mmr1 + theme(legend.position='none')
   
  
  # equity_facet <- ggdraw(plot_grid(plot_grid(mmr1, mmr2, mmr3, mmr4, mmr5, nrow=1),
  #                                  plot_grid(NULL, legend_cascade, nrow=1),
  #                                  rel_widths=c(1,.2)))
  # 
  # test <- plot_grid(mmr1,mmr2,mmr3,NULL,mmr4,mmr5,ncol=3,rel_widths = c(1,1,1,1,1))
  
  mmr <- plot_grid(mmr1,mmr2,mmr3,mmr4,mmr5,ncol=5, align='h',rel_widths = c(.7,1.2,1.1,1,1)) +
  theme(plot.background = element_rect(fill = "white", colour = NA))
  ggsave(plot=mmr, height = 7 , width = 13 , paste0("/Users/EWilson/Desktop/DAC/Delivery/Results/Figure6.png"))
