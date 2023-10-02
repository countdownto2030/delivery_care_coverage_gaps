# last edited 21 Jul 2023
# last run 21 Jul 2023
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

# svydatastrata <- read_csv(paste0(location,"/Results/",date,"_data.strat.svy.csv"))
# svydatastrata$level[svydatastrata$covariate=="education" & svydatastrata$level=="none"] <- "pre-primary or none"
dim(svydata)
svydata <- merge(svydata,MMR,by=c("country"))
dim(svydata)

# add stage of obstetric transition
svydata$mmr <- as.numeric(svydata$mmr)

svydata$stage[svydata$mmr>=700]<-1
svydata$stage[svydata$mmr>=300 & svydata$mmr<700]<-2
svydata$stage[svydata$mmr>=100 & svydata$mmr<300]<-3
svydata$stage[svydata$mmr>=20 & svydata$mmr<100]<-4
svydata$stage[svydata$mmr<20]<-5

data <- subset(svydata, indicator %in% c("score","ideliv"))
# data <- data %>% filter(covariate!="wealth" | (covariate=="wealth" & level %in% c("1","5")))
# data$level[data$covariate=="wealth" & data$level=="1"] <- "lowest"
# data$level[data$covariate=="wealth" & data$level=="5"] <- "highest"

head(data)
# get high/low value for region
# regiondata <- subset(data, covariate %in% c("region"))
# data1 <- regiondata %>% group_by(country) %>% filter(value %in% min(value))
# data1$level <- "min"
# data2 <- regiondata %>% group_by(country) %>% filter(value %in% max(value))
# data2$level <- "max"
# data_region <- rbind(data1,data2)

dataA <- subset(data, indicator %in% c("ideliv"))
dataB <- subset(data, indicator %in% c("score"))
widedata <- merge(dataA,dataB, by = c("country"))

names(widedata) <- gsub(x = names(widedata), pattern = "\\.x", replacement = "ideliv")  
names(widedata) <- gsub(x = names(widedata), pattern = "\\.y", replacement = "score")  
head(widedata)

data2 <- widedata 
data2$valuescore <- data2$valuescore*20/100
data2$CILscore <- data2$CILscore*20/100
data2$CIUscore <- data2$CIUscore*20/100
# %>%
#   group_by(country,indicator) %>%
#   summarise(
#     max = max(value, na.rm = T),
#     min = min(value, na.rm = T)
#   ) %>%
#   arrange(country)
data2$diff <- data2$valueideliv - data2$valuescore
head(data2)
unique(data2$diff)


# average gap by MMR for discussion of how they compare
data2$abs_diff <- abs(data2$`valueideliv` - data2$`valuescore`)
data2 <- subset(data2, !is.na(stagescore))
data_for_gap_avg <- data2 %>% group_by(stagescore) %>% 
  summarize(Mean = mean(diff, na.rm=TRUE))

data_for_gap_avg



getPalette = colorRampPalette(brewer.pal(11, "RdYlGn"))
# covariates <- c('wealth','urban.rural','age','education','marital.status','first.time.mother','region')

wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}



head(data)
head(data2)
subj <- merge(data, data2[,c("country","diff","abs_diff")], by=c("country"))

# reformat names so same scale
data$country[data$country %in% "CentralAfricanRepublic"] <- "CAR"
data$country[data$country %in% "DominicanRepublic"] <- "DR"

subj$value <- ifelse(subj$indicator %in% "score", subj$value*20/100, subj$value)
subj$CIL <- ifelse(subj$indicator %in% "score", subj$CIL*20/100, subj$CIL)
subj$CIU <- ifelse(subj$indicator %in% "score", subj$CIU*20/100, subj$CIU)
head(subj)

sub <- subset(subj, subj$stage == 1)
colourCount = length(unique(sub$indicator))
mmr1 <- ggplot(sub, aes(x=reorder(country,-diff), y=value)) +
  geom_line(aes(group=country))+
  geom_point(size=4, aes(color = indicator)) +
  labs(tag = "(A)",x= "", y = "proportion of sample population") +
  ggtitle(wrapper(paste0("mean diff: 0.14"), width=30)) +
  ylim(0,1) +
  # scale_colour_discrete(name=covariates[j]) +
  theme_bw() +
  theme(text = element_text(size=12), 
        plot.tag.position = c(0.25,.99),
        axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(hjust = .8, size = 12),
        legend.position = "none") 
mmr1

sub <- subset(subj, subj$stage == 2)
head(sub)
colourCount = length(unique(sub$indicator))
mmr2 <- ggplot(sub, aes(x=reorder(country,-diff), y=value)) +
  geom_line(aes(group=country))+
  geom_point(size=4, aes(color = indicator)) +
  labs(tag = "(B)",x= "", y = "") +
  ggtitle(wrapper(paste0("mean diff: 0.15 "), width=45)) +
  ylim(0,1) +
  # scale_colour_discrete(name=covariates[j]) +
  theme_bw() +
  theme(plot.tag.position = c(0.1,.99),
        axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust =.15, size = 12),
        legend.position = "none") +
  theme(plot.margin=margin(l=-0.4,unit="cm"))
mmr2

sub <- subset(subj, subj$stage == 3)
colourCount = length(unique(sub$indiator))
mmr3 <- ggplot(sub, aes(x=reorder(country,-diff), y=value)) +
  geom_line(aes(group=country))+
  geom_point(size=4, aes(color = indicator)) +
  labs(tag = "(C)",x= "", y = "") +
  ggtitle(wrapper(paste0("mean diff: 0.09"), width=45)) +
  ylim(0,1) +
#  scale_colour_discrete(name=covariates[j]) +
  theme_bw() +
  theme(plot.tag.position = c(0.1,.99),
        axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust =.15, size = 12),
        legend.position = "none") +
  theme(plot.margin=margin(l=-0.3,unit="cm"))
mmr3

sub <- subset(subj, subj$stage == 4)
colourCount = length(unique(sub$indicator))
mmr4 <- ggplot(sub, aes(x=reorder(country,-diff), y=value)) +
  geom_line(aes(group=country))+
  geom_point(size=4, aes(color = indicator)) +
  labs(tag = "(D)",x= "", y = "") +
  ggtitle(wrapper(paste0("mean diff: 0.08"), width=45)) +
  ylim(0,1) +
#  scale_colour_discrete(name=covariates[j]) +
  theme_bw() +
  theme(plot.tag.position = c(0.1,.99),
        axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = .15, size = 12),
        legend.position = "none") +
  theme(plot.margin=margin(l=-0.3,unit="cm"))
mmr4

sub$covariate <- "wealth quintile"


subj$indicator[subj$indicator=="ideliv"] <- "facility delivery"
subj$indicator[subj$indicator=="score"] <- "quality delivery"
sub <- subset(subj, subj$stage == 5)
colourCount = length(unique(sub$indicator))
mmr5 <- ggplot(sub, aes(x=reorder(country,-diff), y=value)) +
  geom_line(aes(group=country))+
  geom_point(size=4, aes(color = indicator)) +
  labs(tag = "(E)",x= "", y = "") +
  ggtitle(wrapper(paste0("mean diff: 0.1"), width=45)) +
  ylim(0,1) +
#  scale_colour_discrete(name=covariates[j]) +
  theme_bw() +
  theme(plot.tag.position = c(0.15,.99),
        axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = .75, size = 12)) +
  theme(plot.margin=margin(l=-0.3,unit="cm")) +
  guides(color = guide_legend(title = wrapper(paste0(""), width=10)))

mmr5


# legend <- get_legend(mmr1)

# and replot suppressing the legend
# mmr1 <- mmr1 + theme(legend.position='none')


# equity_facet <- ggdraw(plot_grid(plot_grid(mmr1, mmr2, mmr3, mmr4, mmr5, nrow=1),
#                                  plot_grid(NULL, legend_cascade, nrow=1),
#                                  rel_widths=c(1,.2)))
# 
# test <- plot_grid(mmr1,mmr2,mmr3,NULL,mmr4,mmr5,ncol=3,rel_widths = c(1,1,1,1,1))

mmr <- plot_grid(mmr1,mmr2,mmr3,mmr4,mmr5,ncol=5, align='h',rel_widths = c(.8,1.2,1.1,1,1.25)) +
  theme(plot.background = element_rect(fill = "white", colour = NA))
ggsave(plot=mmr, height = 7 , width = 13 , paste0("/Users/EWilson/Desktop/DAC/Delivery/Results/Figure7.png"))
