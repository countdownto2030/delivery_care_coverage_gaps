# last edited 29 Sep 2023
# last run 29 Sep 2023
# Objective: get Figure5

rm(list=ls())

########################################################### Load Libraries
library(ggplot2)
library(survey) #allows for design based analysis
library(RColorBrewer)
library(forcats)
library(tidyverse)
library(data.table)

library(stringr)
library(cowplot)
library(gridGraphics)
library(gridExtra)
'%!in%' <- function(x,y)!('%in%'(x,y))

# date = substr(date(),5,10)
date <- "Sep 29"
location <- "/Users/EWilson/Desktop/DAC/Delivery"
# setwd(location)

########################################################### GET DATA FILES
data_master <- read.csv(paste0(location,"/Results/",date,"_data_country_cascade.svy.csv"))

# read in obstetric transition values
MMR <- read.csv(paste0(location,"/ReadyforAnalysis_2022-10-26.csv"))
MMR <- MMR[,c("country","mmr")]
head(data_master)
head(MMR)

dim(data_master)
data_master <- merge(data_master,MMR,by=c("country"))
dim(data_master)

# add stage of obstetric transition
data_master$mmr <- as.numeric(data_master$mmr)

data_master$stage[data_master$mmr>=700]<-1
data_master$stage[data_master$mmr>=300 & data_master$mmr<700]<-2
data_master$stage[data_master$mmr>=100 & data_master$mmr<300]<-3
data_master$stage[data_master$mmr>=20 & data_master$mmr<100]<-4
data_master$stage[data_master$mmr<20]<-5



########################################################### COUNTRY CASCADE - FIGURE 5
# plot cascade by obstetric transition stage
data_master$mmr <- as.numeric(data_master$mmr)
data_master <- subset(data_master, !is.na(mmr))
data_master$country[data_master$country=="CentralAfricanRepublic"] <- "CAR"
data_master$country[data_master$country=="DominicanRepublic"] <- "DR"
data_master$country[data_master$country=="NorthMacedonia"] <- "N.Macedonia"

dim(data_master)

wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

# mycolors = c("gray","plum1","purple2","lightgreen","darkgreen")
mycolors = c("plum1","purple2","lightgreen","darkgreen")

cascades = list()

plot_titles <- c("(A) MMR: >=700","(B) MMR: 300-699","(C) MMR: 100-299"," (D) MMR: 20-99","(E) MMR: <20")

for(i in 1:length(sort(unique(data_master$stage)))){
  data <- subset(data_master, stage == sort(unique(data_master$stage))[i] )

  head(data)
  
  data$use.this[data$indicator=='hf'] <-1
  data$use.this[data$indicator=='hf_sba'] <-2
  data$use.this[data$indicator=='hf_sba_24hr'] <-3
  data$use.this[data$indicator=='hf_sba_24hr_check'] <-4
  
  data$indicator <- factor(data$indicator, levels = unique(data$indicator[order(data$use.this)]))
  
  head(data)
  
  if(i>1){ytitle = c("")
  }
  if(i==1){ytitle = c("proportion of sample population")}

  cascade <- ggplot(data, aes(x=reorder(country, value), y=value, fill=indicator)) +
    geom_bar(position="dodge", stat = "identity") +
    scale_y_continuous(limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1)) +
    ylim(0,1) +
    xlab("") +
    theme_bw() +
    ylab(ytitle) +
    ggtitle(plot_titles[i]) +
    labs(col="") +
    scale_fill_manual(values=mycolors) +
    scale_fill_manual(values=mycolors, labels=str_wrap(c("facility delivery","+ attendant","+ 24hr+ stay","+ health check within 2d"),width=20)) +
    scale_colour_manual(values=mycolors) +
    theme(axis.text.x = element_text(size = 6.5, angle = 90, vjust = 1, hjust=1),
          axis.title=element_text(size=9),
          plot.title = element_text(vjust = -1.5, size = 8.4),
          legend.text=element_text(size=6.5),
          legend.title=element_text(size=6.5)) +
    if(i==1){theme(plot.margin=margin(l=0.5,unit="cm"),
                   legend.position = "none")}
    else{theme(plot.margin=margin(l=-0.2,unit="cm"))} +
    if(i>1){theme(axis.text.y=element_blank(),
                   axis.ticks.y=element_blank())} +
    if(i<5){theme(legend.position = "none")}
  
  cascades[[i]] <- cascade
}

# cascades[[1]]
# 
# legend_cascade <- get_legend(cascades[[1]])

# and replot suppressing the legend
# cascade_1 <- cascades[[1]] + theme(legend.position='none')


# cascade_facet <- plot_grid(cascade_1,cascades[[2]],cascades[[3]],cascades[[4]],cascades[[5]],legend_cascade,nrow=1,align='hv',rel_widths = c(.7,1.2,1.2,1.2,.4,.4)) +
cascade_facet <- plot_grid(cascades[[1]],cascades[[2]],cascades[[3]],cascades[[4]],cascades[[5]],nrow=1,align='h',rel_widths = c(.5,1.3,1.1,1.3,1)) +
  theme(plot.background = element_rect(fill = "white", colour = NA))
ggsave(plot=cascade_facet, height = 5 , width = 13 , paste0("/Users/EWilson/Desktop/DAC/Delivery/Results/Figure5.png"))











