# last edited 29 Sep 2023
# last run 29 Sep 2023
# Objective: get Figure3

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
date <- "Aug 14"
location <- "/Users/EWilson/Desktop/DAC/Delivery"
# setwd(location)


########################################################### GET DATA FILES
data_master <- read.csv(paste0(location,"/Results/Sep 29_pooled_data.svy.csv"))
head(data_master)

############################################################ CURRENT VERSION (WEIGHTED)
# CASCADE A
data_sums <- subset(data_master, level=="faclevel 1 and 2")
add_row <- data.frame(indicator=c("pop"),
                      n=c(332447),
                      level=c("faclevel 1 and 2"),
                      value=1.0,
                      se= NA,
                      CIL = NA,
                      CIU = NA)  

data_sums <- rbind(add_row,data_sums)

data_sums$use.this[data_sums$indicator=='pop'] <-1
data_sums$use.this[data_sums$indicator=='hf'] <-2
data_sums$use.this[data_sums$indicator=='hf_sba'] <-3
data_sums$use.this[data_sums$indicator=='hf_sba_24hr'] <-4
data_sums$use.this[data_sums$indicator=='hf_sba_24hr_check'] <-5

data_sums$indicator <- factor(data_sums$indicator, levels = unique(data_sums$indicator[order(data_sums$use.this)]))


wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

mycolors = c("gray","plum1","purple2","lightgreen","darkgreen")
mylabels = c("population","facility","skilled attendant","24hr+ stay","health check 2d")

cascadeA <- ggplot(data_sums, aes(x = indicator, y= value, fill=indicator)) +
  geom_bar(position="dodge", stat = "identity") +
  geom_text(aes(label=round(value,2)), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(tag = "(A)") +
  scale_y_continuous(limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1)) +
  xlab("") +
  ylab("proportion of sample population") +
  ggtitle(wrapper("Delivery care cascade", width=30)) +
  labs(col="") +
  theme_bw() +
  theme(text = element_text(size=15),
        plot.tag.position = c(0.20,.99),
        # axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of y ticks/text
        # axis.title.y=element_blank(), # remove x axis label
        axis.text.x = element_text(size=9, vjust=.5),
        plot.title = element_text(hjust = 0.5, size = 15)) +
  scale_x_discrete(labels=str_wrap(mylabels,width=10))  +
  scale_fill_manual(values=mycolors) +
  scale_colour_manual(values=mycolors) +
  theme(legend.position = "none")
cascadeA



# CASCADE B
data_sums <- subset(data_master, level=="faclevel1")

mycolors = c("plum1","purple2","lightgreen","darkgreen")
mylabels = c(paste0("facility"),paste0("skilled attendant"),paste0("24hr+ stay"),paste0("health check 2d"))

cascadeB <- ggplot(data_sums, aes(x = indicator, y= value, fill=indicator)) +
  geom_bar(position="dodge", stat = "identity") +
  geom_text(aes(label=round(value,2)), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(tag = "(B)") +
  scale_y_continuous(limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1)) +
  xlab("") +
  ylab("") +
  ggtitle(wrapper("Lower-level facility births", width=30)) +
  labs(col="") +
  theme_bw() +
  theme(text = element_text(size=15),
        plot.tag.position = c(0.1,.99),
        axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of y ticks/text
        axis.title.y=element_blank(), # remove x axis label
        axis.text.x = element_text(size=9, vjust=.5),
        plot.title = element_text(hjust = 0.75, size = 15)) +
  scale_x_discrete(labels=str_wrap(mylabels,width=10))  +
  scale_fill_manual(values=mycolors) +
  scale_colour_manual(values=mycolors) +
  theme(legend.position = "none")
cascadeB






# CASCADE C
data_sums <- subset(data_master, level=="faclevel2")

mycolors = c("plum1","purple2","lightgreen","darkgreen")
mylabels = c(paste0("facility"),paste0("skilled attendant"),paste0("24hr+ stay"),paste0("health check 2d"))

cascadeC <- ggplot(data_sums, aes(x = indicator, y= value, fill=indicator)) +
  geom_bar(position="dodge", stat = "identity") +
  geom_text(aes(label=round(value,2)), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(tag = "(C)") +
  scale_y_continuous(limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1)) +
  xlab("") +
  ylab("") +
  ggtitle(wrapper("Hospital births", width=30)) +
  labs(col="") +
  scale_x_discrete(labels=str_wrap(mylabels,width=10))  +
  scale_fill_manual(values=mycolors) +
  scale_colour_manual(values=mycolors) +
  theme_bw() +
  theme(text = element_text(size=15),
        plot.tag.position = c(0.1,.99),
        axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of y ticks/text
        axis.title.y=element_blank(), # remove x axis label
        axis.text.x = element_text(size=9, vjust=.5),
        plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.position = "none")
cascadeC


cascade <- plot_grid(cascadeA,cascadeB,cascadeC,ncol=3, align='h',rel_widths = c(1.2,1,1)) +
  theme(plot.background = element_rect(fill = "white", colour = NA))
ggsave(plot=cascade, height = 7 , width = 10 , paste0("/Users/EWilson/Desktop/DAC/Delivery/Results/Figure3.png"))














