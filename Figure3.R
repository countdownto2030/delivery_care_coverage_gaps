# last edited 3 Jul 2024
# last run 3 Jul 2024
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
date <- "May 13"
location <- "/Users/EWilson/Desktop/DAC/Delivery"
# setwd(location)


########################################################### GET DATA FILES
# data_master <- read.csv(paste0(location,"/Results/Sep 29_pooled_data.svy.csv"))
data_master <- read.csv(paste0(location,"/Results/May 13_pooled_data.svy.csv"))
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

data_sums$value <- data_sums$value*100 

cascadeA <- ggplot(data_sums, aes(x = indicator, y= value, fill=indicator)) +
  geom_bar(position="dodge", stat = "identity") +
  geom_text(aes(label=round(value,0)), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(tag = "(A)") +
  scale_y_continuous(limits=c(0,100),breaks=c(0,20,40,60,80,100)) +
  xlab("") +
  ylab("percentage of sample population") +
  ggtitle(wrapper("All births", width=30)) +
  labs(col="") +
  theme_bw() +
  theme(text = element_text(size=13),
        plot.tag.position = c(0.20,.99),
        # axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of y ticks/text
        # axis.title.y=element_blank(), # remove x axis label
        # axis.text.x = element_text(size=9, vjust=.5),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.20, size = 13)) +
  scale_x_discrete(labels=str_wrap(mylabels,width=10))  +
  # scale_fill_manual(values=mycolors) +
  # scale_colour_manual(values=mycolors) +
  scale_fill_manual(values=mycolors, labels=str_wrap(c("poplulation","facility delivery","+ attendant","+ 24hr+ stay","+ health check within 2d"),width=20)) # +
  # theme(legend.position = "none")
cascadeA



# CASCADE B
data_sums <- subset(data_master, level=="faclevel1")

mycolors = c("plum1","purple2","lightgreen","darkgreen")
mylabels = c(paste0("facility"),paste0("skilled attendant"),paste0("24hr+ stay"),paste0("health check 2d"))

data_sums$value <- data_sums$value*100 

cascadeB <- ggplot(data_sums, aes(x = indicator, y= value, fill=indicator)) +
  geom_bar(position="dodge", stat = "identity") +
  geom_text(aes(label=round(value,0)), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(tag = "(B)") +
  scale_y_continuous(limits=c(0,100),breaks=c(0,20,40,60,80,100)) +
  xlab("") +
  ylab("") +
  ggtitle(wrapper("Lower-level facility births", width=30)) +
  labs(col="") +
  theme_bw() +
  theme(text = element_text(size=13),
        plot.tag.position = c(0.1,.99),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(), # get rid of y ticks/text
        axis.title.y=element_blank(), # remove x axis label
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 13)) +
  scale_x_discrete(labels=str_wrap(mylabels,width=10))  +
  scale_fill_manual(values=mycolors) +
  scale_colour_manual(values=mycolors) +
  theme(legend.position = "none")
cascadeB






# CASCADE C
data_sums <- subset(data_master, level=="faclevel2")

mycolors = c("plum1","purple2","lightgreen","darkgreen")
mylabels = c(paste0("facility"),paste0("skilled attendant"),paste0("24hr+ stay"),paste0("health check 2d"))

data_sums$value <- data_sums$value*100 

cascadeC <- ggplot(data_sums, aes(x = indicator, y= value, fill=indicator)) +
  geom_bar(position="dodge", stat = "identity") +
  geom_text(aes(label=round(value,0)), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(tag = "(C)") +
  scale_y_continuous(limits=c(0,100),breaks=c(0,20,40,60,80,100)) +
  xlab("") +
  ylab("") +
  ggtitle(wrapper("Hospital births", width=30)) +
  labs(col="") +
  scale_x_discrete(labels=str_wrap(mylabels,width=10))  +
  scale_fill_manual(values=mycolors) +
  scale_colour_manual(values=mycolors) +
  theme_bw() +
  theme(text = element_text(size=13),
        plot.tag.position = c(0.1,.99),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(), # get rid of y ticks/text
        axis.title.y=element_blank(), # remove x axis label
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.3, size = 13)) +
  theme(legend.position = "none")
cascadeC




legend_cascade <- get_legend(cascadeA)

# and replot suppressing the legend
cascade_A <- cascadeA + theme(legend.position='none')


cascade <- plot_grid(cascade_A,cascadeB,cascadeC,legend_cascade,ncol=4, align='h',rel_widths = c(1.2,1,1,.5)) +
  theme(plot.background = element_rect(fill = "white", colour = NA))
ggsave(plot=cascade, height = 7, width = 13, dpi = 300, paste0("/Users/EWilson/Desktop/DAC/Delivery/Results/Figure3.jpeg"))














