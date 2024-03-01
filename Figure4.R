# last edited 16 Feb 2024
# last run 16 Feb 2024
# Objective: get Figure4

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
date <- "Sep 17"
location <- "/Users/EWilson/Desktop/DAC/Delivery"
# setwd(location)

########################################################### GET DATA FILES
# data_master <- read.csv(paste0(location,"/Results/Sep 29_pooled_data.svy.csv"))
data_master <- read.csv(paste0(location,"/Results/Mar  1_pooled_data.svy.csv"))

head(data_master)

data_sums <- subset(data_master, level %in% c("stage_1","stage_2","stage_3","stage_4","stage_5"))
############################################################ CURRENT VERSION (WEIGHTED)
cascades = list()

plot_titles <- c("(A) MMR: >=700","(B) MMR: 300-699","(C) MMR: 100-299"," (D) MMR: 20-99","(E) MMR: <20")

wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

mycolors = c("gray","plum1","purple2","lightgreen","darkgreen")
mylabels = c("population","facility","skilled attendant","24hr+ stay","health check 2d")

for(i in 1:length(sort(unique(data_sums$level)))){
  # i <- 1
  data <- subset(data_sums, level==sort(unique(data_sums$level))[i])

#  stage <- unique(data_sums$level)
#  data <- data[,c("caseid","pop","hf","hf_sba","hf_sba_24hr","hf_sba_24hr_check")]
#  data <- melt(setDT(data), id.vars = c("caseid"), variable.name = "indicator")
#  head(data)
  
  
  add_row <- data.frame(indicator=c("pop"),
                        n=c(332447),
                        level=data$level[1],
                        value=1.0,
                        se= NA,
                        CIL = NA,
                        CIU = NA)  
  add_row
  data <- rbind(add_row,data)
  print(data)

  data$use.this[data$indicator=='pop'] <-1
  data$use.this[data$indicator=='hf'] <-2
  data$use.this[data$indicator=='hf_sba'] <-3
  data$use.this[data$indicator=='hf_sba_24hr'] <-4
  data$use.this[data$indicator=='hf_sba_24hr_check'] <-5
  
  data$indicator <- factor(data$indicator, levels = unique(data$indicator[order(data$use.this)]))
  
  head(data)
  
  if(i>1){
    data <- subset(data, indicator != "pop") 
    mycolors = c("plum1","purple2","lightgreen","darkgreen")
    ytitle = c("")
  }
  if(i==1){mycolors = c("grey","plum1","purple2","lightgreen","darkgreen") 
  ytitle = c("percentage of sample population")}
  print(data)
  print(mycolors)
  data$value <- data$value*100
  cascade <- ggplot(data, aes(x = reorder(indicator, -value),y= value, fill=indicator)) +
    geom_bar(position="dodge", stat = "identity") +
    # geom_text(aes(label=round(value,2)), position=position_dodge(width=0.9), vjust=-0.25) +
    geom_text(aes(label=round(value,0)), position=position_dodge(width=0.9), vjust=-0.25) +
    # labs(tag = labels[i]) +
    scale_y_continuous(limits=c(0,100),breaks=c(0,20,40,60,80,100)) +
    xlab("") +
    ylab(ytitle) +
    ggtitle(plot_titles[i]) +
    labs(col="") +
    scale_fill_manual(values=mycolors) +
    scale_fill_manual(values=mycolors, labels=str_wrap(c("poplulation","facility delivery","+ attendant","+ 24hr+ stay","+ health check within 2d"),width=20)) +
    scale_colour_manual(values=mycolors) +
    theme_bw() +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y = element_text(size=12),
          plot.title = element_text(hjust = 0.5, size = 14),
          axis.title.y=element_text(size = 16)) +
    if(i!=1){theme(axis.text.y=element_blank(),
                       axis.ticks.y=element_blank(),
                       legend.position = "none")}
  
  cascades[[i]] <- cascade
}


cascades[[1]]

legend_cascade <- get_legend(cascades[[1]])

# and replot suppressing the legend
cascade_1 <- cascades[[1]] + theme(legend.position='none')

cascade_facet <- plot_grid(cascade_1,cascades[[2]],cascades[[3]],cascades[[4]],cascades[[5]],legend_cascade,nrow=1,rel_widths = c(1.2,1,1,1,1,.8)) +
  theme(plot.background = element_rect(fill = "white", colour = NA))
ggsave(plot=cascade_facet, height = 7 , width = 13 , paste0("/Users/EWilson/Desktop/DAC/Delivery/Results/Figure4.png"))


































