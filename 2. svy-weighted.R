# last edited 29 Sep 2023
# last run 29 Sep 2023
# Objective: get data files for survey-weighted and stratified results

rm(list=ls())
Start.time <- Sys.time()
########################################################### Load Libraries
library(ggplot2)
library(survey) #allows for design based analysis
library(RColorBrewer)
library(forcats)
library(tidyverse)
library(dplyr)

'%!in%' <- function(x,y)!('%in%'(x,y))

date = substr(date(),5,10)
date_ind = "Jul 26"
mics_date <- "Nov 29"
dhs_date <- "Nov 29"

location <- "/Users/EWilson/Desktop/DAC/Delivery"
setwd(location)

########################################################### GET DATA FILES
countrydata_dhs <- read.csv(paste0(location,"/Results/delivery_indicators.dhs_",dhs_date,".csv"),colClasses=c("v023"="character"))
countrydata_mics <- read.csv(paste0(location,"/Results/delivery_indicators.mics_",mics_date,".csv"))

names(countrydata_mics)[names(countrydata_mics)=="wmweight"] <- "v005"
names(countrydata_mics)[names(countrydata_mics)=="wb4"] <- "v012"
names(countrydata_mics)[names(countrydata_mics)=="hh7"] <- "v023"
countrydata_mics$v001 <- NA
countrydata_mics$midx <- NA

setdiff(colnames(countrydata_dhs), colnames(countrydata_mics))
setdiff(colnames(countrydata_mics), colnames(countrydata_dhs))
colnames(countrydata_mics)

countrydata <- rbind(countrydata_dhs,countrydata_mics)
data_master <- countrydata
sort(unique(data_master$country))
head(countrydata)

# check covariates
table(data_master$wealth, exclude=NULL)

data_master$age[data_master$age==0]  <- "20-49"
data_master$age[data_master$age==2] <- 1
data_master$age[data_master$age==1] <- "15-19"
table(data_master$age, exclude=NULL)

data_master$urban.rural[data_master$urban.rural==0]  <- "rural"
data_master$urban.rural[data_master$urban.rural==1]  <- "urban"
table(data_master$urban.rural, exclude=NULL)

data_master$education[data_master$education %in% c(0)] <- "none"
data_master$education[data_master$education %in% c(1)] <- "primary"
data_master$education[data_master$education %in% c(2)] <- "secondary+"
table(data_master$education, exclude=NULL)

data_master$marital.status[data_master$marital.status==0] <- "never married"
data_master$marital.status[data_master$marital.status==1] <- "married/partnered"
data_master$marital.status[data_master$marital.status==2] <- "widowed/divorced/separated"
table(data_master$marital.status, exclude=NULL)

data_master$first.time.mother[data_master$first.time.mother==0] <- "no"
data_master$first.time.mother[data_master$first.time.mother==1] <- "yes"
table(data_master$first.time.mother, exclude=NULL)

########################################################### INDIVIDUAL LEVEL
# parse facility level and score into variables for each level
data_master$faclevel0 <- 0
data_master$faclevel0[data_master$faclevel==0]<-1
data_master$faclevel1 <- 0
data_master$faclevel1[data_master$faclevel==1]<-1
data_master$faclevel2 <- 0
data_master$faclevel2[data_master$faclevel==2]<-1

data_master$score0 <- 0
data_master$score0[data_master$score==0]<-1
data_master$score1 <- 0
data_master$score1[data_master$score==1]<-1
data_master$score2 <- 0
data_master$score2[data_master$score==2]<-1
data_master$score3 <- 0
data_master$score3[data_master$score==3]<-1
data_master$score4 <- 0
data_master$score4[data_master$score==4]<-1
data_master$score5 <- 0
data_master$score5[data_master$score==5]<-1

# create composite coverage variable
table(data_master$faclevel, data_master$ideliv, exclude=NULL)
data_master$qcov<-0
data_master$qcov[data_master$ideliv==1 & data_master$sba==1 & data_master$ideliv24hr==1 & data_master$pncwm==1] <- 1
table(data_master$qcov, exclude=NULL)

dim(data_master)
data_master <- data_master %>% filter(country!='SouthAfrica' | v023 != 17) # Only 1 PSU in this cluster which is problem for svyby
data_master <- data_master %>% filter(country!='India' | v023 %!in% c(241,32221,3261,63221,8731,89821)) # Only 1 PSU in this cluster which is problem for svyby
dim(data_master)

# write.csv(data_master,paste0("/Users/EWilson/Desktop/DAC/Delivery/Results/",date,"_data.ind.csv"))

########################################################### SURVEY-WEIGHTED COUNTRY ESTIMATES 
############################# CASCADE INDICATORS
function(){
indicators <- c("hf","hf_sba","hf_sba_24hr","hf_sba_24hr_check")
indiclist = list()
datalist = list()

sort(unique(data_master$country))

data <- data_master
head(data)

data$hf <- 0
data$hf[data$ideliv==1] <- 1
table(data$hf, data$ideliv, exclude=NULL)

data$hf_sba <- 0
data$hf_sba[data$ideliv==1 & data$sba==1] <- 1
table(data$hf_sba, data$sba, exclude=NULL)
table(data$hf_sba, data$ideliv, exclude=NULL)

data$hf_sba_24hr <- 0
data$hf_sba_24hr[data$ideliv==1 & data$sba==1 & data$ideliv24hr==1] <- 1
table(data$hf_sba_24hr, data$ideliv24hr, exclude=NULL)
table(data$hf_sba_24hr, data$ideliv, exclude=NULL)
table(data$hf_sba_24hr, data$sba, exclude=NULL)

data$hf_sba_24hr_check <- 0
data$hf_sba_24hr_check[data$ideliv==1 & data$sba==1 & data$ideliv24hr==1 & data$pncwm==1] <- 1
table(data$hf_sba_24hr_check, data$pncwm, exclude=NULL)
table(data$hf_sba_24hr_check, data$ideliv, exclude=NULL)
table(data$hf_sba_24hr_check, data$sba, exclude=NULL)
table(data$hf_sba_24hr_check, data$ideliv24hr, exclude=NULL)


data_country_cascade <- data
data_country_cascade$sw <- data_country_cascade$v005/(10^6)
data_country_cascade$v001 <- 1

for(k in 1:length(sort(unique(data_country_cascade$country)))){
  data <- subset(data_country_cascade, country %in% sort(unique(data_country_cascade$country))[k])

  # WEIGHTED ESTIMATES
  svydata1 <- svydesign(id=~caseid,strata=~v023, data=data, weights=~sw, nest=TRUE)
  for(i in 1:length(indicators)){
    denom <- sum(table(data[[indicators[i]]]))
    indic <-svyby(make.formula(indicators[i]), ~v001 ,svydata1, svymean, na.rm=TRUE) 
    indic$n <- denom
    indic$CIL <- indic[,2] - 1.96*indic[,3]
    indic$CIU <- indic[,2] + 1.96*indic[,3]
    
    indic$indicator <- indicators[i]
    indic$country <- sort(unique(data_master$country))[k]
    names(indic)[names(indic) == indicators[i]] <- 'value'
    indic <- indic[,c("country","indicator","n","value","se","CIL","CIU")]
    
    print(sort(unique(data_master$country))[k])
    print(indic)
    indiclist[[i]] <- indic
  }
  
  datalist[[k]] <- do.call(rbind,indiclist)
  
}



svy_data = do.call(rbind, datalist)
svy_data
sort(unique(svy_data$country))
write.csv(svy_data,paste0("/Users/EWilson/Desktop/DAC/Delivery/Results/",date,"_data_country_cascade.svy.csv"), row.names = FALSE)

}
############################# STANDARD INDICATORS

indicators <- c('sba','ideliv','sbaORideliv','ideliv24hr','pncwm','faclevel','faclevel0','faclevel1','faclevel2','score','score0','score1','score2','score3','score4','score5','qcov')
indiclist = list()
datalist = list()

sort(unique(data_master$country))

##
#datbkup <- data_master
# data_master <- subset(datbkup,country=="India")
# dim(data_master)
##

for(k in 1:length(sort(unique(data_master$country)))){
  data <- subset(data_master, country %in% sort(unique(data_master$country))[k])
  data$sw <- data$v005/(10^6)
  data$v001 <- 1
  # table(data$v023, exclude=NULL)

# WEIGHTED ESTIMATES
svydata1 <- svydesign(id=~caseid,strata=~v023, data=data, weights=~sw, nest=TRUE)
for(i in 1:length(indicators)){
  denom <- sum(table(data[[indicators[i]]]))
  indic <-svyby(make.formula(indicators[i]), ~v001 ,svydata1, svymean, na.rm=TRUE) 
  indic$n <- denom
  indic$CIL <- indic[,2] - 1.96*indic[,3]
  indic$CIU <- indic[,2] + 1.96*indic[,3]
  
  indic$indicator <- indicators[i]
  indic$country <- sort(unique(data_master$country))[k]
  names(indic)[names(indic) == indicators[i]] <- 'value'
  indic <- indic[,c("country","indicator","n","value","se","CIL","CIU")]
  
  print(sort(unique(data_master$country))[k])
  print(indic)
  indiclist[[i]] <- indic
}

datalist[[k]] <- do.call(rbind,indiclist)

}

svy_data = do.call(rbind, datalist)
svy_data
sort(unique(svy_data$country))
write.csv(svy_data,paste0("/Users/EWilson/Desktop/DAC/Delivery/Results/",date,"_data.svy.csv"), row.names = FALSE)


########################################################### SURVEY-WEIGHTED COUNTRY ESTIMATES STRATIFIED
indicators <- c('score','qcov')
# covariates <- c('wealth','urban.rural','age','education','marital.status','first.time.mother','region')
covariates <- c('wealth')

indiclist = list()
covlist = list()
datalist = list()

sort(unique(data_master$country))

for(k in 1:length(sort(unique(data_master$country)))){
  data <- subset(data_master, country %in% sort(unique(data_master$country))[k])
  data$sw <- data$v005/(10^6)
  data$v001 <- 1

for(j in 1:length(covariates)){
  
  for(i in 1:length(indicators)){
    data$sw <- data$v005/(10^6)
    data$v001 <- 1
    svydata1 <- svydesign(id=~caseid,strata=~v023, data=data, weights=~sw) # including strata affects standard error only, not estimate
    denom <- sum(table(data[[indicators[i]]]))
    indic <-svyby(make.formula(indicators[i]), make.formula(covariates[j]), svydata1, svymean, na.rm=TRUE)
    indic$n <- denom
    indic$CIL <- indic[,2] - 1.96*indic[,3]
    indic$CIU <- indic[,2] + 1.96*indic[,3]
    
    indic$indicator <- indicators[i]
    indic$covariate <- covariates[j]
    indic$country <- sort(unique(data_master$country))[k]
    names(indic)[names(indic) == indicators[i]] <- 'value'
    names(indic)[names(indic) == covariates[j]] <- 'level'
    indic <- indic[,c("country","indicator","covariate","level","n","value","se","CIL","CIU")]
    
    print(indic)
    indiclist[[i]] <- indic
  }
  
  covlist[[j]] <- do.call(rbind,indiclist)
}

datalist[[k]] <- do.call(rbind,covlist)
}

strat_svy_data = do.call(rbind, datalist)
strat_svy_data
data1 <- strat_svy_data # Senegal to Zimbabwe
unique(strat_svy_data$country)

write.csv(strat_svy_data,paste0("/Users/EWilson/Desktop/DAC/Delivery/Results/",date,"_data.strat.svy.csv"), row.names = FALSE)












########################################################### SURVEY-WEIGHTED POOLED
data_master <- read.csv(paste0(location,"/Results/",date_ind,"_data.ind.csv"))
table(data_master$ideliv, data_master$faclevel, exclude=NULL)

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

data_master <- subset(data_master, !is.na(stage))
unique(data_master$country)
table(data_master$ideliv, exclude=NULL) 
# data_master <- subset(data_master, !is.na(ideliv))
data_master$pop <- 1

# count n for each country
sample_population_country <- data_master %>% group_by(country, stage) %>%
  summarize(n = n())
sample_population_country$num = seq(1, by = 1, length.out = nrow(sample_population_country))
head(sample_population_country) 

# count n for total sample
sample_population_total <- data_master %>% summarize(n = n())
head(sample_population_total)

data_cols <- data_master[,c("country","caseid","v001","v005","ideliv","sba","sbaORideliv","ideliv24hr","pncwm","faclevel","faclevel0","faclevel1","faclevel2","stage","score0","score1","score2","score3","score4","score5","score")]
all_data <- merge(data_cols,sample_population_country[,c("country","n","num")], by=c("country"))
names(all_data)[names(all_data)=="n"] <- "country_n"
all_data <- merge(all_data,sample_population_total[,c("n")])
names(all_data)[names(all_data)=="y"] <- "n"

# add weights inversely proportional to country size
all_data$prop_pooled <- all_data$country_n/all_data$n # each country's proportion of pooled sample
all_data$adj <- 1/all_data$prop_pooled     # inverse of proportion of pooled sample; adjustment to individual weights

all_data$sw <- all_data$v005/(10^6)
all_data$adj_sw <- all_data$adj*all_data$sw
all_data$v001 <- 1

all_data$pooled_strata <- paste0(all_data$num, all_data$v023)
all_data$pooled_caseid <- paste0(all_data$num, all_data$caseid)



#### POOLED ESTIMATES FOR CASCADES ###########

indiclist_all = list()
indiclist_faclevel1 = list()
indiclist_faclevel2 = list()
datalist = list()
indicators <- c("hf","hf_sba","hf_sba_24hr","hf_sba_24hr_check")

# CASCADE TOTAL
data <- all_data
head(data)

  data$hf <- 0
  data$hf[data$ideliv==1] <- 1
  table(data$hf, data$ideliv, exclude=NULL)
  
  data$hf_sba <- 0
  data$hf_sba[data$ideliv==1 & data$sba==1] <- 1
  table(data$hf_sba, data$sba, exclude=NULL)
  table(data$hf_sba, data$ideliv, exclude=NULL)
  
  data$hf_sba_24hr <- 0
  data$hf_sba_24hr[data$ideliv==1 & data$sba==1 & data$ideliv24hr==1] <- 1
  table(data$hf_sba_24hr, data$ideliv24hr, exclude=NULL)
  table(data$hf_sba_24hr, data$ideliv, exclude=NULL)
  table(data$hf_sba_24hr, data$sba, exclude=NULL)
  
  data$hf_sba_24hr_check <- 0
  data$hf_sba_24hr_check[data$ideliv==1 & data$sba==1 & data$ideliv24hr==1 & data$pncwm==1] <- 1
  table(data$hf_sba_24hr_check, data$pncwm, exclude=NULL)
  table(data$hf_sba_24hr_check, data$ideliv, exclude=NULL)
  table(data$hf_sba_24hr_check, data$sba, exclude=NULL)
  table(data$hf_sba_24hr_check, data$ideliv24hr, exclude=NULL)
  
  head(data)
  
svydata <- svydesign(id=~pooled_caseid,strata=~pooled_strata, data=data, weights=~adj_sw, nest=TRUE) # including strata affects standard error only, not estimate

for(i in 1:length(indicators)){
  denom <- sum(table(data[[indicators[i]]]))
  indic <-svyby(make.formula(indicators[i]), ~v001 ,svydata, svymean, na.rm=TRUE) 
  indic$n <- denom
  indic$CIL <- indic[,2] - 1.96*indic[,3]
  indic$CIU <- indic[,2] + 1.96*indic[,3]
  
  indic$indicator <- indicators[i]
  indic$level <- "faclevel 1 and 2"
  names(indic)[names(indic) == indicators[i]] <- 'value'
  indic <- indic[,c("indicator","n","level","value","se","CIL","CIU")]
  
  print(indic)
  indiclist_all[[i]] <- indic
}

pooled_svy_combined = do.call(rbind, indiclist_all)
pooled_svy_combined




# CASCADE LOWER-LEVEL FACILITY BIRTHS
data <- all_data
head(data)

# data$sub_pop[data$faclevel == 1] <- 1 

data$hf <- 0  
data$hf[data$faclevel==1] <- 1

data$hf_sba <- 0
data$hf_sba[data$faclevel==1 & data$sba==1] <- 1

data$hf_sba_24hr <- 0 
data$hf_sba_24hr[data$faclevel==1 & data$sba==1 & data$ideliv24hr==1] <- 1

data$hf_sba_24hr_check <- 0
data$hf_sba_24hr_check[data$faclevel==1 & data$sba==1 & data$ideliv24hr==1 & data$pncwm==1] <- 1

head(data)

svydata1 <- svydesign(id=~pooled_caseid,strata=~pooled_strata, data=data, weights=~adj_sw, nest=TRUE) # including strata affects standard error only, not estimate
  
for(i in 1:length(indicators)){
  denom <- sum(table(data[[indicators[i]]]))
  indic <-svyby(make.formula(indicators[i]), ~v001, svydata1, svymean, na.rm=TRUE) 
  indic$n <- denom
  indic$CIL <- indic[,2] - 1.96*indic[,3]
  indic$CIU <- indic[,2] + 1.96*indic[,3]
  
  indic$indicator <- indicators[i]
  indic$level <- "faclevel1"
  names(indic)[names(indic) == indicators[i]] <- 'value'
  indic <- indic[,c("indicator","level","n","value","se","CIL","CIU")]
  
  print(indic)
  indiclist_faclevel1[[i]] <- indic
}
  
pooled_svy_faclevel1 = do.call(rbind, indiclist_faclevel1)
pooled_svy_faclevel1




# CASCADE HOSPITAL BIRTHS
data <- all_data
head(data)

data$sub_pop[data$faclevel == 2] <- 1 

data$hf <- 0  
data$hf[data$faclevel==2] <- 1

data$hf_sba <- 0
data$hf_sba[data$faclevel==2 & data$sba==1] <- 1

data$hf_sba_24hr <- 0 
data$hf_sba_24hr[data$faclevel==2 & data$sba==1 & data$ideliv24hr==1] <- 1

data$hf_sba_24hr_check <- 0
data$hf_sba_24hr_check[data$faclevel==2 & data$sba==1 & data$ideliv24hr==1 & data$pncwm==1] <- 1

svydata2 <- svydesign(id=~pooled_caseid,strata=~pooled_strata, data=data, weights=~adj_sw, nest=TRUE) # including strata affects standard error only, not estimate

for(i in 1:length(indicators)){
  denom <- sum(table(data[[indicators[i]]]))
  indic <-svyby(make.formula(indicators[i]), ~v001, svydata2, svymean, na.rm=TRUE) 
  indic$n <- denom
  indic$CIL <- indic[,2] - 1.96*indic[,3]
  indic$CIU <- indic[,2] + 1.96*indic[,3]
  
  indic$indicator <- indicators[i]
  indic$level <- "faclevel2"
  names(indic)[names(indic) == indicators[i]] <- 'value'
  indic <- indic[,c("indicator","level","n","value","se","CIL","CIU")]
  
  print(indic)
  indiclist_faclevel2[[i]] <- indic
}

pooled_svy_faclevel2 = do.call(rbind, indiclist_faclevel2)
pooled_svy_faclevel2





###################################### FIGURE 4
indiclist_stage = list()
datalist = list()
indicators <- c("hf","hf_sba","hf_sba_24hr","hf_sba_24hr_check")

# MMR
data <- all_data
head(data)
dim(data)

data$hf <- 0
data$hf[data$ideliv==1] <- 1

data$hf_sba <- 0
data$hf_sba[data$ideliv==1 & data$sba==1] <- 1

data$hf_sba_24hr <- 0
data$hf_sba_24hr[data$ideliv==1 & data$sba==1 & data$ideliv24hr==1] <- 1

data$hf_sba_24hr_check <- 0
data$hf_sba_24hr_check[data$ideliv==1 & data$sba==1 & data$ideliv24hr==1 & data$pncwm==1] <- 1

for(k in 1:length(unique(data$stage))){
  data_sub <- subset(data, stage %in% k)
  
  svydata_sub <- svydesign(id=~pooled_caseid,strata=~pooled_strata, data=data_sub, weights=~adj_sw, nest=TRUE) 
  
  for(i in 1:length(indicators)){
    denom <- sum(table(data_sub[[indicators[i]]]))
    indic <-svyby(make.formula(indicators[i]), ~v001 ,svydata_sub, svymean, na.rm=TRUE) 
    indic$n <- denom
    indic$CIL <- indic[,2] - 1.96*indic[,3]
    indic$CIU <- indic[,2] + 1.96*indic[,3]
    
    indic$indicator <- indicators[i]
    names(indic)[names(indic) == indicators[i]] <- 'value'
    indic$level <- paste0("stage_",data_sub$stage[k])
    indic <- indic[,c("indicator","level","n","value","se","CIL","CIU")]
    
    print(indic)
    indiclist_stage[[i]] <- indic
  }
  datalist[[k]] <- do.call(rbind,indiclist_stage)
}

pooled_svy_stage = do.call(rbind, datalist)
pooled_svy_stage







pooled_svy_data <- rbind(pooled_svy_combined,pooled_svy_faclevel1,pooled_svy_faclevel2,pooled_svy_stage)


write.csv(pooled_svy_data,paste0("/Users/EWilson/Desktop/DAC/Delivery/Results/",date,"_pooled_data.svy.csv"), row.names = FALSE)











End.time <- Sys.time()
Run.time <- Start.time - End.time

Run.time






# MOVED FROM LINE 347

function(){
  
  
  # POOLED VALUE FOR EACH INDICATOR
  indiclist_std_all = list()
  datalist = list()
  indicators <- c("faclevel0","faclevel1","faclevel2","ideliv","sba","sbaORideliv","ideliv24hr","pncwm","score0","score1","score2","score3","score4","score5","score")
  
  # FIGURE 1 POOLED AVERAGE
  data <- all_data
  head(data)
  dim(data)
  
  svydata <- svydesign(id=~pooled_caseid,strata=~pooled_strata, data=data, weights=~adj_sw, nest=TRUE) # including strata affects standard error only, not estimate
  
  for(i in 1:length(indicators)){
    denom <- sum(table(data[[indicators[i]]]))
    indic <-svyby(make.formula(indicators[i]), ~v001 ,svydata, svymean, na.rm=TRUE) 
    indic$n <- denom
    indic$CIL <- indic[,2] - 1.96*indic[,3]
    indic$CIU <- indic[,2] + 1.96*indic[,3]
    
    indic$indicator <- indicators[i]
    indic$level <- "pooled standard"
    names(indic)[names(indic) == indicators[i]] <- 'value'
    indic <- indic[,c("indicator","n","level","value","se","CIL","CIU")]
    
    print(indic)
    indiclist_std_all[[i]] <- indic
  }
  
  pooled_standard_svy_combined = do.call(rbind, indiclist_std_all)
  pooled_standard_svy_combined
  write.csv(pooled_standard_svy_combined,paste0("/Users/EWilson/Desktop/DAC/Delivery/Results/",date,"_pooled_standard_data.svy.csv"), row.names = FALSE)
  
  
  ########## raw rates for pooled data - why are weighted lower-level and hospital numbers the same even though there are more hospital births by raw numbers
  fac0 <- subset(data, faclevel==0)
  fac1 <- subset(data, faclevel==1)
  fac2 <- subset(data, faclevel==2)
  summary(fac0$adj_sw)
  summary(fac1$adj_sw)
  summary(fac2$adj_sw)
  
  country_faclevel <- as.data.frame.matrix(table(data$country, data$faclevel))
  country_faclevel$country <- row.names(country_faclevel)
  all_data_collapse <- as.data.frame(all_data %>% group_by(country,prop_pooled) %>% summarise(n = n()))
  country_faclevel_prop <- merge(country_faclevel, all_data_collapse, by=c("country"))
  head(country_faclevel_prop)
  write.csv(country_faclevel_prop,"/Users/EWilson/Desktop/DAC/Delivery/Results/country_faclevel_prop.csv", row.names = FALSE)
  
  svydata <- svydesign(id=~pooled_caseid,strata=~pooled_strata, data=data, weights=~v001, nest=TRUE) # including strata affects standard error only, not estimate
  
  for(i in 1:length(indicators)){
    denom <- sum(table(data[[indicators[i]]]))
    indic <-svyby(make.formula(indicators[i]), ~v001 ,svydata, svymean, na.rm=TRUE) 
    indic$n <- denom
    indic$CIL <- indic[,2] - 1.96*indic[,3]
    indic$CIU <- indic[,2] + 1.96*indic[,3]
    
    indic$indicator <- indicators[i]
    indic$level <- "pooled standard"
    names(indic)[names(indic) == indicators[i]] <- 'value'
    indic <- indic[,c("indicator","n","level","value","se","CIL","CIU")]
    
    print(indic)
    indiclist_std_all[[i]] <- indic
  }
  pooled_standard_raw_combined = do.call(rbind, indiclist_std_all)
  write.csv(pooled_standard_raw_combined,paste0("/Users/EWilson/Desktop/DAC/Delivery/Results/",date,"_pooled_standard_data.raw.csv"), row.names = FALSE)
  
}











