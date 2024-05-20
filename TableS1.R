# last edited 14 May 2024
# last run 14 May 2024
# Objective: get tables for survey-weighted and stratified results

rm(list=ls())
Sys.time()

########################################################### Load Libraries
library(dplyr)
library(reshape2)
library(readr)

############### reorganize data for output
location <- "/Users/EWilson/Desktop/DAC/Delivery"
export_location = "/Users/EWilson/Desktop/DAC/Delivery/Results/"
date = substr(date(),5,10)
date = "May 13"

data <- read_csv(paste0(location,"/Results/",date,"_data.svy.csv"))

data <- subset(data, country!="Palestine")

colnames(data)
table(data$country,data$indicator)
unique(data$indicator)

class(data$country)
data$id <- paste0(data$indicator)

df.est <- dcast(data, country ~ id , value.var = "value", fun.aggregate = mean)
df.CIL<-dcast(data,country ~ id, value.var = c("CIL"), fun.aggregate = mean)
df.CIU<-dcast(data,country ~ id, value.var = c("CIU"), fun.aggregate = mean)

colnames(df.CIL) <- paste0(colnames(df.CIL),".CIL")
df.CIL$country <- df.CIL$country.CIL
df.CIL$country.CIL <- NULL

colnames(df.CIU) <- paste0(colnames(df.CIU),".CIU")
df.CIU$country <- df.CIU$country.CIU
df.CIU$country.CIU <- NULL

head(df.est)
head(df.CIL)
head(df.CIU)

df <- merge(df.CIL,df.CIU, by=("country"))
df <- merge(df,df.est, by=("country"))
df
colnames(df)
df<- df[,order(names(df))]
df <- df %>% select(country, everything())

colnames(df)
############# format sig digits
dim(df)
df <- cbind(df[,1],df[,2:ncol(df)])
colnames(df)[1] <-"country"

############# get median, min, max
medians <- as.data.frame(t(apply(df[,2:ncol(df)], 2, FUN = median, na.rm=TRUE)))
medians$country <- "Median"
mins <- as.data.frame(t(apply(df[,2:ncol(df)], 2, FUN = min, na.rm=TRUE))) 
mins$country <- "Min"
maximums <- as.data.frame(t(apply(df[,2:ncol(df)], 2, FUN = max, na.rm=TRUE))) 
maximums$country <- "Max"

summary <- rbind(medians, mins, maximums)
summary <- summary %>% select(country, everything())

df <- rbind(df,summary) 
colnames(df)
df <- df[,c("country",
            "faclevel0","faclevel0.CIL","faclevel0.CIU","faclevel1","faclevel1.CIL","faclevel1.CIU","faclevel2","faclevel2.CIL","faclevel2.CIU",
            "ideliv","ideliv.CIL","ideliv.CIU",
            "sba","sba.CIL","sba.CIU","sbaORideliv","sbaORideliv.CIL","sbaORideliv.CIU","ideliv24hr","ideliv24hr.CIL","ideliv24hr.CIU","pncwm","pncwm.CIL","pncwm.CIU",
            "score0","score0.CIL","score0.CIU","score1","score1.CIL","score1.CIU","score2","score2.CIL","score2.CIU",
            "score3","score3.CIL","score3.CIU","score4","score4.CIL","score4.CIU","score5","score5.CIL","score5.CIU",
            "faclevel","faclevel.CIL","faclevel.CIU",
            "score","score.CIL","score.CIU")]

tail(df)
dim(df)

df1 <- df[,1]
df2 <- 100*df[,2:(ncol(df)-6)]
df3 <- df[,(ncol(df)-5):ncol(df)]
df <- cbind(df1,df2,df3)
names(df)[names(df)=="df1"]<-"country"
head(df)
colnames(df)

############# get corresponding country for median, min, max
# min
countrylist <- list()
min1 <- c("Min country")
min2 <- summary[2,]

for(i in c(2:ncol(df))){

    country.min <- filter(df, df[,i]  == min(df[,i],na.rm=TRUE)) %>% select(country)
    
    country.min$country <- as.character(country.min$country)
    country.min <- subset(country.min, country != "Min")
    
    print(country.min)

    countrylist[[i]] <- as.vector(country.min$country)
    countrylist[[i]] <- paste (countrylist[[i]],sep="", collapse=", ")
}

master = do.call(cbind, countrylist)
min2 <- as.data.frame(cbind(min1,master))

names(min2)<-colnames(df)

# max
countrylist <- list()
max1 <- c("Max country")
max2 <- summary[3,]

for(i in c(2:ncol(df))){
  
  country.max <- filter(df, df[,i]  == max(df[,i],na.rm=TRUE)) %>% select(country)
  
  country.max$country <- as.character(country.max$country)
  country.max <- subset(country.max, country != "Max")
  
  print(country.max)
  
  countrylist[[i]] <- as.vector(country.max$country)
  countrylist[[i]] <- paste (countrylist[[i]],sep="", collapse=", ")
}

master = do.call(cbind, countrylist)
max2 <- as.data.frame(cbind(max1,master))

names(max2)<-colnames(df)

df2 <-round(df[,2:ncol(df)],1)

df <- cbind(df[,1],df2)
colnames(df)[1] <-"country"

df <- rbind(df,min2,max2)
df




############# format columns to be cleaner
df$`faclevel0 CI` <- paste0("[",df$`faclevel0.CIL`,", ",df$`faclevel0.CIU`,"]")
df$`faclevel0.CIL` <- NULL
df$`faclevel0.CIU` <- NULL
df$`faclevel1 CI` <- paste0("[",df$`faclevel1.CIL`,", ",df$`faclevel1.CIU`,"]")
df$`faclevel1.CIL` <- NULL
df$`faclevel1.CIU` <- NULL
df$`faclevel2 CI` <- paste0("[",df$`faclevel2.CIL`,", ",df$`faclevel2.CIU`,"]")
df$`faclevel2.CIL` <- NULL
df$`faclevel2.CIU` <- NULL
df$`ideliv CI` <- paste0("[",df$`ideliv.CIL`,", ",df$`ideliv.CIU`,"]")
df$`ideliv.CIL` <- NULL
df$`ideliv.CIU` <- NULL
df$`sba CI` <- paste0("[",df$`sba.CIL`,", ",df$`sba.CIU`,"]")
df$`sba.CIL` <- NULL
df$`sba.CIU` <- NULL
df$`sbaORideliv CI` <- paste0("[",df$`sbaORideliv.CIL`,", ",df$`sbaORideliv.CIU`,"]")
df$`sbaORideliv.CIL` <- NULL
df$`sbaORideliv.CIU` <- NULL
df$`ideliv24hr CI` <- paste0("[",df$`ideliv24hr.CIL`,", ",df$`ideliv24hr.CIU`,"]")
df$`ideliv24hr.CIL` <- NULL
df$`ideliv24hr.CIU` <- NULL
df$`pncwm CI` <- paste0("[",df$`pncwm.CIL`,", ",df$`pncwm.CIU`,"]")
df$`pncwm.CIL` <- NULL
df$`pncwm.CIU` <- NULL
df$`score0 CI` <- paste0("[",df$`score0.CIL`,", ",df$`score0.CIU`,"]")
df$`score0.CIL` <- NULL
df$`score0.CIU` <- NULL
df$`score1 CI` <- paste0("[",df$`score1.CIL`,", ",df$`score1.CIU`,"]")
df$`score1.CIL` <- NULL
df$`score1.CIU` <- NULL
df$`score2 CI` <- paste0("[",df$`score2.CIL`,", ",df$`score2.CIU`,"]")
df$`score2.CIL` <- NULL
df$`score2.CIU` <- NULL
df$`score3 CI` <- paste0("[",df$`score3.CIL`,", ",df$`score3.CIU`,"]")
df$`score3.CIL` <- NULL
df$`score3.CIU` <- NULL
df$`score4 CI` <- paste0("[",df$`score4.CIL`,", ",df$`score4.CIU`,"]")
df$`score4.CIL` <- NULL
df$`score4.CIU` <- NULL
df$`score5 CI` <- paste0("[",df$`score5.CIL`,", ",df$`score5.CIU`,"]")
df$`score5.CIL` <- NULL
df$`score5.CIU` <- NULL
df$`score CI` <- paste0("[",df$`score.CIL`,", ",df$`score.CIU`,"]")
df$`score.CIL` <- NULL
df$`score.CIU` <- NULL
df$`faclevel CI` <- paste0("[",df$`faclevel.CIL`,", ",df$`faclevel.CIU`,"]")
df$`faclevel.CIL` <- NULL
df$`faclevel.CIU` <- NULL



df <- df[,c("country",
            "faclevel0","faclevel0 CI","faclevel1","faclevel1 CI", "faclevel2","faclevel2 CI",
            "ideliv","ideliv CI",
            "sba","sba CI","sbaORideliv","sbaORideliv CI","ideliv24hr","ideliv24hr CI","pncwm","pncwm CI",
            "score0","score0 CI","score1","score1 CI", "score2","score2 CI","score3","score3 CI",
            "score4","score4 CI","score5","score5 CI",
            "score","score CI")]

df
odd_indexes<-seq(3,ncol(df),2)
df[(dim(df)[1]-1):dim(df)[1],odd_indexes] <- ""

df[ df == "NaN" ] <- "NA"
df[ df == "[NaN, NaN]" ] <- "[NA, NA]"

write.csv(df,"/Users/EWilson/Desktop/DAC/Delivery/Results/TableS1.csv",row.names = FALSE)

# df2 = subset(df, select = c(country, score,`score CI`) )
# write.csv(df2,"/Users/EWilson/Desktop/DAC/Delivery/Results/MultiCountryTable.abr.csv",row.names = FALSE)
# 



