# Set Library
library("tidyverse")
library("plyr")
library("dplyr")
library("moments")
library("csv")
library("csvread")
library("readr")
library("rlist")
library("lubridate")

# Set Data
data<-read_csv("owid-covid-data.csv")     # data
iso_code<-data[["iso_code"]]              # iso_code
continent<-data[["continent"]]            # continent
location<-data[["location"]]              # location
date<-data[["date"]]                      # date
new_cases<-data[["new_cases"]]            # new_cases
new_deaths<-data[["new_deaths"]]          # new_deaths
national<-c("Andorra", "Slovenia", "United Kingdom")
c1_data<-subset(data, location == national[1] & new_cases >= 0 & new_deaths >= 0)
c2_data<-subset(data, location == national[2] & new_cases >= 0 & new_deaths >= 0)
c3_data<-subset(data, location == national[3] & new_cases >= 0 & new_deaths >= 0)
# Sorting needed countries (by date)
AND_data <- (data %>% filter_all(any_vars(. %in% c("Andorra"))))
SVN_data <- (data %>% filter_all(any_vars(. %in% c("Slovenia"))))
GBR_data <- (data %>% filter_all(any_vars(. %in% c("United Kingdom"))))

#1
AND_length<-length(AND_data$date)
SVN_length<-length(SVN_data$date)
GBR_length<-length(GBR_data$date)
AND_data_avr<-AND_data
SVN_data_avr<-SVN_data
GBR_data_avr<-GBR_data
AND_data$new_cases[is.na(AND_data$new_cases)]=0
AND_data$new_deaths[is.na(AND_data$new_deaths)]=0
SVN_data$new_cases[is.na(SVN_data$new_cases)]=0
SVN_data$new_deaths[is.na(SVN_data$new_deaths)]=0
GBR_data$new_cases[is.na(GBR_data$new_cases)]=0
GBR_data$new_deaths[is.na(GBR_data$new_deaths)]=0
for (i in 1: AND_length){
  if (i<7){
    s<-0
    ss<-0
    for (j in 1:i){
      s<-s+AND_data$new_cases[j]
      ss<-ss+AND_data$new_deaths[j]
    }
    AND_data_avr$new_cases[i]<-s/i
    AND_data_avr$new_deaths[i]<-ss/i
  }
  else{
    s<-0
    ss<-0
    ii<-i-6
    for (j in ii:i){
      s<-s+AND_data$new_cases[j]
      ss<-ss+AND_data$new_deaths[j]
    }
    AND_data_avr$new_cases[i]<-s/7
    AND_data_avr$new_deaths[i]<-ss/7
  }
}
for (i in 1: SVN_length){
  if (i<7){
    s<-0
    ss<-0
    for (j in 1:i){
      s<-s+SVN_data$new_cases[j]
      ss<-ss+SVN_data$new_deaths[j]
    }
    SVN_data_avr$new_cases[i]<-s/i
    SVN_data_avr$new_deaths[i]<-ss/i
  }
  else{
    s<-0
    ss<-0
    ii<-i-6
    for (j in ii:i){
      s<-s+SVN_data$new_cases[j]
      ss<-ss+SVN_data$new_deaths[j]
    }
    SVN_data_avr$new_cases[i]<-s/7
    SVN_data_avr$new_deaths[i]<-ss/7
  }
}
for (i in 1: GBR_length){
  if (i<7){
    s<-0
    ss<-0
    for (j in 1:i){
      s<-s+GBR_data$new_cases[j]
      ss<-ss+GBR_data$new_deaths[j]
    }
    GBR_data_avr$new_cases[i]<-s/i
    GBR_data_avr$new_deaths[i]<-ss/i
  }
  else{
    s<-0
    ss<-0
    ii<-i-6
    for (j in ii:i){
      s<-s+GBR_data$new_cases[j]
      ss<-ss+GBR_data$new_deaths[j]
    }
    GBR_data_avr$new_cases[i]<-s/7
    GBR_data_avr$new_deaths[i]<-ss/7
  }
}
AND_data_avr$date<-as.character(AND_data_avr$date,"%m/%d/%Y")
AND_data_avr$date<-as.Date(AND_data_avr$date,"%m/%d/%Y")
AND_loc<-subset(AND_data_avr,month(AND_data_avr$date)==1|month(AND_data_avr$date)==2|month(AND_data_avr$date)==4|month(AND_data_avr$date)==7)
AND_pos<-which(year(AND_loc$date)==2021)
AND_pos2021<-AND_pos[1]
AND_pos<-which(year(AND_loc$date)==2022)
AND_pos2022<-AND_pos[1]
barplot(AND_loc$new_cases[1:AND_pos2021-1],
        xlab="Days",
        ylab="New Cases",
        main="New Cases Andorra in 2020",
        names.arg=AND_loc$date[1:AND_pos2021-1],
        col="blue")
barplot(AND_loc$new_cases[62:181],
        xlab="Days",
        ylab="New Cases",
        main="New Cases Andorra in 2021",
        names.arg=AND_loc$date[62:181],
        col="blue")
barplot(AND_loc$new_cases[AND_pos2022:length(AND_loc$date)],
        xlab="Days",
        ylab="New Cases",
        main="New Cases Andorra in 2022",
        names.arg=AND_loc$date[AND_pos2022:length(AND_loc$date)],
        col="blue")
SVN_data_avr$date<-as.character(SVN_data_avr$date,"%m/%d/%Y")
SVN_data_avr$date<-as.Date(SVN_data_avr$date,"%m/%d/%Y")
SVN_loc<-subset(SVN_data_avr,month(SVN_data_avr$date)==1|month(SVN_data_avr$date)==2|month(SVN_data_avr$date)==4|month(SVN_data_avr$date)==7)
SVN_pos<-which(year(SVN_loc$date)==2021)
SVN_pos2021<-SVN_pos[1]
SVN_pos<-which(year(SVN_loc$date)==2022)
SVN_pos2022<-SVN_pos[1]
barplot(SVN_loc$new_cases[1:SVN_pos2021-1],
        xlab="Days",
        ylab="New Cases",
        main="New Cases Slovenia in 2020",
        names.arg=SVN_loc$date[1:SVN_pos2021-1],
        col="blue")
barplot(SVN_loc$new_cases[90:209],
        xlab="Days",
        ylab="New Cases",
        main="New Cases Slovenia in 2021",
        names.arg=SVN_loc$date[90:209],
        col="blue")
barplot(SVN_loc$new_cases[SVN_pos2022:length(SVN_loc$date)],
        xlab="Days",
        ylab="New Cases",
        main="New Cases Slovenia in 2022",
        names.arg=SVN_loc$date[SVN_pos2022:length(SVN_loc$date)],
        col="blue")
GBR_data_avr$date<-as.character(GBR_data_avr$date,"%m/%d/%Y")
GBR_data_avr$date<-as.Date(GBR_data_avr$date,"%m/%d/%Y")
GBR_loc<-subset(GBR_data_avr,month(GBR_data_avr$date)==1|month(GBR_data_avr$date)==2|month(GBR_data_avr$date)==4|month(GBR_data_avr$date)==7)
GBR_pos<-which(year(GBR_loc$date)==2021)
GBR_pos2021<-GBR_pos[1]
GBR_pos<-which(year(GBR_loc$date)==2022)
GBR_pos2022<-GBR_pos[1]
barplot(GBR_loc$new_cases[1:GBR_pos2021-1],
        xlab="Days",
        ylab="New Cases",
        main="New Cases United Kingdom in 2020",
        names.arg=GBR_loc$date[1:GBR_pos2021-1],
        col="blue")
barplot(GBR_loc$new_cases[92:211],
        xlab="Days",
        ylab="New Cases",
        main="New Cases United Kingdom in 2021",
        names.arg=GBR_loc$date[92:211],
        col="blue")
barplot(GBR_loc$new_cases[GBR_pos2022:length(GBR_loc$date)],
        xlab="Days",
        ylab="New Cases",
        main="New Cases United Kingdom in 2022",
        names.arg=GBR_loc$date[GBR_pos2022:length(GBR_loc$date)],
        col="blue")

#2
barplot(AND_loc$new_deaths[1:AND_pos2021-1],
        xlab="Days",
        ylab="New Deaths",
        main="New Deaths Andorra in 2020",
        names.arg=AND_loc$date[1:AND_pos2021-1],
        col="blue")
barplot(AND_loc$new_deaths[62:181],
        xlab="Days",
        ylab="New Deaths",
        main="New Deaths Andorra in 2021",
        names.arg=AND_loc$date[62:181],
        col="blue")
barplot(AND_loc$new_deaths[AND_pos2022:length(AND_loc$date)],
        xlab="Days",
        ylab="New Deaths",
        main="New Deaths Andorra in 2022",
        names.arg=AND_loc$date[AND_pos2022:length(AND_loc$date)],
        col="blue")
barplot(SVN_loc$new_deaths[1:SVN_pos2021-1],
        xlab="Days",
        ylab="New Deaths",
        main="New Deaths Slovenia in 2020",
        names.arg=SVN_loc$date[1:SVN_pos2021-1],
        col="blue")
barplot(SVN_loc$new_deaths[90:209],
        xlab="Days",
        ylab="New Deaths",
        main="New Deaths Slovenia in 2021",
        names.arg=SVN_loc$date[90:209],
        col="blue")
barplot(SVN_loc$new_deaths[SVN_pos2022:length(SVN_loc$date)],
        xlab="Days",
        ylab="New Deaths",
        main="New Deaths Slovenia in 2022",
        names.arg=SVN_loc$date[SVN_pos2022:length(SVN_loc$date)],
        col="blue")
barplot(GBR_loc$new_deaths[1:GBR_pos2021-1],
        xlab="Days",
        ylab="New Deaths",
        main="New Deaths United Kingdom in 2020",
        names.arg=GBR_loc$date[1:GBR_pos2021-1],
        col="blue")
barplot(GBR_loc$new_deaths[92:211],
        xlab="Days",
        ylab="New Deaths",
        main="New Deaths United Kingdom in 2021",
        names.arg=GBR_loc$date[92:211],
        col="blue")
barplot(GBR_loc$new_deaths[GBR_pos2022:length(GBR_loc$date)],
        xlab="Days",
        ylab="New Deaths",
        main="New Deaths United Kingdom in 2022",
        names.arg=GBR_loc$date[GBR_pos2022:length(GBR_loc$date)],
        col="blue")

#3
AND_loc_cases_deaths_2020<-rbind(AND_loc$new_cases[1:AND_pos2021-1],AND_loc$new_deaths[1:AND_pos2021-1])
row.names(AND_loc_cases_deaths_2020)<-c("new cases","new deaths")
colnames(AND_loc_cases_deaths_2020)<-as.character(AND_loc$date[1:AND_pos2021-1])
barplot(AND_loc_cases_deaths_2020,main="New Cases and New Deaths Andorra 2020",
        beside=TRUE,col=c(4,2),legend=TRUE,ylim=c(0,30),las=1)
AND_loc_cases_deaths_2021<-rbind(AND_loc$new_cases[62:181],AND_loc$new_deaths[62:181])
row.names(AND_loc_cases_deaths_2021)<-c("new cases","new deaths")
colnames(AND_loc_cases_deaths_2021)<-as.character(AND_loc$date[62:181])
barplot(AND_loc_cases_deaths_2021,main="New Cases and New Deaths Andorra 2021",
        beside=TRUE,col=c(4,2),legend=TRUE,ylim=c(0,80),las=1)
AND_loc_cases_deaths_2022<-rbind(AND_loc$new_cases[AND_pos2022:length(AND_loc$date)],AND_loc$new_deaths[AND_pos2022:length(AND_loc$date)])
row.names(AND_loc_cases_deaths_2022)<-c("new cases","new deaths")
colnames(AND_loc_cases_deaths_2022)<-as.character(AND_loc$date[AND_pos2022:length(AND_loc$date)])
barplot(AND_loc_cases_deaths_2022,main="New Cases and New Deaths Andorra 2022",
        beside=TRUE,col=c(4,2),legend=TRUE,ylim=c(0,800),las=1)


SVN_loc_cases_deaths_2020<-rbind(SVN_loc$new_cases[1:SVN_pos2021-1],SVN_loc$new_deaths[1:SVN_pos2021-1])
row.names(SVN_loc_cases_deaths_2020)<-c("new cases","new deaths")
colnames(SVN_loc_cases_deaths_2020)<-as.character(SVN_loc$date[1:SVN_pos2021-1])
barplot(SVN_loc_cases_deaths_2020,main="New Cases and New Deaths Slovenia 2020",
        beside=TRUE,col=c(4,2),legend=TRUE,ylim=c(0,50),las=1)
SVN_loc_cases_deaths_2021<-rbind(SVN_loc$new_cases[90:209],SVN_loc$new_deaths[90:209])
row.names(SVN_loc_cases_deaths_2021)<-c("new cases","new deaths")
colnames(SVN_loc_cases_deaths_2021)<-as.character(SVN_loc$date[90:209])
barplot(SVN_loc_cases_deaths_2021,main="New Cases and New Deaths Slovenia 2021",
        beside=TRUE,col=c(4,2),legend=TRUE,ylim=c(0,2500),las=1)
SVN_loc_cases_deaths_2022<-rbind(SVN_loc$new_cases[SVN_pos2022:length(SVN_loc$date)],SVN_loc$new_deaths[SVN_pos2022:length(SVN_loc$date)])
row.names(SVN_loc_cases_deaths_2022)<-c("new cases","new deaths")
colnames(SVN_loc_cases_deaths_2022)<-as.character(SVN_loc$date[SVN_pos2022:length(SVN_loc$date)])
barplot(SVN_loc_cases_deaths_2022,main="New Cases and New Deaths Slovenia 2022",
        beside=TRUE,col=c(4,2),legend=TRUE,ylim=c(0,16000),las=1)


GBR_loc_cases_deaths_2020<-rbind(GBR_loc$new_cases[1:GBR_pos2021-1],GBR_loc$new_deaths[1:GBR_pos2021-1])
row.names(GBR_loc_cases_deaths_2020)<-c("new cases","new deaths")
colnames(GBR_loc_cases_deaths_2020)<-as.character(GBR_loc$date[1:GBR_pos2021-1])
barplot(GBR_loc_cases_deaths_2020,main="New Cases and New Deaths United Kingdom 2020",
        beside=TRUE,col=c(4,2),legend=TRUE,ylim=c(0,5000),las=1)
GBR_loc_cases_deaths_2021<-rbind(GBR_loc$new_cases[92:211],GBR_loc$new_deaths[92:211])
row.names(GBR_loc_cases_deaths_2021)<-c("new cases","new deaths")
colnames(GBR_loc_cases_deaths_2021)<-as.character(GBR_loc$date[92:211])
barplot(GBR_loc_cases_deaths_2021,main="New Cases and New Deaths United Kingdom 2021",
        beside=TRUE,col=c(4,2),legend=TRUE,ylim=c(0,60000),las=1)
GBR_loc_cases_deaths_2022<-rbind(GBR_loc$new_cases[GBR_pos2022:length(GBR_loc$date)],GBR_loc$new_deaths[GBR_pos2022:length(GBR_loc$date)])
row.names(GBR_loc_cases_deaths_2022)<-c("new cases","new deaths")
colnames(GBR_loc_cases_deaths_2022)<-as.character(GBR_loc$date[GBR_pos2022:length(GBR_loc$date)])
barplot(GBR_loc_cases_deaths_2022,main="New Cases and New Deaths United Kingdom 2022",
        beside=TRUE,col=c(4,2),legend=TRUE,ylim=c(0,200000),las=1)

#4
AND_loc2months<-subset(AND_data_avr,month(AND_data_avr$date)==11|month(AND_data_avr$date)==12)
SVN_loc2months<-subset(SVN_data_avr,month(SVN_data_avr$date)==11|month(SVN_data_avr$date)==12)
GBR_loc2months<-subset(GBR_data_avr,month(GBR_data_avr$date)==11|month(GBR_data_avr$date)==12)
barplot(AND_loc2months$new_cases[1:61],xlab="Days",ylab="New Cases",
        main="New Cases Andorra in the last 2 months of 2020",
        names.arg=AND_loc2months$date[1:61],col="blue")
barplot(AND_loc2months$new_cases[62:122],xlab="Days",ylab="New Cases",
        main="New Cases Andorra in the last 2 months of 2021",
        names.arg=AND_loc2months$date[62:122],col="blue")

barplot(SVN_loc2months$new_cases[1:61],xlab="Days",ylab="New Cases",
        main="New Cases Slovenia in the last 2 months of 2020",
        names.arg=SVN_loc2months$date[1:61],col="blue")
barplot(SVN_loc2months$new_cases[62:122],xlab="Days",ylab="New Cases",
        main="New Cases Slovenia in the last 2 months of 2021",
        names.arg=SVN_loc2months$date[62:122],col="blue")

barplot(GBR_loc2months$new_cases[1:61],xlab="Days",ylab="New Cases",
        main="New Cases United Kingdom in the last 2 months of 2020",
        names.arg=GBR_loc2months$date[1:61],col="blue")
barplot(GBR_loc2months$new_cases[62:122],xlab="Days",ylab="New Cases",
        main="New Cases United Kingdom in the last 2 months of 2021",
        names.arg=GBR_loc2months$date[62:122],col="blue")

#5
barplot(AND_loc2months$new_deaths[1:61],xlab="Days",ylab="New Deaths",
        main="New Deaths Andorra in the last 2 months of 2020",
        names.arg=AND_loc2months$date[1:61],col="blue")
barplot(AND_loc2months$new_deaths[62:122],xlab="Days",ylab="New Deaths",
        main="New Deaths Andorra in the last 2 months of 2021",
        names.arg=AND_loc2months$date[62:122],col="blue")

barplot(SVN_loc2months$new_deaths[1:61],xlab="Days",ylab="New Deaths",
        main="New Deaths Slovenia in the last 2 months of 2020",
        names.arg=SVN_loc2months$date[1:61],col="blue")
barplot(SVN_loc2months$new_deaths[62:122],xlab="Days",ylab="New Deaths",
        main="New Deaths Slovenia in the last 2 months of 2021",
        names.arg=SVN_loc2months$date[62:122],col="blue")

barplot(GBR_loc2months$new_deaths[1:61],xlab="Days",ylab="New Deaths",
        main="New Deaths United Kingdom in the last 2 months of 2020",
        names.arg=GBR_loc2months$date[1:61],col="blue")
barplot(GBR_loc2months$new_deaths[62:122],xlab="Days",ylab="New Deaths",
        main="New Deaths United Kingdom in the last 2 months of 2021",
        names.arg=GBR_loc2months$date[62:122],col="blue")

#6
AND_loc2months_cases_deaths_2020<-rbind(AND_loc2months$new_cases[1:61],AND_loc2months$new_deaths[1:61])
row.names(AND_loc2months_cases_deaths_2020)<-c("new cases","new deaths")
colnames(AND_loc2months_cases_deaths_2020)<-as.character(AND_loc2months$date[1:61])
barplot(AND_loc2months_cases_deaths_2020,
        main="New Cases and New Deaths Andorra in the last 2 months of 2020",
        beside=TRUE,col=c(4,2),legend=TRUE,las=1)
AND_loc2months_cases_deaths_2021<-rbind(AND_loc2months$new_cases[62:122],AND_loc2months$new_deaths[62:122])
row.names(AND_loc2months_cases_deaths_2021)<-c("new cases","new deaths")
colnames(AND_loc2months_cases_deaths_2021)<-as.character(AND_loc2months$date[62:122])
barplot(AND_loc2months_cases_deaths_2021,
        main="New Cases and New Deaths Andorra in the last 2 months of 2021",
        beside=TRUE,col=c(4,2),legend=TRUE,ylim=c(0,400),las=1)

SVN_loc2months_cases_deaths_2020<-rbind(SVN_loc2months$new_cases[1:61],SVN_loc2months$new_deaths[1:61])
row.names(SVN_loc2months_cases_deaths_2020)<-c("new cases","new deaths")
colnames(SVN_loc2months_cases_deaths_2020)<-as.character(SVN_loc2months$date[1:61])
barplot(SVN_loc2months_cases_deaths_2020,
        main="New Cases and New Deaths Slovenia in the last 2 months of 2020",
        beside=TRUE,col=c(4,2),legend=TRUE,ylim=c(0,2500),las=1)
SVN_loc2months_cases_deaths_2021<-rbind(SVN_loc2months$new_cases[62:122],SVN_loc2months$new_deaths[62:122])
row.names(SVN_loc2months_cases_deaths_2021)<-c("new cases","new deaths")
colnames(SVN_loc2months_cases_deaths_2021)<-as.character(SVN_loc2months$date[62:122])
barplot(SVN_loc2months_cases_deaths_2021,
        main="New Cases and New Deaths Slovenia in the last 2 months of 2021",
        beside=TRUE,col=c(4,2),legend=TRUE,las=1)

GBR_loc2months_cases_deaths_2020<-rbind(GBR_loc2months$new_cases[1:61],GBR_loc2months$new_deaths[1:61])
row.names(GBR_loc2months_cases_deaths_2020)<-c("new cases","new deaths")
colnames(GBR_loc2months_cases_deaths_2020)<-as.character(GBR_loc2months$date[1:61])
barplot(GBR_loc2months_cases_deaths_2020,
        main="New Cases and New Deaths United Kingdom in the last 2 months of 2020",
        beside=TRUE,col=c(4,2),legend=TRUE,ylim=c(0,70000),las=1)
GBR_loc2months_cases_deaths_2021<-rbind(GBR_loc2months$new_cases[62:122],GBR_loc2months$new_deaths[62:122])
row.names(GBR_loc2months_cases_deaths_2021)<-c("new cases","new deaths")
colnames(GBR_loc2months_cases_deaths_2021)<-as.character(GBR_loc2months$date[62:122])
barplot(GBR_loc2months_cases_deaths_2021,
        main="New Cases and New Deaths United Kingdom in the last 2 months of 2021",
        beside=TRUE,col=c(4,2),legend=TRUE,ylim=c(0,200000),las=1)

#7
AND_data_tichluy<-AND_data_avr
for (i in 2:AND_length){
  AND_data_tichluy$new_cases[i]<-AND_data_tichluy$new_cases[i]+AND_data_tichluy$new_cases[i-1]
  AND_data_tichluy$new_deaths[i]<-AND_data_tichluy$new_deaths[i]+AND_data_tichluy$new_deaths[i-1]
}
SVN_data_tichluy<-SVN_data_avr
for (i in 2:SVN_length){
  SVN_data_tichluy$new_cases[i]<-SVN_data_tichluy$new_cases[i]+SVN_data_tichluy$new_cases[i-1]
  SVN_data_tichluy$new_deaths[i]<-SVN_data_tichluy$new_deaths[i]+SVN_data_tichluy$new_deaths[i-1]
}
GBR_data_tichluy<-GBR_data_avr
for (i in 2:GBR_length){
  GBR_data_tichluy$new_cases[i]<-GBR_data_tichluy$new_cases[i]+GBR_data_tichluy$new_cases[i-1]
  GBR_data_tichluy$new_deaths[i]<-GBR_data_tichluy$new_deaths[i]+GBR_data_tichluy$new_deaths[i-1]
}
AND_loc_tichluy<-subset(AND_data_tichluy,month(AND_data_tichluy$date)==1|month(AND_data_tichluy$date)==2|month(AND_data_tichluy$date)==4|month(AND_data_tichluy$date)==7)
barplot(AND_loc_tichluy$new_cases[1:61],xlab="Days",ylab="New Cases",
        main="New Cases cumulative relative frequency Andorra in 2020",
        names.arg=AND_loc_tichluy$date[1:61],col="blue")
barplot(AND_loc_tichluy$new_cases[62:181],xlab="Days",ylab="New Cases",
        main="New Cases cumulative relative frequency Andorra in 2021",
        names.arg=AND_loc_tichluy$date[62:181],col="blue")
barplot(AND_loc_tichluy$new_cases[182:length(AND_loc_tichluy$date)],xlab="Days",ylab="New Cases",
        main="New Cases cumulative relative frequency Andorra in 2022",
        names.arg=AND_loc_tichluy$date[182:length(AND_loc_tichluy$date)],col="blue")

SVN_loc_tichluy<-subset(SVN_data_tichluy,month(SVN_data_tichluy$date)==1|month(SVN_data_tichluy$date)==2|month(SVN_data_tichluy$date)==4|month(SVN_data_tichluy$date)==7)
barplot(SVN_loc_tichluy$new_cases[1:89],xlab="Days",ylab="New Cases",
        main="New Cases cumulative relative frequency Slovenia in 2020",
        names.arg=SVN_loc_tichluy$date[1:89],col="blue")
barplot(SVN_loc_tichluy$new_cases[90:209],xlab="Days",ylab="New Cases",
        main="New Cases cumulative relative frequency Slovenia in 2021",
        names.arg=SVN_loc_tichluy$date[90:209],col="blue")
barplot(SVN_loc_tichluy$new_cases[210:length(SVN_loc_tichluy$date)],xlab="Days",ylab="New Cases",
        main="New Cases cumulative relative frequency Slovenia in 2022",
        names.arg=SVN_loc_tichluy$date[210:length(SVN_loc_tichluy$date)],col="blue")

GBR_loc_tichluy<-subset(GBR_data_tichluy,month(GBR_data_tichluy$date)==1|month(GBR_data_tichluy$date)==2|month(GBR_data_tichluy$date)==4|month(GBR_data_tichluy$date)==7)
barplot(GBR_loc_tichluy$new_cases[1:91],xlab="Days",ylab="New Cases",
        main="New Cases cumulative relative frequency United Kingdom in 2020",
        names.arg=GBR_loc_tichluy$date[1:91],col="blue")
barplot(GBR_loc_tichluy$new_cases[92:211],xlab="Days",ylab="New Cases",
        main="New Cases cumulative relative frequency United Kingdom in 2021",
        names.arg=GBR_loc_tichluy$date[92:211],col="blue")
barplot(GBR_loc_tichluy$new_cases[212:length(GBR_loc_tichluy$date)],xlab="Days",ylab="New Cases",
        main="New Cases cumulative relative frequency United Kingdom in 2022",
        names.arg=GBR_loc_tichluy$date[212:length(GBR_loc_tichluy$date)],col="blue")

#8
barplot(AND_loc_tichluy$new_deaths[1:61],xlab="Days",ylab="New Deaths",
        main="New Deaths cumulative relative frequency Andorra in 2020",
        names.arg=AND_loc_tichluy$date[1:61],col="blue")
barplot(AND_loc_tichluy$new_deaths[62:181],xlab="Days",ylab="New Deaths",
        main="New Deaths cumulative relative frequency Andorra in 2021",
        names.arg=AND_loc_tichluy$date[62:181],col="blue")
barplot(AND_loc_tichluy$new_deaths[182:length(AND_loc_tichluy$date)],xlab="Days",ylab="New Deaths",
        main="New Deaths cumulative relative frequency Andorra in 2022",
        names.arg=AND_loc_tichluy$date[182:length(AND_loc_tichluy$date)],col="blue")

barplot(SVN_loc_tichluy$new_deaths[1:89],xlab="Days",ylab="New Deaths",
        main="New Deaths cumulative relative frequency Slovenia in 2020",
        names.arg=SVN_loc_tichluy$date[1:89],col="blue")
barplot(SVN_loc_tichluy$new_deaths[90:209],xlab="Days",ylab="New Deaths",
        main="New Deaths cumulative relative frequency Slovenia in 2021",
        names.arg=SVN_loc_tichluy$date[90:209],col="blue")
barplot(SVN_loc_tichluy$new_deaths[210:length(SVN_loc_tichluy$date)],xlab="Days",ylab="New Deaths",
        main="New Deaths cumulative relative frequency Slovenia in 2022",
        names.arg=SVN_loc_tichluy$date[210:length(SVN_loc_tichluy$date)],col="blue")

barplot(GBR_loc_tichluy$new_deaths[1:91],xlab="Days",ylab="New Deaths",
        main="New Deaths cumulative relative frequency United Kingdom in 2020",
        names.arg=GBR_loc_tichluy$date[1:91],col="blue")
barplot(GBR_loc_tichluy$new_deaths[92:211],xlab="Days",ylab="New Deaths",
        main="New Deaths cumulative relative frequency United Kingdom in 2021",
        names.arg=GBR_loc_tichluy$date[92:211],col="blue")
barplot(GBR_loc_tichluy$new_deaths[212:length(GBR_loc_tichluy$date)],xlab="Days",ylab="New Deaths",
        main="New Deaths cumulative relative frequency United Kingdom in 2022",
        names.arg=GBR_loc_tichluy$date[212:length(GBR_loc_tichluy$date)],col="blue")