# Set Library
library("tidyverse")
library("plyr")
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

#Processing
all_nation<-filter(data,!is.na(data$continent)&!is.na(data$new_cases)&!is.na(data$new_deaths)&data$new_cases>=0&data$new_deaths>=0,.preserve=FALSE)
all_nation %>% arrange(mdy(all_nation$date))

#Plotting
#7 days plotting
#New cases

#1
all_length<-length(all_nation$date)
all_nation_avr<-all_nation
for (i in 1: all_length){
  if (i<7){
    s<-0
    ss<-0
    for (j in 1:i){
      s<-s+all_nation$new_cases[j]
      ss<-ss+all_nation$new_deaths[j]
    }
    all_nation_avr$new_cases[i]<-s/i
    all_nation_avr$new_deaths[i]<-ss/i
  }
  else{
    s<-0
    ss<-0
    ii<-i-6
    for (j in ii:i){
      s<-s+all_nation$new_cases[j]
      ss<-ss+all_nation$new_deaths[j]
    }
    all_nation_avr$new_cases[i]<-s/7
    all_nation_avr$new_deaths[i]<-ss/7
  }
}
all_nation_avr$date<-as.character(all_nation_avr$date,"%m/%d/%Y")
all_nation_avr$date<-as.Date(all_nation_avr$date,"%m/%d/%Y")
all_loc<-subset(all_nation_avr,month(all_nation_avr$date)==1|month(all_nation_avr$date)==2|month(all_nation_avr$date)==4|month(all_nation_avr$date)==7)
all_pos<-which(year(all_loc$date)==2021)
all_pos2021<-all_pos[1]
all_pos<-which(year(all_loc$date)==2022)
all_pos2022<-all_pos[1]
barplot(all_loc$new_cases[1:all_pos2021-1],
        xlab="Days",
        ylab="New Cases",
        main="World new Cases in 2020",
        names.arg=all_loc$date[1:all_pos2021-1],
        col="blue")
barplot(all_loc$new_cases[62:181],
        xlab="Days",
        ylab="New Cases",
        main="World new Cases in 2021",
        names.arg=all_loc$date[62:181],
        col="blue")
barplot(all_loc$new_cases[all_pos2022:length(all_loc$date)],
        xlab="Days",
        ylab="New Cases", xlim=c(0,54), ylim=c(0,800),
        main="World new Cases in 2022", 
        names.arg=all_loc$date[all_pos2022:length(all_loc$date)],
        col="blue")

#2
barplot(all_loc$new_deaths[1:all_pos2021-1],
        xlab="Days",
        ylab="New Deaths",
        main="World new Deaths  in 2020",
        names.arg=all_loc$date[1:all_pos2021-1],
        col="blue")
barplot(all_loc$new_deaths[62:181],
        xlab="Days",
        ylab="New Deaths",
        main="World new Deaths  in 2021",
        names.arg=all_loc$date[62:181],
        col="blue")
barplot(all_loc$new_deaths[all_pos2022:length(all_loc$date)],
        xlab="Days",
        ylab="New Deaths",
        main="World new Deaths  in 2022", xlim=c(0,54), ylim=c(0,10),
        names.arg=all_loc$date[all_pos2022:length(all_loc$date)],
        col="blue")

#3
all_loc2months<-subset(all_nation_avr,month(all_nation_avr$date)==11|month(all_nation_avr$date)==12)
barplot(all_loc2months$new_cases[1:61],xlab="Days",ylab="New Cases",
        main="World new Cases in the last 2 months of 2020",
        names.arg=all_loc2months$date[1:61],col="blue")
barplot(all_loc2months$new_cases[62:122],xlab="Days",ylab="New Cases",
        main="World new Cases in the last 2 months of 2021",
        names.arg=all_loc2months$date[62:122],col="blue")

#4
barplot(all_loc2months$new_deaths[1:61],xlab="Days",ylab="New Cases",
        main="World new Deaths in the last 2 months of 2020",
        names.arg=all_loc2months$date[1:61],col="blue")
barplot(all_loc2months$new_deaths[62:122],xlab="Days",ylab="New Cases",
        main="World new Deaths in the last 2 months of 2021",
        names.arg=all_loc2months$date[62:122],col="blue")

#5
all_nation_tichluy<-all_nation_avr
for (i in 2:all_length){
  all_nation_tichluy$new_cases[i]<-all_nation_tichluy$new_cases[i]+all_nation_tichluy$new_cases[i-1]
  all_nation_tichluy$new_deaths[i]<-all_nation_tichluy$new_deaths[i]+all_nation_tichluy$new_deaths[i-1]
}
all_loc_tichluy<-subset(all_nation_tichluy,month(all_nation_tichluy$date)==1|month(all_nation_tichluy$date)==2|month(all_nation_tichluy$date)==4|month(all_nation_tichluy$date)==7)
barplot(all_loc_tichluy$new_cases[1:61],xlab="Days",ylab="New Cases",
        main="World new Cases cumulative relative frequency in 2020",
        names.arg=all_loc_tichluy$date[1:61],col="blue")
barplot(all_loc_tichluy$new_cases[62:181],xlab="Days",ylab="New Cases",
        main="World new Cases cumulative relative frequency in 2021",
        names.arg=all_loc_tichluy$date[62:181],col="blue")
barplot(all_loc_tichluy$new_cases[182:length(all_loc_tichluy$date)],xlab="Days",ylab="New Cases",xlim=c(0,54), ylim=c(0,200000),
        main="World new Cases cumulative relative frequency in 2022",
        names.arg=all_loc_tichluy$date[182:length(all_loc_tichluy$date)],col="blue")

#6
barplot(all_loc_tichluy$new_deaths[1:61],xlab="Days",ylab="New Deaths",
        main="World new Deaths cumulative relative frequency in 2020",
        names.arg=all_loc_tichluy$date[1:61],col="blue")
barplot(all_loc_tichluy$new_deaths[62:181],xlab="Days",ylab="New Deaths",
        main="World new Deaths cumulative relative frequency in 2021",
        names.arg=all_loc_tichluy$date[62:181],col="blue")
barplot(all_loc_tichluy$new_deaths[182:length(all_loc_tichluy$date)],xlab="Days",ylab="New Deaths",xlim=c(0,54), ylim=c(0,8000),
        main="World new Deaths cumulative relative frequency in 2022",
        names.arg=all_loc_tichluy$date[182:length(all_loc_tichluy$date)],col="blue")
