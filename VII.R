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
#1
GBR_data <- (data %>% filter_all(any_vars(. %in% c("United Kingdom"))))
GBR_data_avr$date<-as.character(GBR_data_avr$date,"%Y-%m-%d")
GBR_data_avr$date<-as.Date(GBR_data_avr$date,"%Y-%m-%d")
GBR_loc<-subset(GBR_data_avr,month(GBR_data_avr$date)==1|month(GBR_data_avr$date)==2|month(GBR_data_avr$date)==4|month(GBR_data_avr$date)==7)
date_month1<-c("2020-01-01","2020-01-02", "2020-01-03", "2020-01-04", "2020-01-05", "2020-01-06",
               "2020-01-07", "2020-01-08", "2020-01-09", "2020-01-10", "2020-01-11", "2020-01-12", 
               "2020-01-13","2020-01-14", "2020-01-15", "2020-01-16", "2020-01-17", "2020-01-18", 
               "2020-01-19", "2020-01-20","2020-01-21", "2020-01-22", "2020-01-23", "2020-01-24", 
               "2020-01-25", "2020-01-26", "2020-01-27","2020-01-28", "2020-01-29","2020-01-30")
data$date<-as.character(data$date,"%m/%d/%Y")
data$date<-as.Date(data$date,"%m/%d/%Y")
data_loc<-subset(data,month(data$date)==1|month(data$date)==2|month(data$date)==4|month(data$date)==7)
data_loc$date<-as.character(data_loc$date,"%Y-%m-%d")
date_all<-c()
GBR_loc$date<-as.character(GBR_loc$date,"%Y-%m-%d")
for (i in 1:30){
  date_all[i]=date_month1[i]
}
for (i in 31:291){
  date_all[i]=GBR_loc$date[i-30]
}
sum_new_cases<-c()
sum_new_deaths<-c()
for (i in 1:291){
  loc_days<-subset(data_loc,data_loc$date==date_all[i])
  sum_new_cases[i]=0
  sum_new_deaths[i]=0
  for (j in 1:length(loc_days$date)){
    if (is.na(loc_days$new_cases[j])==FALSE)
      sum_new_cases[i]=sum_new_cases[i]+loc_days$new_cases[j]
    if (is.na(loc_days$new_deaths[j])==FALSE)
      sum_new_deaths[i]=sum_new_deaths[i]+loc_days$new_deaths[j]
  }
}
barplot(sum_new_cases[23:121],main="New Cases in 2020",
        xlab="Days",ylab="New Cases",
        names.arg = date_all[23:121],col="blue")
barplot(sum_new_cases[122:241],main="New Cases in 2021",
        xlab="Days",ylab="New Cases",
        names.arg = date_all[122:241],col="blue")
barplot(sum_new_cases[242:291],main="New Cases in 2022",
        xlab="Days",ylab="New Cases",
        names.arg = date_all[242:291],col="blue")



#2
barplot(sum_new_deaths[23:121],main="New Deaths in 2020",
        xlab="Days",ylab="New Deaths",
        names.arg = date_all[23:121],col="blue")
barplot(sum_new_deaths[122:241],main="New Deaths in 2021",
        xlab="Days",ylab="New Deaths",
        names.arg = date_all[122:241],col="blue")
barplot(sum_new_deaths[242:291],main="New Deaths in 2022",
        xlab="Days",ylab="New Deaths",
        names.arg = date_all[242:291],col="blue")



#3
data_loc_2months<-subset(data,month(data$date)==11|month(data$date)==12)
date_2months=data_loc_2months$date[1:122]
sum_new_cases_2months<-c()
sum_new_deaths_2months<-c()
for (i in 1:122){
  loc_days_2months<-subset(data_loc_2months,data_loc_2months$date==date_2months[i])
  sum_new_cases_2months[i]=0
  sum_new_deaths_2months[i]=0
  for (j in 1:length(loc_days_2months$date)){
    if (is.na(loc_days_2months$new_cases[j])==FALSE)
      sum_new_cases_2months[i]=sum_new_cases_2months[i]+loc_days_2months$new_cases[j]
    if (is.na(loc_days_2months$new_deaths[j])==FALSE)
      sum_new_deaths_2months[i]=sum_new_deaths_2months[i]+loc_days_2months$new_deaths[j]
  }
}
barplot(sum_new_cases_2months[1:61],main="New Cases in the last 2 months of 2020",
        xlab="Days",ylab="New Cases",
        names.arg = date_2months[1:61],col="blue")
barplot(sum_new_cases_2months[62:122],main="New Cases in the last 2 months of 2021",
        xlab="Days",ylab="New Cases",
        names.arg = date_2months[62:122],col="blue")



#4
barplot(sum_new_deaths_2months[1:61],main="New Deaths in the last 2 months of 2020",
        xlab="Days",ylab="New Deaths",
        names.arg = date_2months[1:61],col="blue")
barplot(sum_new_deaths_2months[62:122],main="New Deaths in the last 2 months of 2021",
        xlab="Days",ylab="New Deaths",
        names.arg = date_2months[62:122],col="blue")



#5
sum_new_cases_2months_tichluy<-sum_new_cases_2months
sum_new_deaths_2months_tichluy<-sum_new_deaths_2months
for (i in 2:122){
  sum_new_cases_2months_tichluy[i]=sum_new_cases_2months_tichluy[i]+sum_new_cases_2months_tichluy[i-1]
  sum_new_deaths_2months_tichluy[i]=sum_new_deaths_2months_tichluy[i]+sum_new_deaths_2months_tichluy[i-1]
}
barplot(sum_new_cases_2months_tichluy[1:61],
        main="New Cases cumulative relative frequency in the last 2 months of 2020",
        xlab="Days",ylab="New Cases",names.arg = date_2months[1:61],col="blue")
barplot(sum_new_cases_2months_tichluy[62:122],
        main="New Cases cumulative relative frequency in the last 2 months of 2021",
        xlab="Days",ylab="New Cases",names.arg = date_2months[62:122],col="blue")



#6
barplot(sum_new_deaths_2months_tichluy[1:61],
        main="New Deaths cumulative relative frequency in the last 2 months of 2020",
        xlab="Days",ylab="New Deaths",names.arg = date_2months[1:61],col="blue")
barplot(sum_new_deaths_2months_tichluy[62:122],
        main="New Deaths cumulative relative frequency in the last 2 months of 2021",
        xlab="Days",ylab="New Deaths",names.arg = date_2months[62:122],col="blue")





