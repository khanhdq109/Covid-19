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
setwd("D:/Khanh/BK/HK212/Discrete Structures/CODE")      # path
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

# x> NHOM CAU HOI RIENG

# Processing
# Sorting needed countries (by date)
AND_data <- (data %>% filter_all(any_vars(. %in% c("Andorra"))))
SVN_data <- (data %>% filter_all(any_vars(. %in% c("Slovenia"))))
GBR_data <- (data %>% filter_all(any_vars(. %in% c("United Kingdom"))))

# Sorting needed countries (removing false data)
AND_data<-filter(AND_data,!is.na(AND_data$new_cases)&!is.na(AND_data$new_deaths)&AND_data$new_cases>=0&AND_data$new_deaths>=0,.preserve=FALSE)
SVN_data<-filter(SVN_data,!is.na(SVN_data$new_cases)&!is.na(SVN_data$new_deaths)&SVN_data$new_cases>=0&SVN_data$new_deaths>=0,.preserve=FALSE)
GBR_data<-filter(GBR_data,!is.na(GBR_data$new_cases)&!is.na(GBR_data$new_deaths)&GBR_data$new_cases>=0&GBR_data$new_deaths>=0,.preserve=FALSE)

# Sorting data by date
AND_data %>% arrange(mdy(AND_data$date))
SVN_data %>% arrange(mdy(SVN_data$date))
GBR_data %>% arrange(mdy(GBR_data$date))

# Plotting
# Last 7 days plotting
# New cases
for (i in c(1:3))
{
  color<- rgb (208 / 255, 25 / 255, 31 / 255)
  if (i == 2) 
    color<-rgb (15 / 255, 142 / 255, 11 / 255)
  else if (i == 3)
    color<-rgb (36 / 255, 41 / 255, 172 / 255)
  par(mar=c(3.9,3.9,1.5,0.3))
  if (i==1)  
    barplot(AND_data$new_cases[(which.max(mdy(AND_data$date))-6):which.max(mdy(AND_data$date))],
            xlab = "Days",
            ylab = "New Cases",
            main = national[i],
            names.arg = AND_data$date[(which.max(mdy(AND_data$date))-6):which.max(mdy(AND_data$date))],
            col = color)
  else if (i==2)
    barplot(SVN_data$new_cases[(which.max(mdy(SVN_data$date))-6):which.max(mdy(SVN_data$date))],
            xlab = "Days",
            ylab = "New Cases",
            main = national[i],
            names.arg = SVN_data$date[(which.max(mdy(SVN_data$date))-6):which.max(mdy(SVN_data$date))],
            col = color)
  else if (i==3)
    barplot(GBR_data$new_cases[(which.max(mdy(GBR_data$date))-6):which.max(mdy(GBR_data$date))],
            xlab = "Days",
            ylab = "New Cases",
            main = national[i],
            names.arg = GBR_data$date[(which.max(mdy(GBR_data$date))-6):which.max(mdy(GBR_data$date))],
            col = color)
}

# New deaths
for (i in c(1:3))
{
  color<- rgb (208 / 255, 25 / 255, 31 / 255)
  if (i == 2) 
    color<-rgb (15 / 255, 142 / 255, 11 / 255)
  else if (i == 3)
    color<-rgb (36 / 255, 41 / 255, 172 / 255)
  par(mar=c(3.9,3.9,1.5,0.3))
  if (i==1)  
    barplot(AND_data$new_deaths[(which.max(mdy(AND_data$date))-6):which.max(mdy(AND_data$date))],
            xlab = "Days",
            ylab = "New Deaths",
            main = national[i],
            names.arg = AND_data$date[(which.max(mdy(AND_data$date))-6):which.max(mdy(AND_data$date))],
            col = color)
  else if (i==2)
    barplot(SVN_data$new_deaths[(which.max(mdy(SVN_data$date))-6):which.max(mdy(SVN_data$date))],
            xlab = "Days",
            ylab = "New Deaths",
            main = national[i],
            names.arg = SVN_data$date[(which.max(mdy(SVN_data$date))-6):which.max(mdy(SVN_data$date))],
            col = color)
  else if (i==3)
    barplot(GBR_data$new_deaths[(which.max(mdy(GBR_data$date))-6):which.max(mdy(GBR_data$date))],
            xlab = "Days",
            ylab = "New Deaths",
            main = national[i],
            names.arg = GBR_data$date[(which.max(mdy(GBR_data$date))-6):which.max(mdy(GBR_data$date))],
            col = color)
}

# General plotting
# New case plotting
for (i in c(1:3))
{
  color<- rgb (208 / 255, 25 / 255, 31 / 255)
  if (i == 2) 
    color<-rgb (15 / 255, 142 / 255, 11 / 255)
  else if (i == 3)
    color<-rgb (36 / 255, 41 / 255, 172 / 255)
  par(mar=c(3.9,3.9,1.5,0.3))
  if (i==1)  
    barplot(AND_data$new_cases[1:which.max(mdy(AND_data$date))],
            xlab = "Days",
            ylab = "New Cases",
            main = national[i],
            names.arg = AND_data$date[1:which.max(mdy(AND_data$date))],
            col = color)
  else if (i==2)
    barplot(SVN_data$new_cases[1:which.max(mdy(SVN_data$date))],
            xlab = "Days",
            ylab = "New Cases",
            main = national[i],
            names.arg = SVN_data$date[1:which.max(mdy(SVN_data$date))],
            col = color)
  else if (i==3)
    barplot(GBR_data$new_cases[1:which.max(mdy(GBR_data$date))],
            xlab = "Days",
            ylab = "New Cases",
            main = national[i],
            names.arg = GBR_data$date[1:which.max(mdy(GBR_data$date))],
            col = color)
}

# New deaths plotting
for (i in c(1:3))
{
  color<- rgb (208 / 255, 25 / 255, 31 / 255)
  if (i == 2) 
    color<-rgb (15 / 255, 142 / 255, 11 / 255)
  else if (i == 3)
    color<-rgb (36 / 255, 41 / 255, 172 / 255)
  par(mar=c(3.9,3.9,1.5,0.3))
  if (i==1)  
    barplot(AND_data$new_deaths[1:which.max(mdy(AND_data$date))],
            xlab = "Days",
            ylab = "New Deaths",
            main = national[i],
            names.arg = AND_data$date[1:which.max(mdy(AND_data$date))],
            col = color)
  else if (i==2)
    barplot(SVN_data$new_deaths[1:which.max(mdy(SVN_data$date))],
            xlab = "Days",
            ylab = "New Deaths",
            main = national[i],
            names.arg = SVN_data$date[1:which.max(mdy(SVN_data$date))],
            col = color)
  else if (i==3)
    barplot(GBR_data$new_deaths[1:which.max(mdy(GBR_data$date))],
            xlab = "Days",
            ylab = "New Deaths",
            main = national[i],
            names.arg = GBR_data$date[1:which.max(mdy(GBR_data$date))],
            col = color)
}
rm(i,color)