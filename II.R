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
setwd("path")      # path
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

  # 1/ Gia tri lon nhat nho nhat
ct1_maxnewcase<-max(c1_data$new_cases, na.rm = T)
cat("Andorra's new cases - Maximum: ", ct1_maxnewcase, '\n')
ct1_minnewcase<-min(c1_data$new_cases, na.rm = T)
cat("Andorra's new cases - Minimum: ", ct1_minnewcase, '\n')
ct1_maxnewdeath<-max(c1_data$new_deaths, na.rm = T)
cat("Andorra's new deaths - Maximum: ", ct1_maxnewdeath, '\n')
ct1_minnewdeath<-min(c1_data$new_deaths, na.rm = T)
cat("Andorra's new deaths - Minimum: ", ct1_minnewdeath, '\n')

ct2_maxnewcase<-max(c2_data$new_cases, na.rm = T)
cat("Slovenia's new cases - Maximum: ", ct2_maxnewcase, '\n')
ct2_minnewcase<-min(c2_data$new_cases, na.rm = T)
cat("Slovenia's new cases - Minimum: ", ct2_minnewcase, '\n')
ct2_maxnewdeath<-max(c2_data$new_deaths, na.rm = T)
cat("Slovenia's new deaths - Maximum: ", ct2_maxnewdeath, '\n')
ct2_minnewdeath<-min(c2_data$new_deaths, na.rm = T)
cat("Slovenia's new deaths - Minimum: ", ct2_minnewdeath, '\n')

ct3_maxnewcase<-max(c3_data$new_cases, na.rm = T)
cat("United Kingdom's new cases - Maximum: ", ct3_maxnewcase, '\n')
ct3_minnewcase<-min(c3_data$new_cases, na.rm = T)
cat("United Kingdom's new cases - Minimum: ", ct3_minnewcase, '\n')
ct3_maxnewdeath<-max(c3_data$new_deaths, na.rm = T)
cat("United Kingdom's new deaths - Maximum: ", ct3_maxnewdeath, '\n')
ct3_minnewdeath<-min(c3_data$new_deaths, na.rm = T)
cat("United Kingdom's new deaths - Minimum: ", ct3_minnewdeath, '\n')

  # 2/ Tinh tu phan vi
Q_newcasecountry1<-quantile(c1_data$new_cases, prob = c(.25, .5, .75))
cat("Andorra's new cases - Quartile: ", Q_newcasecountry1, '\n')
Q_newdeathcountry1<-quantile(c1_data$new_deaths, prob = c(.25, .5, .75))
cat("Andorra's new deaths - Quartile: ", Q_newdeathcountry1, '\n')

Q_newcasecountry2<-quantile(c2_data$new_cases, prob = c(.25, .5, .75))
cat("Slovenia's new cases - Quartile: ", Q_newcasecountry2, '\n')
Q_newdeathcountry2<-quantile(c2_data$new_deaths, prob = c(.25, .5, .75))
cat("Slovenia's new deaths - Quartile: ", Q_newdeathcountry2, '\n')

Q_newcasecountry3<-quantile(c3_data$new_cases, prob = c(.25, .5, .75))
cat("United Kingdom's new cases - Quartile: ", Q_newcasecountry3, '\n')
Q_newdeathcountry3<-quantile(c3_data$new_deaths, prob = c(.25, .5, .75))
cat("United Kingdom's new deaths - Quartile: ", Q_newdeathcountry3, '\n')

  # 3/ Tinh Average
avgct1_newcase<-mean(c1_data$new_cases)
cat("Andorra's new cases - Average: ", avgct1_newcase, '\n')
avgct1_newdeath<-mean(c1_data$new_deaths)
cat("Andorra's new deaths - Average: ", avgct1_newdeath, '\n')

avgct2_newcase<-mean(c2_data$new_cases)
cat("Slovenia's new cases - Average: ", avgct2_newcase, '\n')
avgct2_newdeath<-mean(c2_data$new_deaths)
cat("Slovenia's new deaths - Average: ", avgct2_newdeath, '\n')

avgct3_newcase<-mean(c3_data$new_cases)
cat("United Kingdom's new cases - Average: ", avgct3_newcase, '\n')
avgct3_newdeath<-mean(c3_data$new_deaths)
cat("United Kingdom's new deaths - Average: ", avgct3_newdeath, '\n')

  # 4/ Tinh do lech chuan
m1_newcase<-sd(c1_data$new_cases)
m1_newdeath<-sd(c1_data$new_deaths)
cat("Andorra's new cases - Standard Deviation: ", m1_newcase, '\n')
cat("Andorra's new deaths - Standard Deviation: ", m1_newdeath, '\n')

m2_newcase<-sd(c2_data$new_cases)
m2_newdeath<-sd(c2_data$new_deaths)
cat("Slovenia's new cases - Standard Deviation: ", m2_newcase, '\n')
cat("Slovenia's new deaths - Standard Deviation: ", m2_newdeath, '\n')

m3_newcase<-sd(c3_data$new_cases)
m3_newdeath<-sd(c3_data$new_deaths)
cat("United Kingdom's new cases - Standard Deviation: ", m3_newcase, '\n')
cat("United Kingdom's new deaths - Standard Deviation: ", m3_newdeath, '\n')

  # 5/ Tinh so outlier
Q_newcasecountry1<-quantile(c1_data$new_cases)
Q_newcasecountry2<-quantile(c2_data$new_cases)
Q_newcasecountry3<-quantile(c3_data$new_cases)

Q_newdeathcountry1<-quantile(c1_data$new_deaths)
Q_newdeathcountry2<-quantile(c2_data$new_deaths)
Q_newdeathcountry3<-quantile(c3_data$new_deaths)

IQRnewcase1<-(Q_newcasecountry1[[4]] - Q_newcasecountry1[[2]])
lower_newcase1<-(Q_newcasecountry1[[2]] - 1.5 * IQRnewcase1)
upper_newcase1<-(Q_newcasecountry1[[4]] + 1.5 * IQRnewcase1)
outlier_newcase1<-subset(c1_data, c1_data$new_cases < lower_newcase1 | c1_data$new_cases > upper_newcase1)
count_outlier_newcase1<-sum(table(outlier_newcase1))
cat("Andorra's new cases - Outlier: ", count_outlier_newcase1, '\n')
IQRnewdeath1<-(Q_newdeathcountry1[[4]] - Q_newdeathcountry1[[2]])
lower_newdeath1<-(Q_newdeathcountry1[[2]] - 1.5*IQRnewdeath1)
upper_newdeath1<-(Q_newdeathcountry1[[4]] + 1.5*IQRnewdeath1)
outlier_newdeath1<-subset(c1_data, c1_data$new_deaths < lower_newdeath1 | c1_data$new_deaths > upper_newdeath1)
count_outlier_newdeath1<-sum(table(outlier_newdeath1))
cat("Andorra's new deaths - Outlier: ", count_outlier_newdeath1, '\n')

IQRnewcase2<-(Q_newcasecountry2[[4]] - Q_newcasecountry2[[2]])
lower_newcase2<-(Q_newcasecountry2[[2]] - 1.5 * IQRnewcase2)
upper_newcase2<-(Q_newcasecountry2[[4]] + 1.5 * IQRnewcase2)
outlier_newcase2<-subset(c2_data, c2_data$new_cases < lower_newcase2 | c2_data$new_cases > upper_newcase2)
count_outlier_newcase2<-sum(table(outlier_newcase2))
cat("Slovenia's new cases - Outlier: ", count_outlier_newcase2, '\n')
IQRnewdeath2<-(Q_newdeathcountry2[[4]] - Q_newdeathcountry2[[2]])
lower_newdeath2<-(Q_newdeathcountry2[[2]] - 1.5 * IQRnewdeath2)
upper_newdeath2<-(Q_newdeathcountry2[[4]] + 1.5 * IQRnewdeath2)
outlier_newdeath2<-subset(c2_data, c2_data$new_deaths < lower_newdeath2 | c2_data$new_deaths > upper_newdeath2)
count_outlier_newdeath2<-sum(table(outlier_newdeath2))
cat("Slovenia's new deaths - Outlier: ", count_outlier_newdeath2, '\n')

IQRnewcase3<-(Q_newcasecountry3[[4]] - Q_newcasecountry3[[2]])
lower_newcase3<-(Q_newcasecountry3[[2]] - 1.5*IQRnewcase3)
upper_newcase3<-(Q_newcasecountry3[[4]] + 1.5*IQRnewcase3)
outlier_newcase3<-subset(c3_data, c3_data$new_cases < lower_newcase3 | c3_data$new_cases > upper_newcase3)
count_outlier_newcase3<-sum(table(outlier_newcase3))
cat("United Kingdom's new cases - Outlier: ", count_outlier_newcase3, '\n')
IQRnewdeath3<-(Q_newdeathcountry3[[4]]-Q_newdeathcountry3[[2]])
lower_newdeath3<-(Q_newdeathcountry3[[2]] - 1.5 * IQRnewdeath3)
upper_newdeath3<-(Q_newdeathcountry3[[4]] + 1.5 * IQRnewdeath3)
outlier_newdeath3<-subset(c3_data, c3_data$new_deaths < lower_newdeath3 | c3_data$new_deaths > upper_newdeath3)
count_outlier_newdeath3<-sum(table(outlier_newdeath3))
cat("United Kingdom's new deaths - Outlier: ", count_outlier_newdeath3, '\n')

  # 6/ Lap bang so lieu
datatable_newcase<-data.frame(Country = c(national[1], national[2], national[3]), Min = c(ct1_minnewcase, ct2_minnewcase, ct3_minnewcase),
                              Q1 = c(Q_newcasecountry1[[2]], Q_newcasecountry2[[2]], Q_newcasecountry3[[2]]),
                              Q2 = c(Q_newcasecountry1[[3]], Q_newcasecountry2[[3]], Q_newcasecountry3[[3]]),
                              Q3 = c(Q_newcasecountry1[[4]], Q_newcasecountry2[[4]], Q_newcasecountry3[[4]]),
                              Max = c(ct1_maxnewcase, ct2_maxnewcase, ct3_maxnewcase),
                              Avg = c(avgct1_newcase, avgct2_newcase, avgct3_newcase),
                              Std = c(m1_newcase, m2_newcase, m3_newcase),
                              Outlier = c(count_outlier_newcase1, count_outlier_newcase2, count_outlier_newcase3))
cat("New cases: \n")
datatable_newcase

datatable_newdeath<-data.frame(Country = c(national[1], national[2], national[3]), Min = c(ct1_minnewdeath, ct2_minnewdeath, ct3_minnewdeath),
                               Q1 = c(Q_newdeathcountry1[[2]], Q_newdeathcountry2[[2]], Q_newdeathcountry3[[2]]),
                               Q2 = c(Q_newdeathcountry1[[3]], Q_newdeathcountry2[[3]], Q_newdeathcountry3[[3]]),
                               Q3 = c(Q_newdeathcountry1[[4]], Q_newdeathcountry2[[4]], Q_newdeathcountry3[[4]]),
                               Max = c(ct1_maxnewdeath,  ct2_maxnewdeath, ct3_maxnewdeath),
                               Avg = c(avgct1_newdeath, avgct2_newdeath, avgct3_newdeath),
                               Std = c(m1_newdeath, m2_newdeath, m3_newdeath),
                               Outlier = c(count_outlier_newdeath1, count_outlier_newdeath2, count_outlier_newdeath3))
cat("New deaths: \n")
datatable_newdeath

  # 7/ Ve boxplot
data_3country<-rbind(c1_data,c2_data,c3_data)
boxplot(data_3country$new_cases~data_3country$location,
        xlab = "Countries",
        ylab = 'New cases')
