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
library("lattice")

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

# i> NHOM CAU HOI LIEN QUAN DEN TONG QUAT DU LIEU

# ii> NHOM CAU HOI LIEN QUAN DEN MO TA THONG KE CO BAN DU LIEU
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
Q_newcasecountry1<-quantile(c1_data$new_cases)
cat("Andorra's new cases - Quartile: ", Q_newcasecountry1, '\n')
Q_newcasecountry1
Q_newdeathcountry1<-quantile(c1_data$new_deaths)
cat("Andorra's new deaths - Quartile: ", Q_newdeathcountry1, '\n')

Q_newcasecountry2<-quantile(c2_data$new_cases)
cat("Slovenia's new cases - Quartile: ", Q_newcasecountry2, '\n')
Q_newdeathcountry2<-quantile(c2_data$new_deaths)
cat("Slovenia's new deaths - Quartile: ", Q_newdeathcountry2, '\n')

Q_newcasecountry3<-quantile(c3_data$new_cases)
cat("United Kingdom's new cases - Quartile: ", Q_newcasecountry3, '\n')
Q_newdeathcountry3<-quantile(c3_data$new_deaths)
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
boxplot(data_3country$new_cases~data_3country$location)
boxplot(data_3country$new_deaths~data_3country$location)

# iii> NHOM CAU HOI LIEN QUAN DEN DU LIEU THU THAP

# iv> NHOM CAU HOI LIEN QUAN DEN TRUC QUAN DU LIEU

  # 1/ Ve bieu do tan so tich luy quoc gia cho cac chau luc
ctn<-continent[!duplicated(continent)]
ctn<-na.omit(ctn)
num_countries<-c()
for (i in c(1:length(ctn))) {
  temp<-subset(data, continent == ctn[i])
  temp<-temp$location
  temp2<-temp[!duplicated(temp)]
  num_countries[i]<-length(temp2)
}
freq<-c(0, 0, 0, 0, 0)
for (i in c(1:5)) {
  for (j in c(1:length(num_countries))) {
    if (num_countries[j] > i * 10 & num_countries[j] <= (i + 1) * 10) {
      freq[i]<-freq[i] + 1
    }
  }
}
cumfreq<-cumsum(freq)
breaks<-seq(10, 50, by = 10)
plot(breaks, cumfreq,
     pch = 19,
     col = "red",
     main = "Country Cumulative Frequency Plot for Continents",
     xlab = "Number of Contries",
     ylab = "Number of Continents")
lines(breaks, cumfreq, col = "red")

  # 2/ Ve bieu do tan so tuong doi quoc gia cho cac chau luc
histogram(num_countries,
          breaks = 4,
          col = rgb (67 / 255, 128 / 255, 13 / 255),
          main = "Relative Frequency Histogram",
          xlab = "Number of Countries",
          ylab = "Percent of Total")

  # 3/ Ve bieu do the hien nhiem benh da bao cao cua cac quoc gia trong 7 ngay cuoi cung cua nam cuoi cung
lastDay<-c()
for (i in c(1:3)) {
  temp<-which(location == national[i])
  lastDay[i]<-temp[length(temp)]
}
for (i in c(1:3)) {
  color<- rgb (208 / 255, 25 / 255, 31 / 255)
  if (i == 2) 
    color<-rgb (15 / 255, 142 / 255, 11 / 255)
  else if (i == 3)
    color<-rgb (36 / 255, 41 / 255, 172 / 255)
  barplot(new_cases[(lastDay[i] - 6):lastDay[i]],
          xlab = "Days",
          ylab = "New Cases",
          main = national[i],
          names.arg = date[(lastDay[i] - 6):lastDay[i]],
          col = color)
}

  # 4/ Ve bieu do the hien tu vong da bao cao cua cac quoc gia trong 7 ngay cuoi cung cua nam cuoi cung
for (i in c(1:3)) {
  color<- rgb (208 / 255, 25 / 255, 31 / 255)
  if (i == 2) 
    color<-rgb (15 / 255, 142 / 255, 11 / 255)
  else if (i == 3)
    color<-rgb (36 / 255, 41 / 255, 172 / 255)
  barplot(new_deaths[(lastDay[i] - 6):lastDay[i]],
          xlab = "Days",
          ylab = "New Deaths",
          main = national[i],
          names.arg = date[(lastDay[i] - 6):lastDay[i]],
          col = color)
}

  # 5/ Ve bieu do pho dat nuoc xuat hien outliers cho nhiem benh
Q_newcasecountry1<-quantile(c1_data$new_cases)
Q_newcasecountry2<-quantile(c2_data$new_cases)
Q_newcasecountry3<-quantile(c3_data$new_cases)

IQRnewcase1<-(Q_newcasecountry1[[4]] - Q_newcasecountry1[[2]])
lower_newcase1<-(Q_newcasecountry1[[2]] - 1.5 * IQRnewcase1)
upper_newcase1<-(Q_newcasecountry1[[4]] + 1.5 * IQRnewcase1)
outlier_newcase1<-subset(c1_data, c1_data$new_cases < lower_newcase1 | c1_data$new_cases > upper_newcase1)
count_outlier_newcase1<-sum(table(outlier_newcase1))

IQRnewcase2<-(Q_newcasecountry2[[4]] - Q_newcasecountry2[[2]])
lower_newcase2<-(Q_newcasecountry2[[2]] - 1.5 * IQRnewcase2)
upper_newcase2<-(Q_newcasecountry2[[4]] + 1.5 * IQRnewcase2)
outlier_newcase2<-subset(c2_data, c2_data$new_cases < lower_newcase2 | c2_data$new_cases > upper_newcase2)
count_outlier_newcase2<-sum(table(outlier_newcase2))

IQRnewcase3<-(Q_newcasecountry3[[4]] - Q_newcasecountry3[[2]])
lower_newcase3<-(Q_newcasecountry3[[2]] - 1.5 * IQRnewcase3)
upper_newcase3<-(Q_newcasecountry3[[4]] + 1.5 * IQRnewcase3)
outlier_newcase3<-subset(c3_data, c3_data$new_cases < lower_newcase3 | c3_data$new_cases > upper_newcase3)
count_outlier_newcase3<-sum(table(outlier_newcase3))

outlier_newcase<-c(count_outlier_newcase1, count_outlier_newcase2, count_outlier_newcase3)

barplot(outlier_newcase,
        xlab = "Country",
        ylab = "Outliers",
        main = "Outliers for new cases",
        names.arg = national,
        col = rgb (67 / 255, 128 / 255, 13 / 255))

  # 6/ Ve bieu do pho dat nuoc xuat hien outliers cho tu vong
Q_newdeathcountry1<-quantile(c1_data$new_deaths)
Q_newdeathcountry2<-quantile(c2_data$new_deaths)
Q_newdeathcountry3<-quantile(c3_data$new_deaths)

IQRnewdeath1<-(Q_newdeathcountry1[[4]] - Q_newdeathcountry1[[2]])
lower_newdeath1<-(Q_newdeathcountry1[[2]] - 1.5 * IQRnewdeath1)
upper_newdeath1<-(Q_newdeathcountry1[[4]] + 1.5 * IQRnewdeath1)
outlier_newdeath1<-subset(c1_data, c1_data$new_deaths < lower_newdeath1 | c1_data$new_deaths > upper_newdeath1)
count_outlier_newdeath1<-sum(table(outlier_newdeath1))

IQRnewdeath2<-(Q_newdeathcountry2[[4]] - Q_newdeathcountry2[[2]])
lower_newdeath2<-(Q_newdeathcountry2[[2]] - 1.5 * IQRnewdeath2)
upper_newdeath2<-(Q_newdeathcountry2[[4]] + 1.5 * IQRnewdeath2)
outlier_newdeath2<-subset(c2_data, c2_data$new_deaths < lower_newdeath2 | c2_data$new_deaths > upper_newdeath2)
count_outlier_newdeath2<-sum(table(outlier_newdeath2))

IQRnewdeath3<-(Q_newdeathcountry3[[4]]-Q_newdeathcountry3[[2]])
lower_newdeath3<-(Q_newdeathcountry3[[2]] - 1.5 * IQRnewdeath3)
upper_newdeath3<-(Q_newdeathcountry3[[4]] + 1.5 * IQRnewdeath3)
outlier_newdeath3<-subset(c3_data, c3_data$new_deaths < lower_newdeath3 | c3_data$new_deaths > upper_newdeath3)
count_outlier_newdeath3<-sum(table(outlier_newdeath3))

outlier_newdeath<-c(count_outlier_newdeath1, count_outlier_newdeath2, count_outlier_newdeath3)

barplot(outlier_newdeath,
        xlab = "Country",
        ylab = "Outliers",
        main = "Outliers fow new deaths",
        names.arg = national,
        col = rgb (67 / 255, 128 / 255, 13 / 255))

# v> NHOM CAU HOI LIEN QUAN DEN TRUC QUAN DU LIEU THEO THOI GIAN LA THANG

# vi> NHOM CAU HOI LIEN QUAN DEN TRUC QUAN DU LIEU THEO TRUNG BINH 7 NGAY GAN NHAT

# vii> NHOM CAU HOI LIEN QUAN DEN TAT CA QUOC GIA THEO THOI GIAN LA THANG

# viii> NHOM CAU HOI LIEN QUAN DEN TAT CA QUOC GIA THEO TRUNG BINH 7 NGAY GAN NHAT

# ix> NHOM CAU HOI LIEN QUAN DEN SU TUONG QUAN GIUA NHIEM BENH VA TU VONG

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

  # 1/ Ve bieu do
# Getting total cases/deaths of each country
AND_totalcases<-0
AND_totaldeaths<-0
for (i in c(1:(as.numeric(which.max(mdy(AND_data$date))))))
{
  AND_totalcases<-AND_totalcases+AND_data$new_cases[i]
  AND_totaldeaths<-AND_totaldeaths+AND_data$new_deaths[i]
}  

SVN_totalcases<-0
SVN_totaldeaths<-0
for (i in c(1:(as.numeric(which.max(mdy(SVN_data$date))))))
{
  SVN_totalcases<-SVN_totalcases+SVN_data$new_cases[i]
  SVN_totaldeaths<-SVN_totaldeaths+SVN_data$new_deaths[i]
}  

GBR_totalcases<-0
GBR_totaldeaths<-0
for (i in c(1:(as.numeric(which.max(mdy(GBR_data$date))))))
{
  GBR_totalcases<-GBR_totalcases+GBR_data$new_cases[i]
  GBR_totaldeaths<-GBR_totaldeaths+GBR_data$new_deaths[i]
}  
rm(i)

# Calculating accumulative data of each country
AND_aCases<-c(1:(as.numeric(which.max(mdy(AND_data$date)))))
AND_aDeaths<-c(1:(as.numeric(which.max(mdy(AND_data$date)))))
for (i in c(1:(as.numeric(which.max(mdy(AND_data$date))))))
{
  sumCases<-0
  sumDeaths<-0
  for (j in c(1:i))
  {  
    sumCases<-sumCases+AND_data$new_cases[j]
    sumDeaths<-sumDeaths+AND_data$new_deaths[j]   
  }
  AND_aCases[i]<-sumCases/AND_totalcases
  AND_aDeaths[i]<-sumDeaths/AND_totaldeaths
}

SVN_aCases<-c(1:(as.numeric(which.max(mdy(SVN_data$date)))))
SVN_aDeaths<-c(1:(as.numeric(which.max(mdy(SVN_data$date)))))
for (i in c(1:(as.numeric(which.max(mdy(SVN_data$date))))))
{
  sumCases<-0
  sumDeaths<-0
  for (j in c(1:i))
  {  
    sumCases<-sumCases+SVN_data$new_cases[j]
    sumDeaths<-sumDeaths+SVN_data$new_deaths[j]   
  }
  SVN_aCases[i]<-sumCases/SVN_totalcases
  SVN_aDeaths[i]<-sumDeaths/SVN_totaldeaths
}

GBR_aCases<-c(1:(as.numeric(which.max(mdy(GBR_data$date)))))
GBR_aDeaths<-c(1:(as.numeric(which.max(mdy(GBR_data$date)))))
for (i in c(1:(as.numeric(which.max(mdy(GBR_data$date))))))
{
  sumCases<-0
  sumDeaths<-0
  for (j in c(1:i))
  {  
    sumCases<-sumCases+GBR_data$new_cases[j]
    sumDeaths<-sumDeaths+GBR_data$new_deaths[j]   
  }
  GBR_aCases[i]<-sumCases/GBR_totalcases
  GBR_aDeaths[i]<-sumDeaths/GBR_totaldeaths
}
rm(i,j)

# plotting accumulative data
for (i in c(1:3))
{
  if (i==1)  
  {
    plot(c(1:which.max(mdy(AND_data$date))),AND_aCases,
         xlab = "Days",
         ylab = "Accumulative New Cases",
         main = national[i],
         names.arg = AND_data$date[1:which.max(mdy(AND_data$date))],
         col = "yellow",
         pch=20)
    points(c(1:which.max(mdy(AND_data$date))),AND_aDeaths,col="red",pch=20)
  }
  else if (i==2)
  {
    plot(c(1:which.max(mdy(SVN_data$date))),SVN_aCases,
         xlab = "Days",
         ylab = "Accumulative New Cases",
         main = national[i],
         names.arg = SVN_data$date[1:which.max(mdy(SVN_data$date))],
         col = "yellow",
         pch=20)
    points(c(1:which.max(mdy(SVN_data$date))),SVN_aDeaths,col="red",pch=20)
  }
  else if (i==3)
  {  
    plot(c(1:which.max(mdy(GBR_data$date))),GBR_aCases,
         xlab = "Days",
         ylab = "Accumulative New Cases",
         main = national[i],
         names.arg = GBR_data$date[1:which.max(mdy(GBR_data$date))],
         col = "yellow",
         pch=20)
    points(c(1:which.max(mdy(GBR_data$date))),GBR_aDeaths,col="red",pch=20)
  }
}

rm(i,sumCases,sumDeaths)

  # 2/Xet tuong quan trong moi thang
From<-c()
To<-c()
Country<-c()
R<-c()
years<-c("2020","2021","2022")
months<-c("1","2","4","7")
longerMonths<-c("1","3","5","7","8","10","12")
for (i in c(1:3))
  for (j in c(1:4))
  {
    startTime<-paste(months[j],"1",years[i],sep="/")
    if (months[j]%in%longerMonths)
      endTime<-paste(months[j],"31",years[i],sep="/")
    else
      if (months[j]!="2")
        endTime<-paste(months[j],"30",years[i],sep="/")
      else 
        if (years[i]=="2020")
          endTime<-paste(months[j],"29",years[i],sep="/")
        else
          endTime<-paste(months[j],"28",years[i],sep="/")
        
        if (mdy(endTime)<=mdy(AND_data$date[1])|mdy(endTime)<=mdy(SVN_data$date[1])|mdy(endTime)<=mdy(GBR_data$date[1]))
          break;
        AND_dataMonth<-filter(AND_data,mdy(AND_data$date)>=mdy(startTime),mdy(AND_data$date)<=mdy(endTime),.preserve=FALSE)
        SVN_dataMonth<-filter(SVN_data,mdy(SVN_data$date)>=mdy(startTime),mdy(SVN_data$date)<=mdy(endTime),.preserve=FALSE)
        GBR_dataMonth<-filter(GBR_data,mdy(GBR_data$date)>=mdy(startTime),mdy(GBR_data$date)<=mdy(endTime),.preserve=FALSE)
        
        R<-c(R,cor(c(1:which.max(mdy(AND_dataMonth$date))),AND_dataMonth$new_cases))
        Country<-c(Country,"AND")
        R<-c(R,cor(c(1:which.max(mdy(SVN_dataMonth$date))),SVN_dataMonth$new_cases))
        Country<-c(Country,"SVN")
        R<-c(R,cor(c(1:which.max(mdy(GBR_dataMonth$date))),GBR_dataMonth$new_cases))
        Country<-c(Country,"GBR")
        for (k in c(1:3))
        {
          From<-c(From,startTime)
          To<-c(To,endTime)
        }
        #plotting
        currentPeriod<-paste(startTime,endTime,sep = " to ")
        par(mar=c(3.9,3.9,1.5,0.3))
        plot(c(1:which.max(mdy(AND_dataMonth$date))),AND_dataMonth$new_cases,
             xlab = "Days",
             ylab = "New cases",
             main = paste("Andorra",currentPeriod,sep=" "),
             names.arg = AND_dataMonth$date[1:which.max(mdy(AND_dataMonth$date))])
        abline(lm(new_cases~c(1:which.max(mdy(AND_dataMonth$date))),data=AND_dataMonth),col='red')
        
        par(mar=c(3.9,3.9,1.5,0.3))
        plot(c(1:which.max(mdy(SVN_dataMonth$date))),SVN_dataMonth$new_cases,
             xlab = "Days",
             ylab = "New cases",
             main = paste("Slovenia",currentPeriod,sep=" "),
             names.arg = SVN_dataMonth$date[1:which.max(mdy(SVN_dataMonth$date))])
        abline(lm(new_cases~c(1:which.max(mdy(SVN_dataMonth$date))),data=SVN_dataMonth),col='green')
        
        par(mar=c(3.9,3.9,1.5,0.3))
        plot(c(1:which.max(mdy(GBR_dataMonth$date))),GBR_dataMonth$new_cases,
             xlab = "Days",
             ylab = "New cases",
             main = paste("United Kingdom",currentPeriod,sep=" "),
             names.arg = GBR_dataMonth$date[1:which.max(mdy(GBR_dataMonth$date))])
        abline(lm(new_cases~c(1:which.max(mdy(GBR_dataMonth$date))),data=GBR_dataMonth),col='blue')
  }


data_cor<-data.frame(From,To,Country,R)
print(data_cor)
rm(startTime,endTime,i,j,k,From,To,R,years,months,longerMonths,currentPeriod,Country,AND_dataMonth,GBR_dataMonth,SVN_dataMonth)

  # 3/ Xet tuong quan trong moi thang theo trung binh 7 ngay gan nhat
From<-c()
To<-c()
Country<-c()
R<-c()
years<-c("2020","2021","2022")
months<-c("1","2","4","7")
longerMonths<-c("1","3","5","7","8","10","12")
for (i in c(1:3))
  for (j in c(1:4))
  {
    if (months[j]%in%longerMonths)
      startTime<-paste(months[j],"25",years[i],sep="/")
    else
      if (months[j]!="2")
        startTime<-paste(months[j],"24",years[i],sep="/")
      else 
        if (years[i]=="2020")
          startTime<-paste(months[j],"23",years[i],sep="/")
        else
          startTime<-paste(months[j],"22",years[i],sep="/")
        
        if (months[j]%in%longerMonths)
          endTime<-paste(months[j],"31",years[i],sep="/")
        else
          if (months[j]!="2")
            endTime<-paste(months[j],"30",years[i],sep="/")
          else 
            if (years[i]=="2020")
              endTime<-paste(months[j],"29",years[i],sep="/")
            else
              endTime<-paste(months[j],"28",years[i],sep="/")
            
            if (mdy(endTime)<=mdy(AND_data$date[1])|mdy(endTime)<=mdy(SVN_data$date[1])|mdy(endTime)<=mdy(GBR_data$date[1]))
              break;
            AND_dataMonth<-filter(AND_data,mdy(AND_data$date)>=mdy(startTime),mdy(AND_data$date)<=mdy(endTime),.preserve=FALSE)
            SVN_dataMonth<-filter(SVN_data,mdy(SVN_data$date)>=mdy(startTime),mdy(SVN_data$date)<=mdy(endTime),.preserve=FALSE)
            GBR_dataMonth<-filter(GBR_data,mdy(GBR_data$date)>=mdy(startTime),mdy(GBR_data$date)<=mdy(endTime),.preserve=FALSE)
            
            R<-c(R,cor(c(1:which.max(mdy(AND_dataMonth$date))),AND_dataMonth$new_cases))
            Country<-c(Country,"AND")
            R<-c(R,cor(c(1:which.max(mdy(SVN_dataMonth$date))),SVN_dataMonth$new_cases))
            Country<-c(Country,"SVN")
            R<-c(R,cor(c(1:which.max(mdy(GBR_dataMonth$date))),GBR_dataMonth$new_cases))
            Country<-c(Country,"GBR")
            for (k in c(1:3))
            {
              From<-c(From,startTime)
              To<-c(To,endTime)
            }
            #plotting
            currentPeriod<-paste(startTime,endTime,sep = " to ")
            par(mar=c(3.9,3.9,1.5,0.3))
            plot(c(1:which.max(mdy(AND_dataMonth$date))),AND_dataMonth$new_cases,
                 xlab = "Days",
                 ylab = "New cases",
                 main = paste("Andorra",currentPeriod,sep=" "),
                 names.arg = AND_dataMonth$date[1:which.max(mdy(AND_dataMonth$date))])
            abline(lm(new_cases~c(1:which.max(mdy(AND_dataMonth$date))),data=AND_dataMonth),col='red')
            
            par(mar=c(3.9,3.9,1.5,0.3))
            plot(c(1:which.max(mdy(SVN_dataMonth$date))),SVN_dataMonth$new_cases,
                 xlab = "Days",
                 ylab = "New cases",
                 main = paste("Slovenia",currentPeriod,sep=" "),
                 names.arg = SVN_dataMonth$date[1:which.max(mdy(SVN_dataMonth$date))])
            abline(lm(new_cases~c(1:which.max(mdy(SVN_dataMonth$date))),data=SVN_dataMonth),col='green')
            
            par(mar=c(3.9,3.9,1.5,0.3))
            plot(c(1:which.max(mdy(GBR_dataMonth$date))),GBR_dataMonth$new_cases,
                 xlab = "Days",
                 ylab = "New cases",
                 main = paste("United Kingdom",currentPeriod,sep=" "),
                 names.arg = GBR_dataMonth$date[1:which.max(mdy(GBR_dataMonth$date))])
            abline(lm(new_cases~c(1:which.max(mdy(GBR_dataMonth$date))),data=GBR_dataMonth),col='blue')
  }

data_cor<-data.frame(From,To,Country,R)
print(data_cor)
rm(startTime,endTime,i,j,k,From,To,R,years,months,longerMonths,currentPeriod,Country,AND_dataMonth,SVN_dataMonth,GBR_dataMonth)

# x> NHOM CAU HOI RIENG

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