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

  # 1/ Bieu do tan so tich luy quoc gia cho cac chau luc
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
     main = "Plot",
     xlab = "Contries",
     ylab = "Continents")
lines(breaks, cumfreq, col = "red")

  # 2/ Bieu do tan so tuong doi quoc gia cho cac chau luc
histogram(num_countries,
          breaks = 4,
          col = rgb (67 / 255, 128 / 255, 13 / 255),
          main = "Relative Frequency Histogram",
          xlab = "Number of Countries",
          ylab = "Percent of Total")

  # 3/ Bieu do the hien nhiem benh da bao cao cua cac quoc gia trong 7 ngay cuoi cung cua nam cuoi cung
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

  # 4/ Bieu do the hien tu vong da bao cao cua cac quoc gia trong 7 ngay cuoi cung cua nam cuoi cung
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

  # 5/ Bieu do pho dat nuoc xuat hien outliers cho nhiem benh
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

  # 6/ Bieu do pho dat nuoc xuat hien outliers cho tu vong
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
