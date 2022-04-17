# Set library
library("tidyverse")
library("plyr")
library("moments")
library("csv")
library("csvread")
library("readr")
library("rlist")
library("lubridate")

# Set data
setwd("C:/Users/tminh/Desktop/BTL CTRR")
data<-read_csv("owid-covid-data.csv")     # data
iso_code<-data[["iso_code"]]              # iso_code
continent<-data[["continent"]]            # continent
location<-data[["location"]]              # location
date<-data[["date"]]                      # date
new_cases<-data[["new_cases"]]            # new_cases
new_deaths<-data[["new_deaths"]]          # new_deaths

# Axis
c1<-c("Andorra")
c2<-c("Slovenia")
c3<-c("United Kingdom")
ct<-c(c1,c2,c3)
s2020<-c("4/2020","7/2020")
s2021<-c("1/2021","2/2021","4/2021","7/2021")
s2022<-c("1/2022","2/2022")
ss2020<-c("11/2020","12/2020")
ss2021<-c("11/2021","12/2021")
sss2020<-c("4/2020","4/2020","7/2020","7/2020")
sss2021<-c("1/2021","1/2021","2/2021","2/2021","4/2021","4/2021","7/2021","7/2021")
sss2022<-c("1/2022","1/2022","2/2022","2/2022")
ssss2020<-c("11/2020","11/2020","12/2020","12/2020")
ssss2021<-c("11/2021","11/2021","12/2021","12/2021")
send<-c("4/2020","7/2020","1/2021","2/2021","4/2021","7/2021","1/2022","2/2022")
s<-c(s2020,s2021,s2022)

# Color
color1<-rgb (208 / 255, 25 / 255, 31 / 255)
color2<-rgb (15 / 255, 142 / 255, 11 / 255)
color3<-rgb (36 / 255, 41 / 255, 172 / 255)
# color4<-rgb (208 / 255, 25 / 255, 31 / 255)
# color5<-rgb (15 / 255, 142 / 255, 11 / 255)
# color6<-rgb (36 / 255, 41 / 255, 172 / 255)
color<-c(color1, color2, color3)

# Preprocessing
data$date <- as.POSIXct(data$date, format="%m/%d/%Y")
# get Andorra data 
c1_2020_4<-subset(data,year(data$date)==2020&month(data$date)==4&data$location==c1)
c1_2020_7<-subset(data,year(data$date)==2020&month(data$date)==7&data$location==c1)
c1_2021_1<-subset(data,year(data$date)==2021&month(data$date)==1&data$location==c1)
c1_2021_2<-subset(data,year(data$date)==2021&month(data$date)==2&data$location==c1)
c1_2021_4<-subset(data,year(data$date)==2021&month(data$date)==4&data$location==c1)
c1_2021_7<-subset(data,year(data$date)==2021&month(data$date)==7&data$location==c1)
c1_2022_1<-subset(data,year(data$date)==2022&month(data$date)==1&data$location==c1)
c1_2022_2<-subset(data,year(data$date)==2022&month(data$date)==2&data$location==c1)
c1_2020_11<-subset(data,year(data$date)==2020&month(data$date)==11&data$location==c1)
c1_2020_12<-subset(data,year(data$date)==2020&month(data$date)==12&data$location==c1)
c1_2021_11<-subset(data,year(data$date)==2021&month(data$date)==11&data$location==c1)
c1_2021_12<-subset(data,year(data$date)==2021&month(data$date)==12&data$location==c1)
# get Slovenia data 
c2_2020_4<-subset(data,year(data$date)==2020&month(data$date)==4&data$location==c2)
c2_2020_7<-subset(data,year(data$date)==2020&month(data$date)==7&data$location==c2)
c2_2021_1<-subset(data,year(data$date)==2021&month(data$date)==1&data$location==c2)
c2_2021_2<-subset(data,year(data$date)==2021&month(data$date)==2&data$location==c2)
c2_2021_4<-subset(data,year(data$date)==2021&month(data$date)==4&data$location==c2)
c2_2021_7<-subset(data,year(data$date)==2021&month(data$date)==7&data$location==c2)
c2_2022_1<-subset(data,year(data$date)==2022&month(data$date)==1&data$location==c2)
c2_2022_2<-subset(data,year(data$date)==2022&month(data$date)==2&data$location==c2)
c2_2020_11<-subset(data,year(data$date)==2020&month(data$date)==11&data$location==c2)
c2_2020_12<-subset(data,year(data$date)==2020&month(data$date)==12&data$location==c2)
c2_2021_11<-subset(data,year(data$date)==2021&month(data$date)==11&data$location==c2)
c2_2021_12<-subset(data,year(data$date)==2021&month(data$date)==12&data$location==c2)
# get UK data 
c3_2020_4<-subset(data,year(data$date)==2020&month(data$date)==4&data$location==c3)
c3_2020_7<-subset(data,year(data$date)==2020&month(data$date)==7&data$location==c3)
c3_2021_1<-subset(data,year(data$date)==2021&month(data$date)==1&data$location==c3)
c3_2021_2<-subset(data,year(data$date)==2021&month(data$date)==2&data$location==c3)
c3_2021_4<-subset(data,year(data$date)==2021&month(data$date)==4&data$location==c3)
c3_2021_7<-subset(data,year(data$date)==2021&month(data$date)==7&data$location==c3)
c3_2022_1<-subset(data,year(data$date)==2022&month(data$date)==1&data$location==c3)
c3_2022_2<-subset(data,year(data$date)==2022&month(data$date)==2&data$location==c3)
c3_2020_11<-subset(data,year(data$date)==2020&month(data$date)==11&data$location==c3)
c3_2020_12<-subset(data,year(data$date)==2020&month(data$date)==12&data$location==c3)
c3_2021_11<-subset(data,year(data$date)==2021&month(data$date)==11&data$location==c3)
c3_2021_12<-subset(data,year(data$date)==2021&month(data$date)==12&data$location==c3)
# Evaluate sum--------------------------------------------------------------------------------------------------------
# New_cases----------------------------------------------------------------------------------------------------
# Andorra
sum1_2020_4c<-sum(c1_2020_4$new_cases, na.rm=TRUE)
sum1_2020_7c<-sum(c1_2020_7$new_cases, na.rm=TRUE)
sum1_2021_1c<-sum(c1_2021_1$new_cases, na.rm=TRUE)
sum1_2021_2c<-sum(c1_2021_2$new_cases, na.rm=TRUE)
sum1_2021_4c<-sum(c1_2021_4$new_cases, na.rm=TRUE)
sum1_2021_7c<-sum(c1_2021_7$new_cases, na.rm=TRUE)
sum1_2022_1c<-sum(c1_2022_1$new_cases, na.rm=TRUE)
sum1_2022_2c<-sum(c1_2022_2$new_cases, na.rm=TRUE)
sum1_2020_11c<-sum(c1_2020_11$new_cases, na.rm=TRUE)
sum1_2020_12c<-sum(c1_2020_12$new_cases, na.rm=TRUE)
sum1_2021_11c<-sum(c1_2021_11$new_cases, na.rm=TRUE)
sum1_2021_12c<-sum(c1_2021_12$new_cases, na.rm=TRUE)
# Slovenia
sum2_2020_4c<-sum(c2_2020_4$new_cases, na.rm=TRUE)
sum2_2020_7c<-sum(c2_2020_7$new_cases, na.rm=TRUE)
sum2_2021_1c<-sum(c2_2021_1$new_cases, na.rm=TRUE)
sum2_2021_2c<-sum(c2_2021_2$new_cases, na.rm=TRUE)
sum2_2021_4c<-sum(c2_2021_4$new_cases, na.rm=TRUE)
sum2_2021_7c<-sum(c2_2021_7$new_cases, na.rm=TRUE)
sum2_2022_1c<-sum(c2_2022_1$new_cases, na.rm=TRUE)
sum2_2022_2c<-sum(c2_2022_2$new_cases, na.rm=TRUE)
sum2_2020_11c<-sum(c2_2020_11$new_cases, na.rm=TRUE)
sum2_2020_12c<-sum(c2_2020_12$new_cases, na.rm=TRUE)
sum2_2021_11c<-sum(c2_2021_11$new_cases, na.rm=TRUE)
sum2_2021_12c<-sum(c2_2021_12$new_cases, na.rm=TRUE)
# UK
sum3_2020_4c<-sum(c3_2020_4$new_cases, na.rm=TRUE)
sum3_2020_7c<-sum(c3_2020_7$new_cases, na.rm=TRUE)
sum3_2021_1c<-sum(c3_2021_1$new_cases, na.rm=TRUE)
sum3_2021_2c<-sum(c3_2021_2$new_cases, na.rm=TRUE)
sum3_2021_4c<-sum(c3_2021_4$new_cases, na.rm=TRUE)
sum3_2021_7c<-sum(c3_2021_7$new_cases, na.rm=TRUE)
sum3_2022_1c<-sum(c3_2022_1$new_cases, na.rm=TRUE)
sum3_2022_2c<-sum(c3_2022_2$new_cases, na.rm=TRUE)
sum3_2020_11c<-sum(c3_2020_11$new_cases, na.rm=TRUE)
sum3_2020_12c<-sum(c3_2020_12$new_cases, na.rm=TRUE)
sum3_2021_11c<-sum(c3_2021_11$new_cases, na.rm=TRUE)
sum3_2021_12c<-sum(c3_2021_12$new_cases, na.rm=TRUE)

# New_deaths
# Andorra
sum1_2020_4d<-sum(c1_2020_4$new_deaths, na.rm=TRUE)
sum1_2020_7d<-sum(c1_2020_7$new_deaths, na.rm=TRUE)
sum1_2021_1d<-sum(c1_2021_1$new_deaths, na.rm=TRUE)
sum1_2021_2d<-sum(c1_2021_2$new_deaths, na.rm=TRUE)
sum1_2021_4d<-sum(c1_2021_4$new_deaths, na.rm=TRUE)
sum1_2021_7d<-sum(c1_2021_7$new_deaths, na.rm=TRUE)
sum1_2022_1d<-sum(c1_2022_1$new_deaths, na.rm=TRUE)
sum1_2022_2d<-sum(c1_2022_2$new_deaths, na.rm=TRUE)
sum1_2020_11d<-sum(c1_2020_11$new_deaths, na.rm=TRUE)
sum1_2020_12d<-sum(c1_2020_12$new_deaths, na.rm=TRUE)
sum1_2021_11d<-sum(c1_2021_11$new_deaths, na.rm=TRUE)
sum1_2021_12d<-sum(c1_2021_12$new_deaths, na.rm=TRUE)
# Slovenia
sum2_2020_4d<-sum(c2_2020_4$new_deaths, na.rm=TRUE)
sum2_2020_7d<-sum(c2_2020_7$new_deaths, na.rm=TRUE)
sum2_2021_1d<-sum(c2_2021_1$new_deaths, na.rm=TRUE)
sum2_2021_2d<-sum(c2_2021_2$new_deaths, na.rm=TRUE)
sum2_2021_4d<-sum(c2_2021_4$new_deaths, na.rm=TRUE)
sum2_2021_7d<-sum(c2_2021_7$new_deaths, na.rm=TRUE)
sum2_2022_1d<-sum(c2_2022_1$new_deaths, na.rm=TRUE)
sum2_2022_2d<-sum(c2_2022_2$new_deaths, na.rm=TRUE)
sum2_2020_11d<-sum(c2_2020_11$new_deaths, na.rm=TRUE)
sum2_2020_12d<-sum(c2_2020_12$new_deaths, na.rm=TRUE)
sum2_2021_11d<-sum(c2_2021_11$new_deaths, na.rm=TRUE)
sum2_2021_12d<-sum(c2_2021_12$new_deaths, na.rm=TRUE)
# UK
sum3_2020_4d<-sum(c3_2020_4$new_deaths, na.rm=TRUE)
sum3_2020_7d<-sum(c3_2020_7$new_deaths, na.rm=TRUE)
sum3_2021_1d<-sum(c3_2021_1$new_deaths, na.rm=TRUE)
sum3_2021_2d<-sum(c3_2021_2$new_deaths, na.rm=TRUE)
sum3_2021_4d<-sum(c3_2021_4$new_deaths, na.rm=TRUE)
sum3_2021_7d<-sum(c3_2021_7$new_deaths, na.rm=TRUE)
sum3_2022_1d<-sum(c3_2022_1$new_deaths, na.rm=TRUE)
sum3_2022_2d<-sum(c3_2022_2$new_deaths, na.rm=TRUE)
sum3_2020_11d<-sum(c3_2020_11$new_deaths, na.rm=TRUE)
sum3_2020_12d<-sum(c3_2020_12$new_deaths, na.rm=TRUE)
sum3_2021_11d<-sum(c3_2021_11$new_deaths, na.rm=TRUE)
sum3_2021_12d<-sum(c3_2021_12$new_deaths, na.rm=TRUE)

# Define
sumc1_2020<-c(sum1_2020_4c,sum1_2020_7c)
sumc2_2020<-c(sum2_2020_4c,sum2_2020_7c)
sumc3_2020<-c(sum3_2020_4c,sum3_2020_7c)

sumc1_2021<-c(sum1_2021_1c,sum1_2021_2c,sum1_2021_4c,sum1_2021_7c)
sumc2_2021<-c(sum2_2021_1c,sum2_2021_2c,sum2_2021_4c,sum2_2021_7c)
sumc3_2021<-c(sum3_2021_1c,sum3_2021_2c,sum3_2021_4c,sum3_2021_7c)

sumc1_2022<-c(sum1_2022_1c,sum1_2022_2c)
sumc2_2022<-c(sum2_2022_1c,sum2_2022_2c)
sumc3_2022<-c(sum3_2022_1c,sum3_2022_2c)

sumd1_2020<-c(sum1_2020_4d,sum1_2020_7d)
sumd2_2020<-c(sum2_2020_4d,sum2_2020_7d)
sumd3_2020<-c(sum3_2020_4d,sum3_2020_7d)

sumd1_2021<-c(sum1_2021_1d,sum1_2021_2d,sum1_2021_4d,sum1_2021_7d)
sumd2_2021<-c(sum2_2021_1d,sum2_2021_2d,sum2_2021_4d,sum2_2021_7d)
sumd3_2021<-c(sum3_2021_1d,sum3_2021_2d,sum3_2021_4d,sum3_2021_7d)

sumd1_2022<-c(sum1_2022_1d,sum1_2022_2d)
sumd2_2022<-c(sum2_2022_1d,sum2_2022_2d)
sumd3_2022<-c(sum3_2022_1d,sum3_2022_2d)

ssumc1_2020<-c(sum1_2020_11c,sum1_2020_12c)
ssumc2_2020<-c(sum2_2020_11c,sum2_2020_12c)
ssumc3_2020<-c(sum3_2020_11c,sum3_2020_12c)

ssumc1_2021<-c(sum1_2021_11c,sum1_2021_12c)
ssumc2_2021<-c(sum2_2021_11c,sum2_2021_12c)
ssumc3_2021<-c(sum3_2021_11c,sum3_2021_12c)

ssumd1_2020<-c(sum1_2020_11d,sum1_2020_12d)
ssumd2_2020<-c(sum2_2020_11d,sum2_2020_12d)
ssumd3_2020<-c(sum3_2020_11d,sum3_2020_12d)

ssumd1_2021<-c(sum1_2021_11d,sum1_2021_12d)
ssumd2_2021<-c(sum2_2021_11d,sum2_2021_12d)
ssumd3_2021<-c(sum3_2021_11d,sum3_2021_12d)

# <1> Bieu do the hien thu thap du lieu nhiem benh cho tung thang
 # 2020
 barplot(sumc1_2020, xlab="Months", ylab="New_cases",main=ct[1], names.arg=s2020, col=color[1])
 barplot(sumc2_2020, xlab="Months", ylab="New_cases",main=ct[2], names.arg=s2020, col=color[2])
 barplot(sumc3_2020, xlab="Months", ylab="New_cases",main=ct[3], names.arg=s2020, col=color[3])
 # 2021
 barplot(sumc1_2021, xlab="Months", ylab="New_cases",main=ct[1], names.arg=s2021, col=color[1])
 barplot(sumc2_2021, xlab="Months", ylab="New_cases",main=ct[2], names.arg=s2021, col=color[2])
 barplot(sumc3_2021, xlab="Months", ylab="New_cases",main=ct[3], names.arg=s2021, col=color[3])
 # 2022
 barplot(sumc1_2022, xlab="Months", ylab="New_cases",main=ct[1], names.arg=s2022, col=color[1])
 barplot(sumc2_2022, xlab="Months", ylab="New_cases",main=ct[2], names.arg=s2022, col=color[2])
 barplot(sumc3_2022, xlab="Months", ylab="New_cases",main=ct[3], names.arg=s2022, col=color[3])

# <2> Bieu do the hien thu thap du lieu tu vong cho tung thang
 # 2020
 barplot(sumd1_2020, xlab="Months", ylab="New_cases",main=ct[1], names.arg=s2020, col=color[1])
 barplot(sumd2_2020, xlab="Months", ylab="New_cases",main=ct[2], names.arg=s2020, col=color[2])
 barplot(sumd3_2020, xlab="Months", ylab="New_cases",main=ct[3], names.arg=s2020, col=color[3])
 # 2021
 barplot(sumd1_2021, xlab="Months", ylab="New_cases",main=ct[1], names.arg=s2021, col=color[1])
 barplot(sumd2_2021, xlab="Months", ylab="New_cases",main=ct[2], names.arg=s2021, col=color[2])
 barplot(sumd3_2021, xlab="Months", ylab="New_cases",main=ct[3], names.arg=s2021, col=color[3])
 # 2022
 barplot(sumd1_2022, xlab="Months", ylab="New_cases",main=ct[1], names.arg=s2022, col=color[1])
 barplot(sumd2_2022, xlab="Months", ylab="New_cases",main=ct[2], names.arg=s2022, col=color[2])
 barplot(sumd3_2022, xlab="Months", ylab="New_cases",main=ct[3], names.arg=s2022, col=color[3])

# <3> Bieu do the hien thu thap du lieu tu vong va nhiem benh cho tung thang
 # 2020
 barplot(c(rbind(sumc1_2020,sumd1_2020)), xlab="Months", ylab="New_cases/New_deaths",main=ct[1], names.arg=sss2020, col=c(color1,color2))
 legend("topright", 
        legend = c("New_cases", "New_deaths"), 
        fill = c(color1,color2))
 barplot(c(rbind(sumc2_2020,sumd2_2020)), xlab="Months", ylab="New_cases/New_deaths",main=ct[2], names.arg=sss2020, col=c(color1,color2))
 legend("topright", 
        legend = c("New_cases", "New_deaths"), 
        fill = c(color1,color2))
 barplot(c(rbind(sumc3_2020,sumd3_2020)), xlab="Months", ylab="New_cases/New_deaths",main=ct[3], names.arg=sss2020, col=c(color1,color2))
 legend("topright", 
        legend = c("New_cases", "New_deaths"), 
        fill = c(color1,color2))
 # 2021
 barplot(c(rbind(sumc1_2021,sumd1_2021)), xlab="Months", ylab="New_cases/New_deaths",main=ct[1], names.arg=sss2021, col=c(color1,color2))
 legend("topright", 
        legend = c("New_cases", "New_deaths"), 
        fill = c(color1,color2))
 barplot(c(rbind(sumc2_2021,sumd2_2021)), xlab="Months", ylab="New_cases/New_deaths",main=ct[2], names.arg=sss2021, col=c(color1,color2))
 legend("topright", 
        legend = c("New_cases", "New_deaths"), 
        fill = c(color1,color2))
 barplot(c(rbind(sumc3_2021,sumd3_2021)), xlab="Months", ylab="New_cases/New_deaths",main=ct[3], names.arg=sss2021, col=c(color1,color2))
 legend("topright", 
        legend = c("New_cases", "New_deaths"), 
        fill = c(color1,color2))
 # 2022
 barplot(c(rbind(sumc1_2022,sumd1_2022)), xlab="Months", ylab="New_cases/New_deaths",main=ct[1], names.arg=sss2022, col=c(color1,color2))
 legend("topright", 
        legend = c("New_cases", "New_deaths"), 
        fill = c(color1,color2))
 barplot(c(rbind(sumc2_2022,sumd2_2022)), xlab="Months", ylab="New_cases/New_deaths",main=ct[2], names.arg=sss2022, col=c(color1,color2))
 legend("topright", 
        legend = c("New_cases", "New_deaths"), 
        fill = c(color1,color2))
 barplot(c(rbind(sumc3_2022,sumd3_2022)), xlab="Months", ylab="New_cases/New_deaths",main=ct[3], names.arg=sss2022, col=c(color1,color2))
 legend("topright", 
        legend = c("New_cases", "New_deaths"), 
        fill = c(color1,color2))

# <4> Bieu do the hien thu thap du lieu nhiem benh gom 2 thang cuoi cua nam
 # 2020
 barplot(ssumc1_2020, xlab="Months", ylab="New_cases",main=ct[1], names.arg=ss2020, col=color[1])
 barplot(ssumc2_2020, xlab="Months", ylab="New_cases",main=ct[2], names.arg=ss2020, col=color[2])
 barplot(ssumc3_2020, xlab="Months", ylab="New_cases",main=ct[3], names.arg=ss2020, col=color[3])
 # 2021
 barplot(ssumc1_2021, xlab="Months", ylab="New_cases",main=ct[1], names.arg=ss2021, col=color[1])
 barplot(ssumc2_2021, xlab="Months", ylab="New_cases",main=ct[2], names.arg=ss2021, col=color[2])
 barplot(ssumc3_2021, xlab="Months", ylab="New_cases",main=ct[3], names.arg=ss2021, col=color[3])

# <5> Bieu do the hien thu thap du lieu tu vong gom 2 thang cuoi cua nam
 # 2020
 barplot(ssumd1_2020, xlab="Months", ylab="New_cases",main=ct[1], names.arg=ss2020, col=color[1])
 barplot(ssumd2_2020, xlab="Months", ylab="New_cases",main=ct[2], names.arg=ss2020, col=color[2])
 barplot(ssumd3_2020, xlab="Months", ylab="New_cases",main=ct[3], names.arg=ss2020, col=color[3])
 #2021
 barplot(ssumd1_2021, xlab="Months", ylab="New_cases",main=ct[1], names.arg=ss2021, col=color[1])
 barplot(ssumd2_2021, xlab="Months", ylab="New_cases",main=ct[2], names.arg=ss2021, col=color[2])
 barplot(ssumd3_2021, xlab="Months", ylab="New_cases",main=ct[3], names.arg=ss2021, col=color[3])

# <6> Bieu do the hien thu thap du lieu tu vong va nhiem benh gom 2 thang cuoi cua nam 
 # 2020
 barplot(c(rbind(ssumc1_2020,ssumd1_2020)), xlab="Months", ylab="New_cases/New_deaths",main=ct[1], names.arg=ssss2020, col=c(color1,color2))
 legend("topright", 
        legend = c("New_cases", "New_deaths"), 
        fill = c(color1,color2))
 barplot(c(rbind(ssumc2_2020,ssumd2_2020)), xlab="Months", ylab="New_cases/New_deaths",main=ct[2], names.arg=ssss2020, col=c(color1,color2))
 legend("topright", 
        legend = c("New_cases", "New_deaths"), 
        fill = c(color1,color2))
 barplot(c(rbind(ssumc3_2020,ssumd3_2020)), xlab="Months", ylab="New_cases/New_deaths",main=ct[3], names.arg=ssss2020, col=c(color1,color2))
 legend("topright", 
        legend = c("New_cases", "New_deaths"), 
        fill = c(color1,color2))
 # 2021
 barplot(c(rbind(ssumc1_2021,ssumd1_2021)), xlab="Months", ylab="New_cases/New_deaths",main=ct[1], names.arg=ssss2021, col=c(color1,color2))
 legend("topright", 
        legend = c("New_cases", "New_deaths"), 
        fill = c(color1,color2))
 barplot(c(rbind(ssumc2_2021,ssumd2_2021)), xlab="Months", ylab="New_cases/New_deaths",main=ct[2], names.arg=ssss2021, col=c(color1,color2))
 legend("topright", 
        legend = c("New_cases", "New_deaths"), 
        fill = c(color1,color2))
 barplot(c(rbind(ssumc3_2021,ssumd3_2021)), xlab="Months", ylab="New_cases/New_deaths",main=ct[3], names.arg=ssss2021, col=c(color1,color2))
 legend("topright", 
        legend = c("New_cases", "New_deaths"), 
        fill = c(color1,color2))

# <7> Bieu do the hien thu thap du lieu nhiem benh tich luy cho tung thang
 # Andorra
 c1_61a<-subset(data, year(data$date)<2021 & month(data$date)<5&data$location==c1)
 c1_62a<-subset(data, year(data$date)<2021 & month(data$date)<8&data$location==c1)
 c1_63a<-subset(data, ((year(data$date)==2021 & month(data$date)<2)|year(data$date)==2020)&data$location==c1)
 c1_64a<-subset(data, ((year(data$date)==2021 & month(data$date)<3)|year(data$date)==2020)&data$location==c1)
 c1_65a<-subset(data, ((year(data$date)==2021 & month(data$date)<5)|year(data$date)==2020)&data$location==c1)
 c1_66a<-subset(data, ((year(data$date)==2021 & month(data$date)<8)|year(data$date)==2020)&data$location==c1)
 c1_67a<-subset(data, ((year(data$date)==2022 & month(data$date)<2)|year(data$date)==2020|year(data$date)==2021)&data$location==c1)
 c1_68a<-subset(data, ((year(data$date)==2022 & month(data$date)<3)|year(data$date)==2020|year(data$date)==2021)&data$location==c1)
 c1_1a=sum(c1_61a$new_cases,na.rm=TRUE)
 c1_2a=sum(c1_62a$new_cases,na.rm=TRUE)
 c1_3a=sum(c1_63a$new_cases,na.rm=TRUE)
 c1_4a=sum(c1_64a$new_cases,na.rm=TRUE)
 c1_5a=sum(c1_65a$new_cases,na.rm=TRUE)
 c1_6a=sum(c1_66a$new_cases,na.rm=TRUE)
 c1_7a=sum(c1_67a$new_cases,na.rm=TRUE)
 c1_8a=sum(c1_68a$new_cases,na.rm=TRUE)
 # Slovenia
 c2_61a<-subset(data, year(data$date)<2021 & month(data$date)<5&data$location==c2)
 c2_62a<-subset(data, year(data$date)<2021 & month(data$date)<8&data$location==c2)
 c2_63a<-subset(data, ((year(data$date)==2021 & month(data$date)<2)|year(data$date)==2020)&data$location==c2)
 c2_64a<-subset(data, ((year(data$date)==2021 & month(data$date)<3)|year(data$date)==2020)&data$location==c2)
 c2_65a<-subset(data, ((year(data$date)==2021 & month(data$date)<5)|year(data$date)==2020)&data$location==c2)
 c2_66a<-subset(data, ((year(data$date)==2021 & month(data$date)<8)|year(data$date)==2020)&data$location==c2)
 c2_67a<-subset(data, ((year(data$date)==2022 & month(data$date)<2)|year(data$date)==2020|year(data$date)==2021)&data$location==c2)
 c2_68a<-subset(data, ((year(data$date)==2022 & month(data$date)<3)|year(data$date)==2020|year(data$date)==2021)&data$location==c2)
 c2_1a=sum(c2_61a$new_cases,na.rm=TRUE)
 c2_2a=sum(c2_62a$new_cases,na.rm=TRUE)
 c2_3a=sum(c2_63a$new_cases,na.rm=TRUE)
 c2_4a=sum(c2_64a$new_cases,na.rm=TRUE)
 c2_5a=sum(c2_65a$new_cases,na.rm=TRUE)
 c2_6a=sum(c2_66a$new_cases,na.rm=TRUE)
 c2_7a=sum(c2_67a$new_cases,na.rm=TRUE)
 c2_8a=sum(c2_68a$new_cases,na.rm=TRUE)
 # UK
 c3_61a<-subset(data, year(data$date)<2021 & month(data$date)<5&data$location==c3)
 c3_62a<-subset(data, year(data$date)<2021 & month(data$date)<8&data$location==c3)
 c3_63a<-subset(data, ((year(data$date)==2021 & month(data$date)<2)|year(data$date)==2020)&data$location==c3)
 c3_64a<-subset(data, ((year(data$date)==2021 & month(data$date)<3)|year(data$date)==2020)&data$location==c3)
 c3_65a<-subset(data, ((year(data$date)==2021 & month(data$date)<5)|year(data$date)==2020)&data$location==c3)
 c3_66a<-subset(data, ((year(data$date)==2021 & month(data$date)<8)|year(data$date)==2020)&data$location==c3)
 c3_67a<-subset(data, ((year(data$date)==2022 & month(data$date)<2)|year(data$date)==2020|year(data$date)==2021)&data$location==c3)
 c3_68a<-subset(data, ((year(data$date)==2022 & month(data$date)<3)|year(data$date)==2020|year(data$date)==2021)&data$location==c3)
 c3_1a=sum(c3_61a$new_cases,na.rm=TRUE)
 c3_2a=sum(c3_62a$new_cases,na.rm=TRUE)
 c3_3a=sum(c3_63a$new_cases,na.rm=TRUE)
 c3_4a=sum(c3_64a$new_cases,na.rm=TRUE)
 c3_5a=sum(c3_65a$new_cases,na.rm=TRUE)
 c3_6a=sum(c3_66a$new_cases,na.rm=TRUE)
 c3_7a=sum(c3_67a$new_cases,na.rm=TRUE)
 c3_8a=sum(c3_68a$new_cases,na.rm=TRUE)
 
 c17<-c(c1_1a,c1_2a,c1_3a,c1_4a,c1_5a,c1_6a,c1_7a,c1_8a)
 c27<-c(c2_1a,c2_2a,c2_3a,c2_4a,c2_5a,c2_6a,c2_7a,c2_8a)
 c37<-c(c3_1a,c3_2a,c3_3a,c3_4a,c3_5a,c3_6a,c3_7a,c3_8a)
 
 barplot(c17, xlab="Months", ylab="New_cases",main=ct[1], names.arg=send, col=color1)
 barplot(c27, xlab="Months", ylab="New_cases",main=ct[2], names.arg=send, col=color1)
 barplot(c37, xlab="Months", ylab="New_cases",main=ct[3], names.arg=send, col=color1)

# <8> Bieu do the hien thu thap du lieu tu vong tichg luy cho tung thang
 # Andorra
 c1_61b<-subset(data, year(data$date)<2021 & month(data$date)<5&data$location==c1)
 c1_62b<-subset(data, year(data$date)<2021 & month(data$date)<8&data$location==c1)
 c1_63b<-subset(data, ((year(data$date)==2021 & month(data$date)<2)|year(data$date)==2020)&data$location==c1)
 c1_64b<-subset(data, ((year(data$date)==2021 & month(data$date)<3)|year(data$date)==2020)&data$location==c1)
 c1_65b<-subset(data, ((year(data$date)==2021 & month(data$date)<5)|year(data$date)==2020)&data$location==c1)
 c1_66b<-subset(data, ((year(data$date)==2021 & month(data$date)<8)|year(data$date)==2020)&data$location==c1)
 c1_67b<-subset(data, ((year(data$date)==2022 & month(data$date)<2)|year(data$date)==2020|year(data$date)==2021)&data$location==c1)
 c1_68b<-subset(data, ((year(data$date)==2022 & month(data$date)<3)|year(data$date)==2020|year(data$date)==2021)&data$location==c1)
 c1_1b=sum(c1_61b$new_deaths,na.rm=TRUE)
 c1_2b=sum(c1_62b$new_deaths,na.rm=TRUE)
 c1_3b=sum(c1_63b$new_deaths,na.rm=TRUE)
 c1_4b=sum(c1_64b$new_deaths,na.rm=TRUE)
 c1_5b=sum(c1_65b$new_deaths,na.rm=TRUE)
 c1_6b=sum(c1_66b$new_deaths,na.rm=TRUE)
 c1_7b=sum(c1_67b$new_deaths,na.rm=TRUE)
 c1_8b=sum(c1_68b$new_deaths,na.rm=TRUE)
 # Slovenia
 c2_61b<-subset(data, year(data$date)<2021 & month(data$date)<5&data$location==c2)
 c2_62b<-subset(data, year(data$date)<2021 & month(data$date)<8&data$location==c2)
 c2_63b<-subset(data, ((year(data$date)==2021 & month(data$date)<2)|year(data$date)==2020)&data$location==c2)
 c2_64b<-subset(data, ((year(data$date)==2021 & month(data$date)<3)|year(data$date)==2020)&data$location==c2)
 c2_65b<-subset(data, ((year(data$date)==2021 & month(data$date)<5)|year(data$date)==2020)&data$location==c2)
 c2_66b<-subset(data, ((year(data$date)==2021 & month(data$date)<8)|year(data$date)==2020)&data$location==c2)
 c2_67b<-subset(data, ((year(data$date)==2022 & month(data$date)<2)|year(data$date)==2020|year(data$date)==2021)&data$location==c2)
 c2_68b<-subset(data, ((year(data$date)==2022 & month(data$date)<3)|year(data$date)==2020|year(data$date)==2021)&data$location==c2)
 c2_1b=sum(c2_61b$new_deaths,na.rm=TRUE)
 c2_2b=sum(c2_62b$new_deaths,na.rm=TRUE)
 c2_3b=sum(c2_63b$new_deaths,na.rm=TRUE)
 c2_4b=sum(c2_64b$new_deaths,na.rm=TRUE)
 c2_5b=sum(c2_65b$new_deaths,na.rm=TRUE)
 c2_6b=sum(c2_66b$new_deaths,na.rm=TRUE)
 c2_7b=sum(c2_67b$new_deaths,na.rm=TRUE)
 c2_8b=sum(c2_68b$new_deaths,na.rm=TRUE)
 # UK
 c3_61b<-subset(data, year(data$date)<2021 & month(data$date)<5&data$location==c3)
 c3_62b<-subset(data, year(data$date)<2021 & month(data$date)<8&data$location==c3)
 c3_63b<-subset(data, ((year(data$date)==2021 & month(data$date)<2)|year(data$date)==2020)&data$location==c3)
 c3_64b<-subset(data, ((year(data$date)==2021 & month(data$date)<3)|year(data$date)==2020)&data$location==c3)
 c3_65b<-subset(data, ((year(data$date)==2021 & month(data$date)<5)|year(data$date)==2020)&data$location==c3)
 c3_66b<-subset(data, ((year(data$date)==2021 & month(data$date)<8)|year(data$date)==2020)&data$location==c3)
 c3_67b<-subset(data, ((year(data$date)==2022 & month(data$date)<2)|year(data$date)==2020|year(data$date)==2021)&data$location==c3)
 c3_68b<-subset(data, ((year(data$date)==2022 & month(data$date)<3)|year(data$date)==2020|year(data$date)==2021)&data$location==c3)
 c3_1b=sum(c3_61b$new_deaths,na.rm=TRUE)
 c3_2b=sum(c3_62b$new_deaths,na.rm=TRUE)
 c3_3b=sum(c3_63b$new_deaths,na.rm=TRUE)
 c3_4b=sum(c3_64b$new_deaths,na.rm=TRUE)
 c3_5b=sum(c3_65b$new_deaths,na.rm=TRUE)
 c3_6b=sum(c3_66b$new_deaths,na.rm=TRUE)
 c3_7b=sum(c3_67b$new_deaths,na.rm=TRUE)
 c3_8b=sum(c3_68b$new_deaths,na.rm=TRUE)
 
 c18<-c(c1_1b,c1_2b,c1_3b,c1_4b,c1_5b,c1_6b,c1_7b,c1_8b)
 c28<-c(c2_1b,c2_2b,c2_3b,c2_4b,c2_5b,c2_6b,c2_7b,c2_8b)
 c38<-c(c3_1b,c3_2b,c3_3b,c3_4b,c3_5b,c3_6b,c3_7b,c3_8b)
 print(c3_8b)
 barplot(c18, xlab="Months", ylab="New_deaths",main=ct[1], names.arg=send, col=color1)
 barplot(c28, xlab="Months", ylab="New_deaths",main=ct[2], names.arg=send, col=color1)
 barplot(c38, xlab="Months", ylab="New_deaths",main=ct[3], names.arg=send, col=color1)