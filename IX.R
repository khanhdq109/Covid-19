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
# Getting totalcases/deaths of each country
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
