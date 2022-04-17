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
setwd("C:/TONG HOP CODE")
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

#Processing 
#1/Du lieu duoc thu thap vao cac nam
print("#1")
date<-mdy(data$date)
cat("Du lieu thu thap tu nam")
print(min(format(date,format="%Y")))
cat("den nam")
print(max(format(date,format="%Y")))


#2/So luong dat nuoc va dinh danh 
print("#2")
cat("So luong dat nuoc ")
print(length(table(data$iso_code)))
cat("Dinh danh 10 dat nuoc dau")
print(head(table(data$iso_code),10))

#3/So luong chau luc trong tap mau
print("#3")
cat("So luong chau luc trong tap mau")
print(length(table(data$continent)))
#4/So luong du lieu thu thap duoc trong tung chau luc va tong so
print("#4")
cat("So luong du lieu thu thap duoc trong tung chau luc va tong so")
print("Tong so:")
print(length(data$continent))
print("Trong tung chau luc")
print(table(data$continent))


#5/So luong du lieu thu thap duoc trong tung dat nuoc va tong so
print("#5")
cat("So luong du lieu thu thap duoc trong tung dat nuoc (10 nuoc cuoi) ")
print(tail(table(data$iso_code),10))
cat(" Tong so")
print(sum(table(data$iso_code)))

#6/Chau luc co luong du lieu thu thap nho nhat
print("#6")
cat("Chau luc co luong du lieu thu thap nho nhat")
which.min(table(data$continent))
print(min(table(data$continent)))

#7/Chau luc co luong du lieu thu thap lon nhat
print("#7")
cat("Chau luc co luong du lieu thu thap lon nhat")
which.max(table(data$continent))
print(max(table(data$continent)))

#8/Nuoc co luong du lieu thu thap nho nhat
print("#8")
cat("Nuoc co luong du lieu thu thap nho nhat")
which.min(table(data$iso_code))
print(min(table(data$iso_code)))

#9/Nuoc co luong du lieu thu thap lon nhat
print("#9")
cat("Nuoc co luong du lieu thu thap lon nhat")
which.max(table(data$iso_code))
print(max(table(data$iso_code)))

#10/Date co luong du lieu thu thap nho nhat
print("#10")
cat("Date co luong du lieu thu thap nho nhat")
which.min(table(data$date))
print(min(table(data$date)))

#11/Date co luong du lieu thu thap nho nhat
print("#11")
cat("Date co luong du lieu thu thap nho nhat")
which.max(table(data$date))
print(max(table(data$date)))

#12/So luong du lieu thu thap theo date va chau luc
print("#12")
cat("So luong du lieu thu thap theo date va chau luc")
print(table(data.frame(data$continent,data$date)))

#13/So luong du lieu thu thap lon nhat theo date va continent
print("#13")
cat("So luong du lieu thu thap lon nhat theo date va continent")
print(max(table(data.frame(data$continent,data$date))))

#14/So luong du lieu thu thap nho nhat theo date va continent
print("#14")
cat("So luong du lieu thu thap nho nhat theo date va continent")
print(min(table(data.frame(data$continent,data$date))))

#15/Tim so luong du lieu thu thap duoc voi date va continent cho truoc
print("#15")
cat("So luong du lieu thu thap duoc voi date va continent cho truoc")
k<-strptime("11/6/2021",format="%m/%d/%y")
t<-"Africa"
print(table(strptime(data$date,format="%m/%d/%y")==k,data$continent==t)[2,2])


freq_data<-count(data$iso_code)
freq_data
freqfreq_data<-count(freq_data$freq)

freqfreq_data<-subset(freqfreq_data,freq>1)
freqfreq_data
sentence="Cac quoc gia co cung so lan thu thap du lieu ="
#16/Cac quoc gia cung so lan thu thap du lieu
print("#16")
for(i in freqfreq_data$x){
  cat(sentence)
  print(i)
  a<-subset(freq_data$x,freq_data$freq==i)
  print(a)

}

#17/ Cac quoc gia co iso_code >3
print("#17")
isogreaterthan3<-subset(data,str_length(iso_code)>3)
isogreaterthan3<-isogreaterthan3[,c(1,3)]
print(unique(isogreaterthan3))

