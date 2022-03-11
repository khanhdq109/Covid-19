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

# i> NHOM CAU HOI LIEN QUAN DEN TONG QUAT DU LIEU

# ii> NHOM CAU HOI LIEN QUAN DEN MO TA THONG KE CO BAN DU LIEU

# iii> NHOM CAU HOI LIEN QUAN DEN DU LIEU THU THAP

# iv> NHOM CAU HOI LIEN QUAN DEN TRUC QUAN DU LIEU
# 1/ Ve bieu do tan so tich luy quoc gia cho cac chau luc

# 2/ Ve bieu do tan so tuong doi quoc gia cho cac chau luc

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

# 6/ Ve bieu do pho dat nuoc xuat hien outliers cho tu vong

# v> NHOM CAU HOI LIEN QUAN DEN TRUC QUAN DU LIEU THEO THOI GIAN LA THANG

# vi> NHOM CAU HOI LIEN QUAN DEN TRUC QUAN DU LIEU THEO TRUNG BINH 7 NGAY GAN NHAT

# vii> NHOM CAU HOI LIEN QUAN DEN TAT CA QUOC GIA THEO THOI GIAN LA THANG

# viii> NHOM CAU HOI LIEN QUAN DEN TAT CA QUOC GIA THEO TRUNG BINH 7 NGAY GAN NHAT

# ix> NHOM CAU HOI LIEN QUAN DEN SU TUONG QUAN GIUA NHIEM BENH VA TU VONG

# x> NHOM CAU HOI RIENG