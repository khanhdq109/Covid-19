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
c1<-c("Andorra")
c2<-c("Slovenia")
c3<-c("United Kingdom")
ctr1<-subset(data, data$location==c1)
ctr2<-subset(data, data$location==c2)
ctr3<-subset(data, data$location==c3)

# <iii> Nhom cau hoi lien quan den du lieu the hien thu thap du lieu (Voi moi quoc gia can tinh)

# <1> Co bao nhieu ngay co so lan du lieu khong duoc bao cao moi
  # Ca nhiem
  c1_1a<-subset(data,data$location==c1&(data$new_cases==0|is.na(data$new_cases)))
  c2_1a<-subset(data,data$location==c2&(data$new_cases==0|is.na(data$new_cases)))
  c3_1a<-subset(data,data$location==c3&(data$new_cases==0|is.na(data$new_cases)))
  print("So ngay co so ca nhiem khong duoc bao cao moi cua Andorra")
  print(length(c1_1a$new_cases))
  print("So ngay co so ca nhiem khong duoc bao cao moi cua Slovenia")
  print(length(c2_1a$new_cases))
  print("So ngay co so ca nhiem khong duoc bao cao moi cua UK")
  print(length(c3_1a$new_cases))
  # Tu vong
  c1_1b<-subset(data,data$location==c1&(data$new_deaths==0|is.na(data$new_deaths)))
  c2_1b<-subset(data,data$location==c2&(data$new_deaths==0|is.na(data$new_deaths)))
  c3_1b<-subset(data,data$location==c3&(data$new_deaths==0|is.na(data$new_deaths)))
  print("So ngay co so ca tu vong khong duoc bao cao moi cua Andorra")
  print(length(c1_1b$new_deaths))
  print("So ngay co so ca tu vong khong duoc bao cao moi cua Slovenia")
  print(length(c2_1b$new_deaths))
  print("So ngay co so ca tu vong khong duoc bao cao moi cua UK")
  print(length(c3_1b$new_deaths))

# <2> Co bao nhieu ngay co so ca nhiem/tu vong la thap nhat duoc bao cao moi
  # Ca nhiem
  c1_2a<-subset(data,data$location==c1&data$new_cases>0)
  c2_2a<-subset(data,data$location==c2&data$new_cases>0)
  c3_2a<-subset(data,data$location==c3&data$new_cases>0)
  count1=0
  count2=0
  count3=0
  for (i in 1:length(c1_2a$new_cases))
  {
    if (c1_2a$new_cases[i]==min(c1_2a$new_cases))
      count1=count1+1
  }
  for (i in 1:length(c2_2a$new_cases))
  {
    if (c2_2a$new_cases[i]==min(c2_2a$new_cases))
      count2=count2+1
  }
  for (i in 1:length(c3_2a$new_cases))
  {
    if (c3_2a$new_cases[i]==min(c3_2a$new_cases))
      count3=count3+1
  }
  print("So ngay co so ca nhiem la thap nhat duoc bao cao moi cua Andorra")
  print(count1)
  print("So ngay co so ca nhiem la thap nhat duoc bao cao moi cua Slovenia")
  print(count2)
  print("So ngay co so ca nhiem la thap nhat duoc bao cao moi cua UK")
  print(count3)
  # Tu vong
  c1_2b<-subset(data,data$location==c1&data$new_deaths>0)
  c2_2b<-subset(data,data$location==c2&data$new_deaths>0)
  c3_2b<-subset(data,data$location==c3&data$new_deaths>0)
  count4=0
  count5=0
  count6=0
  for (i in 1:length(c1_2b$new_deaths))
  {
    if (c1_2b$new_deaths[i]==min(c1_2b$new_deaths))
      count4=count4+1
  }
  for (i in 1:length(c2_2b$new_deaths))
  {
    if (c2_2b$new_deaths[i]==min(c2_2b$new_deaths))
      count5=count5+1
  }
  for (i in 1:length(c3_2b$new_deaths))
  {
    if (c3_2b$new_deaths[i]==min(c3_2b$new_deaths))
      count6=count4+1
  }
  print("So ngay co so ca tu vong la thap nhat duoc bao cao moi cua Andorra")
  print(count4)
  print("So ngay co so ca tu vong la thap nhat duoc bao cao moi cua Slovenia")
  print(count5)
  print("So ngay co so ca tu vong la thap nhat duoc bao cao moi cua UK")
  print(count6)
  
# <3> Co bao nhieu ngay co so ca nhiem/tu vong la cao nhap duoc bao cao moi
  # Ca nhiem
  # c1_2a<-subset(data,data$location==c1&data$new_cases>0)
  # c2_2a<-subset(data,data$location==c2&data$new_cases>0)
  # c3_2a<-subset(data,data$location==c3&data$new_cases>0)
  count7=0
  count8=0
  count9=0
  for (i in 1:length(c1_2a$new_cases))
  {
    if (c1_2a$new_cases[i]==max(c1_2a$new_cases))
      count7=count7+1
  }
  for (i in 1:length(c2_2a$new_cases))
  {
    if (c2_2a$new_cases[i]==max(c2_2a$new_cases))
      count8=count8+1
  }
  for (i in 1:length(c3_2a$new_cases))
  {
    if (c3_2a$new_cases[i]==max(c3_2a$new_cases))
      count9=count9+1
  }
  print("So ngay co so ca nhiem la cao nhat duoc bao cao moi cua Andorra")
  print(count7)
  print("So ngay co so ca nhiem la cao nhat duoc bao cao moi cua Slovenia")
  print(count8)
  print("So ngay co so ca nhiem la cao nhat duoc bao cao moi cua AUK")
  print(count9)
  # Tu vong
  # c1_2b<-subset(data,data$location==c1&data$new_deaths>0)
  # c2_2b<-subset(data,data$location==c2&data$new_deaths>0)
  # c3_2b<-subset(data,data$location==c3&data$new_deaths>0)
  count10=0
  count11=0
  count12=0
  for (i in 1:length(c1_2b$new_deaths))
  {
    if (c1_2b$new_deaths[i]==max(c1_2b$new_deaths))
      count10=count10+1
  }
  for (i in 1:length(c2_2b$new_deaths))
  {
    if (c2_2b$new_deaths[i]==max(c2_2b$new_deaths))
      count11=count11+1
  }
  for (i in 1:length(c3_2b$new_deaths))
  {
    if (c3_2b$new_deaths[i]==max(c3_2b$new_deaths))
      count12=count12+1
  }
  print("So ngay co so ca tu vong la cao nhat duoc bao cao moi cua Andorra")
  print(count10)
  print("So ngay co so ca tu vong la cao nhat duoc bao cao moi cua Andorra")
  print(count11)
  print("So ngay co so ca tu vong la cao nhat duoc bao cao moi cua Andorra")
  print(count12)

# <4> The hien bang so lieu
  Countries<-c( c1, c2, c3)
  Infections<-c(length(c1_1a$new_cases),length(c2_1a$new_cases),length(c3_1a$new_cases))
  Deaths<-c(length(c1_1b$new_deaths),length(c2_1b$new_deaths),length(c3_1b$new_deaths))
  datatable1<-data.frame(Countries,Infections,Deaths)
  print("Bang so lieu")
  print(datatable1)
  Infectionss<-c(length(ctr1$new_cases)-length(c1_1a$new_cases),length(ctr2$new_cases)-length(c2_1a$new_cases),length(ctr3$new_cases)-length(c3_1a$new_cases))
  Deathss<-c(length(ctr1$new_cases)-length(c1_1b$new_deaths),length(ctr2$new_cases)-length(c2_1b$new_deaths),length(ctr3$new_cases)-length(c3_1b$new_deaths))
  datatable2<-data.frame(Countries,Infectionss,Deathss)
  print("Bang so lieu")
  print(datatable2)

# <5> Cho biet so ngay ngan nhat lien tiep ma khong co du lieu duoc bao cao
  c1_5a<-subset(data, data$location==c1)
  c2_5a<-subset(data, data$location==c2)
  c3_5a<-subset(data, data$location==c3)
  c1_5a[is.na(c1_5a)]<-0
  c2_5a[is.na(c2_5a)]<-0
  c3_5a[is.na(c3_5a)]<-0
  # Ca nhiem
  ## Andorra
  a51=0
  min51=10000
  max51=0
  count51=0
    if(c1_5a$new_cases[1]==0)
    {
      count51=0
      a51=1
      while (c1_5a$new_cases[a51]==0)
      {
        a51=a51+1 
        count51=count51+1
      }
      if (count51<min51)
      {
        min51=count51
      }
      if (count51>max51)
      {
        max51=count51
      }
      count51=0
    }  
  for (i in 2:length(c1_5a$new_cases))
  {
    if ((c1_5a$new_cases[i]==0)&(c1_5a$new_cases[i-1]>0))
    {
      count51=0
      a51=i
      while (c1_5a$new_cases[a51]==0)
      {
        a51=a51+1 
        count51=count51+1
        if (a51==721)
        {
          break
        }
      }
      if (count51<min51)
      {
        min51=count51
      }
      if (count51>max51)
      {
        max51=count51
      }
      count51=0
    }
  }   
  ## Slovenia
  a52=0
  min52=10000
  max52=0
  count52=0
  if(c2_5a$new_cases[1]==0)
  {
    count52=0
    a52=1
    while (c2_5a$new_cases[a52]==0)
    {
      a52=a52+1 
      count52=count52+1
    }
    if (count52<min52)
    {
      min52=count52
    }
    if (count52>max52)
    {
      max52=count52
    }
    count52=0
  }  
  for (i in 2:length(c2_5a$new_cases))
  {
    if ((c2_5a$new_cases[i]==0)&(c2_5a$new_cases[i-1]>0))
    {
      count52=0
      a52=i
      while (c2_5a$new_cases[a52]==0)
      {
        a52=a52+1 
        count52=count52+1
      }
      if (count52<min52)
      {
        min52=count52
      }
      if (count52>max52)
      {
        max52=count52
      }
      count52=0
    }
  }
  
  ## UK
  a53=0
  min53=10000
  max53=0
  count53=0
  if(c3_5a$new_cases[1]==0)
  {
    count53=0
    a53=1
    while (c3_5a$new_cases[a53]==0)
    {
      a53=a53+1 
      count53=count53+1
    }
    if (count53<min53)
    {
      min53=count53
    }
    if (count53>max53)
    {
      max53=count53
    }
    count53=0
  }  
  for (i in 2:length(c3_5a$new_cases))
  {
    if ((c3_5a$new_cases[i]==0)&(c3_5a$new_cases[i-1]>0))
    {
      count53=0
      a53=i
      while (c3_5a$new_cases[a53]==0)
      {
        a53=a53+1 
        count53=count53+1
      }
      if (count53<min53)
      {
        min53=count53
      }
      if (count53>max53)
      {
        max53=count53
      }
      count53=0
    }
  }
  
  # Tu vong
  ## Andorra
  a54=0
  min54=10000
  max54=0
  count54=0
  if(c1_5a$new_deaths[1]==0)
  {
    count54=0
    a54=1
    while (c1_5a$new_deaths[a54]==0)
    {
      a54=a54+1 
      count54=count54+1
    }
    if (count54<min54)
    {
      min54=count54
    }
    if (count54>max54)
    {
      max54=count54
    }
    count54=0
  }  
  for (i in 2:length(c1_5a$new_deaths))
  {
    if ((c1_5a$new_deaths[i]==0)&(c1_5a$new_deaths[i-1]>0))
    {
      count54=0
      a54=i
      while (c1_5a$new_deaths[a54]==0)
      {
        a54=a54+1 
        count54=count54+1
        if (a54==721)
        {
          break
        }
      }
      if (count54<min54)
      {
        min54=count54
      }
      if (count54>max54)
      {
        max54=count54
      }
      count54=0
    }
  }
  
  ## Slovenia
  a55=0
  min55=10000
  max55=0
  count55=0
  if(c2_5a$new_deaths[1]==0)
  {
    count55=0
    a55=1
    while (c2_5a$new_deaths[a55]==0)
    {
      a55=a55+1 
      count55=count55+1
    }
    if (count55<min55)
    {
      min55=count55
    }
    if (count55>max55)
    {
      max55=count55
    }
    count55=0
  }  
  for (i in 2:length(c2_5a$new_deaths))
  {
    if ((c2_5a$new_deaths[i]==0)&(c2_5a$new_deaths[i-1]>0))
    {
      count55=0
      a55=i
      while (c2_5a$new_deaths[a55]==0)
      {
        a55=a55+1 
        count55=count55+1
      }
      if (count55<min55)
      {
        min55=count55
      }
      if (count55>max55)
      {
      max55=count55
      }
      count55=0
    }
  }
  
  ## UK
  a56=0
  min56=10000
  max56=0
  count56=0
  if(c3_5a$new_deaths[1]==0)
  {
    count56=0
    a56=1
    while (c3_5a$new_deaths[a56]==0)
    {

      a56=a56+1 
      count56=count56+1
      
    }
    if (count56<min56)
    {
      min56=count56
    }
    if (count56>max56)
    {
      max56=count56
    }
    count56=0
  }  
  for (i in 2:length(c3_5a$new_deaths))
  {
    if ((c3_5a$new_deaths[i]==0)&(c3_5a$new_deaths[i-1]>0))
    {
      count56=0
      a56=i
      while (c3_5a$new_deaths[a56]==0)
      {
        a56=a56+1 
        count56=count56+1
      }
      if (count56<min56)
      {
        min56=count56
      }
      if (count56>max56)
    {
      max56=count56
    }
    count56=0
    }
  }
  print("So ngay ngan nhat lien tiep ma khong co so ca nhiem duoc bao cao cua Andorra")
  print(min51)
  print("So ngay ngan nhat lien tiep ma khong co so ca nhiem duoc bao cao cua Slovenia")
  print(min52)
  print("So ngay ngan nhat lien tiep ma khong co so ca nhiem duoc bao cao cua UK")
  print(min53)
  print("So ngay ngan nhat lien tiep ma khong co so ca tu vong duoc bao cao cua Andorra")
  print(min54)
  print("So ngay ngan nhat lien tiep ma khong co so ca tu vong duoc bao cao cua Slovenia")
  print(min55)
  print("So ngay ngan nhat lien tiep ma khong co so ca tu vong duoc bao cao cua UK")
  print(min56)

# <6> Cho biet so ngay dai nhat lien tiep ma khong co du lieu duoc bao cao
  # Ca nhiem
  print("So ngay dai nhat lien tiep ma khong co so ca nhiem duoc bao cao cua Andorra")
  print(max51)
  print("So ngay dai nhat lien tiep ma khong co so ca nhiem duoc bao cao cua Slovenia")
  print(max52)
  print("So ngay dai nhat lien tiep ma khong co so ca nhiem duoc bao cao cua UK")
  print(max53)
  #Tu vong
  print("So ngay dai nhat lien tiep ma khong co so ca tu vong duoc bao cao cua Andorra")
  print(max54)
  print("So ngay dai nhat lien tiep ma khong co so ca tu vong duoc bao cao cua Slovenia")
  print(max55)
  print("So ngay dai nhat lien tiep ma khong co so ca tu vong duoc bao cao cua UK")
  print(max56)

# <7> Cho biet so ngay ngan nhat lien tiep ma khong co nguoi nhiem benh moi
  c1_6a<-subset(data, data$location==c1 & !is.na(data$new_cases))
  c2_6a<-subset(data, data$location==c2 & !is.na(data$new_cases))
  c3_6a<-subset(data, data$location==c3 & !is.na(data$new_cases))
  ## Andorra
  a61=0
  min61=10000
  max61=0
  count61=0
  if(c1_6a$new_cases[1]==0)
  {
    count61=0
    a61=1
    while (c1_6a$new_cases[a61]==0)
    {
      a61=a61+1 
      count61=count61+1
    }
    if (count61<min61)
    {
      min61=count61
    }
    if (count61>max61)
    {
      max61=count61
    }
    count61=0
  }  
  for (i in 2:length(c1_6a$new_cases))
  {
    if ((c1_6a$new_cases[i]==0)&(c1_6a$new_cases[i-1]>0))
    {
      count61=0
      a61=i
      while (c1_6a$new_cases[a61]==0)
      {
        a61=a61+1 
        count61=count61+1
        if (a61==721)
        {
          break
        }
      }
      if (count61<min61)
      {
        min61=count61
      }
      if (count61>max61)
      {
        max61=count61
      }
      count61=0
    }
  }   
  
  ## Slovenia
  a62=0
  min62=10000
  max62=0
  count62=0
  if(c2_6a$new_cases[1]==0)
  {
    count62=0
    a62=1
    while (c2_6a$new_cases[a62]==0)
    {
      a62=a62+1 
      count62=count62+1
    }
    if (count62<min62)
    {
      min62=count62
    }
    if (count62>max62)
    {
      max62=count62
    }
    count62=0
  }  
  for (i in 2:length(c2_6a$new_cases))
  {
    if ((c2_6a$new_cases[i]==0)&(c2_6a$new_cases[i-1]>0))
    {
      count62=0
      a62=i
      while (c2_6a$new_cases[a62]==0)
      {
        a62=a62+1 
        count62=count62+1
      }
      if (count62<min62)
      {
        min62=count62
      }
      if (count62>max62)
      {
        max62=count62
      }
      count62=0
    }
  }
  
  ## UK
  a63=0
  min63=10000
  max63=0
  count63=0
  if(c3_6a$new_cases[1]==0)
  {
    count63=0
    a63=1
    while (c3_6a$new_cases[a63]==0)
    {
      a63=a63+1 
      count63=count63+1
    }
    if (count63<min63)
    {
      min63=count63
    }
    if (count63>max63)
    {
      max63=count63
    }
    count63=0
  }  
  for (i in 2:length(c3_6a$new_cases))
  {
    if ((c3_6a$new_cases[i]==0)&(c3_6a$new_cases[i-1]>0))
    {
      count63=0
      a63=i
      while (c3_6a$new_cases[a63]==0)
      {
        a63=a63+1 
        count63=count63+1
      }
      if (count63<min63)
      {
        min63=count63
      }
      if (count63>max63)
      {
        max63=count63
      }
      count63=0
    }
  }
  print("So ngay ngan nhat lien tiep ma khong co nguoi nhiem benh moi o Andorra")
  print(min61)
  print("So ngay ngan nhat lien tiep ma khong co nguoi nhiem benh moi o Slovenia")
  print(min62)
  print("So ngay ngan nhat lien tiep ma khong co nguoi nhiem benh moi o UK")
  print(min63)

# <8> Cho biet so ngay dai nhat lien tiep ma khong co nguoi nhiem benh moi
  print("So ngay dai nhat lien tiep ma khong co nguoi nhiem benh moi o Andorra")
  print(max61)
  print("So ngay dai nhat lien tiep ma khong co nguoi nhiem benh moi o Slovenia")
  print(max62)
  print("So ngay dai nhat lien tiep ma khong co nguoi nhiem benh moi o UK")
  print(max63)
  
  
  
