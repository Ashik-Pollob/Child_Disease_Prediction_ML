---
Title: "Child Diseases Data Split"
Author: "S.M. Ashikul Islam Pollob"
Date: "09-09-2023"
---

# libraries needed
library(dplyr)
library(caret)

#Cross Validation
train_control <- trainControl(method = "repeatedcv",number = 5, repeats = 25, classProbs = TRUE)



#ARI 
AriData<-AData %>%dplyr:: select(Division,Residence, Wealth, F_Edu, M_Edu,M_Job, Household_member,Child_Age,Breastfeeding, Birth_order,
                                 Media,Mother_age,BMI,Delivery_place, Age_1stBirth,Toilet, Solid_Fuel,Ceaserean,Birth_interval,ch_ari)
# Splitting
set.seed(11)
index <- sample(1:nrow(AriData),round(0.80*nrow(AriData)))
ari.train <- AriData[index,]
ari.test <- AriData[-index,]
levels(ari.train$ch_ari) <- c("Yes", "No")
levels(ari.test$ch_ari) <- c("Yes", "No")


#Stunting
StData<-Data %>%dplyr:: select(Division,Residence, Wealth, F_Edu, M_Edu, Child_Age,Breastfeeding,Birth_order,Media, Mother_age,BMI,
                               Delivery_place,Age_1stBirth, Toilet, Solid_Fuel,Water,Ceaserean,Birth_interval, nt_ch_stunt)
# Splitting
set.seed(11)
index <- sample(1:nrow(StData),round(0.80*nrow(StData)))
stn.train <- StData[index,]
stn.test <- StData[-index,]
levels(stn.train$nt_ch_stunt) <- c("Yes", "No")
levels(stn.test$nt_ch_stunt) <- c("Yes", "No")


#Wasting
WstData<-Data %>%dplyr:: select(Division,Residence, Wealth, F_Edu, M_Edu,M_Job, Child_Age,Breastfeeding,Birth_order,Media, Mother_age,
                                BMI,Age_1stBirth,Toilet, Solid_Fuel, Ceaserean,Birth_interval,nt_ch_wast)
# Splitting
set.seed(11)
index <- sample(1:nrow(WstData),round(0.80*nrow(WstData)))
wst.train <- WstData[index,]
wst.test <- WstData[-index,]
levels(wst.train$nt_ch_wast) <- c("Yes", "No")
levels(wst.test$nt_ch_wast) <- c("Yes", "No")


#Underweight
UwtData<-Data %>%dplyr:: select(Division,Residence, Wealth, F_Edu, M_Edu,F_Job, Child_Age,Breastfeeding,Birth_order,Media, Mother_age,
                                BMI,Age_1stBirth,Toilet, Solid_Fuel, Ceaserean,Birth_interval, nt_ch_underwt)
# Splitting
set.seed(11)
index <- sample(1:nrow(UwtData),round(0.80*nrow(UwtData)))
und.train <- UwtData[index,]
und.test <- UwtData[-index,]
levels(und.train$nt_ch_underwt) <- c("Yes", "No")
levels(und.test$nt_ch_underwt) <- c("Yes", "No")


#Diarrhea
DiarData<-DData %>%dplyr:: select(Division,Residence, Wealth, F_Edu, M_Edu,M_Job, Household_member,Child_Age,Breastfeeding, Birth_order,
                                  Media,Mother_age,BMI,Delivery_place,  Age_1stBirth,Toilet, Solid_Fuel,Ceaserean, Birth_interval,ch_diar)
# Splitting
set.seed(11)
index <- sample(1:nrow(DiarData),round(0.80*nrow(DiarData)))
dia.train <- DiarData[index,]
dia.test <- DiarData[-index,]
levels(dia.train$ch_diar) <- c("Yes", "No")
levels(dia.test$ch_diar) <- c("Yes", "No")

