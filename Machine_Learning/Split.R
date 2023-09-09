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

# Select dataset for ARI
AriData<-Data %>%dplyr:: select(Division, Residence, Wealth, F_Edu, M_Edu, F_Job, M_Job, Household_member, Child_Age, Child_Sex, Breastfeeding, 
                              Birth_order, Media, Mother_age, BMI, Delivery_place, Age_1stBirth, Toilet, Solid_Fuel, Water, Vit_A, Ceaserean, 
                              Birth_interval,ch_ari)

# Splitting
set.seed(11)
index <- sample(1:nrow(AriData),round(0.80*nrow(AriData)))
ari.train <- AriData[index,]
ari.test <- AriData[-index,]
levels(ari.train$ch_ari) <- c("Yes", "No")
levels(ari.test$ch_ari) <- c("Yes", "No")



# Select dataset for Stunting
StData<-Data %>%dplyr:: select(Division, Residence, Wealth, F_Edu, M_Edu, F_Job, M_Job, Household_member, Child_Age, Child_Sex, Breastfeeding, 
                              Birth_order, Media, Mother_age, BMI, Delivery_place, Age_1stBirth, Toilet, Solid_Fuel, Water, Vit_A, Ceaserean, 
                              Birth_interval, nt_ch_stunt)

# Splitting
set.seed(11)
index <- sample(1:nrow(StData),round(0.80*nrow(StData)))
stn.train <- StData[index,]
stn.test <- StData[-index,]
levels(stn.train$nt_ch_stunt) <- c("Yes", "No")
levels(stn.test$nt_ch_stunt) <- c("Yes", "No")



# Select dataset for Wasting
WstData<-Data %>%dplyr:: select(Division, Residence, Wealth, F_Edu, M_Edu, F_Job, M_Job, Household_member, Child_Age, Child_Sex, Breastfeeding, 
                              Birth_order, Media, Mother_age, BMI, Delivery_place, Age_1stBirth, Toilet, Solid_Fuel, Water, Vit_A, Ceaserean, 
                              Birth_interval,,nt_ch_wast)

# Splitting
set.seed(11)
index <- sample(1:nrow(WstData),round(0.80*nrow(WstData)))
wst.train <- WstData[index,]
wst.test <- WstData[-index,]
levels(wst.train$nt_ch_wast) <- c("Yes", "No")
levels(wst.test$nt_ch_wast) <- c("Yes", "No")



# Select dataset for Underweight
UwtData<-Data %>%dplyr:: select(Division, Residence, Wealth, F_Edu, M_Edu, F_Job, M_Job, Household_member, Child_Age, Child_Sex, Breastfeeding, 
                              Birth_order, Media, Mother_age, BMI, Delivery_place, Age_1stBirth, Toilet, Solid_Fuel, Water, Vit_A, Ceaserean, 
                              Birth_interval,, nt_ch_underwt)

# Splitting
set.seed(11)
index <- sample(1:nrow(UwtData),round(0.80*nrow(UwtData)))
und.train <- UwtData[index,]
und.test <- UwtData[-index,]
levels(und.train$nt_ch_underwt) <- c("Yes", "No")
levels(und.test$nt_ch_underwt) <- c("Yes", "No")



# Select dataset for Diarrhea
DiarData<-Data %>%dplyr:: select(Division, Residence, Wealth, F_Edu, M_Edu, F_Job, M_Job, Household_member, Child_Age, Child_Sex, Breastfeeding, 
                              Birth_order, Media, Mother_age, BMI, Delivery_place, Age_1stBirth, Toilet, Solid_Fuel, Water, Vit_A, Ceaserean, 
                              Birth_interval,,ch_diar)

# Splitting
set.seed(11)
index <- sample(1:nrow(DiarData),round(0.80*nrow(DiarData)))
dia.train <- DiarData[index,]
dia.test <- DiarData[-index,]
levels(dia.train$ch_diar) <- c("Yes", "No")
levels(dia.test$ch_diar) <- c("Yes", "No")

