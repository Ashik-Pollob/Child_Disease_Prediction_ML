---
Title: "Child Diseases Feature Selection"
Author: "S.M. Ashikul Islam Pollob"
Date: "16-09-2023"
---

# libraries needed
library(Boruta)

##BORUTA
#ARI
AData<-Data %>%dplyr:: select(Division, Residence, Wealth, F_Edu, M_Edu, F_Job, M_Job, Household_member, Child_Age, Child_Sex, 
                              Breastfeeding, Birth_order, Media, Mother_age, BMI, Delivery_place, Age_1stBirth, Toilet, Solid_Fuel, 
                              Water, Vit_A, Ceaserean, Birth_interval, ch_ari)

Aboruta <- Boruta(ch_ari~Division+Residence+Wealth+F_Edu+M_Edu+F_Job+M_Job+Household_member+Child_Age+Child_Sex+Breastfeeding+
                  Birth_order+Media+Mother_age+BMI+Delivery_place+Age_1stBirth+Toilet+Solid_Fuel+Water+Vit_A+Ceaserean+Birth_interval,
                  data = AData, doTrace = 2)

final.boruta <- TentativeRoughFix(Aboruta)
boruta.df <- attStats(final.boruta)
print(boruta.df)
plot(final.boruta, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(final.boruta$ImpHistory),function(i)
  final.boruta$ImpHistory[is.finite(final.boruta$ImpHistory[,i]),i])
names(lz) <- colnames(final.boruta$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(final.boruta$ImpHistory), cex.axis = 0.7)


           
#Stunting
SData<-Data %>%dplyr:: select(Division, Residence, Wealth, F_Edu, M_Edu, F_Job, M_Job, Household_member, Child_Age, Child_Sex,
                              Breastfeeding, Birth_order, Media, Mother_age, BMI, Delivery_place, Age_1stBirth, Toilet, Solid_Fuel, 
                              Water, Vit_A, Ceaserean, Birth_interval, nt_ch_stunt)
           
Sboruta <- Boruta(nt_ch_stunt~Division+Residence+Wealth+F_Edu+M_Edu+F_Job+M_Job+Household_member+Child_Age+Child_Sex+Breastfeeding+
                  Birth_order+Media+Mother_age+BMI+Delivery_place+Age_1stBirth+Toilet+Solid_Fuel+Water+Vit_A+Ceaserean+Birth_interval,
                  data = SData, doTrace = 2)

final.boruta <- TentativeRoughFix(Sboruta)
boruta.df <- attStats(final.boruta)
print(boruta.df)
plot(final.boruta, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(final.boruta$ImpHistory),function(i)
  final.boruta$ImpHistory[is.finite(final.boruta$ImpHistory[,i]),i])
names(lz) <- colnames(final.boruta$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(final.boruta$ImpHistory), cex.axis = 0.7)


           
#Wasting
WData<-Data %>%dplyr:: select(Division, Residence, Wealth, F_Edu, M_Edu, F_Job, M_Job, Household_member, Child_Age, Child_Sex, 
                              Breastfeeding, Birth_order, Media, Mother_age, BMI, Delivery_place, Age_1stBirth, Toilet, Solid_Fuel, 
                              Water, Vit_A, Ceaserean, Birth_interval,,nt_ch_wast)

Wboruta <- Boruta(nt_ch_wast~Division+Residence+Wealth+F_Edu+M_Edu+F_Job+M_Job+Household_member+Child_Age+Child_Sex+Breastfeeding+
                  Birth_order+Media+Mother_age+BMI+Delivery_place+Age_1stBirth+Toilet+Solid_Fuel+Water+Vit_A+Ceaserean+Birth_interval,
                  data = WData, doTrace = 2)

final.boruta <- TentativeRoughFix(Wboruta)
boruta.df <- attStats(final.boruta)
print(boruta.df)
plot(final.boruta, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(final.boruta$ImpHistory),function(i)
  final.boruta$ImpHistory[is.finite(final.boruta$ImpHistory[,i]),i])
names(lz) <- colnames(final.boruta$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(final.boruta$ImpHistory), cex.axis = 0.7)


           
#Underweight
UData<-Data %>%dplyr:: select(Division, Residence, Wealth, F_Edu, M_Edu, F_Job, M_Job, Household_member, Child_Age, Child_Sex, 
                              Breastfeeding, Birth_order, Media, Mother_age, BMI, Delivery_place, Age_1stBirth, Toilet, Solid_Fuel, 
                              Water, Vit_A, Ceaserean, Birth_interval,, nt_ch_underwt)
           
Uboruta <- Boruta(nt_ch_underwt~Division+Residence+Wealth+F_Edu+M_Edu+F_Job+M_Job+Household_member+Child_Age+Child_Sex+Breastfeeding+
                  Birth_order+Media+Mother_age+BMI+Delivery_place+Age_1stBirth+Toilet+Solid_Fuel+Water+Vit_A+Ceaserean+Birth_interval,
                  data = UData, doTrace = 2)

final.boruta <- TentativeRoughFix(Uboruta)
boruta.df <- attStats(final.boruta)
print(boruta.df)
plot(final.boruta, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(final.boruta$ImpHistory),function(i)
  final.boruta$ImpHistory[is.finite(final.boruta$ImpHistory[,i]),i])
names(lz) <- colnames(final.boruta$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(final.boruta$ImpHistory), cex.axis = 0.7)



#Diarrhea
DData<-Data %>%dplyr:: select(Division, Residence, Wealth, F_Edu, M_Edu, F_Job, M_Job, Household_member, Child_Age, Child_Sex, 
                              Breastfeeding, Birth_order, Media, Mother_age, BMI, Delivery_place, Age_1stBirth, Toilet, Solid_Fuel,
                              Water, Vit_A, Ceaserean, Birth_interval,,ch_diar)
           
Dboruta <- Boruta(ch_diar~Division+Residence+Wealth+F_Edu+M_Edu+F_Job+M_Job+Household_member+Child_Age+Child_Sex+Breastfeeding+
                  Birth_order+Media+Mother_age+BMI+Delivery_place+Age_1stBirth+Toilet+Solid_Fuel+Water+Vit_A+Ceaserean+Birth_interval,
                  data = DData, doTrace = 2)

final.boruta <- TentativeRoughFix(Dboruta)
boruta.df <- attStats(final.boruta)
print(boruta.df)
plot(final.boruta, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(final.boruta$ImpHistory),function(i)
  final.boruta$ImpHistory[is.finite(final.boruta$ImpHistory[,i]),i])
names(lz) <- colnames(final.boruta$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(final.boruta$ImpHistory), cex.axis = 0.7)
