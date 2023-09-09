---
Title: "Child Diseases Cross Tabulation"
Author: "S.M. Ashikul Islam Pollob"
Date: "09-09-2023"
---

# libraries needed
library(crosstable)


# Acute Respiratory Infection (ARI)
crosstable(Fdata, c(Division, Residence, Wealth, F_Edu, M_Edu, F_Job, M_Job, Household_member, Child_Age, Child_Sex, Breastfeeding, 
                    Birth_order, Media, Mother_age, BMI, Delivery_place, Age_1stBirth, Toilet, Solid_Fuel, Water, Vit_A, Ceaserean, 
                    Birth_interval), by=c(ch_ari), 
           label=FALSE, num_digits=3, funs=c(mean, quantile), funs_arg=list(probs=c(.25,.75))) %>% as_flextable(compact=TRUE, header_show_n=1:2)


# Stunting
crosstable(Fdata, c(Division, Residence, Wealth, F_Edu, M_Edu, F_Job, M_Job, Household_member, Child_Age, Child_Sex, Breastfeeding, 
                    Birth_order, Media, Mother_age, BMI, Delivery_place, Age_1stBirth, Toilet, Solid_Fuel, Water, Vit_A, Ceaserean, 
                    Birth_interval), by=c(nt_ch_stunt), 
           label=FALSE, num_digits=3, funs=c(mean, quantile), funs_arg=list(probs=c(.25,.75))) %>% as_flextable(compact=TRUE, header_show_n=1:2)


# Wasting
crosstable(Fdata, c(Division, Residence, Wealth, F_Edu, M_Edu, F_Job, M_Job, Household_member, Child_Age, Child_Sex, Breastfeeding, 
                    Birth_order, Media, Mother_age, BMI, Delivery_place, Age_1stBirth, Toilet, Solid_Fuel, Water, Vit_A, Ceaserean, 
                    Birth_interval), by=c(nt_ch_wast), 
           label=FALSE, num_digits=3, funs=c(mean, quantile), funs_arg=list(probs=c(.25,.75))) %>% as_flextable(compact=TRUE, header_show_n=1:2)


# Underweight
crosstable(Fdata, c(Division, Residence, Wealth, F_Edu, M_Edu, F_Job, M_Job, Household_member, Child_Age, Child_Sex, Breastfeeding, 
                    Birth_order, Media, Mother_age, BMI, Delivery_place, Age_1stBirth, Toilet, Solid_Fuel, Water, Vit_A, Ceaserean, 
                    Birth_interval), by=c(nt_ch_underwt), 
           label=FALSE, num_digits=3, funs=c(mean, quantile), funs_arg=list(probs=c(.25,.75))) %>% as_flextable(compact=TRUE, header_show_n=1:2)


# Diarrhea
crosstable(Fdata, c(Division, Residence, Wealth, F_Edu, M_Edu, F_Job, M_Job, Household_member, Child_Age, Child_Sex, Breastfeeding, 
                    Birth_order, Media, Mother_age, BMI, Delivery_place, Age_1stBirth, Toilet, Solid_Fuel, Water, Vit_A, Ceaserean, 
                    Birth_interval), by=c(ch_diar), 
           label=FALSE, num_digits=3, funs=c(mean, quantile), funs_arg=list(probs=c(.25,.75))) %>% as_flextable(compact=TRUE, header_show_n=1:2)
