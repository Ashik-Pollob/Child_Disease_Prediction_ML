---
Title: "Child Diseases Frequency"
Author: "S.M. Ashikul Islam Pollob"
Date: "09-09-2023"
---

# libraries needed
library(epiDisplay)

# Output of Frequency
pdf("output.pdf")
tab1(Fdata$Division, cum.percent = FALSE)

pdf("output.pdf")
tab1(Fdata$ Residence, cum.percent = FALSE)

pdf("output.pdf")
tab1(Fdata$ Wealth, cum.percent = FALSE)

pdf("output.pdf")
tab1(Fdata$ F_Edu, cum.percent = FALSE)

pdf("output.pdf")
tab1(Fdata$ M_Edu, cum.percent = FALSE)

pdf("output.pdf")
tab1(Fdata$ F_Job, cum.percent = FALSE)

pdf("output.pdf")
tab1(Fdata$ M_Job, cum.percent = FALSE)

pdf("output.pdf")
tab1(Fdata$ Household_member, cum.percent = FALSE)

pdf("output.pdf")
tab1(Fdata$ Child_Age, cum.percent = FALSE)

pdf("output.pdf")
tab1(Fdata$ Child_Sex, cum.percent = FALSE)

pdf("output.pdf")
tab1(Fdata$ Breastfeeding, cum.percent = FALSE)

pdf("output.pdf")
tab1(Fdata$ Birth_order, cum.percent = FALSE)

pdf("output.pdf")
tab1(Fdata$Media, cum.percent = FALSE)

pdf("output.pdf")
tab1(Fdata$ Mother_age, cum.percent = FALSE)

pdf("output.pdf")
tab1(Fdata$BMI, cum.percent = FALSE)

pdf("output.pdf")
tab1(Fdata$ Delivery_place, cum.percent = FALSE)

pdf("output.pdf")
tab1(Fdata$ Age_1stBirth, cum.percent = FALSE)

pdf("output.pdf")
tab1(Fdata$ Toilet, cum.percent = FALSE)

pdf("output.pdf")
tab1(Fdata$ Solid_Fuel, cum.percent = FALSE)

pdf("output.pdf")
tab1(Fdata$ Water, cum.percent = FALSE)

pdf("output.pdf")
tab1(Fdata$ Vit_A, cum.percent = FALSE)

pdf("output.pdf")
tab1(Fdata$ Ceaserean, cum.percent = FALSE)

pdf("output.pdf")
tab1(Fdata$ Birth_interval, cum.percent = FALSE)
