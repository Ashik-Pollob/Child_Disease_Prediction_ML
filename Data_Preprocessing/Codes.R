---
Title: "Child Diseases Data Preprocessing"
Author: "S.M. Ashikul Islam Pollob"
Date: "09-09-2023"
---

# libraries needed
library(haven) # to facilitate the import and export of data
library(dplyr) # to perform common data manipulation tasks in a more human-readable and efficient manner
library(labelled)  # to maintain and manipulate this metadata when working with data
library(naniar)  # to work with missing data, allowing you to better understand and manage it
------------------------


## Import dataset from the directory
data <- read_dta("BDHS(2017-18).DTA")
------------------------


## Extract only the required variables
Data <- data %>%dplyr:: select(h11,h31b,h31c,hw70,hw71,hw72,bord,b3,b4,b5,b8,b11,b19,m15,m17,s321f,v005,v008,v106,v113,
                             v116,v136,v139,v140,v157,v158,v159,v161,v190,v212,v218,v404,v437,v438,v447a,v701,v704,v714)
------------------------



## Creating the response variables
# Remove the missing values from response variable categories.
Data <- Data[!is.na(Data$h31b),]
Data <- Data[!is.na(Data$hw70),]
Data <- Data[!is.na(Data$hw71),]
Data <- Data[!is.na(Data$hw72),]
Data <- Data[!is.na(Data$h11),]



# Select only the alive children
if ("TRUE" %in% (!("b19" %in% names(Data))))
  KRdata [[paste("b19")]] <- NA
if ("TRUE" %in% all(is.na(Data$b19)))
{ b19_included <- 0} else { b19_included <- 1}

if (b19_included==1) {
  Data <- Data %>%
    mutate(age = b19)
} else {
  Data <- Data %>%
    mutate(age = v008 - b3)
}




# Acute Respiratory infection (ARI)
if ("TRUE" %in% (!("h31c" %in% names(Data))))
  Data [[paste("h31c")]] <- NA
if ("TRUE" %in% all(is.na(Data$h31c)))
{ h31c_included <- 0} else { h31c_included <- 1}
if (h31c_included==1) {
  Data <- Data %>%
    mutate(ch_ari = 
             case_when(
               h31b==1 & (h31c==1 | h31c==3) & b5==1 ~ 1,
               b5==1 ~ 0  )) %>%
    set_value_labels(ch_ari = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_ari = "ARI symptoms in the 2 weeks before the survey")
}
data.frame(round(prop.table(table(Data$ch_ari)) * 100,2))
table(Data$ch_ari)



# Stunting
Data <- Data %>%
  mutate(nt_ch_stunt =
           case_when(
             hw70< -200  ~ 1 ,
             hw70>= -200 ~ 0 ,
             hw70>=9996 ~ 99)) %>%
  replace_with_na(replace = list(nt_ch_stunt = c(99))) %>%
  set_value_labels(nt_ch_stunt = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_stunt = "Stunted child under 5 years")
data.frame(round(prop.table(table(Data$nt_ch_stunt)) * 100))
table(Data$nt_ch_stunt)



# Wasting
Data <- Data %>%
  mutate(nt_ch_wast =
           case_when(
             hw72< -200  ~ 1 ,
             hw72>= -200 ~ 0 ,
             hw72>=9996 ~ 99)) %>%
  replace_with_na(replace = list(nt_ch_wast = c(99))) %>%
  set_value_labels(nt_ch_wast = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_wast = "Wasted child under 5 years")
data.frame(round(prop.table(table(Data$nt_ch_wast)) * 100))
table(Data$nt_ch_wast)



# Underweight
Data <- Data %>%
  mutate(nt_ch_underwt =
           case_when(
             hw71< -200  ~ 1 ,
             hw71>= -200 ~ 0 ,
             hw71>=9996 ~ 99)) %>%
  replace_with_na(replace = list(nt_ch_underwt = c(99))) %>%
  set_value_labels(nt_ch_underwt = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_underwt = "Underweight child under 5 years")
data.frame(round(prop.table(table(Data$nt_ch_underwt)) * 100))
table(Data$nt_ch_underwt)

# Diarrhea
Data<-Data %>%
  mutate(ch_diar = 
           case_when(
             (h11==1 | h11==2) & b5==1 ~ 1,
             b5==1 ~ 0  )) %>%
  set_value_labels(ch_diar = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar = "Diarrhea in the 2 weeks before the survey")
data.frame(round(prop.table(table(Data$ch_diar)) * 100,2))
table(Data$ch_diar)



# Convert as a factor
Data$ch_ari <- as.factor(Data$ch_ari)
Data$nt_ch_stunt <- as.factor(Data$nt_ch_stunt)
Data$nt_ch_wast <- as.factor(Data$nt_ch_wast)
Data$nt_ch_underwt <- as.factor(Data$nt_ch_underwt)
Data$ch_diar <- as.factor(Data$ch_diar)
------------------------



## Creating the explanatory variables
# Replace unnecessary categories with missing values coded as "NA"
Data["h31b"][Data["h31b"] == 8] <- NA
Data["hw70"][Data["hw70"] == 9998] <- NA
Data["hw71"][Data["hw71"] == 9998] <- NA
Data["hw72"][Data["hw72"] == 9998] <- NA
Data["hw72"][Data["hw72"] == 9996] <- NA
Data["h11"][Data["h11"] == 8] <- NA
Data["v113"][Data["v113"] == 97] <- NA
Data["v116"][Data["v116"] == 97] <- NA
Data["v161"][Data["v161"] == 95] <- NA
Data["v161"][Data["v161"] == 97] <- NA
Data["v139"][Data["v139"] == 97] <- NA
Data["v140"][Data["v140"] == 7] <- NA
Data["v437"][Data["v437"] == 9994] <- NA
Data["v437"][Data["v437"] == 9995] <- NA
Data["v437"][Data["v437"] == 9996] <- NA
Data["v438"][Data["v438"] == 9994] <- NA
Data["v438"][Data["v438"] == 9995] <- NA
Data["v438"][Data["v438"] == 9996] <- NA
Data["v701"][Data["v701"] == 8] <- NA
Data["v704"][Data["v704"] == 99998] <- NA


#Change the value label
# Division#
Data <- expss::apply_labels(
  Data,
  v139=c("Barisal"=1,"Chittagong"=2,"Dhaka"=3,"Khulna"=4,"Mymensingh"=5,"Rajshahi"=6,"Rangpur"=7,"Sylhet"=8))
Data %>% 
  data.table::setnames(
    old = "v139",
    new = "Division") %>% 
  head
Data$Division <- factor(Data$Division, levels = c("Barisal","Chittagong","Dhaka","Khulna","Mymensingh","Rajshahi","Rangpur","Sylhet"))

# Residence
Data <- expss::apply_labels(
  Data,
  v140=c("Urban"=1,"Rural"=2))
Data %>% 
  data.table::setnames(
    old = "v140",
    new = "Residence") %>% 
  head
Data$Residence <- factor(Data$Residence, levels = c("Urban","Rural"))

# Wealth
Data <- expss::apply_labels(
  Data,
  v190=c("Poorest"=1,"Poorer"=2,"Middle"=3,"Richer"=4,"Richest"=5))
Data %>% 
  data.table::setnames(
    old = "v190",
    new = "Wealth") %>% 
  head
Data$Wealth <- factor(Data$Wealth, levels = c("Poorest","Poorer","Middle","Richer","Richest"))

# Father's Education#
Data <- expss::apply_labels(
  Data,
  v701=c("No Education"=0,"Primary"=1,"Secondary"=2,"Higher"=3))
Data %>% 
  data.table::setnames(
    old = "v701",
    new = "F_Edu") %>% 
  head
Data$F_Edu <- factor(Data$F_Edu, levels = c("No Education","Primary","Secondary","Higher"))

# Mother's Education#
Data <- expss::apply_labels(
  Data,
  v106=c("No Education"=0,"Primary"=1,"Secondary"=2,"Higher"=3))
Data %>% 
  data.table::setnames(
    old = "v106",
    new = "M_Edu") %>% 
  head
Data$M_Edu <- factor(Data$M_Edu, levels = c("No Education","Primary","Secondary","Higher"))

# Mother's Occupation#
Data <- expss::apply_labels(
  Data,
  v714=c("No"=0,"Yes"=1))
Data %>% 
  data.table::setnames(
    old = "v714",
    new = "M_Job") %>% 
  head
Data$M_Job <- factor(Data$M_Job, levels = c("No","Yes"))

# Child's Age in months
Data <- expss::apply_labels(
  Data,
  b8=c("<12"=0,"12-23"=1,"24-35"=2,"36-47"=3,"48-59"=4))
Data %>% 
  data.table::setnames(
    old = "b8",
    new = "Child_Age") %>% 
  head
Data$Child_Age <- factor(Data$Child_Age, levels = c("<12","12-23","24-35","36-47","48-59"))

# Child's Gender
Data <- expss::apply_labels(
  Data,
  b4=c("Male"=1,"Female"=2))
Data %>% 
  data.table::setnames(
    old = "b4",
    new = "Child_Sex") %>% 
  head
Data$Child_Sex <- factor(Data$Child_Sex, levels = c("Male","Female"))

# Mother Currently Breastfeeding
Data <- expss::apply_labels(
  Data,
  v404=c("No"=0,"Yes"=1))
Data %>% 
  data.table::setnames(
    old = "v404",
    new = "Breastfeeding") %>% 
  head
Data$Breastfeeding <- factor(Data$Breastfeeding, levels = c("No","Yes"))

# Vitamin A Consumption
Data <- expss::apply_labels(
  Data,
  s321f=c("No"=0,"Yes"=1))
Data %>% 
  data.table::setnames(
    old = "s321f",
    new = "Vit_A") %>% 
  head
Data$Vit_A <- factor(Data$Vit_A, levels = c("No","Yes"))

# Delivery by Cesarean Section
Data <- expss::apply_labels(
  Data,
  m17=c("No"=0,"Yes"=1))
Data %>% 
  data.table::setnames(
    old = "m17",
    new = "Ceaserean") %>% 
  head
Data$Ceaserean <- factor(Data$Ceaserean, levels = c("No","Yes"))
------------------------

# Recode the categories
# Father's Occupation#
Data$F_Job <- recode(Data$v704,
                      '0' = "No",
                      '61' = "No",
                      '11' = "Yes",
                      '12' = "Yes",
                      '13' = "Yes",
                      '14' = "Yes",
                      '15' = "Yes",
                      '16' = "Yes",
                      '21' = "Yes",
                      '22'= "Yes",
                      '23' = "Yes",
                      '31' = "Yes",
                      '41' = "Yes",
                      '51' = "Yes",
                      '52' = "Yes",
                      '96' = "Yes",
                      .keep_value_labels = FALSE)
Data$F_Job <- factor(Data$F_Job, levels = c("No","Yes"))

# Place of Delivery
Data$Delivery_place <- recode(Data$m15,
                               '11' = "Others",
                               '42' = "Others",
                               '96' = "Others",
                               '21' = "Health facility",
                               '22' = "Health facility",
                               '23' = "Health facility",
                               '24' = "Health facility",
                               '25' = "Health facility",
                               '26' = "Health facility",
                               '27' = "Health facility",
                               '28' = "Health facility",
                               '31' = "Health facility",
                               '32' = "Health facility",
                               '33' = "Health facility",
                               '36' = "Health facility",
                               '41' = "Health facility",
                               .keep_value_labels = FALSE)
Data$Delivery_place <- factor(Data$Delivery_place, levels = c("Others","Health facility"))

# Access to Media 
# Read Newspaper/Magazine
Data$v157 <- ifelse(Data$v157 >= 1, 1, Data$v157)
# Listen to Radio
Data$v158 <- ifelse(Data$v158 >= 1, 1, Data$v158)
# Watch Television
Data$v159 <- ifelse(Data$v159 >= 1, 1, Data$v159)
#Aggregating the rows
Data$Media <- rowSums(Data[ , c('v157', 'v158', 'v159')],na.rm=TRUE)
Data$Media <- recode(Data$Media,
                      '0'="No",
                      '1'="Yes",
                      '2'="Yes",
                      '3'="Yes")
Data$Media <- factor(Data$Media, levels = c("No","Yes"))

# Solid Fuel Use
Data$Solid_Fuel <- recode(Data$v161,
                           '1' = "No",
                           '2' = "No",
                           '3' = "No",
                           '4' = "No",
                           '5' = "No",
                           '7' = "Yes",
                           '8' = "Yes",
                           '9' = "Yes",
                           '10' = "Yes",
                           '11' = "Yes",
                           '96' = "No",
                           .keep_value_labels = FALSE)
Data$Solid_Fuel <- factor(Data$Solid_Fuel, levels = c("No","Yes"))
------------------------


# Numeriv to Categorical
# Number of Household Members
Data <- within(Data, { 
  Household_member <- NA
  Household_member[v136 <= 5] <- "leq5"
  Household_member[v136 >5] <- "get5"
} )
Data$Household_member <- factor(Data$Household_member, levels = c("leq5","get5"))

#Birth Order#
Data <- within(Data, { 
  Birth_order <- NA
  Birth_order[bord<=1] <- "First birth"
  Birth_order[bord>1 & bord<=2] <- "Second birth"
  Birth_order[bord>=3] <- "Third birth and so"
} )
Data$Birth_order <- factor(Data$Birth_order, levels = c("First birth","Second birth","Third birth and so"))

#Mother's Age#
Data <- within(Data, { 
  Mother_age <- NA
  Mother_age[v447a<20] <- "Below 20"
  Mother_age[v447a>=20 & v447a<=34] <- "20-34"
  Mother_age[v447a>34] <- "Above 34"
} )
Data$Mother_age <- factor(Data$Mother_age, levels = c("Below 20","20-34","Above 34"))

# Mother's BMI
# Convert height
Data$v437 <- Data$v437/10
# Convery weight
Data$v438 <- Data$v438/1000
# Calculating BMI
Data$bmi <- Data$v437/(Data$v438^2)
Data <- within(Data, { 
  BMI <- NA
  BMI[bmi<18.5] <- "Underweight"
  BMI[bmi>=18.5 & bmi<25] <- "Normal"
  BMI[bmi>=25 & bmi<30] <- "Overweight"
  BMI[bmi>=30] <- "Obese"
} )
Data$BMI <- factor(Data$BMI, levels = c("Underweight","Normal","Overweight","Obese"))

#Mother's age at 1st Birth#
Data <- within(Data, { 
  Age_1stBirth <- NA
  Age_1stBirth[v212<18] <- "Below 18"
  Age_1stBirth[v212>=18] <- "18 or more"
} )
Data$Age_1stBirth <- factor(Data$Age_1stBirth, levels = c("Below 18","18 or more"))

# Toilet Facilities
Data <- within(Data, { 
  Toilet <- NA
  Toilet[v116<=15] <- "Improved"
  Toilet[v116>=21 & v116<=23] <- "Unimproved"
  Toilet[v116>=31] <- "No facility"
} )
Data$Toilet <- factor(Data$Toilet, levels = c("Improved","Unimproved","No facility"))

# Source of Drinking Water
Data <- within(Data, { 
  Water <- NA
  Water [v113<=14] <- "Piped"
  Water [v113==21] <- "Tubewell"
  Water [v113>=31] <- "Others"
} )
Data$Water <- factor(Data$Water, levels = c("Piped","Tubewell","Others"))

# Birth Interval
Data <- within(Data, { 
  Birth_interval <- NA
  Birth_interval[b11<24] <- "<24"
  Birth_interval[b11>=24 & b11<=47] <- "24-47"
  Birth_interval[b11>=48] <- ">47"
} )
Data$Birth_interval <- factor(Data$Birth_interval, levels = c("<24","24-47",">47"))
------------------------


# Select the Created Variables
Data <- Data %>%dplyr:: select(Division, Residence, Wealth, F_Edu, M_Edu, F_Job, M_Job, Household_member, Child_Age, Child_Sex, Breastfeeding, 
                              Birth_order, Media, Mother_age, BMI, Delivery_place, Age_1stBirth, Toilet, Solid_Fuel, Water, Vit_A, Ceaserean, 
                              Birth_interval, ch_ari,ch_diar, nt_ch_stunt,nt_ch_wast, nt_ch_underwt)
