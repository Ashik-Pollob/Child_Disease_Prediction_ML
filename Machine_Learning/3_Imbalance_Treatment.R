---
Title: "Child Diseases Inmbalance Treatment"
Author: "S.M. Ashikul Islam Pollob"
Date: "09-09-2023"
---

# libraries needed
library(ROSE)

# ARI
ari.rose <- ROSE(ch_ari ~ ., data = ari.train, seed = 111)$data


# Stunting
stn.rose <- ROSE(nt_ch_stunt ~ ., data = stn.train, seed = 111)$data


# Wasting
wst.rose <- ROSE(nt_ch_wast ~ ., data = wst.train, seed = 111)$data


# Underweight
und.rose <- ROSE(nt_ch_underwt ~ ., data = und.train, seed = 111)$data


# Diarrhea
dia.rose <- ROSE(ch_diar ~ ., data = dia.train, seed = 111)$data
