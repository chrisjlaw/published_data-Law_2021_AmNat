library(plotrix)
library(plyr)
library(tidyverse)
library(phytools)
library(geiger)
library(phylolm)
library(ratematrix)
library(ggplot2)
library(evomap)
library(RRPP)
library(convevol)
library(OUwie)

options(scipen = 999)

load("Rdata/ERecologydata_1225.RData")
load("Rdata/simmap/simmap_locomotion_1000_sam500.Rdata")
load("Rdata/simmap/simmap_diet_1000_sam500.Rdata")
load("Rdata/simmap/simmap_hunting_1000_sam500.Rdata")


###### size ###### 
size_BM_500 <- list(length=length(simmap_diet_1000_sam500))
for(i in 1:length(simmap_diet_1000_sam500)){
  size_BM_500[[i]] <- OUwie(simmap_diet_1000_sam500[[i]], size_diet, model="BM1", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(size_BM_500, file="size_BM_500.Rdata")

## OU ##
size_OU_500 <- list(length=length(simmap_diet_1000_sam500))
for(i in 1:length(simmap_diet_1000_sam500)){
  size_OU_500[[i]] <- OUwie(simmap_diet_1000_sam500[[i]], size_diet, model="OU1", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(size_OU_500, file="size_OU_500.Rdata")

## OUM locomotion ##
size_OUM_locomotion <- list(length=length(simmap_locomotion_1000_sam500))

for(i in 1:length(simmap_locomotion_1000_sam500)){
  size_OUM_locomotion[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], size_locomotion, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(size_OUM_locomotion, file="size_OUM_locomotion.Rdata")

## OUM hunting ##
size_OUM_hunting <- list(length=length(simmap_hunting_1000_sam500))

for(i in 1:length(simmap_hunting_1000_sam500)){
  size_OUM_hunting[[i]] <- OUwie(simmap_hunting_1000_sam500[[i]], size_hunting, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(size_OUM_hunting, file="size_OUM_hunting.Rdata")

## OUM diet ##
size_OUM_diet <- list(length=length(simmap_diet_1000_sam500))

for(i in 1:length(simmap_diet_1000_sam500)){
  size_OUM_diet[[i]] <- OUwie(simmap_diet_1000_sam500[[i]], size_diet, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(size_OUM_diet, file="size_OUM_diet.Rdata")

## OUM bayou ##
size_OUM_bayou <- list(length=length(simmap_bayou_size_500))
for(i in 1:length(simmap_bayou_size_500)){
  size_OUM_bayou[[i]] <- OUwie(simmap_bayou_size_500[[i]], size_bayou_reg, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=F)
  print(i)
}
save(size_OUM_bayou, file="size_OUM_bayou.Rdata")

###### hbER ###### 
## BM ##
hbER_BM_500 <- list(length=length(simmap_diet_1000_sam500))
for(i in 1:length(simmap_diet_1000_sam500)){
  hbER_BM_500[[i]] <- OUwie(simmap_diet_1000_sam500[[i]], hbER_diet, model="BM1", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(hbER_BM_500, file="hbER_BM_500.Rdata")

## OU ##
hbER_OU_500 <- list(length=length(simmap_diet_1000_sam500))
for(i in 1:length(simmap_diet_1000_sam500)){
  hbER_OU_500[[i]] <- OUwie(simmap_diet_1000_sam500[[i]], hbER_diet, model="OU1", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(hbER_OU_500, file="hbER_OU_500.Rdata")

## OUM locomotion ##
hbER_OUM_locomotion <- list(length=length(simmap_locomotion_1000_sam500))

for(i in 1:length(simmap_locomotion_1000_sam500)){
  hbER_OUM_locomotion[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], hbER_locomotion, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(hbER_OUM_locomotion, file="hbER_OUM_locomotion.Rdata")

## OUM hunting ##
hbER_OUM_hunting <- list(length=length(simmap_hunting_1000_sam500))

for(i in 1:length(simmap_hunting_1000_sam500)){
  hbER_OUM_hunting[[i]] <- OUwie(simmap_hunting_1000_sam500[[i]], hbER_hunting, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(hbER_OUM_hunting, file="hbER_OUM_hunting.Rdata")

## OUM diet ##
hbER_OUM_diet <- list(length=length(simmap_diet_1000_sam500))

for(i in 1:length(simmap_diet_1000_sam500)){
  hbER_OUM_diet[[i]] <- OUwie(simmap_diet_1000_sam500[[i]], hbER_diet, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(hbER_OUM_diet, file="hbER_OUM_diet.Rdata")

## OUM bayou ##
hbER_OUM_bayou <- list(length=length(simmap_bayou_hbER_500))
for(i in 1:length(simmap_bayou_hbER_500)){
  hbER_OUM_bayou[[i]] <- OUwie(simmap_bayou_hbER_500[[i]], hbER_bayou_reg, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=F)
  print(i)
}
save(hbER_OUM_bayou, file="hbER_OUM_bayou.Rdata")

hbER_OUM_bayou <- OUwie(simmap_bayou_hbER_mcc, hbER_bayou_reg, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=F)
save(hbER_OUM_bayou, file="hbER_OUM_bayou.Rdata")

###### headER ###### 
headER_BM_500 <- list(length=length(simmap_locomotion_1000_sam500))
for(i in 1:length(simmap_locomotion_1000_sam500)){
  headER_BM_500[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], headER_diet, model="BM1", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(headER_BM_500, file="headER_BM_500.Rdata")

headER_OU_500 <- list(length=length(simmap_locomotion_1000_sam500))
for(i in 1:length(simmap_locomotion_1000_sam500)){
  headER_OU_500[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], headER_diet, model="OU1", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(headER_OU_500, file="headER_OU_500.Rdata")

## OUM locomotion ##
headER_OUM_locomotion <- list(length=length(simmap_locomotion_1000_sam500))

for(i in 1:length(simmap_locomotion_1000_sam500)){
  headER_OUM_locomotion[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], headER_locomotion, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(headER_OUM_locomotion, file="headER_OUM_locomotion.Rdata")

## OUM hunting ##
headER_OUM_hunting <- list(length=length(simmap_hunting_1000_sam500))

for(i in 1:length(simmap_hunting_1000_sam500)){
  headER_OUM_hunting[[i]] <- OUwie(simmap_hunting_1000_sam500[[i]], headER_hunting, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(headER_OUM_hunting, file="headER_OUM_hunting.Rdata")

## OUM diet ##
headER_OUM_diet <- list(length=length(simmap_diet_1000_sam500))

for(i in 1:length(simmap_diet_1000_sam500)){
  headER_OUM_diet[[i]] <- OUwie(simmap_diet_1000_sam500[[i]], headER_diet, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(headER_OUM_diet, file="headER_OUM_diet.Rdata")

## OUM bayou ##
headER_OUM_bayou <- list(length=length(simmap_bayou_headER_500))
for(i in 1:length(simmap_bayou_headER_500)){
  headER_OUM_bayou[[i]] <- OUwie(simmap_bayou_headER_500[[i]], headER_bayou_reg, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=F)
  print(i)
}
save(headER_OUM_bayou, file="headER_OUM_bayou.Rdata")

###### AEI_C ###### 
AEI_C_BM_500 <- list(length=length(simmap_locomotion_1000_sam500))
for(i in 1:length(simmap_locomotion_1000_sam500)){
  AEI_C_BM_500[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], AEI_C_diet, model="BM1", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(AEI_C_BM_500, file="AEI_C_BM_500.Rdata")

AEI_C_OU_500 <- list(length=length(simmap_locomotion_1000_sam500))
for(i in 1:length(simmap_locomotion_1000_sam500)){
  AEI_C_OU_500[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], AEI_C_diet, model="OU1", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(AEI_C_OU_500, file="AEI_C_OU_500.Rdata")

## OUM locomotion ##
AEI_C_OUM_locomotion <- list(length=length(simmap_locomotion_1000_sam500))

for(i in 1:length(simmap_locomotion_1000_sam500)){
  AEI_C_OUM_locomotion[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], AEI_C_locomotion, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(AEI_C_OUM_locomotion, file="AEI_C_OUM_locomotion.Rdata")

## OUM hunting ##
AEI_C_OUM_hunting <- list(length=length(simmap_hunting_1000_sam500))

for(i in 1:length(simmap_hunting_1000_sam500)){
  AEI_C_OUM_hunting[[i]] <- OUwie(simmap_hunting_1000_sam500[[i]], AEI_C_hunting, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(AEI_C_OUM_hunting, file="AEI_C_OUM_hunting.Rdata")

## OUM diet ##
AEI_C_OUM_diet <- list(length=length(simmap_diet_1000_sam500))

for(i in 1:length(simmap_diet_1000_sam500)){
  AEI_C_OUM_diet[[i]] <- OUwie(simmap_diet_1000_sam500[[i]], AEI_C_diet, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(AEI_C_OUM_diet, file="AEI_C_OUM_diet.Rdata")


###### AEI_T ###### 
AEI_T_BM_500 <- list(length=length(simmap_locomotion_1000_sam500))
for(i in 1:length(simmap_locomotion_1000_sam500)){
  AEI_T_BM_500[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], AEI_T_diet, model="BM1", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(AEI_T_BM_500, file="AEI_T_BM_500.Rdata")

AEI_T_OU_500 <- list(length=length(simmap_locomotion_1000_sam500))
for(i in 1:length(simmap_locomotion_1000_sam500)){
  AEI_T_OU_500[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], AEI_T_diet, model="OU1", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(AEI_T_OU_500, file="AEI_T_OU_500.Rdata")

## OUM locomotion ##
AEI_T_OUM_locomotion <- list(length=length(simmap_locomotion_1000_sam500))

for(i in 1:length(simmap_locomotion_1000_sam500)){
  AEI_T_OUM_locomotion[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], AEI_T_locomotion, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(AEI_T_OUM_locomotion, file="AEI_T_OUM_locomotion.Rdata")

## OUM hunting ##
AEI_T_OUM_hunting <- list(length=length(simmap_hunting_1000_sam500))

for(i in 1:length(simmap_hunting_1000_sam500)){
  AEI_T_OUM_hunting[[i]] <- OUwie(simmap_hunting_1000_sam500[[i]], AEI_T_hunting, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(AEI_T_OUM_hunting, file="AEI_T_OUM_hunting.Rdata")

## OUM diet ##
AEI_T_OUM_diet <- list(length=length(simmap_diet_1000_sam500))

for(i in 1:length(simmap_diet_1000_sam500)){
  AEI_T_OUM_diet[[i]] <- OUwie(simmap_diet_1000_sam500[[i]], AEI_T_diet, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(AEI_T_OUM_diet, file="AEI_T_OUM_diet.Rdata")


###### AEI_L ###### 
AEI_L_BM_500 <- list(length=length(simmap_locomotion_1000_sam500))
for(i in 1:length(simmap_locomotion_1000_sam500)){
  AEI_L_BM_500[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], AEI_L_diet, model="BM1", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(AEI_L_BM_500, file="AEI_L_BM_500.Rdata")

AEI_L_OU_500 <- list(length=length(simmap_locomotion_1000_sam500))
for(i in 1:length(simmap_locomotion_1000_sam500)){
  AEI_L_OU_500[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], AEI_L_diet, model="OU1", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(AEI_L_OU_500, file="AEI_L_OU_500.Rdata")

## OUM locomotion ##
AEI_L_OUM_locomotion <- list(length=length(simmap_locomotion_1000_sam500))

for(i in 1:length(simmap_locomotion_1000_sam500)){
  AEI_L_OUM_locomotion[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], AEI_L_locomotion, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(AEI_L_OUM_locomotion, file="AEI_L_OUM_locomotion.Rdata")

## OUM hunting ##
AEI_L_OUM_hunting <- list(length=length(simmap_hunting_1000_sam500))

for(i in 1:length(simmap_hunting_1000_sam500)){
  AEI_L_OUM_hunting[[i]] <- OUwie(simmap_hunting_1000_sam500[[i]], AEI_L_hunting, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(AEI_L_OUM_hunting, file="AEI_L_OUM_hunting.Rdata")

## OUM diet ##
AEI_L_OUM_diet <- list(length=length(simmap_diet_1000_sam500))

for(i in 1:length(simmap_diet_1000_sam500)){
  AEI_L_OUM_diet[[i]] <- OUwie(simmap_diet_1000_sam500[[i]], AEI_L_diet, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(AEI_L_OUM_diet, file="AEI_L_OUM_diet.Rdata")


###### AEI_S ###### 
AEI_S_BM_500 <- list(length=length(simmap_locomotion_1000_sam500))
for(i in 1:length(simmap_locomotion_1000_sam500)){
  AEI_S_BM_500[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], AEI_S_diet, model="BM1", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(AEI_S_BM_500, file="AEI_S_BM_500.Rdata")

AEI_S_OU_500 <- list(length=length(simmap_locomotion_1000_sam500))
for(i in 1:length(simmap_locomotion_1000_sam500)){
  AEI_S_OU_500[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], AEI_S_diet, model="OU1", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(AEI_S_OU_500, file="AEI_S_OU_500.Rdata")

## OUM locomotion ##
AEI_S_OUM_locomotion <- list(length=length(simmap_locomotion_1000_sam500))

for(i in 1:length(simmap_locomotion_1000_sam500)){
  AEI_S_OUM_locomotion[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], AEI_S_locomotion, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(AEI_S_OUM_locomotion, file="AEI_S_OUM_locomotion.Rdata")

## OUM hunting ##
AEI_S_OUM_hunting <- list(length=length(simmap_hunting_1000_sam500))

for(i in 1:length(simmap_hunting_1000_sam500)){
  AEI_S_OUM_hunting[[i]] <- OUwie(simmap_hunting_1000_sam500[[i]], AEI_S_hunting, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(AEI_S_OUM_hunting, file="AEI_S_OUM_hunting.Rdata")

## OUM diet ##
AEI_S_OUM_diet <- list(length=length(simmap_diet_1000_sam500))

for(i in 1:length(simmap_diet_1000_sam500)){
  AEI_S_OUM_diet[[i]] <- OUwie(simmap_diet_1000_sam500[[i]], AEI_S_diet, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(AEI_S_OUM_diet, file="AEI_S_OUM_diet.Rdata")

## OUM bayou ##
AEI_S_OUM_bayou <- list(length=length(simmap_bayou_AEI_S_500))
for(i in 1:length(simmap_bayou_AEI_S_500)){
  AEI_S_OUM_bayou[[i]] <- OUwie(simmap_bayou_AEI_S_500[[i]], AEI_S_bayou_reg, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=F)
  print(i)
}
save(AEI_S_OUM_bayou, file="AEI_S_OUM_bayou.Rdata")


###### rib_sc ###### 
rib_sc_BM_500 <- list(length=length(simmap_locomotion_1000_sam500))
for(i in 1:length(simmap_locomotion_1000_sam500)){
  rib_sc_BM_500[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], rib_sc_diet, model="BM1", simmap.tree=TRUE,root.station=TRUE, mserr = "none", diagn=T)
  print(i)
}
save(rib_sc_BM_500, file="rib_sc_BM_500.Rdata")

rib_sc_OU_500 <- list(length=length(simmap_locomotion_1000_sam500))
for(i in 1:length(simmap_locomotion_1000_sam500)){
  rib_sc_OU_500[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], rib_sc_diet, model="OU1", simmap.tree=TRUE,root.station=TRUE, mserr = "none", diagn=T)
  print(i)
}
save(rib_sc_OU_500, file="rib_sc_OU_500.Rdata")

## OUM locomotion ##
rib_sc_OUM_locomotion <- list(length=length(simmap_locomotion_1000_sam500))

for(i in 1:length(simmap_locomotion_1000_sam500)){
  rib_sc_OUM_locomotion[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], rib_sc_locomotion, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "none", diagn=T)
  print(i)
}
save(rib_sc_OUM_locomotion, file="rib_sc_OUM_locomotion.Rdata")

## OUM hunting ##
rib_sc_OUM_hunting <- list(length=length(simmap_hunting_1000_sam500))

for(i in 1:length(simmap_hunting_1000_sam500)){
  rib_sc_OUM_hunting[[i]] <- OUwie(simmap_hunting_1000_sam500[[i]], rib_sc_hunting, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "none", diagn=T)
  print(i)
}
save(rib_sc_OUM_hunting, file="rib_sc_OUM_hunting.Rdata")

## OUM diet ##
rib_sc_OUM_diet <- list(length=length(simmap_diet_1000_sam500))

for(i in 1:length(simmap_diet_1000_sam500)){
  rib_sc_OUM_diet[[i]] <- OUwie(simmap_diet_1000_sam500[[i]], rib_sc_diet, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "none", diagn=T)
  print(i)
}
save(rib_sc_OUM_diet, file="rib_sc_OUM_diet.Rdata")

## OUM bayou #3
rib_sc_OUM_bayou <- list(length=length(simmap_bayou_rib_sc_500))

for(i in 1:length(simmap_bayou_rib_sc_500)){
  rib_sc_OUM_bayou[[i]] <- OUwie(simmap_bayou_rib_sc_500[[i]], rib_sc_bayou_reg, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "none", diagn=T)
  print(i)
}
save(rib_sc_OUM_bayou, file="rib_sc_OUM_bayou.Rdata")




#### size AIC ####
load("Rdata/OUwie_models/OUwie_500/size_BM_500.Rdata")
load("Rdata/OUwie_models/OUwie_500/size_OU_500.Rdata")
load("Rdata/OUwie_models/OUwie_500/size_OUM_diet.Rdata")
load("Rdata/OUwie_models/OUwie_500/size_OUM_locomotion.Rdata")
load("Rdata/OUwie_models/OUwie_500/size_OUM_hunting.Rdata")
load("Rdata/OUwie_models/OUwie_500/size_OUM_bayou.Rdata")

load("Rdata/simmap/simmap_bayou_size_500.Rdata")

aic.table_size <- matrix(,length(simmap_diet_1000_sam500),6, dimnames=list(c(1: length(simmap_diet_1000_sam500)), c("BM1", "OU1", "OUM_locomotion", "OUM_hunting", "OUM_diet", "bayou")))

for(i in 1:length(simmap_diet_1000_sam500)){
  aic.table_size[i,1] <- size_BM_500[[i]]$AICc
  aic.table_size[i,2] <- size_OU_500[[i]]$AICc
  aic.table_size[i,3] <- size_OUM_locomotion[[i]]$AICc
  aic.table_size[i,4] <- size_OUM_hunting[[i]]$AICc
  aic.table_size[i,5] <- size_OUM_diet[[i]]$AICc
  aic.table_size[i,6] <- size_OUM_bayou[[i]]$AICc
  
  
}
aic.table_size

aicw_table_size<- 
  aicw(c(
    mean(aic.table_size[,1]),
    mean(aic.table_size[,2]),
    mean(aic.table_size[,3]),
    mean(aic.table_size[,4]),
    mean(aic.table_size[,5]),
    mean(aic.table_size[,6])
  ))
rownames(aicw_table_size) <- c("BM1", "OU1", "OUM_locomotion", "OUM_hunting", "OUM_diet", "bayou")
aicw_table_size
write.csv(aicw_table_size, file="aicw_table_size_full.csv")

aicw_all_size <- apply(aic.table_size, 1, aicw)

a_size <- data.frame(aicw_all_size) %>%
  rownames_to_column("model") %>%
  gather(-model, key = "type", value = "value") %>%
  separate(type, into = c("simulation_no", "type")) %>%
  mutate(simulation_no = as.numeric(gsub("X", "", simulation_no))) %>%
  spread(type, value)

aicw_table_size_dist <- a_size  %>% 
  group_by(model) %>% #groups by term
  summarise(mean_fit = mean(fit), mean_delta = mean(delta), mean_w = mean(w)) 
write.csv(aicw_table_size_dist, file="aicw_table_size_dist.csv")

#### hbER AIC ####
load("Rdata/OUwie_models/OUwie_500/hbER_BM_500.Rdata")
load("Rdata/OUwie_models/OUwie_500/hbER_OU_500.Rdata")
load("Rdata/OUwie_models/OUwie_500/hbER_OUM_diet.Rdata")
load("Rdata/OUwie_models/OUwie_500/hbER_OUM_locomotion.Rdata")
load("Rdata/OUwie_models/OUwie_500/hbER_OUM_hunting.Rdata")
load("Rdata/OUwie_models/OUwie_500/hbER_OUM_bayou.Rdata")

load("Rdata/simmap/simmap_bayou_hbER_500.Rdata")

aic.table_hbER <- matrix(,length(simmap_diet_1000_sam500),6, dimnames=list(c(1: length(simmap_diet_1000_sam500)), c("BM1", "OU1", "OUM_locomotion", "OUM_hunting", "OUM_diet", "bayou")))

for(i in 1:length(simmap_diet_1000_sam500)){
  aic.table_hbER[i,1] <- hbER_BM_500[[i]]$AICc
  aic.table_hbER[i,2] <- hbER_OU_500[[i]]$AICc
  aic.table_hbER[i,3] <- hbER_OUM_locomotion[[i]]$AICc
  aic.table_hbER[i,4] <- hbER_OUM_hunting[[i]]$AICc
  aic.table_hbER[i,5] <- hbER_OUM_diet[[i]]$AICc
  aic.table_hbER[i,6] <- hbER_OUM_bayou$AICc
  
}
aic.table_hbER

aicw_table_hbER<- 
  aicw(c(
    mean(aic.table_hbER[,1]),
    mean(aic.table_hbER[,2]),
    mean(aic.table_hbER[,3]),
    mean(aic.table_hbER[,4]),
    mean(aic.table_hbER[,5]),
    mean(aic.table_hbER[,6])
  ))
rownames(aicw_table_hbER) <- c("BM1", "OU1", "OUM_locomotion", "OUM_hunting", "OUM_diet", "bayou")
aicw_table_hbER
write.csv(aicw_table_hbER, file="aicw_table_hbER_full.csv")

aicw_all_hbER <- apply(aic.table_hbER, 1, aicw)

a_hbER <- data.frame(aicw_all_hbER) %>%
  rownames_to_column("model") %>%
  gather(-model, key = "type", value = "value") %>%
  separate(type, into = c("simulation_no", "type")) %>%
  mutate(simulation_no = as.numeric(gsub("X", "", simulation_no))) %>%
  spread(type, value)

aicw_table_hbER_dist <- a_hbER  %>% 
  group_by(model) %>% #groups by term
  summarise(mean_fit = mean(fit), mean_delta = mean(delta), mean_w = mean(w)) 
write.csv(aicw_table_hbER_dist, file="aicw_table_hbER_dist.csv")

#### headER AIC ####
load("Rdata/OUwie_models/OUwie_500/headER_BM_500.Rdata")
load("Rdata/OUwie_models/OUwie_500/headER_OU_500.Rdata")
load("Rdata/OUwie_models/OUwie_500/headER_OUM_diet.Rdata")
load("Rdata/OUwie_models/OUwie_500/headER_OUM_locomotion.Rdata")
load("Rdata/OUwie_models/OUwie_500/headER_OUM_hunting.Rdata")
load("Rdata/OUwie_models/OUwie_500/headER_OUM_bayou.Rdata")

load("Rdata/simmap/simmap_bayou_headER_500.Rdata")

aic.table_headER <- matrix(,length(simmap_diet_1000_sam500),6, dimnames=list(c(1: length(simmap_diet_1000_sam500)), c("BM1", "OU1", "OUM_locomotion", "OUM_hunting", "OUM_diet", "OUM_bayou")))

for(i in 1:length(simmap_diet_1000_sam500)){
  aic.table_headER[i,1] <- headER_BM_500[[i]]$AICc
  aic.table_headER[i,2] <- headER_OU_500[[i]]$AICc
  aic.table_headER[i,3] <- headER_OUM_locomotion[[i]]$AICc
  aic.table_headER[i,4] <- headER_OUM_hunting[[i]]$AICc
  aic.table_headER[i,5] <- headER_OUM_diet[[i]]$AICc
  aic.table_headER[i,6] <- headER_OUM_bayou[[i]]$AICc
  
  
}
aic.table_headER

aicw_table_headER<- 
  aicw(c(
    mean(aic.table_headER[,1]),
    mean(aic.table_headER[,2]),
    mean(aic.table_headER[,3]),
    mean(aic.table_headER[,4]),
    mean(aic.table_headER[,5]),
    mean(aic.table_headER[,6])
  ))
rownames(aicw_table_headER) <- c("BM1", "OU1", "OUM_locomotion", "OUM_hunting", "OUM_diet", "OUM_bayou")
aicw_table_headER
write.csv(aicw_table_headER, file="aicw_table_headER_full.csv")

aicw_all_headER <- apply(aic.table_headER, 1, geiger::aicw)

a_headER <- data.frame(aicw_all_headER) %>%
  rownames_to_column("model") %>%
  gather(-model, key = "type", value = "value") %>%
  separate(type, into = c("simulation_no", "type")) %>%
  mutate(simulation_no = as.numeric(gsub("X", "", simulation_no))) %>%
  spread(type, value)
write.csv(a_headER, file="a_headER.csv")

aicw_table_headER_dist <- a_headER  %>% 
  group_by(model) %>% #groups by term
  summarise(mean_fit = mean(fit), mean_delta = mean(delta), mean_w = mean(w)) 
write.csv(aicw_table_headER_dist, file="aicw_table_headER_dist.csv")

#### AEI_C AIC ####
load("Rdata/OUwie_models/OUwie_500/AEI_C_BM_500.Rdata")
load("Rdata/OUwie_models/OUwie_500/AEI_C_OU_500.Rdata")
load("Rdata/OUwie_models/OUwie_500/AEI_C_OUM_diet.Rdata")
load("Rdata/OUwie_models/OUwie_500/AEI_C_OUM_locomotion.Rdata")
load("Rdata/OUwie_models/OUwie_500/AEI_C_OUM_hunting.Rdata")



aic.table_AEI_C <- matrix(,length(simmap_diet_1000_sam500),5, dimnames=list(c(1: length(simmap_diet_1000_sam500)), c("BM1", "OU1", "OUM_locomotion", "OUM_hunting", "OUM_diet")))

for(i in 1:length(simmap_diet_1000_sam500)){
  aic.table_AEI_C[i,1] <- AEI_C_BM_500[[i]]$AICc
  aic.table_AEI_C[i,2] <- AEI_C_OU_500[[i]]$AICc
  aic.table_AEI_C[i,3] <- AEI_C_OUM_locomotion[[i]]$AICc
  aic.table_AEI_C[i,4] <- AEI_C_OUM_hunting[[i]]$AICc
  aic.table_AEI_C[i,5] <- AEI_C_OUM_diet[[i]]$AICc
  
}
aic.table_AEI_C

aicw_table_AEI_C<- 
  geiger::aicw(c(
    mean(aic.table_AEI_C[,1]),
    mean(aic.table_AEI_C[,2]),
    mean(aic.table_AEI_C[,3]),
    mean(aic.table_AEI_C[,4]),
    mean(aic.table_AEI_C[,5])
  ))
rownames(aicw_table_AEI_C) <- c("BM1", "OU1", "OUM_locomotion", "OUM_hunting", "OUM_diet")
aicw_table_AEI_C
write.csv(aicw_table_AEI_C, file="aicw_table_AEI_C.csv")

aicw_all_AEI_C <- apply(aic.table_AEI_C, 1, geiger::aicw)

a_AEI_C <- data.frame(aicw_all_AEI_C) %>%
  rownames_to_column("model") %>%
  gather(-model, key = "type", value = "value") %>%
  separate(type, into = c("simulation_no", "type")) %>%
  mutate(simulation_no = as.numeric(gsub("X", "", simulation_no))) %>%
  spread(type, value)
write.csv(a_AEI_C, file="a_AEI_C.csv")

aicw_table_AEI_C_dist <- a_AEI_C  %>% 
  group_by(model) %>% #groups by term
  summarise(mean_fit = mean(fit), mean_delta = mean(delta), mean_w = mean(w)) 
write.csv(aicw_table_AEI_C_dist, file="aicw_table_AEI_C_dist.csv")


#### AEI_T AIC ####
load("Rdata/OUwie_models/OUwie_500/AEI_T_BM_500.Rdata")
load("Rdata/OUwie_models/OUwie_500/AEI_T_OU_500.Rdata")
load("Rdata/OUwie_models/OUwie_500/AEI_T_OUM_diet.Rdata")
load("Rdata/OUwie_models/OUwie_500/AEI_T_OUM_locomotion.Rdata")
load("Rdata/OUwie_models/OUwie_500/AEI_T_OUM_hunting.Rdata")


aic.table_AEI_T <- matrix(,length(simmap_diet_1000_sam500),5, dimnames=list(c(1: length(simmap_diet_1000_sam500)), c("BM1", "OU1", "OUM_locomotion", "OUM_hunting", "OUM_diet")))

for(i in 1:length(simmap_diet_1000_sam500)){
  aic.table_AEI_T[i,1] <- AEI_T_BM_500[[i]]$AICc
  aic.table_AEI_T[i,2] <- AEI_T_OU_500[[i]]$AICc
  aic.table_AEI_T[i,3] <- AEI_T_OUM_locomotion[[i]]$AICc
  aic.table_AEI_T[i,4] <- AEI_T_OUM_hunting[[i]]$AICc
  aic.table_AEI_T[i,5] <- AEI_T_OUM_diet[[i]]$AICc
  
}
aic.table_AEI_T

aicw_table_AEI_T<- 
  geiger::aicw(c(
    mean(aic.table_AEI_T[,1]),
    mean(aic.table_AEI_T[,2]),
    mean(aic.table_AEI_T[,3]),
    mean(aic.table_AEI_T[,4]),
    mean(aic.table_AEI_T[,5])
  ))
rownames(aicw_table_AEI_T) <- c("BM1", "OU1", "OUM_locomotion", "OUM_hunting", "OUM_diet")
aicw_table_AEI_T
write.csv(aicw_table_AEI_T, file="aicw_table_AEI_T.csv")

aicw_all_AEI_T <- apply(aic.table_AEI_T, 1, geiger::aicw)

a_AEI_T <- data.frame(aicw_all_AEI_T) %>%
  rownames_to_column("model") %>%
  gather(-model, key = "type", value = "value") %>%
  separate(type, into = c("simulation_no", "type")) %>%
  mutate(simulation_no = as.numeric(gsub("X", "", simulation_no))) %>%
  spread(type, value)
write.csv(a_AEI_T, file="a_AEI_T.csv")

aicw_table_AEI_T_dist <- a_AEI_T  %>% 
  group_by(model) %>% #groups by term
  summarise(mean_fit = mean(fit), mean_delta = mean(delta), mean_w = mean(w)) 
write.csv(aicw_table_AEI_T_dist, file="aicw_table_AEI_T_dist.csv")

#### AEI_L AIC ####
load("Rdata/OUwie_models/OUwie_500/AEI_L_BM_500.Rdata")
load("Rdata/OUwie_models/OUwie_500/AEI_L_OU_500.Rdata")
load("Rdata/OUwie_models/OUwie_500/AEI_L_OUM_diet.Rdata")
load("Rdata/OUwie_models/OUwie_500/AEI_L_OUM_locomotion.Rdata")
load("Rdata/OUwie_models/OUwie_500/AEI_L_OUM_hunting.Rdata")


aic.table_AEI_L <- matrix(,length(simmap_diet_1000_sam500),5, dimnames=list(c(1: length(simmap_diet_1000_sam500)), c("BM1", "OU1", "OUM_locomotion", "OUM_hunting", "OUM_diet")))

for(i in 1:length(simmap_diet_1000_sam500)){
  aic.table_AEI_L[i,1] <- AEI_L_BM_500[[i]]$AICc
  aic.table_AEI_L[i,2] <- AEI_L_OU_500[[i]]$AICc
  aic.table_AEI_L[i,3] <- AEI_L_OUM_locomotion[[i]]$AICc
  aic.table_AEI_L[i,4] <- AEI_L_OUM_hunting[[i]]$AICc
  aic.table_AEI_L[i,5] <- AEI_L_OUM_diet[[i]]$AICc
  
}
aic.table_AEI_L

aicw_table_AEI_L<- 
  geiger::aicw(c(
    mean(aic.table_AEI_L[,1]),
    mean(aic.table_AEI_L[,2]),
    mean(aic.table_AEI_L[,3]),
    mean(aic.table_AEI_L[,4]),
    mean(aic.table_AEI_L[,5])
  ))
rownames(aicw_table_AEI_L) <- c("BM1", "OU1", "OUM_locomotion", "OUM_hunting", "OUM_diet")
aicw_table_AEI_L
write.csv(aicw_table_AEI_L, file="aicw_table_AEI_L.csv")

aicw_all_AEI_L <- apply(aic.table_AEI_L, 1, geiger::aicw)

a_AEI_L <- data.frame(aicw_all_AEI_L) %>%
  rownames_to_column("model") %>%
  gather(-model, key = "type", value = "value") %>%
  separate(type, into = c("simulation_no", "type")) %>%
  mutate(simulation_no = as.numeric(gsub("X", "", simulation_no))) %>%
  spread(type, value)
write.csv(a_AEI_L, file="a_AEI_L.csv")

aicw_table_AEI_L_dist <- a_AEI_L  %>% 
  group_by(model) %>% #groups by term
  summarise(mean_fit = mean(fit), mean_delta = mean(delta), mean_w = mean(w)) 
write.csv(aicw_table_AEI_L_dist, file="aicw_table_AEI_L_dist.csv")

#### AEI_S AIC ####
load("Rdata/OUwie_models/OUwie_500/AEI_S_BM_500.Rdata")
load("Rdata/OUwie_models/OUwie_500/AEI_S_OU_500.Rdata")
load("Rdata/OUwie_models/OUwie_500/AEI_S_OUM_diet.Rdata")
load("Rdata/OUwie_models/OUwie_500/AEI_S_OUM_locomotion.Rdata")
load("Rdata/OUwie_models/OUwie_500/AEI_S_OUM_hunting.Rdata")
load("Rdata/OUwie_models/OUwie_500/AEI_S_OUM_bayou.Rdata")

load("Rdata/simmap/simmap_bayou_AEI_S_500.Rdata")

aic.table_AEI_S <- matrix(,length(simmap_diet_1000_sam500),6, dimnames=list(c(1: length(simmap_diet_1000_sam500)), c("BM1", "OU1", "OUM_locomotion", "OUM_hunting", "OUM_diet", "OUM_bayou")))

for(i in 1:length(simmap_diet_1000_sam500)){
  aic.table_AEI_S[i,1] <- AEI_S_BM_500[[i]]$AICc
  aic.table_AEI_S[i,2] <- AEI_S_OU_500[[i]]$AICc
  aic.table_AEI_S[i,3] <- AEI_S_OUM_locomotion[[i]]$AICc
  aic.table_AEI_S[i,4] <- AEI_S_OUM_hunting[[i]]$AICc
  aic.table_AEI_S[i,5] <- AEI_S_OUM_diet[[i]]$AICc
  aic.table_AEI_S[i,6] <- AEI_S_OUM_bayou[[i]]$AICc
  
  
}
aic.table_AEI_S

aicw_table_AEI_S<- 
  aicw(c(
    mean(aic.table_AEI_S[,1]),
    mean(aic.table_AEI_S[,2]),
    mean(aic.table_AEI_S[,3]),
    mean(aic.table_AEI_S[,4]),
    mean(aic.table_AEI_S[,5]),
    mean(aic.table_AEI_S[,6])
  ))
rownames(aicw_table_AEI_S) <- c("BM1", "OU1", "OUM_locomotion", "OUM_hunting", "OUM_diet", "OUM_bayou")
aicw_table_AEI_S
write.csv(aicw_table_AEI_S, file="aicw_table_AEI_S_full.csv")

aicw_all_AEI_S <- apply(aic.table_AEI_S, 1, geiger::aicw)

a_AEI_S <- data.frame(aicw_all_AEI_S) %>%
  rownames_to_column("model") %>%
  gather(-model, key = "type", value = "value") %>%
  separate(type, into = c("simulation_no", "type")) %>%
  mutate(simulation_no = as.numeric(gsub("X", "", simulation_no))) %>%
  spread(type, value)
write.csv(a_AEI_S, file="a_AEI_S.csv")

aicw_table_AEI_S_dist <- a_AEI_S  %>% 
  group_by(model) %>% #groups by term
  summarise(mean_fit = mean(fit), mean_delta = mean(delta), mean_w = mean(w)) 
write.csv(aicw_table_AEI_S_dist, file="aicw_table_AEI_S_dist.csv")


#### rib_sc AIC ####
load("Rdata/OUwie_models/OUwie_500/rib_sc_BM_500.Rdata")
load("Rdata/OUwie_models/OUwie_500/rib_sc_OU_500.Rdata")
load("Rdata/OUwie_models/OUwie_500/rib_sc_OUM_diet.Rdata")
load("Rdata/OUwie_models/OUwie_500/rib_sc_OUM_locomotion.Rdata")
load("Rdata/OUwie_models/OUwie_500/rib_sc_OUM_hunting.Rdata")
load("Rdata/OUwie_models/OUwie_500/rib_sc_OUM_bayou.Rdata")

load("Rdata/simmap/simmap_bayou_rib_sc_500.Rdata")

aic.table_rib_sc <- matrix(,length(simmap_diet_1000_sam500),6, dimnames=list(c(1: length(simmap_diet_1000_sam500)), c("BM1", "OU1", "OUM_locomotion", "OUM_hunting", "OUM_diet", "OUM_bayou")))

for(i in 1:length(simmap_diet_1000_sam500)){
  aic.table_rib_sc[i,1] <- rib_sc_BM_500[[i]]$AICc
  aic.table_rib_sc[i,2] <- rib_sc_OU_500[[i]]$AICc
  aic.table_rib_sc[i,3] <- rib_sc_OUM_locomotion[[i]]$AICc
  aic.table_rib_sc[i,4] <- rib_sc_OUM_hunting[[i]]$AICc
  aic.table_rib_sc[i,5] <- rib_sc_OUM_diet[[i]]$AICc
  aic.table_rib_sc[i,6] <- rib_sc_OUM_bayou[[i]]$AICc
}

aic.table_rib_sc

aicw_table_rib_sc<- 
  aicw(c(
    mean(aic.table_rib_sc[,1]),
    mean(aic.table_rib_sc[,2]),
    mean(aic.table_rib_sc[,3]),
    mean(aic.table_rib_sc[,4]),
    mean(aic.table_rib_sc[,5]),
    mean(aic.table_rib_sc[,6])
  ))
rownames(aicw_table_rib_sc) <- c("BM1", "OU1", "OUM_locomotion", "OUM_hunting", "OUM_diet", "OUM_bayou")
aicw_table_rib_sc
write.csv(aicw_table_rib_sc, file="aicw_table_rib_sc_full.csv")

aicw_all_rib_sc <- apply(aic.table_rib_sc, 1, geiger::aicw)

a_rib_sc <- data.frame(aicw_all_rib_sc) %>%
  rownames_to_column("model") %>%
  gather(-model, key = "type", value = "value") %>%
  separate(type, into = c("simulation_no", "type")) %>%
  mutate(simulation_no = as.numeric(gsub("X", "", simulation_no))) %>%
  spread(type, value)
write.csv(a_rib_sc, file="a_rib_sc.csv")

aicw_table_rib_sc_dist <- a_rib_sc  %>% 
  group_by(model) %>% #groups by term
  summarise(mean_fit = mean(fit), mean_delta = mean(delta), mean_w = mean(w)) 
write.csv(aicw_table_rib_sc_dist, file="aicw_table_rib_sc_dist.csv")

