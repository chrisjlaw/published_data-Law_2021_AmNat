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

###### hbER_sc ###### 
lnhbER_sc_all <- phyl.resid(tree_prune, lnsize_all, lnhbER_all, method = "lambda")$resid
colnames(lnhbER_sc_all) <- "lnhbER_sc"
lnhbER_sc_all<-as.numeric(as.character(lnhbER_sc_all))
names(lnhbER_sc_all) <- rownames(data_mean_pr)

hbER_sc_diet <- data.frame(species_all, diet, lnhbER_sc_all)
hbER_sc_locomotion <- data.frame(species_all, locomotion, lnhbER_sc_all)
hbER_sc_hunting <- data.frame(species_all, hunting, lnhbER_sc_all)

## BM ##
hbER_sc_BM_500 <- list(length=length(simmap_diet_1000_sam500))
for(i in 1:length(simmap_diet_1000_sam500)){
  hbER_sc_BM_500[[i]] <- OUwie(simmap_diet_1000_sam500[[i]], hbER_sc_diet, model="BM1", simmap.tree=TRUE,root.station=TRUE, mserr = "none", diagn=T)
  print(i)
}
save(hbER_sc_BM_500, file="hbER_sc_BM_500.Rdata")

## OU ##
hbER_sc_OU_500 <- list(length=length(simmap_diet_1000_sam500))
for(i in 1:length(simmap_diet_1000_sam500)){
  hbER_sc_OU_500[[i]] <- OUwie(simmap_diet_1000_sam500[[i]], hbER_sc_diet, model="OU1", simmap.tree=TRUE,root.station=TRUE, mserr = "none", diagn=T)
  print(i)
}
save(hbER_sc_OU_500, file="hbER_sc_OU_500.Rdata")

## OUM locomotion ##
hbER_sc_OUM_locomotion <- list(length=length(simmap_locomotion_1000_sam500))

for(i in 1:length(simmap_locomotion_1000_sam500)){
  hbER_sc_OUM_locomotion[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], hbER_sc_locomotion, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "none", diagn=T)
  print(i)
}
save(hbER_sc_OUM_locomotion, file="hbER_sc_OUM_locomotion.Rdata")

## OUM hunting ##
hbER_sc_OUM_hunting <- list(length=length(simmap_hunting_1000_sam500))

for(i in 1:length(simmap_hunting_1000_sam500)){
  hbER_sc_OUM_hunting[[i]] <- OUwie(simmap_hunting_1000_sam500[[i]], hbER_sc_hunting, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "none", diagn=T)
  print(i)
}
save(hbER_sc_OUM_hunting, file="hbER_sc_OUM_hunting.Rdata")

## OUM diet ##
hbER_sc_OUM_diet <- list(length=length(simmap_diet_1000_sam500))

for(i in 1:length(simmap_diet_1000_sam500)){
  hbER_sc_OUM_diet[[i]] <- OUwie(simmap_diet_1000_sam500[[i]], hbER_sc_diet, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "none", diagn=T)
  print(i)
}
save(hbER_sc_OUM_diet, file="hbER_sc_OUM_diet.Rdata")

## OUM bayou ##
hbER_sc_OUM_bayou <- OUwie(simmap_bayou_hbER_mcc, hbER_sc_bayou_reg, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "none", diagn=F)
save(hbER_sc_OUM_bayou, file="hbER_sc_OUM_bayou.Rdata")


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


#### hbER AIC ####
load("Rdata/OUwie_models/OUwie_500_sc/hbER_sc_BM_500.Rdata")
load("Rdata/OUwie_models/OUwie_500_sc/hbER_sc_OU_500.Rdata")
load("Rdata/OUwie_models/OUwie_500_sc/hbER_sc_OUM_diet.Rdata")
load("Rdata/OUwie_models/OUwie_500_sc/hbER_sc_OUM_locomotion.Rdata")
load("Rdata/OUwie_models/OUwie_500_sc/hbER_sc_OUM_hunting.Rdata")
load("Rdata/OUwie_models/OUwie_500_sc/hbER_sc_OUM_bayou.Rdata")

load("Rdata/simmap/simmap_bayou_hbER_500.Rdata")

aic.table_hbER_sc <- matrix(,length(simmap_diet_1000_sam500),6, dimnames=list(c(1: length(simmap_diet_1000_sam500)), c("BM1", "OU1", "OUM_locomotion", "OUM_hunting", "OUM_diet", "bayou")))

for(i in 1:length(simmap_diet_1000_sam500)){
  aic.table_hbER_sc[i,1] <- hbER_sc_BM_500[[i]]$AICc
  aic.table_hbER_sc[i,2] <- hbER_sc_OU_500[[i]]$AICc
  aic.table_hbER_sc[i,3] <- hbER_sc_OUM_locomotion[[i]]$AICc
  aic.table_hbER_sc[i,4] <- hbER_sc_OUM_hunting[[i]]$AICc
  aic.table_hbER_sc[i,5] <- hbER_sc_OUM_diet[[i]]$AICc
  aic.table_hbER_sc[i,6] <- hbER_sc_OUM_bayou$AICc
  
}
aic.table_hbER_sc

aicw_table_hbER_sc<- 
  aicw(c(
    mean(aic.table_hbER_sc[,1]),
    mean(aic.table_hbER_sc[,2]),
    mean(aic.table_hbER_sc[,3]),
    mean(aic.table_hbER_sc[,4]),
    mean(aic.table_hbER_sc[,5]),
    mean(aic.table_hbER_sc[,6])
  ))
rownames(aicw_table_hbER_sc) <- c("BM1", "OU1", "OUM_locomotion", "OUM_hunting", "OUM_diet", "bayou")
aicw_table_hbER_sc
write.csv(aicw_table_hbER_sc, file="aicw_table_hbER_sc_full.csv")

aicw_all_hbER_sc <- apply(aic.table_hbER_sc, 1, aicw)

a_hbER_sc <- data.frame(aicw_all_hbER_sc) %>%
  rownames_to_column("model") %>%
  gather(-model, key = "type", value = "value") %>%
  separate(type, into = c("simulation_no", "type")) %>%
  mutate(simulation_no = as.numeric(gsub("X", "", simulation_no))) %>%
  spread(type, value)

aicw_table_hbER_sc_dist <- a_hbER_sc  %>% 
  group_by(model) %>% #groups by term
  summarise(mean_fit = mean(fit), mean_delta = mean(delta), mean_w = mean(w)) 
write.csv(aicw_table_hbER_sc_dist, file="aicw_table_hbER_sc_dist.csv")

#### headER AIC ####
load("Rdata/OUwie_models/OUwie_500_sc/headER_BM_500.Rdata")
load("Rdata/OUwie_models/OUwie_500_sc/headER_OU_500.Rdata")
load("Rdata/OUwie_models/OUwie_500_sc/headER_OUM_diet.Rdata")
load("Rdata/OUwie_models/OUwie_500_sc/headER_OUM_locomotion.Rdata")
load("Rdata/OUwie_models/OUwie_500_sc/headER_OUM_hunting.Rdata")
load("Rdata/OUwie_models/OUwie_500_sc/headER_OUM_bayou.Rdata")

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


#### AEI_C AIC ####
load("Rdata/OUwie_models/OUwie_500_sc/AEI_C_BM_500.Rdata")
load("Rdata/OUwie_models/OUwie_500_sc/AEI_C_OU_500.Rdata")
load("Rdata/OUwie_models/OUwie_500_sc/AEI_C_OUM_diet.Rdata")
load("Rdata/OUwie_models/OUwie_500_sc/AEI_C_OUM_locomotion.Rdata")
load("Rdata/OUwie_models/OUwie_500_sc/AEI_C_OUM_hunting.Rdata")


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
  aicw(c(
    mean(aic.table_AEI_C[,1]),
    mean(aic.table_AEI_C[,2]),
    mean(aic.table_AEI_C[,3]),
    mean(aic.table_AEI_C[,4]),
    mean(aic.table_AEI_C[,5])
  ))
rownames(aicw_table_AEI_C) <- c("BM1", "OU1", "OUM_locomotion", "OUM_hunting", "OUM_diet")
aicw_table_AEI_C
write.csv(aicw_table_AEI_C, file="aicw_table_AEI_C.csv")


#### AEI_T AIC ####
load("Rdata/OUwie_models/OUwie_500_sc/AEI_T_BM_500.Rdata")
load("Rdata/OUwie_models/OUwie_500_sc/AEI_T_OU_500.Rdata")
load("Rdata/OUwie_models/OUwie_500_sc/AEI_T_OUM_diet.Rdata")
load("Rdata/OUwie_models/OUwie_500_sc/AEI_T_OUM_locomotion.Rdata")
load("Rdata/OUwie_models/OUwie_500_sc/AEI_T_OUM_hunting.Rdata")


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
  aicw(c(
    mean(aic.table_AEI_T[,1]),
    mean(aic.table_AEI_T[,2]),
    mean(aic.table_AEI_T[,3]),
    mean(aic.table_AEI_T[,4]),
    mean(aic.table_AEI_T[,5])
  ))
rownames(aicw_table_AEI_T) <- c("BM1", "OU1", "OUM_locomotion", "OUM_hunting", "OUM_diet")
aicw_table_AEI_T
write.csv(aicw_table_AEI_T, file="aicw_table_AEI_T.csv")


#### AEI_L AIC ####
load("Rdata/OUwie_models/OUwie_500_sc/AEI_L_BM_500.Rdata")
load("Rdata/OUwie_models/OUwie_500_sc/AEI_L_OU_500.Rdata")
load("Rdata/OUwie_models/OUwie_500_sc/AEI_L_OUM_diet.Rdata")
load("Rdata/OUwie_models/OUwie_500_sc/AEI_L_OUM_locomotion.Rdata")
load("Rdata/OUwie_models/OUwie_500_sc/AEI_L_OUM_hunting.Rdata")


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
  aicw(c(
    mean(aic.table_AEI_L[,1]),
    mean(aic.table_AEI_L[,2]),
    mean(aic.table_AEI_L[,3]),
    mean(aic.table_AEI_L[,4]),
    mean(aic.table_AEI_L[,5])
  ))
rownames(aicw_table_AEI_L) <- c("BM1", "OU1", "OUM_locomotion", "OUM_hunting", "OUM_diet")
aicw_table_AEI_L
write.csv(aicw_table_AEI_L, file="aicw_table_AEI_L.csv")


#### AEI_S AIC ####
load("Rdata/OUwie_models/OUwie_500_sc/AEI_S_BM_500.Rdata")
load("Rdata/OUwie_models/OUwie_500_sc/AEI_S_OU_500.Rdata")
load("Rdata/OUwie_models/OUwie_500_sc/AEI_S_OUM_diet.Rdata")
load("Rdata/OUwie_models/OUwie_500_sc/AEI_S_OUM_locomotion.Rdata")
load("Rdata/OUwie_models/OUwie_500_sc/AEI_S_OUM_hunting.Rdata")
load("Rdata/OUwie_models/OUwie_500_sc/AEI_S_OUM_bayou.Rdata")

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


#### rib_sc AIC ####
load("Rdata/OUwie_models/OUwie_500_sc/rib_sc_BM_500.Rdata")
load("Rdata/OUwie_models/OUwie_500_sc/rib_sc_OU_500.Rdata")
load("Rdata/OUwie_models/OUwie_500_sc/rib_sc_OUM_diet.Rdata")
load("Rdata/OUwie_models/OUwie_500_sc/rib_sc_OUM_locomotion.Rdata")
load("Rdata/OUwie_models/OUwie_500_sc/rib_sc_OUM_hunting.Rdata")
load("Rdata/OUwie_models/OUwie_500_sc/rib_sc_OUM_bayou.Rdata")

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

