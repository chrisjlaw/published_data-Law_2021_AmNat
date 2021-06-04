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

load("Rdata/ERecologydata_1226_no_pinnipeds.Rdata")
load("Rdata/simmap/simmap_no_pinnipeds/simmap_locomotion_1000_sam500_no_pinnipeds.Rdata")
load("Rdata/simmap/simmap_no_pinnipeds/simmap_diet_1000_sam500_no_pinnipeds.Rdata")
load("Rdata/simmap/simmap_no_pinnipeds/simmap_hunting_1000_sam500_no_pinnipeds.Rdata")
load("Rdata/simmap/simmap_no_pinnipeds/simmap_diet_func_1000_sam500_no_pinnipeds.Rdata")


###### hbER ###### 
## BM ##
hbER_BM_500 <- list(length=length(simmap_diet_1000_sam500))
for(i in 1:length(simmap_diet_1000_sam500)){
  hbER_BM_500[[i]] <- OUwie(simmap_diet_1000_sam500[[i]], hbER_diet, model="BM1", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(hbER_BM_500, file="hbER_BM_500_no_pinnipeds.Rdata")

## OU ##
hbER_OU_500 <- list(length=length(simmap_diet_1000_sam500))
for(i in 1:length(simmap_diet_1000_sam500)){
  hbER_OU_500[[i]] <- OUwie(simmap_diet_1000_sam500[[i]], hbER_diet, model="OU1", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(hbER_OU_500, file="hbER_OU_500_no_pinnipeds.Rdata")

## OUM locomotion ##
hbER_OUM_locomotion <- list(length=length(simmap_locomotion_1000_sam500))

for(i in 1:length(simmap_locomotion_1000_sam500)){
  hbER_OUM_locomotion[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], hbER_locomotion, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(hbER_OUM_locomotion, file="hbER_OUM_locomotion_no_pinnipeds.Rdata")

## OUM hunting ##
hbER_OUM_hunting <- list(length=length(simmap_hunting_1000_sam500))

for(i in 1:length(simmap_hunting_1000_sam500)){
  hbER_OUM_hunting[[i]] <- OUwie(simmap_hunting_1000_sam500[[i]], hbER_hunting, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(hbER_OUM_hunting, file="hbER_OUM_hunting_no_pinnipeds.Rdata")

## OUM diet ##
hbER_OUM_diet <- list(length=length(simmap_diet_1000_sam500))

for(i in 1:length(simmap_diet_1000_sam500)){
  hbER_OUM_diet[[i]] <- OUwie(simmap_diet_1000_sam500[[i]], hbER_diet, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(hbER_OUM_diet, file="hbER_OUM_diet_no_pinnipeds.Rdata")

## OUM bayou ##

hbER_OUM_bayou <- OUwie(simmap_bayou_hbER_mcc, hbER_bayou, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=F)
  print(i)

save(hbER_OUM_bayou, file="hbER_OUM_bayou_no_pinnipeds.Rdata")


#### hbER no pinnipeds AIC ####
load("Rdata/OUwie_models/OUwie_500_no_pinnipeds/hbER_BM_500_no_pinnipeds.Rdata")
load("Rdata/OUwie_models/OUwie_500_no_pinnipeds/hbER_OU_500_no_pinnipeds.Rdata")
load("Rdata/OUwie_models/OUwie_500_no_pinnipeds/hbER_OUM_diet_no_pinnipeds.Rdata")
load("Rdata/OUwie_models/OUwie_500_no_pinnipeds/hbER_OUM_locomotion_no_pinnipeds.Rdata")
load("Rdata/OUwie_models/OUwie_500_no_pinnipeds/hbER_OUM_hunting_no_pinnipeds.Rdata")
load("Rdata/OUwie_models/OUwie_500_no_pinnipeds/hbER_OUM_bayou_no_pinnipeds.Rdata")

load("Rdata/simmap/simmap_bayou_hbER_500.Rdata")

aic.table_hbER <- matrix(,length(simmap_diet_1000_sam500),6, dimnames=list(c(1: length(simmap_diet_1000_sam500)), c("BM1", "OU1", "OUM_locomotion", "OUM_hunting", "OUM_diet", "OUM_bayou")))

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
  geiger::aicw(c(
    mean(aic.table_hbER[,1]),
    mean(aic.table_hbER[,2]),
    mean(aic.table_hbER[,3]),
    mean(aic.table_hbER[,4]),
    mean(aic.table_hbER[,5]),
    mean(aic.table_hbER[,6])
  ))
rownames(aicw_table_hbER) <- c("BM1", "OU1", "OUM_locomotion", "OUM_hunting", "OUM_diet", "bayou")
aicw_table_hbER
write.csv(aicw_table_hbER, file="aicw_table_hbER_no_pinnipeds_full.csv")

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
