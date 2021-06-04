library(tidyverse)
library(data.table)


load("Rdata/simmap/simmap_locomotion_1000_sam500.Rdata")
load("Rdata/simmap/simmap_hunting_1000_sam500.Rdata")
load("Rdata/simmap/simmap_diet_1000_sam500.Rdata")


###### OUwie.sim for size ###### 
load("Rdata/OUwie_models/OUwie_500/size_OUM_hunting.Rdata")

#alpha
size_OU_alpha <- matrix(,length(simmap_hunting_1000_sam500),1,dimnames=list(c(1: length(simmap_hunting_1000_sam500)),c("size_OU_alpha")))
for(i in 1:length(simmap_hunting_1000_sam500)){
  size_OU_alpha[i,1] <- size_OUM_hunting[[i]]$solution[1,1]
}

#sigma
size_OU_sigma <- matrix(,length(simmap_hunting_1000_sam500),1,dimnames=list(c(1: length(simmap_hunting_1000_sam500)),c("size_OU_sigma")))
for(i in 1:length(simmap_hunting_1000_sam500)){
  size_OU_sigma[i,1] <- size_OUM_hunting[[i]]$solution[2,1]
}

size_OUM_hunting_theta <- matrix(,length(simmap_hunting_1000_sam500),6, dimnames=list(c(1: length(simmap_hunting_1000_sam500)), c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial")))
for(i in 1:length(simmap_hunting_1000_sam500)){
  size_OUM_hunting_theta[i,1] <- size_OUM_hunting[[i]]$theta[1,1]
  size_OUM_hunting_theta[i,2] <- size_OUM_hunting[[i]]$theta[2,1]
  size_OUM_hunting_theta[i,3] <- size_OUM_hunting[[i]]$theta[3,1]
  size_OUM_hunting_theta[i,4] <- size_OUM_hunting[[i]]$theta[4,1]
  size_OUM_hunting_theta[i,5] <- size_OUM_hunting[[i]]$theta[5,1]
  size_OUM_hunting_theta[i,6] <- size_OUM_hunting[[i]]$theta[6,1]
}

size_OU_sim.data <- list(length=length(simmap_hunting_1000_sam500))
for(i in 1:length(simmap_hunting_1000_sam500)){
  size_OU_sim.data[[i]]<-OUwie.sim(simmap_hunting_1000_sam500[[i]], simmap.tree=TRUE,scaleHeight=FALSE, alpha=c(
    mean(size_OU_alpha),
    mean(size_OU_alpha),
    mean(size_OU_alpha),
    mean(size_OU_alpha),
    mean(size_OU_alpha),
    mean(size_OU_alpha)),
    sigma.sq=c(mean(size_OU_sigma),
               mean(size_OU_sigma),
               mean(size_OU_sigma),
               mean(size_OU_sigma),
               mean(size_OU_sigma),
               mean(size_OU_sigma)),
    theta=c(mean(size_OUM_hunting_theta[,1]), 
            mean(size_OUM_hunting_theta[,2]), 
            mean(size_OUM_hunting_theta[,3]), 
            mean(size_OUM_hunting_theta[,4]), 
            mean(size_OUM_hunting_theta[,5]), 
            mean(size_OUM_hunting_theta[,6])), 
    theta0=1.0)
}

#turn list into dataframe
size_OU_sim.data.df <- rbindlist(size_OU_sim.data)

#calculate mean by genus_spp
#size_OU_sim.data.df %>%
# group_by(Genus_species) %>%
#dplyr::summarise(mean = mean(X))

lnsize_OU_sim <- ddply(size_OU_sim.data.df, .(Genus_species), summarize,  size_OU_sim=mean(X))
rownames(lnsize_OU_sim) <- lnsize_OU_sim[,1]
lnsize_OU_sim <- data.frame(treedata(phy = tree_prune, data = lnsize_OU_sim, sort=TRUE)$data)

size_diet_sim <- data.frame(species_all, diet, as.numeric(as.character(lnsize_OU_sim$size_OU_sim)), size_se_all)
size_locomotion_sim <- data.frame(species_all, locomotion, as.numeric(as.character(lnsize_OU_sim$size_OU_sim)), size_se_all)
size_hunting_sim <- data.frame(species_all, hunting, as.numeric(as.character(lnsize_OU_sim$size_OU_sim)), size_se_all)

## BM ##
size_BM_500_sim <- list(length=length(simmap_locomotion_1000_sam500))
for(i in 1:length(simmap_locomotion_1000_sam500)){
  size_BM_500_sim[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], size_diet_sim, model="BM1", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(size_BM_500_sim, file="size_BM_500_sim.Rdata")

size_OU_500_sim <- list(length=length(simmap_locomotion_1000_sam500))
for(i in 1:length(simmap_locomotion_1000_sam500)){
  size_OU_500_sim[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], size_diet_sim, model="OU1", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(size_OU_500_sim, file="size_OU_500_sim.Rdata")

## OUM locomotion ##
size_OUM_locomotion_sim <- list(length=length(simmap_locomotion_1000_sam500))

for(i in 1:length(simmap_locomotion_1000_sam500)){
  size_OUM_locomotion_sim[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], size_locomotion_sim, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(size_OUM_locomotion_sim, file="size_OUM_locomotion_sim.Rdata")

## OUM diet ##
size_OUM_diet_sim <- list(length=length(simmap_diet_1000_sam500))

for(i in 1:length(simmap_diet_1000_sam500)){
  size_OUM_diet_sim[[i]] <- OUwie(simmap_diet_1000_sam500[[i]], size_diet_sim, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(size_OUM_diet_sim, file="size_OUM_diet_sim.Rdata")

## OUM hunting ##
size_OUM_hunting_sim <- list(length=length(simmap_hunting_1000_sam500))

for(i in 1:length(simmap_hunting_1000_sam500)){
  size_OUM_hunting_sim[[i]] <- OUwie(simmap_hunting_1000_sam500[[i]], size_hunting_sim, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(size_OUM_hunting_sim, file="size_OUM_hunting_sim.Rdata")


###### OUwie.sim for hbER ###### 
load("Rdata/OUwie_models/OUwie_500/hbER_OUM_hunting.Rdata")

#alpha
hbER_OU_alpha <- matrix(,length(simmap_hunting_1000_sam500),1,dimnames=list(c(1: length(simmap_hunting_1000_sam500)),c("hbER_OU_alpha")))
for(i in 1:length(simmap_hunting_1000_sam500)){
  hbER_OU_alpha[i,1] <- hbER_OUM_hunting[[i]]$solution[1,1]
}

#sigma
hbER_OU_sigma <- matrix(,length(simmap_hunting_1000_sam500),1,dimnames=list(c(1: length(simmap_hunting_1000_sam500)),c("hbER_OU_sigma")))
for(i in 1:length(simmap_hunting_1000_sam500)){
  hbER_OU_sigma[i,1] <- hbER_OUM_hunting[[i]]$solution[2,1]
}

hbER_OUM_hunting_theta <- matrix(,length(simmap_hunting_1000_sam500),6, dimnames=list(c(1: length(simmap_hunting_1000_sam500)), c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial")))
for(i in 1:length(simmap_hunting_1000_sam500)){
  hbER_OUM_hunting_theta[i,1] <- hbER_OUM_hunting[[i]]$theta[1,1]
  hbER_OUM_hunting_theta[i,2] <- hbER_OUM_hunting[[i]]$theta[2,1]
  hbER_OUM_hunting_theta[i,3] <- hbER_OUM_hunting[[i]]$theta[3,1]
  hbER_OUM_hunting_theta[i,4] <- hbER_OUM_hunting[[i]]$theta[4,1]
  hbER_OUM_hunting_theta[i,5] <- hbER_OUM_hunting[[i]]$theta[5,1]
  hbER_OUM_hunting_theta[i,6] <- hbER_OUM_hunting[[i]]$theta[6,1]
}

hbER_OU_sim.data <- list(length=length(simmap_hunting_1000_sam500))
for(i in 1:length(simmap_hunting_1000_sam500)){
  hbER_OU_sim.data[[i]]<-OUwie.sim(simmap_hunting_1000_sam500[[i]], simmap.tree=TRUE,scaleHeight=FALSE, alpha=c(
    mean(hbER_OU_alpha),
    mean(hbER_OU_alpha),
    mean(hbER_OU_alpha),
    mean(hbER_OU_alpha),
    mean(hbER_OU_alpha),
    mean(hbER_OU_alpha)),
    sigma.sq=c(mean(hbER_OU_sigma),
               mean(hbER_OU_sigma),
               mean(hbER_OU_sigma),
               mean(hbER_OU_sigma),
               mean(hbER_OU_sigma),
               mean(hbER_OU_sigma)),
    theta=c(mean(hbER_OUM_hunting_theta[,1]), 
            mean(hbER_OUM_hunting_theta[,2]), 
            mean(hbER_OUM_hunting_theta[,3]), 
            mean(hbER_OUM_hunting_theta[,4]), 
            mean(hbER_OUM_hunting_theta[,5]), 
            mean(hbER_OUM_hunting_theta[,6])), 
    theta0=1.0)
}

#turn list into dataframe
hbER_OU_sim.data.df <- rbindlist(hbER_OU_sim.data)

#calculate mean by genus_spp
#hbER_OU_sim.data.df %>%
# group_by(Genus_species) %>%
#dplyr::summarise(mean = mean(X))

lnhbER_OU_sim <- ddply(hbER_OU_sim.data.df, .(Genus_species), summarize,  hbER_OU_sim=mean(X))
rownames(lnhbER_OU_sim) <- lnhbER_OU_sim[,1]
lnhbER_OU_sim <- data.frame(treedata(phy = tree_prune, data = lnhbER_OU_sim, sort=TRUE)$data)

hbER_diet_sim <- data.frame(species_all, diet, as.numeric(as.character(lnhbER_OU_sim$hbER_OU_sim)), hbER_se_all)
hbER_locomotion_sim <- data.frame(species_all, locomotion, as.numeric(as.character(lnhbER_OU_sim$hbER_OU_sim)), hbER_se_all)
hbER_hunting_sim <- data.frame(species_all, hunting, as.numeric(as.character(lnhbER_OU_sim$hbER_OU_sim)), hbER_se_all)

## BM ##
hbER_BM_500_sim <- list(length=length(simmap_locomotion_1000_sam500))
for(i in 1:length(simmap_locomotion_1000_sam500)){
  hbER_BM_500_sim[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], hbER_diet_sim, model="BM1", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(hbER_BM_500_sim, file="hbER_BM_500_sim.Rdata")

hbER_OU_500_sim <- list(length=length(simmap_locomotion_1000_sam500))
for(i in 1:length(simmap_locomotion_1000_sam500)){
  hbER_OU_500_sim[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], hbER_diet_sim, model="OU1", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(hbER_OU_500_sim, file="hbER_OU_500_sim.Rdata")

## OUM locomotion ##
hbER_OUM_locomotion_sim <- list(length=length(simmap_locomotion_1000_sam500))

for(i in 1:length(simmap_locomotion_1000_sam500)){
  hbER_OUM_locomotion_sim[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], hbER_locomotion_sim, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(hbER_OUM_locomotion_sim, file="hbER_OUM_locomotion_sim.Rdata")

## OUM diet ##
hbER_OUM_diet_sim <- list(length=length(simmap_diet_1000_sam500))

for(i in 1:length(simmap_diet_1000_sam500)){
  hbER_OUM_diet_sim[[i]] <- OUwie(simmap_diet_1000_sam500[[i]], hbER_diet_sim, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(hbER_OUM_diet_sim, file="hbER_OUM_diet_sim.Rdata")

## OUM hunting ##
hbER_OUM_hunting_sim <- list(length=length(simmap_hunting_1000_sam500))

for(i in 1:length(simmap_hunting_1000_sam500)){
  hbER_OUM_hunting_sim[[i]] <- OUwie(simmap_hunting_1000_sam500[[i]], hbER_hunting_sim, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(hbER_OUM_hunting_sim, file="hbER_OUM_hunting_sim.Rdata")



###### OUwie.sim for headER ###### 
load("Rdata/OUwie_models/OUwie_500/headER_OUM_hunting.Rdata")

#alpha
headER_OU_alpha <- matrix(,length(simmap_hunting_1000_sam500),1,dimnames=list(c(1: length(simmap_hunting_1000_sam500)),c("headER_OU_alpha")))
for(i in 1:length(simmap_hunting_1000_sam500)){
  headER_OU_alpha[i,1] <- headER_OUM_hunting[[i]]$solution[1,1]
}

#sigma
headER_OU_sigma <- matrix(,length(simmap_hunting_1000_sam500),1,dimnames=list(c(1: length(simmap_hunting_1000_sam500)),c("headER_OU_sigma")))
for(i in 1:length(simmap_hunting_1000_sam500)){
  headER_OU_sigma[i,1] <- headER_OUM_hunting[[i]]$solution[2,1]
}

headER_OUM_hunting_theta <- matrix(,length(simmap_hunting_1000_sam500),6, dimnames=list(c(1: length(simmap_hunting_1000_sam500)), c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial")))
for(i in 1:length(simmap_hunting_1000_sam500)){
  headER_OUM_hunting_theta[i,1] <- headER_OUM_hunting[[i]]$theta[1,1]
  headER_OUM_hunting_theta[i,2] <- headER_OUM_hunting[[i]]$theta[2,1]
  headER_OUM_hunting_theta[i,3] <- headER_OUM_hunting[[i]]$theta[3,1]
  headER_OUM_hunting_theta[i,4] <- headER_OUM_hunting[[i]]$theta[4,1]
  headER_OUM_hunting_theta[i,5] <- headER_OUM_hunting[[i]]$theta[5,1]
  headER_OUM_hunting_theta[i,6] <- headER_OUM_hunting[[i]]$theta[6,1]
}

headER_OU_sim.data <- list(length=length(simmap_hunting_1000_sam500))
for(i in 1:length(simmap_hunting_1000_sam500)){
  headER_OU_sim.data[[i]]<-OUwie.sim(simmap_hunting_1000_sam500[[i]], simmap.tree=TRUE,scaleHeight=FALSE, alpha=c(
    mean(headER_OU_alpha),
    mean(headER_OU_alpha),
    mean(headER_OU_alpha),
    mean(headER_OU_alpha),
    mean(headER_OU_alpha),
    mean(headER_OU_alpha)),
    sigma.sq=c(mean(headER_OU_sigma),
               mean(headER_OU_sigma),
               mean(headER_OU_sigma),
               mean(headER_OU_sigma),
               mean(headER_OU_sigma),
               mean(headER_OU_sigma)),
    theta=c(mean(headER_OUM_hunting_theta[,1]), 
            mean(headER_OUM_hunting_theta[,2]), 
            mean(headER_OUM_hunting_theta[,3]), 
            mean(headER_OUM_hunting_theta[,4]), 
            mean(headER_OUM_hunting_theta[,5]), 
            mean(headER_OUM_hunting_theta[,6])), 
    theta0=1.0)
}

#turn list into dataframe
headER_OU_sim.data.df <- rbindlist(headER_OU_sim.data)

#calculate mean by genus_spp
#headER_OU_sim.data.df %>%
# group_by(Genus_species) %>%
#dplyr::summarise(mean = mean(X))

lnheadER_OU_sim <- ddply(headER_OU_sim.data.df, .(Genus_species), summarize,  headER_OU_sim=mean(X))
rownames(lnheadER_OU_sim) <- lnheadER_OU_sim[,1]
lnheadER_OU_sim <- data.frame(treedata(phy = tree_prune, data = lnheadER_OU_sim, sort=TRUE)$data)

headER_diet_sim <- data.frame(species_all, diet, as.numeric(as.character(lnheadER_OU_sim$headER_OU_sim)), headER_se_all)
headER_locomotion_sim <- data.frame(species_all, locomotion, as.numeric(as.character(lnheadER_OU_sim$headER_OU_sim)), headER_se_all)
headER_hunting_sim <- data.frame(species_all, hunting, as.numeric(as.character(lnheadER_OU_sim$headER_OU_sim)), headER_se_all)

## BM ##
headER_BM_500_sim <- list(length=length(simmap_locomotion_1000_sam500))
for(i in 1:length(simmap_locomotion_1000_sam500)){
  headER_BM_500_sim[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], headER_diet_sim, model="BM1", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(headER_BM_500_sim, file="headER_BM_500_sim.Rdata")

headER_OU_500_sim <- list(length=length(simmap_locomotion_1000_sam500))
for(i in 1:length(simmap_locomotion_1000_sam500)){
  headER_OU_500_sim[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], headER_diet_sim, model="OU1", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(headER_OU_500_sim, file="headER_OU_500_sim.Rdata")

## OUM locomotion ##
headER_OUM_locomotion_sim <- list(length=length(simmap_locomotion_1000_sam500))

for(i in 1:length(simmap_locomotion_1000_sam500)){
  headER_OUM_locomotion_sim[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], headER_locomotion_sim, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(headER_OUM_locomotion_sim, file="headER_OUM_locomotion_sim.Rdata")

## OUM diet ##
headER_OUM_diet_sim <- list(length=length(simmap_diet_1000_sam500))

for(i in 1:length(simmap_diet_1000_sam500)){
  headER_OUM_diet_sim[[i]] <- OUwie(simmap_diet_1000_sam500[[i]], headER_diet_sim, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(headER_OUM_diet_sim, file="headER_OUM_diet_sim.Rdata")

## OUM hunting ##
headER_OUM_hunting_sim <- list(length=length(simmap_hunting_1000_sam500))

for(i in 1:length(simmap_hunting_1000_sam500)){
  headER_OUM_hunting_sim[[i]] <- OUwie(simmap_hunting_1000_sam500[[i]], headER_hunting_sim, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(headER_OUM_hunting_sim, file="headER_OUM_hunting_sim.Rdata")


###### OUwie.sim for AEI_C ###### 
load("Rdata/OUwie_models/OUwie_500/AEI_C_OUM_hunting.Rdata")

#alpha
AEI_C_OU_alpha <- matrix(,length(simmap_hunting_1000_sam500),1,dimnames=list(c(1: length(simmap_hunting_1000_sam500)),c("AEI_C_OU_alpha")))
for(i in 1:length(simmap_hunting_1000_sam500)){
  AEI_C_OU_alpha[i,1] <- AEI_C_OUM_hunting[[i]]$solution[1,1]
}

#sigma
AEI_C_OU_sigma <- matrix(,length(simmap_hunting_1000_sam500),1,dimnames=list(c(1: length(simmap_hunting_1000_sam500)),c("AEI_C_OU_sigma")))
for(i in 1:length(simmap_hunting_1000_sam500)){
  AEI_C_OU_sigma[i,1] <- AEI_C_OUM_hunting[[i]]$solution[2,1]
}

AEI_C_OUM_hunting_theta <- matrix(,length(simmap_hunting_1000_sam500),6, dimnames=list(c(1: length(simmap_hunting_1000_sam500)), c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial")))
for(i in 1:length(simmap_hunting_1000_sam500)){
  AEI_C_OUM_hunting_theta[i,1] <- AEI_C_OUM_hunting[[i]]$theta[1,1]
  AEI_C_OUM_hunting_theta[i,2] <- AEI_C_OUM_hunting[[i]]$theta[2,1]
  AEI_C_OUM_hunting_theta[i,3] <- AEI_C_OUM_hunting[[i]]$theta[3,1]
  AEI_C_OUM_hunting_theta[i,4] <- AEI_C_OUM_hunting[[i]]$theta[4,1]
  AEI_C_OUM_hunting_theta[i,5] <- AEI_C_OUM_hunting[[i]]$theta[5,1]
  AEI_C_OUM_hunting_theta[i,6] <- AEI_C_OUM_hunting[[i]]$theta[6,1]
}

AEI_C_OU_sim.data <- list(length=length(simmap_hunting_1000_sam500))
for(i in 1:length(simmap_hunting_1000_sam500)){
  AEI_C_OU_sim.data[[i]]<-OUwie.sim(simmap_hunting_1000_sam500[[i]], simmap.tree=TRUE,scaleHeight=FALSE, alpha=c(
    mean(AEI_C_OU_alpha),
    mean(AEI_C_OU_alpha),
    mean(AEI_C_OU_alpha),
    mean(AEI_C_OU_alpha),
    mean(AEI_C_OU_alpha),
    mean(AEI_C_OU_alpha)),
    sigma.sq=c(mean(AEI_C_OU_sigma),
               mean(AEI_C_OU_sigma),
               mean(AEI_C_OU_sigma),
               mean(AEI_C_OU_sigma),
               mean(AEI_C_OU_sigma),
               mean(AEI_C_OU_sigma)),
    theta=c(mean(AEI_C_OUM_hunting_theta[,1]), 
            mean(AEI_C_OUM_hunting_theta[,2]), 
            mean(AEI_C_OUM_hunting_theta[,3]), 
            mean(AEI_C_OUM_hunting_theta[,4]), 
            mean(AEI_C_OUM_hunting_theta[,5]), 
            mean(AEI_C_OUM_hunting_theta[,6])), 
    theta0=1.0)
}

#turn list into dataframe
AEI_C_OU_sim.data.df <- rbindlist(AEI_C_OU_sim.data)

#calculate mean by genus_spp
#AEI_C_OU_sim.data.df %>%
# group_by(Genus_species) %>%
#dplyr::summarise(mean = mean(X))

lnAEI_C_OU_sim <- ddply(AEI_C_OU_sim.data.df, .(Genus_species), summarize,  AEI_C_OU_sim=mean(X))
rownames(lnAEI_C_OU_sim) <- lnAEI_C_OU_sim[,1]
lnAEI_C_OU_sim <- data.frame(treedata(phy = tree_prune, data = lnAEI_C_OU_sim, sort=TRUE)$data)

AEI_C_diet_sim <- data.frame(species_all, diet, as.numeric(as.character(lnAEI_C_OU_sim$AEI_C_OU_sim)), AEI_C_se_all)
AEI_C_locomotion_sim <- data.frame(species_all, locomotion, as.numeric(as.character(lnAEI_C_OU_sim$AEI_C_OU_sim)), AEI_C_se_all)
AEI_C_hunting_sim <- data.frame(species_all, hunting, as.numeric(as.character(lnAEI_C_OU_sim$AEI_C_OU_sim)), AEI_C_se_all)

## BM ##
AEI_C_BM_500_sim <- list(length=length(simmap_locomotion_1000_sam500))
for(i in 1:length(simmap_locomotion_1000_sam500)){
  AEI_C_BM_500_sim[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], AEI_C_diet_sim, model="BM1", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(AEI_C_BM_500_sim, file="AEI_C_BM_500_sim.Rdata")

AEI_C_OU_500_sim <- list(length=length(simmap_locomotion_1000_sam500))
for(i in 1:length(simmap_locomotion_1000_sam500)){
  AEI_C_OU_500_sim[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], AEI_C_diet_sim, model="OU1", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(AEI_C_OU_500_sim, file="AEI_C_OU_500_sim.Rdata")

## OUM locomotion ##
AEI_C_OUM_locomotion_sim <- list(length=length(simmap_locomotion_1000_sam500))

for(i in 1:length(simmap_locomotion_1000_sam500)){
  AEI_C_OUM_locomotion_sim[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], AEI_C_locomotion_sim, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(AEI_C_OUM_locomotion_sim, file="AEI_C_OUM_locomotion_sim.Rdata")

## OUM diet ##
AEI_C_OUM_diet_sim <- list(length=length(simmap_diet_1000_sam500))

for(i in 1:length(simmap_diet_1000_sam500)){
  AEI_C_OUM_diet_sim[[i]] <- OUwie(simmap_diet_1000_sam500[[i]], AEI_C_diet_sim, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(AEI_C_OUM_diet_sim, file="AEI_C_OUM_diet_sim.Rdata")

## OUM hunting ##
AEI_C_OUM_hunting_sim <- list(length=length(simmap_hunting_1000_sam500))

for(i in 1:length(simmap_hunting_1000_sam500)){
  AEI_C_OUM_hunting_sim[[i]] <- OUwie(simmap_hunting_1000_sam500[[i]], AEI_C_hunting_sim, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(AEI_C_OUM_hunting_sim, file="AEI_C_OUM_hunting_sim.Rdata")


###### OUwie.sim for AEI_L ###### 
load("Rdata/OUwie_models/OUwie_500/AEI_L_OUM_hunting.Rdata")

#alpha
AEI_L_OU_alpha <- matrix(,length(simmap_hunting_1000_sam500),1,dimnames=list(c(1: length(simmap_hunting_1000_sam500)),c("AEI_L_OU_alpha")))
for(i in 1:length(simmap_hunting_1000_sam500)){
  AEI_L_OU_alpha[i,1] <- AEI_L_OUM_hunting[[i]]$solution[1,1]
}

#sigma
AEI_L_OU_sigma <- matrix(,length(simmap_hunting_1000_sam500),1,dimnames=list(c(1: length(simmap_hunting_1000_sam500)),c("AEI_L_OU_sigma")))
for(i in 1:length(simmap_hunting_1000_sam500)){
  AEI_L_OU_sigma[i,1] <- AEI_L_OUM_hunting[[i]]$solution[2,1]
}

AEI_L_OUM_hunting_theta <- matrix(,length(simmap_hunting_1000_sam500),6, dimnames=list(c(1: length(simmap_hunting_1000_sam500)), c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial")))
for(i in 1:length(simmap_hunting_1000_sam500)){
  AEI_L_OUM_hunting_theta[i,1] <- AEI_L_OUM_hunting[[i]]$theta[1,1]
  AEI_L_OUM_hunting_theta[i,2] <- AEI_L_OUM_hunting[[i]]$theta[2,1]
  AEI_L_OUM_hunting_theta[i,3] <- AEI_L_OUM_hunting[[i]]$theta[3,1]
  AEI_L_OUM_hunting_theta[i,4] <- AEI_L_OUM_hunting[[i]]$theta[4,1]
  AEI_L_OUM_hunting_theta[i,5] <- AEI_L_OUM_hunting[[i]]$theta[5,1]
  AEI_L_OUM_hunting_theta[i,6] <- AEI_L_OUM_hunting[[i]]$theta[6,1]
}

AEI_L_OU_sim.data <- list(length=length(simmap_hunting_1000_sam500))
for(i in 1:length(simmap_hunting_1000_sam500)){
  AEI_L_OU_sim.data[[i]]<-OUwie.sim(simmap_hunting_1000_sam500[[i]], simmap.tree=TRUE,scaleHeight=FALSE, alpha=c(
    mean(AEI_L_OU_alpha),
    mean(AEI_L_OU_alpha),
    mean(AEI_L_OU_alpha),
    mean(AEI_L_OU_alpha),
    mean(AEI_L_OU_alpha),
    mean(AEI_L_OU_alpha)),
    sigma.sq=c(mean(AEI_L_OU_sigma),
               mean(AEI_L_OU_sigma),
               mean(AEI_L_OU_sigma),
               mean(AEI_L_OU_sigma),
               mean(AEI_L_OU_sigma),
               mean(AEI_L_OU_sigma)),
    theta=c(mean(AEI_L_OUM_hunting_theta[,1]), 
            mean(AEI_L_OUM_hunting_theta[,2]), 
            mean(AEI_L_OUM_hunting_theta[,3]), 
            mean(AEI_L_OUM_hunting_theta[,4]), 
            mean(AEI_L_OUM_hunting_theta[,5]), 
            mean(AEI_L_OUM_hunting_theta[,6])), 
    theta0=1.0)
}

#turn list into dataframe
AEI_L_OU_sim.data.df <- rbindlist(AEI_L_OU_sim.data)

#calculate mean by genus_spp
#AEI_L_OU_sim.data.df %>%
# group_by(Genus_species) %>%
#dplyr::summarise(mean = mean(X))

lnAEI_L_OU_sim <- ddply(AEI_L_OU_sim.data.df, .(Genus_species), summarize,  AEI_L_OU_sim=mean(X))
rownames(lnAEI_L_OU_sim) <- lnAEI_L_OU_sim[,1]
lnAEI_L_OU_sim <- data.frame(treedata(phy = tree_prune, data = lnAEI_L_OU_sim, sort=TRUE)$data)

AEI_L_diet_sim <- data.frame(species_all, diet, as.numeric(as.character(lnAEI_L_OU_sim$AEI_L_OU_sim)), AEI_L_se_all)
AEI_L_locomotion_sim <- data.frame(species_all, locomotion, as.numeric(as.character(lnAEI_L_OU_sim$AEI_L_OU_sim)), AEI_L_se_all)
AEI_L_hunting_sim <- data.frame(species_all, hunting, as.numeric(as.character(lnAEI_L_OU_sim$AEI_L_OU_sim)), AEI_L_se_all)

## BM ##
AEI_L_BM_500_sim <- list(length=length(simmap_locomotion_1000_sam500))
for(i in 1:length(simmap_locomotion_1000_sam500)){
  AEI_L_BM_500_sim[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], AEI_L_diet_sim, model="BM1", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(AEI_L_BM_500_sim, file="AEI_L_BM_500_sim.Rdata")

AEI_L_OU_500_sim <- list(length=length(simmap_locomotion_1000_sam500))
for(i in 1:length(simmap_locomotion_1000_sam500)){
  AEI_L_OU_500_sim[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], AEI_L_diet_sim, model="OU1", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(AEI_L_OU_500_sim, file="AEI_L_OU_500_sim.Rdata")

## OUM locomotion ##
AEI_L_OUM_locomotion_sim <- list(length=length(simmap_locomotion_1000_sam500))

for(i in 1:length(simmap_locomotion_1000_sam500)){
  AEI_L_OUM_locomotion_sim[[i]] <- OUwie(simmap_locomotion_1000_sam500[[i]], AEI_L_locomotion_sim, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(AEI_L_OUM_locomotion_sim, file="AEI_L_OUM_locomotion_sim.Rdata")

## OUM diet ##
AEI_L_OUM_diet_sim <- list(length=length(simmap_diet_1000_sam500))

for(i in 1:length(simmap_diet_1000_sam500)){
  AEI_L_OUM_diet_sim[[i]] <- OUwie(simmap_diet_1000_sam500[[i]], AEI_L_diet_sim, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(AEI_L_OUM_diet_sim, file="AEI_L_OUM_diet_sim.Rdata")

## OUM hunting ##
AEI_L_OUM_hunting_sim <- list(length=length(simmap_hunting_1000_sam500))

for(i in 1:length(simmap_hunting_1000_sam500)){
  AEI_L_OUM_hunting_sim[[i]] <- OUwie(simmap_hunting_1000_sam500[[i]], AEI_L_hunting_sim, model="OUM", simmap.tree=TRUE,root.station=TRUE, mserr = "known", diagn=T)
  print(i)
}
save(AEI_L_OUM_hunting_sim, file="AEI_L_OUM_hunting_sim.Rdata")


#### size AIC ####
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/size_BM_500_sim.Rdata")
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/size_OU_500_sim.Rdata")
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/size_OUM_diet_sim.Rdata")
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/size_OUM_locomotion_sim.Rdata")
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/size_OUM_hunting_sim.Rdata")


aic.table_size <- matrix(,length(simmap_diet_1000_sam500),5, dimnames=list(c(1: length(simmap_diet_1000_sam500)), c("BM1", "OU1", "OUM_locomotion_sim", "OUM_hunting_sim", "OUM_diet_sim" )))

for(i in 1:length(simmap_diet_1000_sam500)){
  aic.table_size[i,1] <- size_BM_500_sim[[i]]$AICc
  aic.table_size[i,2] <- size_OU_500_sim[[i]]$AICc
  aic.table_size[i,3] <- size_OUM_locomotion_sim[[i]]$AICc
  aic.table_size[i,4] <- size_OUM_hunting_sim[[i]]$AICc
  aic.table_size[i,5] <- size_OUM_diet_sim[[i]]$AICc
  
}
aic.table_size

aicw_table_size<- 
  aicw(c(
    mean(aic.table_size[,1]),
    mean(aic.table_size[,2]),
    mean(aic.table_size[,3]),
    mean(aic.table_size[,4]),
    mean(aic.table_size[,5])
  ))
rownames(aicw_table_size) <- c("BM1", "OU1", "OUM_locomotion_sim", "OUM_hunting_sim", "OUM_diet_sim" )
aicw_table_size
write.csv(aicw_table_size, file="aicw_table_size_sim.csv")


#### hbER AIC ####
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/hbER_BM_500_sim.Rdata")
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/hbER_OU_500_sim.Rdata")
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/hbER_OUM_diet_sim.Rdata")
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/hbER_OUM_locomotion_sim.Rdata")
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/hbER_OUM_hunting_sim.Rdata")


aic.table_hbER <- matrix(,length(simmap_diet_1000_sam500),5, dimnames=list(c(1: length(simmap_diet_1000_sam500)), c("BM1", "OU1", "OUM_locomotion_sim", "OUM_hunting_sim", "OUM_diet_sim" )))

for(i in 1:length(simmap_diet_1000_sam500)){
  aic.table_hbER[i,1] <- hbER_BM_100_sim[[i]]$AICc
  aic.table_hbER[i,2] <- hbER_OU_100_sim[[i]]$AICc
  aic.table_hbER[i,3] <- hbER_OUM_locomotion_sim[[i]]$AICc
  aic.table_hbER[i,4] <- hbER_OUM_hunting_sim[[i]]$AICc
  aic.table_hbER[i,5] <- hbER_OUM_diet_sim[[i]]$AICc
}
aic.table_hbER

aicw_table_hbER<- 
  aicw(c(
    mean(aic.table_hbER[,1]),
    mean(aic.table_hbER[,2]),
    mean(aic.table_hbER[,3]),
    mean(aic.table_hbER[,4]),
    mean(aic.table_hbER[,5])
  ))
rownames(aicw_table_hbER) <- c("BM1", "OU1", "OUM_locomotion_sim", "OUM_hunting_sim", "OUM_diet_sim" )
aicw_table_hbER
write.csv(aicw_table_hbER, file="aicw_table_hbER_sim.csv")


#### headER AIC ####
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/headER_BM_500_sim.Rdata")
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/headER_OU_500_sim.Rdata")
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/headER_OUM_diet_sim.Rdata")
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/headER_OUM_locomotion_sim.Rdata")
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/headER_OUM_hunting_sim.Rdata")


aic.table_headER <- matrix(,length(simmap_diet_1000_sam500),5, dimnames=list(c(1: length(simmap_diet_1000_sam500)), c("BM1", "OU1", "OUM_locomotion_sim", "OUM_hunting_sim", "OUM_diet_sim" )))

for(i in 1:length(simmap_diet_1000_sam500)){
  aic.table_headER[i,1] <- headER_BM_500_sim[[i]]$AICc
  aic.table_headER[i,2] <- headER_OU_500_sim[[i]]$AICc
  aic.table_headER[i,3] <- headER_OUM_locomotion_sim[[i]]$AICc
  aic.table_headER[i,4] <- headER_OUM_hunting_sim[[i]]$AICc
  aic.table_headER[i,5] <- headER_OUM_diet_sim[[i]]$AICc
  
}
aic.table_headER

aicw_table_headER<- 
  aicw(c(
    mean(aic.table_headER[,1]),
    mean(aic.table_headER[,2]),
    mean(aic.table_headER[,3]),
    mean(aic.table_headER[,4]),
    mean(aic.table_headER[,5])
  ))
rownames(aicw_table_headER) <- c("BM1", "OU1", "OUM_locomotion_sim", "OUM_hunting_sim", "OUM_diet_sim" )
aicw_table_headER
write.csv(aicw_table_headER, file="aicw_table_headER.csv")


#### AEI_C AIC ####
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/AEI_C_BM_500_sim.Rdata")
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/AEI_C_OU_500_sim.Rdata")
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/AEI_C_OUM_diet_sim.Rdata")
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/AEI_C_OUM_locomotion_sim.Rdata")
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/AEI_C_OUM_hunting_sim.Rdata")


aic.table_AEI_C <- matrix(,length(simmap_diet_1000_sam500),5, dimnames=list(c(1: length(simmap_diet_1000_sam500)), c("BM1", "OU1", "OUM_locomotion_sim", "OUM_hunting_sim", "OUM_diet_sim" )))

for(i in 1:length(simmap_diet_1000_sam500)){
  aic.table_AEI_C[i,1] <- AEI_C_BM_500_sim[[i]]$AICc
  aic.table_AEI_C[i,2] <- AEI_C_OU_500_sim[[i]]$AICc
  aic.table_AEI_C[i,3] <- AEI_C_OUM_locomotion_sim[[i]]$AICc
  aic.table_AEI_C[i,4] <- AEI_C_OUM_hunting_sim[[i]]$AICc
  aic.table_AEI_C[i,5] <- AEI_C_OUM_diet_sim[[i]]$AICc
  
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
rownames(aicw_table_AEI_C) <- c("BM1", "OU1", "OUM_locomotion_sim", "OUM_hunting_sim", "OUM_diet_sim" )
aicw_table_AEI_C
write.csv(aicw_table_AEI_C, file="aicw_table_AEI_C.csv")


#### AEI_T AIC ####
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/AEI_T_BM_100_sim.Rdata")
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/AEI_T_OU_100_sim.Rdata")
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/AEI_T_OUM_diet_sim.Rdata")
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/AEI_T_OUM_locomotion_sim.Rdata")
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/AEI_T_OUM_hunting_sim.Rdata")


aic.table_AEI_T <- matrix(,length(simmap_diet_1000_sam500),5, dimnames=list(c(1: length(simmap_diet_1000_sam500)), c("BM1", "OU1", "OUM_locomotion_sim", "OUM_hunting_sim", "OUM_diet_sim" )))

for(i in 1:length(simmap_diet_1000_sam500)){
  aic.table_AEI_T[i,1] <- AEI_T_BM_100_sim[[i]]$AICc
  aic.table_AEI_T[i,2] <- AEI_T_OU_100_sim[[i]]$AICc
  aic.table_AEI_T[i,3] <- AEI_T_OUM_locomotion_sim[[i]]$AICc
  aic.table_AEI_T[i,4] <- AEI_T_OUM_hunting_sim[[i]]$AICc
  aic.table_AEI_T[i,5] <- AEI_T_OUM_diet_sim[[i]]$AICc
  
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
rownames(aicw_table_AEI_T) <- c("BM1", "OU1", "OUM_locomotion_sim", "OUM_hunting_sim", "OUM_diet_sim" )
aicw_table_AEI_T
write.csv(aicw_table_AEI_T, file="aicw_table_AEI_T.csv")


#### AEI_L AIC ####
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/AEI_L_BM_500_sim.Rdata")
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/AEI_L_OU_500_sim.Rdata")
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/AEI_L_OUM_diet_sim.Rdata")
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/AEI_L_OUM_locomotion_sim.Rdata")
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/AEI_L_OUM_hunting_sim.Rdata")


aic.table_AEI_L <- matrix(,length(simmap_diet_1000_sam500),5, dimnames=list(c(1: length(simmap_diet_1000_sam500)), c("BM1", "OU1", "OUM_locomotion_sim", "OUM_hunting_sim", "OUM_diet_sim" )))

for(i in 1:length(simmap_diet_1000_sam500)){
  aic.table_AEI_L[i,1] <- AEI_L_BM_500_sim[[i]]$AICc
  aic.table_AEI_L[i,2] <- AEI_L_OU_500_sim[[i]]$AICc
  aic.table_AEI_L[i,3] <- AEI_L_OUM_locomotion_sim[[i]]$AICc
  aic.table_AEI_L[i,4] <- AEI_L_OUM_hunting_sim[[i]]$AICc
  aic.table_AEI_L[i,5] <- AEI_L_OUM_diet_sim[[i]]$AICc
  
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
rownames(aicw_table_AEI_L) <- c("BM1", "OU1", "OUM_locomotion_sim", "OUM_hunting_sim", "OUM_diet_sim" )
aicw_table_AEI_L
write.csv(aicw_table_AEI_L, file="aicw_table_AEI_L_sim.csv")


#### AEI_S AIC ####
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/AEI_S_BM_100_sim.Rdata")
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/AEI_S_OU_100_sim.Rdata")
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/AEI_S_OUM_diet_sim.Rdata")
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/AEI_S_OUM_locomotion_sim.Rdata")
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/AEI_S_OUM_hunting_sim.Rdata")


aic.table_AEI_S <- matrix(,length(simmap_diet_1000_sam500),5, dimnames=list(c(1: length(simmap_diet_1000_sam500)), c("BM1", "OU1", "OUM_locomotion_sim", "OUM_hunting_sim", "OUM_diet_sim" )))

for(i in 1:length(simmap_diet_1000_sam500)){
  aic.table_AEI_S[i,1] <- AEI_S_BM_100_sim[[i]]$AICc
  aic.table_AEI_S[i,2] <- AEI_S_OU_100_sim[[i]]$AICc
  aic.table_AEI_S[i,3] <- AEI_S_OUM_locomotion_sim[[i]]$AICc
  aic.table_AEI_S[i,4] <- AEI_S_OUM_hunting_sim[[i]]$AICc
  aic.table_AEI_S[i,5] <- AEI_S_OUM_diet_sim[[i]]$AICc
  
}
aic.table_AEI_S

aicw_table_AEI_S<- 
  aicw(c(
    mean(aic.table_AEI_S[,1]),
    mean(aic.table_AEI_S[,2]),
    mean(aic.table_AEI_S[,3]),
    mean(aic.table_AEI_S[,4]),
    mean(aic.table_AEI_S[,5])
  ))
rownames(aicw_table_AEI_S) <- c("BM1", "OU1", "OUM_locomotion_sim", "OUM_hunting_sim", "OUM_diet_sim" )
aicw_table_AEI_S
write.csv(aicw_table_AEI_S, file="aicw_table_AEI_S.csv")


#### rib_sc AIC ####
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/rib_sc_BM_100_sim.Rdata")
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/rib_sc_OU_100_sim.Rdata")
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/rib_sc_OUM_diet_sim.Rdata")
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/rib_sc_OUM_locomotion_sim.Rdata")
load("Rdata/OUwie_models/OUwie_500/OUwie_500_sim/rib_sc_OUM_hunting_sim.Rdata")


aic.table_rib_sc <- matrix(,length(simmap_diet_1000_sam500),5, dimnames=list(c(1: length(simmap_diet_1000_sam500)), c("BM1", "OU1", "OUM_locomotion_sim", "OUM_hunting_sim", "OUM_diet_sim" )))

for(i in 1:length(simmap_diet_1000_sam500)){
  aic.table_rib_sc[i,1] <- rib_sc_BM_100_sim[[i]]$AICc
  aic.table_rib_sc[i,2] <- rib_sc_OU_100_sim[[i]]$AICc
  aic.table_rib_sc[i,3] <- rib_sc_OUM_locomotion_sim[[i]]$AICc
  aic.table_rib_sc[i,4] <- rib_sc_OUM_hunting_sim[[i]]$AICc
  aic.table_rib_sc[i,5] <- rib_sc_OUM_diet_sim[[i]]$AICc
  
}
aic.table_rib_sc

aicw_table_rib_sc<- 
  aicw(c(
    mean(aic.table_rib_sc[,1]),
    mean(aic.table_rib_sc[,2]),
    mean(aic.table_rib_sc[,3]),
    mean(aic.table_rib_sc[,4]),
    mean(aic.table_rib_sc[,5])
  ))
rownames(aicw_table_rib_sc) <- c("BM1", "OU1", "OUM_locomotion_sim", "OUM_hunting_sim", "OUM_diet_sim" )
aicw_table_rib_sc
write.csv(aicw_table_rib_sc, file="aicw_table_rib_sc.csv")
