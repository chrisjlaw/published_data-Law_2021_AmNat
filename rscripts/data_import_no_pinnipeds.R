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


multitree <- read.nexus("data/UphamTrees.nex")
multitree <- lapply(multitree, force.ultrametric)
multitree <- lapply(multitree, ladderize)

tree <- read.nexus("data/UphamMCCtree.tre")
tree <- read.tree("data/carnivore_all.tre")
tree <- ladderize(tree)
tree <- force.ultrametric(tree)
tree <- multi2di(tree)
data <- read.csv("data/data_mean_se_reduced_no_pinnipeds.csv")
rownames(data) <- data[,1]

data_mean_pr <- data.frame(treedata(phy = tree, data = data, sort=TRUE)$data)
tree_prune <- treedata(phy = tree, data = data_mean_pr)$phy
multitree_prune <- list(length=length(multitree))
for(i in 1:length(multitree)){
  multitree_prune[[i]] <- treedata(phy = multitree[[i]], data = data, warnings=TRUE)$phy
}
class(multitree_prune) <- "multiPhylo"

family_all <- data_mean_pr$Family
species_all <- data_mean_pr$Species

locomotion <- data_mean_pr$locomotion
names(locomotion) <- rownames(data_mean_pr)

diet <- data_mean_pr$diet
names(diet) <- rownames(data_mean_pr)

hunting <- data_mean_pr$hunting
names(hunting) <- rownames(data_mean_pr)

bayou <- data_mean_pr$bayou_reg_hbER
names(bayou) <- rownames(data_mean_pr)

data_mean_pr %>% group_by(diet) %>% tally()
data_mean_pr %>% group_by(locomotion) %>% tally()
data_mean_pr %>% group_by(hunting) %>% tally()


#### size data ####
size_all <- (as.numeric(as.character(data_mean_pr$geomean)))
names(size_all) <- rownames(data_mean_pr)
lnsize_all <- log(size_all)
size_se_all <- (as.numeric(as.character(data_mean_pr$ln_se_geomean)))
names(size_se_all) <- rownames(data_mean_pr)
size_se_all[is.na(size_se_all)] <- mean(size_se_all, na.rm=TRUE)

size_diet <- data.frame(species_all, diet, lnsize_all, size_se_all)
size_locomotion <- data.frame(species_all, locomotion, lnsize_all, size_se_all)
size_hunting <- data.frame(species_all, hunting, lnsize_all, size_se_all)

#### hbER data ####
hbER_all <- (as.numeric(as.character(data_mean_pr$hbER)))
names(hbER_all) <- rownames(data_mean_pr)
lnhbER_all <- log(hbER_all)
hbER_se_all <- (as.numeric(as.character(data_mean_pr$ln_se_hbER)))
names(hbER_se_all) <- rownames(data_mean_pr)
hbER_se_all[is.na(hbER_se_all)] <- mean(hbER_se_all, na.rm=TRUE)

hbER_diet <- data.frame(species_all, diet, lnhbER_all, hbER_se_all)
hbER_locomotion <- data.frame(species_all, locomotion, lnhbER_all, hbER_se_all)
hbER_hunting <- data.frame(species_all, hunting, lnhbER_all, hbER_se_all)
hbER_bayou <- data.frame(species_all, bayou, lnhbER_all, hbER_se_all)

#### headER data ####
headER_all <- (as.numeric(as.character(data_mean_pr$headER)))
names(headER_all) <- rownames(data_mean_pr)
lnheadER_all <- log(headER_all)
headER_se_all <- (as.numeric(as.character(data_mean_pr$ln_se_headER)))
names(headER_se_all) <- rownames(data_mean_pr)
headER_se_all[is.na(headER_se_all)] <- mean(headER_se_all, na.rm=TRUE)

headER_diet <- data.frame(species_all, diet, lnheadER_all, headER_se_all)
headER_locomotion <- data.frame(species_all, locomotion, lnheadER_all, headER_se_all)
headER_hunting <- data.frame(species_all, hunting, lnheadER_all, headER_se_all)

#### AEI_C data ####
AEI_C_all <- (as.numeric(as.character(data_mean_pr$AEI_C)))
names(AEI_C_all) <- rownames(data_mean_pr)
lnAEI_C_all <- log(AEI_C_all)
AEI_C_se_all <- (as.numeric(as.character(data_mean_pr$ln_se_AEI_C)))
names(AEI_C_se_all) <- rownames(data_mean_pr)
AEI_C_se_all[is.na(AEI_C_se_all)] <- mean(AEI_C_se_all, na.rm=TRUE)

AEI_C_diet <- data.frame(species_all, diet, lnAEI_C_all, AEI_C_se_all)
AEI_C_locomotion <- data.frame(species_all, locomotion, lnAEI_C_all, AEI_C_se_all)
AEI_C_hunting <- data.frame(species_all, hunting, lnAEI_C_all, AEI_C_se_all)

#### AEI_T data ####
AEI_T_all <- (as.numeric(as.character(data_mean_pr$AEI_T)))
names(AEI_T_all) <- rownames(data_mean_pr)
lnAEI_T_all <- log(AEI_T_all)
AEI_T_se_all <- (as.numeric(as.character(data_mean_pr$ln_se_AEI_T)))
names(AEI_T_se_all) <- rownames(data_mean_pr)
AEI_T_se_all[is.na(AEI_T_se_all)] <- mean(AEI_T_se_all, na.rm=TRUE)

AEI_T_diet <- data.frame(species_all, diet, lnAEI_T_all, AEI_T_se_all)
AEI_T_locomotion <- data.frame(species_all, locomotion, lnAEI_T_all, AEI_T_se_all)
AEI_T_hunting <- data.frame(species_all, hunting, lnAEI_T_all, AEI_T_se_all)

#### AEI_L data ####
AEI_L_all <- (as.numeric(as.character(data_mean_pr$AEI_L)))
names(AEI_L_all) <- rownames(data_mean_pr)
lnAEI_L_all <- log(AEI_L_all)
AEI_L_se_all <- (as.numeric(as.character(data_mean_pr$ln_se_AEI_L)))
names(AEI_L_se_all) <- rownames(data_mean_pr)
AEI_L_se_all[is.na(AEI_L_se_all)] <- mean(AEI_L_se_all, na.rm=TRUE)

AEI_L_diet <- data.frame(species_all, diet, lnAEI_L_all, AEI_L_se_all)
AEI_L_locomotion <- data.frame(species_all, locomotion, lnAEI_L_all, AEI_L_se_all)
AEI_L_hunting <- data.frame(species_all, hunting, lnAEI_L_all, AEI_L_se_all)

#### AEI_S data ####
AEI_S_all <- (as.numeric(as.character(data_mean_pr$AEI_S)))
names(AEI_S_all) <- rownames(data_mean_pr)
lnAEI_S_all <- log(AEI_S_all)
AEI_S_se_all <- (as.numeric(as.character(data_mean_pr$ln_se_AEI_S)))
names(AEI_S_se_all) <- rownames(data_mean_pr)
AEI_S_se_all[is.na(AEI_S_se_all)] <- mean(AEI_S_se_all, na.rm=TRUE)

AEI_S_diet <- data.frame(species_all, diet, lnAEI_S_all, AEI_S_se_all)
AEI_S_locomotion <- data.frame(species_all, locomotion, lnAEI_S_all, AEI_S_se_all)
AEI_S_hunting <- data.frame(species_all, hunting, lnAEI_S_all, AEI_S_se_all)

#### rib_sc data ####
rib_all <- (as.numeric(as.character(data_mean_pr$avgRibLength)))
names(rib_all) <- rownames(data_mean_pr)
lnrib_all <- log(rib_all)

lnrib_sc_all <- phyl.resid(tree_prune, lnsize_all, lnrib_all, method = "lambda")$resid
colnames(lnrib_sc_all) <- "lnrib_sc"
as.numeric(as.character(lnrib_sc_all))
names(lnrib_sc_all) <- rownames(data_mean_pr)

rib_sc_diet <- data.frame(species_all, diet, lnrib_sc_all)
rib_sc_locomotion <- data.frame(species_all, locomotion, lnrib_sc_all)
rib_sc_hunting <- data.frame(species_all, hunting, lnrib_sc_all)


### Simmap ###
simmap_diet_1000 <- make.simmap(multitree_prune,diet, nsim=10)
save(simmap_diet_1000, file="simmap_diet_1000no_pinnipeds.Rdata")
simmap_diet_1000_sam500 <- sample(simmap_diet_1000[1:10000],500)
save(simmap_diet_1000_sam500, file="simmap_diet_1000_sam500no_pinnipeds.Rdata")
simmap_diet_1000_con <- describe.simmap(simmap_diet_1000)
col_diet <- setNames(c("blue", "red", "green","black","orange"), 
                     c("aquatic", "carnivore","herbivore", "insectivore", "omnivore"))

simmap_locomotion_1000 <- make.simmap(multitree_prune,locomotion, nsim=10)
save(simmap_locomotion_1000, file="simmap_locomotion_1000no_pinnipeds.Rdata")
simmap_locomotion_1000_sam500 <- sample(simmap_locomotion_1000[1:10000],500)
save(simmap_locomotion_1000_sam500, file="simmap_locomotion_1000_sam500no_pinnipeds.Rdata")
simmap_locomotion_1000_con <- describe.simmap(simmap_locomotion_1000)
col_locomotion <- setNames(c("blue", "green", "lightblue", "yellow","orange","black"), 
                           c("aquatic", "arboreal", "semi_aquatic","semi_arboreal", "semi_fossorial", "terrestrial"))

simmap_hunting_1000 <- make.simmap(multitree_prune,hunting, nsim=10)
save(simmap_hunting_1000, file="simmap_hunting_1000no_pinnipeds.Rdata")
simmap_hunting_1000_sam500 <- sample(simmap_hunting_1000[1:10000],500)
save(simmap_hunting_1000_sam500, file="simmap_hunting_1000_sam500no_pinnipeds.Rdata")
simmap_hunting_1000_con <- describe.simmap(simmap_hunting_1000)
col_hunting <- setNames(c("red", "blue", "black", "yellow","green","orange"), 
                        c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial"))


### simmap of mcc tree ###
simmap_locomotion_100_mcc_ER <- make.simmap(tree_prune,locomotion, nsim=100, model = "ER")
simmap_locomotion_100_mcc_SYM <- make.simmap(tree_prune,locomotion, nsim=100, model = "SYM")
simmap_locomotion_100_mcc_ARD <- make.simmap(tree_prune,locomotion, nsim=100, model = "ARD")

simmap_locomotion_100_mcc_aic <- matrix(,length(tree_simmap_4cat_500_ER),3,dimnames=list(c(1: length(tree_simmap_4cat_500_ER)),c("ER", "SYM", "ARD")))
for(i in 1:length(tree_simmap_4cat_500_ER)){
  simmap_locomotion_100_mcc_aic[i,1] <- AIC(simmap_locomotion_100_mcc_ER[[i]])
  simmap_locomotion_100_mcc_aic[i,2] <- AIC(simmap_locomotion_100_mcc_SYM[[i]])
  simmap_locomotion_100_mcc_aic[i,3] <- AIC(simmap_locomotion_100_mcc_ARD[[i]])}

simmap_locomotion_100_mcc_aicw <- aicw(c(
  mean(simmap_locomotion_100_mcc_aic[,1]),
  mean(simmap_locomotion_100_mcc_aic[,2]),
  mean(simmap_locomotion_100_mcc_aic[,3])))
rownames(simmap_locomotion_100_mcc_aicw) <- c("ER", "SYM", "ARD")


simmap_hunting_100_mcc_ER <- make.simmap(tree_prune,hunting, nsim=100, model = "ER")
simmap_hunting_100_mcc_SYM <- make.simmap(tree_prune,hunting, nsim=100, model = "SYM")
simmap_hunting_100_mcc_ARD <- make.simmap(tree_prune,hunting, nsim=100, model = "ARD")

simmap_hunting_100_mcc_aic <- matrix(,length(tree_simmap_4cat_500_ER),3,dimnames=list(c(1: length(tree_simmap_4cat_500_ER)),c("ER", "SYM", "ARD")))
for(i in 1:length(tree_simmap_4cat_500_ER)){
  simmap_hunting_100_mcc_aic[i,1] <- AIC(simmap_hunting_100_mcc_ER[[i]])
  simmap_hunting_100_mcc_aic[i,2] <- AIC(simmap_hunting_100_mcc_SYM[[i]])
  simmap_hunting_100_mcc_aic[i,3] <- AIC(simmap_hunting_100_mcc_ARD[[i]])}

simmap_hunting_100_mcc_aicw <- aicw(c(
  mean(simmap_hunting_100_mcc_aic[,1]),
  mean(simmap_hunting_100_mcc_aic[,2]),
  mean(simmap_hunting_100_mcc_aic[,3])))
rownames(simmap_hunting_100_mcc_aicw) <- c("ER", "SYM", "ARD")

simmap_diet_100_mcc_ER <- make.simmap(tree_prune,diet, nsim=100)
simmap_diet_100_mcc_ER <- make.simmap(tree_prune,diet, nsim=100, model = "ER")
simmap_diet_100_mcc_SYM <- make.simmap(tree_prune,diet, nsim=100, model = "SYM")
simmap_diet_100_mcc_ARD <- make.simmap(tree_prune,diet, nsim=100, model = "ARD")

simmap_diet_100_mcc_aic <- matrix(,length(simmap_diet_100_mcc_ER),3,dimnames=list(c(1: length(simmap_diet_100_mcc_ER)),c("ER", "SYM", "ARD")))
for(i in 1:length(simmap_diet_100_mcc_ER)){
  simmap_diet_100_mcc_aic[i,1] <- AIC(simmap_diet_100_mcc_ER[[i]])
  simmap_diet_100_mcc_aic[i,2] <- AIC(simmap_diet_100_mcc_SYM[[i]])
  simmap_diet_100_mcc_aic[i,3] <- AIC(simmap_diet_100_mcc_ARD[[i]])}

simmap_diet_100_mcc_aicw <- aicw(c(
  mean(simmap_diet_100_mcc_aic[,1]),
  mean(simmap_diet_100_mcc_aic[,2]),
  mean(simmap_diet_100_mcc_aic[,3])))
rownames(simmap_diet_100_mcc_aicw) <- c("ER", "SYM", "ARD")


simmap_bayou_hbER_mcc <- make.simmap(tree_prune,bayou, nsim=1)

