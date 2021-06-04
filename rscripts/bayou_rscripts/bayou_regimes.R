
data <- read.csv("data/data_mean_se_reduced.csv")
rownames(data) <- data[,1]

data_mean_pr <- data.frame(treedata(phy = tree, data = data, sort=TRUE)$data)

bayou_reg_size_all <- data_mean_pr$bayou_reg_size
names(bayou_reg_size_all) <- rownames(data_mean_pr)

bayou_reg_hbER_all <- data_mean_pr$bayou_reg_hbER
names(bayou_reg_hbER_all) <- rownames(data_mean_pr)

bayou_reg_headER_all <- data_mean_pr$bayou_reg_headER
names(bayou_reg_headER_all) <- rownames(data_mean_pr)

bayou_reg_AEI_S_all <- data_mean_pr$bayou_reg_AEI_S
names(bayou_reg_AEI_S_all) <- rownames(data_mean_pr)

bayou_reg_rib_sc_all <- data_mean_pr$bayou_reg_rib_sc
names(bayou_reg_rib_sc_all) <- rownames(data_mean_pr)

size_bayou_reg <- data.frame(species_all, bayou_reg_size_all, lnsize_all, size_se_all)
hbER_bayou_reg <- data.frame(species_all, bayou_reg_hbER_all, lnhbER_all, hbER_se_all)
headER_bayou_reg <- data.frame(species_all, bayou_reg_headER_all, lnheadER_all, headER_se_all)
AEI_S_bayou_reg <- data.frame(species_all, bayou_reg_AEI_S_all, lnAEI_S_all, AEI_S_se_all)
rib_sc_bayou_reg <- data.frame(species_all, bayou_reg_rib_sc_all, lnrib_sc_all)


hbER_sc_bayou_reg <- data.frame(species_all, bayou_reg_hbER_all, lnhbER_sc_all)


### simmap of bayou regimes ###
multitree_prune_500 <- sample(multitree_prune[1:1000],500)

simmap_bayou_size_500 <- make.simmap(multitree_prune_500,bayou_reg_size_all, nsim=1)
save(simmap_bayou_size_500, file="simmap_bayou_size_500.Rdata")

simmap_bayou_hbER_500 <- make.simmap(multitree_prune_500,bayou_reg_hbER_all, nsim=1)
save(simmap_bayou_hbER_500, file="simmap_bayou_hbER_500_no.Rdata")

simmap_bayou_headER_500 <- make.simmap(multitree_prune_500,bayou_reg_headER_all, nsim=1)
save(simmap_bayou_headER_500, file="simmap_bayou_headER_500.Rdata")

simmap_bayou_AEI_S_500 <- make.simmap(multitree_prune_500,bayou_reg_AEI_S_all, nsim=1)
save(simmap_bayou_AEI_S_500, file="simmap_bayou_AEI_S_500.Rdata")

simmap_bayou_rib_sc_500 <- make.simmap(multitree_prune_500,bayou_reg_rib_sc_all, nsim=1)
save(simmap_bayou_rib_sc_500, file="simmap_bayou_rib_sc_500.Rdata")

### mcc
simmap_bayou_hbER_mcc <- make.simmap(tree_prune,bayou_reg_hbER_all, nsim=1)
save(simmap_bayou_hbER_mcc, file="simmap_bayou_hbER_mcc.Rdata")


simmap_bayou_hbER_500 <- make.simmap(multitree_prune_500,bayou_reg_hbER_all, nsim=1)
save(simmap_bayou_hbER_500, file="simmap_bayou_hbER_500_no_pinnipeds.Rdata")




