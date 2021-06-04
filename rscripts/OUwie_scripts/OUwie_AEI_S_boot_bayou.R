library(OUwie)
library(plotrix)

options(scipen = 999)

load("Rdata/ERecologydata_1225.RData")
load("Rdata/simmap/simmap_bayou_AEI_S_500.Rdata")

#### Best Model - OUM_bayou ####
load("Rdata/OUwie_models/OUwie_500/AEI_S_OUM_bayou.Rdata")


# alpha constant
AEI_S_OUM_bayou_alpha <- matrix(,length(simmap_bayou_AEI_S_500),1,dimnames=list(c(1: length(simmap_bayou_AEI_S_500)),c("AEI_S_OUM_bayou_alpha")))
for(i in 1:length(simmap_bayou_AEI_S_500)){
  AEI_S_OUM_bayou_alpha[i,1] <- AEI_S_OUM_bayou[[i]]$solution[1,1]
}
mean(AEI_S_OUM_bayou_alpha)
log(2)/mean(AEI_S_OUM_bayou_alpha)

# sigma constant
AEI_S_OUM_bayou_sigma <- matrix(,length(simmap_bayou_AEI_S_500),1,dimnames=list(c(1: length(simmap_bayou_AEI_S_500)),c("AEI_S_OUM_bayou_sigma")))
for(i in 1:length(simmap_bayou_AEI_S_500)){
  AEI_S_OUM_bayou_sigma[i,1] <- AEI_S_OUM_bayou[[i]]$solution[2,1]
}
mean(AEI_S_OUM_bayou_sigma)

# optima regimes 
AEI_S_OUM_bayou_optima <- matrix(,length(simmap_bayou_AEI_S_500),3, dimnames=list(c(1: length(simmap_bayou_AEI_S_500)), c("base", "mephtid", "ursidae")))
for(i in 1:length(simmap_bayou_AEI_S_500)){
  AEI_S_OUM_bayou_optima[i,1] <- AEI_S_OUM_bayou[[i]]$theta[1,1]
  AEI_S_OUM_bayou_optima[i,2] <- AEI_S_OUM_bayou[[i]]$theta[2,1]
  AEI_S_OUM_bayou_optima[i,3] <- AEI_S_OUM_bayou[[i]]$theta[3,1]
}
AEI_S_OUM_bayou_optima

mean_AEI_S_OUM_bayou_optima <- data.frame(rbind(
  mean(AEI_S_OUM_bayou_optima[,1]),
  mean(AEI_S_OUM_bayou_optima[,2]),
  mean(AEI_S_OUM_bayou_optima[,3])))


#### Bootstrapping for AEI_S_OUM_bayou model #####
simmap_bayou_AEI_S_500_10 <- sample(simmap_bayou_AEI_S_500[1:100],10)
AEI_S_OUM_bayou_boot <- list(length=length(simmap_bayou_AEI_S_500_10))
for(i in 1:length(simmap_bayou_AEI_S_500_10)){
  AEI_S_OUM_bayou_boot[[i]] <- OUwie.boot(simmap_bayou_AEI_S_500_10[[i]], AEI_S_bayou_reg, model="OUM",simmap.tree=TRUE, root.station=TRUE, nboot=100, mserr="none",
                                          alpha=c(mean(AEI_S_OUM_bayou_alpha),
                                                  mean(AEI_S_OUM_bayou_alpha),
                                                  mean(AEI_S_OUM_bayou_alpha)),
                                          sigma.sq=c(mean(AEI_S_OUM_bayou_sigma),
                                                     mean(AEI_S_OUM_bayou_sigma),
                                                     mean(AEI_S_OUM_bayou_sigma)),
                                          theta=c(mean(AEI_S_OUM_bayou_optima[,1]), 
                                                  mean(AEI_S_OUM_bayou_optima[,2]),
                                                  mean(AEI_S_OUM_bayou_optima[,3])), 
                                          theta0=mean(AEI_S_OUM_bayou_optima[,1]))
  print(i)
}
save(AEI_S_OUM_bayou_boot, file="AEI_S_OUM_bayou_boot.Rdata")


### AEI_S_OUM_bayou optima ###
AEI_S_OUM_bayou_boot_opt_base <- list()
for(i in 1:length(AEI_S_OUM_bayou_boot)){
  AEI_S_OUM_bayou_boot_opt_base[[i]] <- c(AEI_S_OUM_bayou_boot[[i]][,7])}

AEI_S_OUM_bayou_boot_opt_mephitid <- list()
for(i in 1:length(AEI_S_OUM_bayou_boot)){
  AEI_S_OUM_bayou_boot_opt_mephitid[[i]] <- c(AEI_S_OUM_bayou_boot[[i]][,8])}

AEI_S_OUM_bayou_boot_opt_ursidae <- list()
for(i in 1:length(AEI_S_OUM_bayou_boot)){
  AEI_S_OUM_bayou_boot_opt_ursidae[[i]] <- c(AEI_S_OUM_bayou_boot[[i]][,9])}

mean_AEI_S_OUM_bayou_boot_opt <- rbind(
  mean(unlist(AEI_S_OUM_bayou_boot_opt_base)),
  mean(unlist(AEI_S_OUM_bayou_boot_opt_mephitid)),
  mean(unlist(AEI_S_OUM_bayou_boot_opt_ursidae)))

CI_AEI_S_OUM_bayou_boot_opt <- rbind(
  quantile(unlist(AEI_S_OUM_bayou_boot_opt_base), probs=c(0.025, 0.975)),
  quantile(unlist(AEI_S_OUM_bayou_boot_opt_mephitid), probs=c(0.025, 0.975)),
  quantile(unlist(AEI_S_OUM_bayou_boot_opt_ursidae), probs=c(0.025, 0.975)))

AEI_S_OUM_bayou_boot_opt <- data.frame(mean_AEI_S_OUM_bayou_optima, mean_AEI_S_OUM_bayou_boot_opt,CI_AEI_S_OUM_bayou_boot_opt)
colnames(AEI_S_OUM_bayou_boot_opt) <- c("OUM_optima_mean", "OUM_optima_boot", "L95_boot", "U95_boot")
rownames(AEI_S_OUM_bayou_boot_opt) <- c("base", "mephitid", "ursidae")
exp(AEI_S_OUM_bayou_boot_opt)
write.csv(AEI_S_OUM_bayou_boot_opt, file="AEI_S_OUM_bayou_boot_opt.csv")


