library(OUwie)
library(plotrix)

options(scipen = 999)

load("Rdata/ERecologydata_1225.RData")
load("Rdata/simmap/simmap_bayou_headER_500.Rdata")

#### Best Model - OUM_bayou ####
load("Rdata/OUwie_models/OUwie_500/headER_OUM_bayou.Rdata")


# alpha constant
headER_OUM_bayou_alpha <- matrix(,length(simmap_bayou_headER_500),1,dimnames=list(c(1: length(simmap_bayou_headER_500)),c("headER_OUM_bayou_alpha")))
for(i in 1:length(simmap_bayou_headER_500)){
  headER_OUM_bayou_alpha[i,1] <- headER_OUM_bayou[[i]]$solution[1,1]
}
mean(headER_OUM_bayou_alpha)
log(2)/mean(headER_OUM_bayou_alpha)

# sigma constant
headER_OUM_bayou_sigma <- matrix(,length(simmap_bayou_headER_500),1,dimnames=list(c(1: length(simmap_bayou_headER_500)),c("headER_OUM_bayou_sigma")))
for(i in 1:length(simmap_bayou_headER_500)){
  headER_OUM_bayou_sigma[i,1] <- headER_OUM_bayou[[i]]$solution[2,1]
}
mean(headER_OUM_bayou_sigma)

# optima regimes 
headER_OUM_bayou_optima <- matrix(,length(simmap_bayou_headER_500),2, dimnames=list(c(1: length(simmap_bayou_headER_500)), c("base", "mustela")))
for(i in 1:length(simmap_bayou_headER_500)){
  headER_OUM_bayou_optima[i,1] <- headER_OUM_bayou[[i]]$theta[1,1]
  headER_OUM_bayou_optima[i,2] <- headER_OUM_bayou[[i]]$theta[2,1]
}
headER_OUM_bayou_optima

mean_headER_OUM_bayou_optima <- data.frame(rbind(
  mean(headER_OUM_bayou_optima[,1]),
  mean(headER_OUM_bayou_optima[,2])))
colnames(mean_headER_OUM_bayou_optima) <- c("OUM_optima")
exp(mean_headER_OUM_bayou_optima)


#### Bootstrapping for headER_OUM_bayou model #####
simmap_bayou_headER_500_10 <- sample(simmap_bayou_headER_500[1:100],10)
headER_OUM_bayou_boot <- list(length=length(simmap_bayou_headER_500_10))
for(i in 1:length(simmap_bayou_headER_500_10)){
  headER_OUM_bayou_boot[[i]] <- OUwie.boot(simmap_bayou_headER_500_10[[i]], headER_bayou_reg, model="OUM",simmap.tree=TRUE, root.station=TRUE, nboot=100, mserr="none",
                                           alpha=c(mean(headER_OUM_bayou_alpha),
                                                   mean(headER_OUM_bayou_alpha)),
                                           sigma.sq=c(mean(headER_OUM_bayou_sigma),
                                                      mean(headER_OUM_bayou_sigma)),
                                           theta=c(mean(headER_OUM_bayou_optima[,1]), 
                                                   mean(headER_OUM_bayou_optima[,2])), 
                                           theta0=mean(headER_OUM_bayou_optima[,1]))
  print(i)
}
save(headER_OUM_bayou_boot, file="headER_OUM_bayou_boot.Rdata")


### headER_OUM_bayou optima ###
headER_OUM_bayou_boot_opt_base <- list()
for(i in 1:length(headER_OUM_bayou_boot)){
  headER_OUM_bayou_boot_opt_base[[i]] <- c(headER_OUM_bayou_boot[[i]][,5])}

headER_OUM_bayou_boot_opt_mustela <- list()
for(i in 1:length(headER_OUM_bayou_boot)){
  headER_OUM_bayou_boot_opt_mustela[[i]] <- c(headER_OUM_bayou_boot[[i]][,6])}

mean_headER_OUM_bayou_boot_opt <- rbind(
  mean(unlist(headER_OUM_bayou_boot_opt_base)),
  mean(unlist(headER_OUM_bayou_boot_opt_mustela)))

CI_headER_OUM_bayou_boot_opt <- rbind(
  quantile(unlist(headER_OUM_bayou_boot_opt_base), probs=c(0.025, 0.975)),
  quantile(unlist(headER_OUM_bayou_boot_opt_mustela), probs=c(0.025, 0.975)))

headER_OUM_bayou_boot_opt <- data.frame(mean_headER_OUM_bayou_optima, mean_headER_OUM_bayou_boot_opt,CI_headER_OUM_bayou_boot_opt)
colnames(headER_OUM_bayou_boot_opt) <- c("OUM_optima_mean", "OUM_optima_boot", "L95_boot", "U95_boot")
rownames(headER_OUM_bayou_boot_opt) <- c("base", "mustela")
exp(headER_OUM_bayou_boot_opt)
write.csv(headER_OUM_bayou_boot_opt, file="headER_OUM_bayou_boot_opt.csv")


