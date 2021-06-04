library(OUwie)
library(plotrix)

options(scipen = 999)

load("Rdata/ERecologydata_1225.RData")
load("Rdata/simmap/simmap_bayou_rib_sc_500.Rdata")

#### Best Model - OUM_bayou ####
load("Rdata/OUwie_models/OUwie_500/rib_sc_OUM_bayou.Rdata")


# alpha constant
rib_sc_OUM_bayou_alpha <- matrix(,length(simmap_bayou_rib_sc_500),1,dimnames=list(c(1: length(simmap_bayou_rib_sc_500)),c("rib_sc_OUM_bayou_alpha")))
for(i in 1:length(simmap_bayou_rib_sc_500)){
  rib_sc_OUM_bayou_alpha[i,1] <- rib_sc_OUM_bayou[[i]]$solution[1,1]
}
mean(rib_sc_OUM_bayou_alpha)
log(2)/mean(rib_sc_OUM_bayou_alpha)

# sigma constant
rib_sc_OUM_bayou_sigma <- matrix(,length(simmap_bayou_rib_sc_500),1,dimnames=list(c(1: length(simmap_bayou_rib_sc_500)),c("rib_sc_OUM_bayou_sigma")))
for(i in 1:length(simmap_bayou_rib_sc_500)){
  rib_sc_OUM_bayou_sigma[i,1] <- rib_sc_OUM_bayou[[i]]$solution[2,1]
}
mean(rib_sc_OUM_bayou_sigma)

# optima regimes 
rib_sc_OUM_bayou_optima <- matrix(,length(simmap_bayou_rib_sc_500),7, dimnames=list(c(1: length(simmap_bayou_rib_sc_500)), c("base", "enhydra", "felimformia", "hyaendiae", "monachinae", "musteloid", "pteronura")))
for(i in 1:length(simmap_bayou_rib_sc_500)){
  rib_sc_OUM_bayou_optima[i,1] <- rib_sc_OUM_bayou[[i]]$theta[1,1]
  rib_sc_OUM_bayou_optima[i,2] <- rib_sc_OUM_bayou[[i]]$theta[2,1]
  rib_sc_OUM_bayou_optima[i,3] <- rib_sc_OUM_bayou[[i]]$theta[3,1]
  rib_sc_OUM_bayou_optima[i,4] <- rib_sc_OUM_bayou[[i]]$theta[4,1]
  rib_sc_OUM_bayou_optima[i,5] <- rib_sc_OUM_bayou[[i]]$theta[5,1]
  rib_sc_OUM_bayou_optima[i,6] <- rib_sc_OUM_bayou[[i]]$theta[6,1]
  rib_sc_OUM_bayou_optima[i,7] <- rib_sc_OUM_bayou[[i]]$theta[7,1]
}
rib_sc_OUM_bayou_optima

mean_rib_sc_OUM_bayou_optima <- rbind(
  mean(rib_sc_OUM_bayou_optima[,1]),
  mean(rib_sc_OUM_bayou_optima[,2]),
  mean(rib_sc_OUM_bayou_optima[,3]),
  mean(rib_sc_OUM_bayou_optima[,4]),
  mean(rib_sc_OUM_bayou_optima[,5]),
  mean(rib_sc_OUM_bayou_optima[,6]),
  mean(rib_sc_OUM_bayou_optima[,7]))


#### Bootstrapping for rib_sc_OUM_bayou model #####
simmap_bayou_rib_sc_500_10 <- sample(simmap_bayou_rib_sc_500[1:100],10)
rib_sc_OUM_bayou_boot <- list(length=length(simmap_bayou_rib_sc_500_10))
for(i in 1:length(simmap_bayou_rib_sc_500_10)){
  rib_sc_OUM_bayou_boot[[i]] <- OUwie.boot(simmap_bayou_rib_sc_500_10[[i]], rib_sc_bayou_reg, model="OUM",simmap.tree=TRUE, root.station=TRUE, nboot=100, mserr="none",
                                           alpha=c(mean(rib_sc_OUM_bayou_alpha),
                                                   mean(rib_sc_OUM_bayou_alpha),
                                                   mean(rib_sc_OUM_bayou_alpha),
                                                   mean(rib_sc_OUM_bayou_alpha),
                                                   mean(rib_sc_OUM_bayou_alpha),
                                                   mean(rib_sc_OUM_bayou_alpha),
                                                   mean(rib_sc_OUM_bayou_alpha)),
                                           sigma.sq=c(mean(rib_sc_OUM_bayou_sigma),
                                                      mean(rib_sc_OUM_bayou_sigma),
                                                      mean(rib_sc_OUM_bayou_sigma),
                                                      mean(rib_sc_OUM_bayou_sigma),
                                                      mean(rib_sc_OUM_bayou_sigma),
                                                      mean(rib_sc_OUM_bayou_sigma),
                                                      mean(rib_sc_OUM_bayou_sigma)),
                                           theta=c(mean(rib_sc_OUM_bayou_optima[,1]), 
                                                   mean(rib_sc_OUM_bayou_optima[,2]),
                                                   mean(rib_sc_OUM_bayou_optima[,3]),
                                                   mean(rib_sc_OUM_bayou_optima[,4]),
                                                   mean(rib_sc_OUM_bayou_optima[,5]),
                                                   mean(rib_sc_OUM_bayou_optima[,6]),
                                                   mean(rib_sc_OUM_bayou_optima[,7])), 
                                           theta0=mean(rib_sc_OUM_bayou_optima[,1]))
  print(i)
}
save(rib_sc_OUM_bayou_boot, file="rib_sc_OUM_bayou_boot.Rdata")


### rib_sc_OUM_bayou optima ###
rib_sc_OUM_bayou_boot_opt_base <- list()
for(i in 1:length(rib_sc_OUM_bayou_boot)){
  rib_sc_OUM_bayou_boot_opt_base[[i]] <- c(rib_sc_OUM_bayou_boot[[i]][,15])}

rib_sc_OUM_bayou_boot_opt_enhydra <- list()
for(i in 1:length(rib_sc_OUM_bayou_boot)){
  rib_sc_OUM_bayou_boot_opt_enhydra[[i]] <- c(rib_sc_OUM_bayou_boot[[i]][,16])}

rib_sc_OUM_bayou_boot_opt_feliformia <- list()
for(i in 1:length(rib_sc_OUM_bayou_boot)){
  rib_sc_OUM_bayou_boot_opt_feliformia[[i]] <- c(rib_sc_OUM_bayou_boot[[i]][,17])}

rib_sc_OUM_bayou_boot_opt_hyaendiae <- list()
for(i in 1:length(rib_sc_OUM_bayou_boot)){
  rib_sc_OUM_bayou_boot_opt_hyaendiae[[i]] <- c(rib_sc_OUM_bayou_boot[[i]][,18])}

rib_sc_OUM_bayou_boot_opt_monachinae <- list()
for(i in 1:length(rib_sc_OUM_bayou_boot)){
  rib_sc_OUM_bayou_boot_opt_monachinae[[i]] <- c(rib_sc_OUM_bayou_boot[[i]][,19])}

rib_sc_OUM_bayou_boot_opt_musteloid <- list()
for(i in 1:length(rib_sc_OUM_bayou_boot)){
  rib_sc_OUM_bayou_boot_opt_musteloid[[i]] <- c(rib_sc_OUM_bayou_boot[[i]][,20])}

rib_sc_OUM_bayou_boot_opt_pteronura <- list()
for(i in 1:length(rib_sc_OUM_bayou_boot)){
  rib_sc_OUM_bayou_boot_opt_pteronura[[i]] <- c(rib_sc_OUM_bayou_boot[[i]][,21])}

mean_rib_sc_OUM_bayou_boot_opt <- rbind(
  mean(unlist(rib_sc_OUM_bayou_boot_opt_base)),
  mean(unlist(rib_sc_OUM_bayou_boot_opt_enhydra)),
  mean(unlist(rib_sc_OUM_bayou_boot_opt_feliformia)),
  mean(unlist(rib_sc_OUM_bayou_boot_opt_hyaendiae)),
  mean(unlist(rib_sc_OUM_bayou_boot_opt_monachinae)),
  mean(unlist(rib_sc_OUM_bayou_boot_opt_musteloid)),
  mean(unlist(rib_sc_OUM_bayou_boot_opt_pteronura)))

CI_rib_sc_OUM_bayou_boot_opt <- rbind(
  quantile(unlist(rib_sc_OUM_bayou_boot_opt_base), probs=c(0.025, 0.975)),
  quantile(unlist(rib_sc_OUM_bayou_boot_opt_enhydra), probs=c(0.025, 0.975)),
  quantile(unlist(rib_sc_OUM_bayou_boot_opt_feliformia), probs=c(0.025, 0.975)),
  quantile(unlist(rib_sc_OUM_bayou_boot_opt_hyaendiae), probs=c(0.025, 0.975)),
  quantile(unlist(rib_sc_OUM_bayou_boot_opt_monachinae), probs=c(0.025, 0.975)),
  quantile(unlist(rib_sc_OUM_bayou_boot_opt_musteloid), probs=c(0.025, 0.975)),
  quantile(unlist(rib_sc_OUM_bayou_boot_opt_pteronura), probs=c(0.025, 0.975)))

rib_sc_OUM_bayou_boot_opt <- data.frame(mean_rib_sc_OUM_bayou_optima, mean_rib_sc_OUM_bayou_boot_opt,CI_rib_sc_OUM_bayou_boot_opt)
colnames(rib_sc_OUM_bayou_boot_opt) <- c("OUM_optima_mean", "OUM_optima_boot", "L95_boot", "U95_boot")
rownames(rib_sc_OUM_bayou_boot_opt) <- c("base", "enhydra", "feliformia", "hyaendiae", "monachinae", "musteloid", "pteronura")
exp(rib_sc_OUM_bayou_boot_opt)
write.csv(exp(rib_sc_OUM_bayou_boot_opt), file="rib_sc_OUM_bayou_boot_opt.csv")



