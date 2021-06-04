library(OUwie)
library(plotrix)

options(scipen = 999)

load("Rdata/ERecologydata_1225.RData")
load("Rdata/simmap/simmap_bayou_size_500.Rdata")

#### Best Model - OUM_bayou ####
load("Rdata/OUwie_models/OUwie_500/size_OUM_bayou.Rdata")


# alpha constant
size_OUM_bayou_alpha <- matrix(,length(simmap_bayou_size_500),1,dimnames=list(c(1: length(simmap_bayou_size_500)),c("size_OUM_bayou_alpha")))
for(i in 1:length(simmap_bayou_size_500)){
  size_OUM_bayou_alpha[i,1] <- size_OUM_bayou[[i]]$solution[1,1]
}
mean(size_OUM_bayou_alpha)
log(2)/mean(size_OUM_bayou_alpha)

# sigma constant
size_OUM_bayou_sigma <- matrix(,length(simmap_bayou_size_500),1,dimnames=list(c(1: length(simmap_bayou_size_500)),c("size_OUM_bayou_sigma")))
for(i in 1:length(simmap_bayou_size_500)){
  size_OUM_bayou_sigma[i,1] <- size_OUM_bayou[[i]]$solution[2,1]
}
mean(size_OUM_bayou_sigma)

# optima regimes 
size_OUM_bayou_optima <- matrix(,length(simmap_bayou_size_500),3, dimnames=list(c(1: length(simmap_bayou_size_500)), c("base", "lutrinae", "musteloid")))
for(i in 1:length(simmap_bayou_size_500)){
  size_OUM_bayou_optima[i,1] <- size_OUM_bayou[[i]]$theta[1,1]
  size_OUM_bayou_optima[i,2] <- size_OUM_bayou[[i]]$theta[2,1]
  size_OUM_bayou_optima[i,3] <- size_OUM_bayou[[i]]$theta[3,1]
}
size_OUM_bayou_optima

mean_size_OUM_bayou_optima <- rbind(
  mean(size_OUM_bayou_optima[,1]),
  mean(size_OUM_bayou_optima[,2]),
  mean(size_OUM_bayou_optima[,3]))


#### Bootstrapping for size_OUM_bayou model #####
simmap_bayou_size_500_10 <- sample(simmap_bayou_size_500[1:100],10)
size_OUM_bayou_boot <- list(length=length(simmap_bayou_size_500_10))
for(i in 1:length(simmap_bayou_size_500_10)){
  size_OUM_bayou_boot[[i]] <- OUwie.boot(simmap_bayou_size_500_10[[i]], size_bayou_reg, model="OUM",simmap.tree=TRUE, root.station=TRUE, nboot=100, mserr="none",
                                         alpha=c(mean(size_OUM_bayou_alpha),
                                                 mean(size_OUM_bayou_alpha),
                                                 mean(size_OUM_bayou_alpha)),
                                         sigma.sq=c(mean(size_OUM_bayou_sigma),
                                                    mean(size_OUM_bayou_sigma),
                                                    mean(size_OUM_bayou_sigma)),
                                         theta=c(mean(size_OUM_bayou_optima[,1]), 
                                                 mean(size_OUM_bayou_optima[,2]),
                                                 mean(size_OUM_bayou_optima[,3])), 
                                         theta0=mean(size_OUM_bayou_optima[,1]))
  print(i)
}
save(size_OUM_bayou_boot, file="size_OUM_bayou_boot.Rdata")


### size_OUM_bayou optima ###
size_OUM_bayou_boot_opt_base <- list()
for(i in 1:length(size_OUM_bayou_boot)){
  size_OUM_bayou_boot_opt_base[[i]] <- c(size_OUM_bayou_boot[[i]][,7])}

size_OUM_bayou_boot_opt_lutrinae <- list()
for(i in 1:length(size_OUM_bayou_boot)){
  size_OUM_bayou_boot_opt_lutrinae[[i]] <- c(size_OUM_bayou_boot[[i]][,8])}

size_OUM_bayou_boot_opt_musteloid <- list()
for(i in 1:length(size_OUM_bayou_boot)){
  size_OUM_bayou_boot_opt_musteloid[[i]] <- c(size_OUM_bayou_boot[[i]][,9])}

mean_size_OUM_bayou_boot_opt <- rbind(
  mean(unlist(size_OUM_bayou_boot_opt_base)),
  mean(unlist(size_OUM_bayou_boot_opt_lutrinae)),
  mean(unlist(size_OUM_bayou_boot_opt_musteloid)))

CI_size_OUM_bayou_boot_opt <- rbind(
  quantile(unlist(size_OUM_bayou_boot_opt_base), probs=c(0.025, 0.975)),
  quantile(unlist(size_OUM_bayou_boot_opt_lutrinae), probs=c(0.025, 0.975)),
  quantile(unlist(size_OUM_bayou_boot_opt_musteloid), probs=c(0.025, 0.975)))

size_OUM_bayou_boot_opt <- data.frame(mean_size_OUM_bayou_optima, mean_size_OUM_bayou_boot_opt,CI_size_OUM_bayou_boot_opt)
colnames(size_OUM_bayou_boot_opt) <- c("OUM_optima_mean", "OUM_optima_boot", "L95_boot", "U95_boot")
rownames(size_OUM_bayou_boot_opt) <- c("base", "lutrinae", "musteloid")
exp(size_OUM_bayou_boot_opt)
write.csv(exp(size_OUM_bayou_boot_opt), file="size_OUM_bayou_boot_opt.csv")



