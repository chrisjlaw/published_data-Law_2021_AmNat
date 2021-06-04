library(OUwie)
library(plotrix)

options(scipen = 999)

load("Rdata/ERecologydata_0914.RData")
load("Rdata/simmap/simmap_bayou_hbER_500.Rdata")

#### Best Model - OUM_bayou ####
load("Rdata/OUwie_models/OUwie_500/hbER_OUM_bayou.Rdata")

# alpha constant
hbER_OUM_bayou_alpha <- hbER_OUM_bayou$solution[1,1]
log(2)/mean(hbER_OUM_bayou_alpha)

# sigma constant
hbER_OUM_bayou_sigma <- hbER_OUM_bayou$solution[2,1]

# optima regimes 
hbER_OUM_bayou_optima <- data.frame(hbER_OUM_bayou$theta[,1])

rownames(hbER_OUM_bayou_optima) <- c("base", "bassaricyon", "bassariscus","enhydra", "feliformia", "genetta", "gulo", "hyaendiae", "lutrinae", "monachinae", "mustela", "mustelid", "poecilogale", "ursidae")
colnames(hbER_OUM_bayou_optima) <- "OUM_optima"


#### Bootstrapping for hbER_OUM_bayou model #####
simmap_bayou_hbER_500_10 <- sample(simmap_bayou_hbER_500[1:100],10)
hbER_OUM_bayou_boot <- list(length=length(simmap_bayou_hbER_500_10))
for(i in 1:length(simmap_bayou_hbER_500_10)){
  hbER_OUM_bayou_boot[[i]] <- OUwie.boot(simmap_bayou_hbER_500_10[[i]], hbER_bayou_reg, model="OUM",simmap.tree=TRUE, root.station=TRUE, nboot=100, mserr="none",
                                         alpha=c(hbER_OUM_bayou_alpha,
                                                 hbER_OUM_bayou_alpha,
                                                 hbER_OUM_bayou_alpha,
                                                 hbER_OUM_bayou_alpha,
                                                 hbER_OUM_bayou_alpha,
                                                 hbER_OUM_bayou_alpha,
                                                 hbER_OUM_bayou_alpha,
                                                 hbER_OUM_bayou_alpha,
                                                 hbER_OUM_bayou_alpha,
                                                 hbER_OUM_bayou_alpha,
                                                 hbER_OUM_bayou_alpha,
                                                 hbER_OUM_bayou_alpha,
                                                 hbER_OUM_bayou_alpha,
                                                 hbER_OUM_bayou_alpha),
                                         sigma.sq = c(hbER_OUM_bayou_sigma,
                                                    hbER_OUM_bayou_sigma,
                                                    hbER_OUM_bayou_sigma,
                                                    hbER_OUM_bayou_sigma,
                                                    hbER_OUM_bayou_sigma,
                                                    hbER_OUM_bayou_sigma,
                                                    hbER_OUM_bayou_sigma,
                                                    hbER_OUM_bayou_sigma,
                                                    hbER_OUM_bayou_sigma,
                                                    hbER_OUM_bayou_sigma,
                                                    hbER_OUM_bayou_sigma,
                                                    hbER_OUM_bayou_sigma,
                                                    hbER_OUM_bayou_sigma,
                                                    hbER_OUM_bayou_sigma),
                                         theta=c(hbER_OUM_bayou_optima[1,1], 
                                                 hbER_OUM_bayou_optima[2,1], 
                                                 hbER_OUM_bayou_optima[3,1], 
                                                 hbER_OUM_bayou_optima[4,1], 
                                                 hbER_OUM_bayou_optima[5,1], 
                                                 hbER_OUM_bayou_optima[6,1], 
                                                 hbER_OUM_bayou_optima[7,1], 
                                                 hbER_OUM_bayou_optima[8,1], 
                                                 hbER_OUM_bayou_optima[9,1], 
                                                 hbER_OUM_bayou_optima[10,1], 
                                                 hbER_OUM_bayou_optima[11,1], 
                                                 hbER_OUM_bayou_optima[12,1], 
                                                 hbER_OUM_bayou_optima[13,1], 
                                                 hbER_OUM_bayou_optima[14,1]), 
                                         theta0=mean(hbER_OUM_bayou_optima[1,1]))
  print(i)
}
save(hbER_OUM_bayou_boot, file="hbER_OUM_bayou_boot.Rdata")


### hbER_OUM_bayou optima ###
hbER_OUM_bayou_boot_opt_base <- list()
for(i in 1:length(hbER_OUM_bayou_boot)){
  hbER_OUM_bayou_boot_opt_base[[i]] <- c(hbER_OUM_bayou_boot[[i]][,29])}

hbER_OUM_bayou_boot_opt_bassaricyon <- list()
for(i in 1:length(hbER_OUM_bayou_boot)){
  hbER_OUM_bayou_boot_opt_bassaricyon[[i]] <- c(hbER_OUM_bayou_boot[[i]][,30])}

hbER_OUM_bayou_boot_opt_bassariscus <- list()
for(i in 1:length(hbER_OUM_bayou_boot)){
  hbER_OUM_bayou_boot_opt_bassariscus[[i]] <- c(hbER_OUM_bayou_boot[[i]][,31])}

hbER_OUM_bayou_boot_opt_enhydra <- list()
for(i in 1:length(hbER_OUM_bayou_boot)){
  hbER_OUM_bayou_boot_opt_enhydra[[i]] <- c(hbER_OUM_bayou_boot[[i]][,32])}

hbER_OUM_bayou_boot_opt_feliformia <- list()
for(i in 1:length(hbER_OUM_bayou_boot)){
  hbER_OUM_bayou_boot_opt_feliformia[[i]] <- c(hbER_OUM_bayou_boot[[i]][,33])}

hbER_OUM_bayou_boot_opt_genetta <- list()
for(i in 1:length(hbER_OUM_bayou_boot)){
  hbER_OUM_bayou_boot_opt_genetta[[i]] <- c(hbER_OUM_bayou_boot[[i]][,34])}

hbER_OUM_bayou_boot_opt_gulo <- list()
for(i in 1:length(hbER_OUM_bayou_boot)){
  hbER_OUM_bayou_boot_opt_gulo[[i]] <- c(hbER_OUM_bayou_boot[[i]][,35])}

hbER_OUM_bayou_boot_opt_hyaenidae <- list()
for(i in 1:length(hbER_OUM_bayou_boot)){
  hbER_OUM_bayou_boot_opt_hyaenidae[[i]] <- c(hbER_OUM_bayou_boot[[i]][36])}

hbER_OUM_bayou_boot_opt_lutrinae <- list()
for(i in 1:length(hbER_OUM_bayou_boot)){
  hbER_OUM_bayou_boot_opt_lutrinae[[i]] <- c(hbER_OUM_bayou_boot[[i]][,37])}

hbER_OUM_bayou_boot_opt_monachinae <- list()
for(i in 1:length(hbER_OUM_bayou_boot)){
  hbER_OUM_bayou_boot_opt_monachinae[[i]] <- c(hbER_OUM_bayou_boot[[i]][,38])}

hbER_OUM_bayou_boot_opt_mustela <- list()
for(i in 1:length(hbER_OUM_bayou_boot)){
  hbER_OUM_bayou_boot_opt_mustela[[i]] <- c(hbER_OUM_bayou_boot[[i]][,39])}

hbER_OUM_bayou_boot_opt_mustelid <- list()
for(i in 1:length(hbER_OUM_bayou_boot)){
  hbER_OUM_bayou_boot_opt_mustelid[[i]] <- c(hbER_OUM_bayou_boot[[i]][,40])}

hbER_OUM_bayou_boot_opt_poecilogale <- list()
for(i in 1:length(hbER_OUM_bayou_boot)){
  hbER_OUM_bayou_boot_opt_poecilogale[[i]] <- c(hbER_OUM_bayou_boot[[i]][,41])}

hbER_OUM_bayou_boot_opt_ursidae <- list()
for(i in 1:length(hbER_OUM_bayou_boot)){
  hbER_OUM_bayou_boot_opt_ursidae[[i]] <- c(hbER_OUM_bayou_boot[[i]][,42])}

mean_hbER_OUM_bayou_boot_opt <- rbind(
  mean(unlist(hbER_OUM_bayou_boot_opt_base)),
  mean(unlist(hbER_OUM_bayou_boot_opt_bassaricyon)),
  mean(unlist(hbER_OUM_bayou_boot_opt_bassariscus)),
  mean(unlist(hbER_OUM_bayou_boot_opt_enhydra)),
  mean(unlist(hbER_OUM_bayou_boot_opt_feliformia)),
  mean(unlist(hbER_OUM_bayou_boot_opt_genetta)),
  mean(unlist(hbER_OUM_bayou_boot_opt_gulo)),
  mean(unlist(hbER_OUM_bayou_boot_opt_hyaenidae)),
  mean(unlist(hbER_OUM_bayou_boot_opt_lutrinae)),
  mean(unlist(hbER_OUM_bayou_boot_opt_monachinae)),
  mean(unlist(hbER_OUM_bayou_boot_opt_mustela)),
  mean(unlist(hbER_OUM_bayou_boot_opt_mustelid)),
  mean(unlist(hbER_OUM_bayou_boot_opt_poecilogale)),
  mean(unlist(hbER_OUM_bayou_boot_opt_ursidae)))

CI_hbER_OUM_bayou_boot_opt <- rbind(
  quantile(unlist(hbER_OUM_bayou_boot_opt_base), probs=c(0.025, 0.975)),
  quantile(unlist(hbER_OUM_bayou_boot_opt_bassaricyon), probs=c(0.025, 0.975)),
  quantile(unlist(hbER_OUM_bayou_boot_opt_bassariscus), probs=c(0.025, 0.975)),
  quantile(unlist(hbER_OUM_bayou_boot_opt_enhydra), probs=c(0.025, 0.975)),
  quantile(unlist(hbER_OUM_bayou_boot_opt_feliformia), probs=c(0.025, 0.975)),
  quantile(unlist(hbER_OUM_bayou_boot_opt_genetta), probs=c(0.025, 0.975)),
  quantile(unlist(hbER_OUM_bayou_boot_opt_gulo), probs=c(0.025, 0.975)),
  quantile(unlist(hbER_OUM_bayou_boot_opt_hyaenidae), probs=c(0.025, 0.975)),
  quantile(unlist(hbER_OUM_bayou_boot_opt_lutrinae), probs=c(0.025, 0.975)),
  quantile(unlist(hbER_OUM_bayou_boot_opt_monachinae), probs=c(0.025, 0.975)),
  quantile(unlist(hbER_OUM_bayou_boot_opt_mustela), probs=c(0.025, 0.975)),
  quantile(unlist(hbER_OUM_bayou_boot_opt_mustelid), probs=c(0.025, 0.975)),
  quantile(unlist(hbER_OUM_bayou_boot_opt_poecilogale), probs=c(0.025, 0.975)),
  quantile(unlist(hbER_OUM_bayou_boot_opt_ursidae), probs=c(0.025, 0.975)))


hbER_OUM_bayou_boot_opt <- data.frame(mean_hbER_OUM_bayou_optima, mean_hbER_OUM_bayou_boot_opt,CI_hbER_OUM_bayou_boot_opt)
colnames(hbER_OUM_bayou_boot_opt) <- c("OUM_optima_mean", "OUM_optima_boot", "L95_boot", "U95_boot")
rownames(hbER_OUM_bayou_boot_opt) <- c("base", "enhydra", "feliformia", "hyaendiae", "monachinae", "musteloid", "pteronura")
exp(hbER_OUM_bayou_boot_opt)
write.csv(exp(hbER_OUM_bayou_boot_opt), file="hbER_OUM_bayou_boot_opt.csv")


