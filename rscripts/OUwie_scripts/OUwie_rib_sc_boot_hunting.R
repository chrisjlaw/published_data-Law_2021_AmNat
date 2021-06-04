library(OUwie)
library(plotrix)

options(scipen = 999)

load("Rdata/ERecologydata_0504.RData")
load("Rdata/simmap/simmap_hunting_100_sam.Rdata")

#### Best Model - OUM_hunting ####
load("Rdata/OUwie_models/rib_sc_OUM_hunting.Rdata")

# alpha constant
rib_sc_OUM_hunting_alpha <- matrix(,length(simmap_hunting_100_sam),1,dimnames=list(c(1: length(simmap_hunting_100_sam)),c("rib_sc_OUM_hunting_alpha")))
for(i in 1:length(simmap_hunting_100_sam)){
  rib_sc_OUM_hunting_alpha[i,1] <- rib_sc_OUM_hunting[[i]]$solution[1,1]
}
mean(rib_sc_OUM_hunting_alpha)
log(2)/mean(rib_sc_OUM_hunting_alpha)

# sigma constant
rib_sc_OUM_hunting_sigma <- matrix(,length(simmap_hunting_100_sam),1,dimnames=list(c(1: length(simmap_hunting_100_sam)),c("rib_sc_OUM_hunting_sigma")))
for(i in 1:length(simmap_hunting_100_sam)){
  rib_sc_OUM_hunting_sigma[i,1] <- rib_sc_OUM_hunting[[i]]$solution[2,1]
}
mean(rib_sc_OUM_hunting_sigma)

# optima regimes 
rib_sc_OUM_hunting_optima <- matrix(,length(simmap_hunting_100_sam),6, dimnames=list(c(1: length(simmap_hunting_100_sam)), c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial")))
for(i in 1:length(simmap_hunting_100_sam)){
  rib_sc_OUM_hunting_optima[i,1] <- rib_sc_OUM_hunting[[i]]$theta[1,1]
  rib_sc_OUM_hunting_optima[i,2] <- rib_sc_OUM_hunting[[i]]$theta[2,1]
  rib_sc_OUM_hunting_optima[i,3] <- rib_sc_OUM_hunting[[i]]$theta[3,1]
  rib_sc_OUM_hunting_optima[i,4] <- rib_sc_OUM_hunting[[i]]$theta[4,1]
  rib_sc_OUM_hunting_optima[i,5] <- rib_sc_OUM_hunting[[i]]$theta[5,1]
  rib_sc_OUM_hunting_optima[i,6] <- rib_sc_OUM_hunting[[i]]$theta[6,1]
}
rib_sc_OUM_hunting_optima

mean_rib_sc_OUM_hunting_optima <- rbind(
  mean(rib_sc_OUM_hunting_optima[,1]),
  mean(rib_sc_OUM_hunting_optima[,2]),
  mean(rib_sc_OUM_hunting_optima[,3]),
  mean(rib_sc_OUM_hunting_optima[,4]),
  mean(rib_sc_OUM_hunting_optima[,5]),
  mean(rib_sc_OUM_hunting_optima[,6]))

CI_rib_sc_OUM_hunting_optima <- rbind(
  quantile(rib_sc_OUM_hunting_optima[,1], probs=c(0.025, 0.975)),
  quantile(rib_sc_OUM_hunting_optima[,2], probs=c(0.025, 0.975)),
  quantile(rib_sc_OUM_hunting_optima[,3], probs=c(0.025, 0.975)),
  quantile(rib_sc_OUM_hunting_optima[,4], probs=c(0.025, 0.975)),
  quantile(rib_sc_OUM_hunting_optima[,5], probs=c(0.025, 0.975)),
  quantile(rib_sc_OUM_hunting_optima[,6], probs=c(0.025, 0.975)))

rownames(mean_rib_sc_OUM_hunting_optima) <- c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial")

mean_rib_sc_OUM_hunting_optima <- data.frame(mean_rib_sc_OUM_hunting_optima,CI_rib_sc_OUM_hunting_optima)
colnames(mean_rib_sc_OUM_hunting_optima) <- c("OUM_optima", "L95", "U95")
mean_rib_sc_OUM_hunting_optima
write.csv(mean_rib_sc_OUM_hunting_optima, file="mean_rib_sc_OUM_hunting_optima.csv")

col_hunting <- c("red", "blue", "black", "yellow","green","orange")

pdf("rib_sc_OUM_hunting_opt_line.pdf",width=5,height=7) 
plot(mean_rib_sc_OUM_hunting_optima$OUM_optima, ylim = c(min(mean_rib_sc_OUM_hunting_optima), max(mean_rib_sc_OUM_hunting_optima)),  ylab = "rib_sc",  xaxt = "n", las=1, cex.axis=2)
axis(1, at=c(1:6), labels = c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial"), las=2, cex.axis=2)
plotCI(mean_rib_sc_OUM_hunting_optima$OUM_optima,y=NULL, uiw=mean_rib_sc_OUM_hunting_optima$U95-mean_rib_sc_OUM_hunting_optima$OUM_optima, liw=mean_rib_sc_OUM_hunting_optima$OUM_optima-mean_rib_sc_OUM_hunting_optima$L95, err="y", pch=20, slty=3, col=col_hunting, scol = col_hunting, add=TRUE, cex=3, lwd=3)
dev.off()

rib_sc_OUM_hunting_opt_am <- density(rib_sc_OUM_hunting_optima[,1])
rib_sc_OUM_hunting_opt_aq <- density(rib_sc_OUM_hunting_optima[,2])
rib_sc_OUM_hunting_opt_occ <- density(rib_sc_OUM_hunting_optima[,3])
rib_sc_OUM_hunting_opt_pou <- density(rib_sc_OUM_hunting_optima[,4])
rib_sc_OUM_hunting_opt_pur <- density(rib_sc_OUM_hunting_optima[,5])
rib_sc_OUM_hunting_opt_fos <- density(rib_sc_OUM_hunting_optima[,6])

pdf("rib_sc_OUM_hunting_opt_hist.pdf",width=8,height=7) 
plot(range(rib_sc_OUM_hunting_opt_pou$x, rib_sc_OUM_hunting_opt_pur$x), range(rib_sc_OUM_hunting_opt_aq$y, rib_sc_OUM_hunting_opt_pur$y), type = "n", xlab = "rib_sc_optima",
     ylab = "Density", main = "aq=blue; car=red; fru=yellow; herb=gre; omni=org; ins=black")
polygon(rib_sc_OUM_hunting_opt_am, col = "red")
polygon(rib_sc_OUM_hunting_opt_aq, col = "blue")
polygon(rib_sc_OUM_hunting_opt_occ, col = "black")
polygon(rib_sc_OUM_hunting_opt_pou, col = "yellow")
polygon(rib_sc_OUM_hunting_opt_pur, col = "green")
polygon(rib_sc_OUM_hunting_opt_fos, col = "orange")
dev.off()


#### Bootstrapping for rib_sc_OUM_hunting model #####
simmap_hunting_100_sam_10 <- sample(simmap_hunting_100_sam[1:100],10)
rib_sc_OUM_hunting_boot <- list(length=length(simmap_hunting_100_sam_10))
for(i in 1:length(simmap_hunting_100_sam_10)){
  rib_sc_OUM_hunting_boot[[i]] <- OUwie.boot(simmap_hunting_100_sam_10[[i]], rib_sc_hunting, model="OUM",simmap.tree=TRUE, root.station=TRUE, nboot=100, mserr="none",
                                             alpha=c(mean(rib_sc_OUM_hunting_alpha),
                                                     mean(rib_sc_OUM_hunting_alpha),
                                                     mean(rib_sc_OUM_hunting_alpha),
                                                     mean(rib_sc_OUM_hunting_alpha),
                                                     mean(rib_sc_OUM_hunting_alpha),
                                                     mean(rib_sc_OUM_hunting_alpha)),
                                             sigma.sq=c(mean(rib_sc_OUM_hunting_sigma),
                                                        mean(rib_sc_OUM_hunting_sigma),
                                                        mean(rib_sc_OUM_hunting_sigma),
                                                        mean(rib_sc_OUM_hunting_sigma),
                                                        mean(rib_sc_OUM_hunting_sigma),
                                                        mean(rib_sc_OUM_hunting_sigma)),
                                             theta=c(mean(rib_sc_OUM_hunting_optima[,1]), 
                                                     mean(rib_sc_OUM_hunting_optima[,2]), 
                                                     mean(rib_sc_OUM_hunting_optima[,3]), 
                                                     mean(rib_sc_OUM_hunting_optima[,4]), 
                                                     mean(rib_sc_OUM_hunting_optima[,5]), 
                                                     mean(rib_sc_OUM_hunting_optima[,6])), 
                                             theta0=mean(rib_sc_OUM_hunting_optima[,4]))
  print(i)
}
save(rib_sc_OUM_hunting_boot, file="rib_sc_OUM_hunting_boot.Rdata")


### rib_sc_OUM_hunting optima ###
rib_sc_OUM_hunting_boot_opt_am <- list()
for(i in 1:length(rib_sc_OUM_hunting_boot)){
  rib_sc_OUM_hunting_boot_opt_am[[i]] <- c(rib_sc_OUM_hunting_boot[[i]][,13])}

rib_sc_OUM_hunting_boot_opt_aq <- list()
for(i in 1:length(rib_sc_OUM_hunting_boot)){
  rib_sc_OUM_hunting_boot_opt_aq[[i]] <- c(rib_sc_OUM_hunting_boot[[i]][,14])}

rib_sc_OUM_hunting_boot_opt_occ <- list()
for(i in 1:length(rib_sc_OUM_hunting_boot)){
  rib_sc_OUM_hunting_boot_opt_occ[[i]] <- c(rib_sc_OUM_hunting_boot[[i]][,15])}

rib_sc_OUM_hunting_boot_opt_pou <- list()
for(i in 1:length(rib_sc_OUM_hunting_boot)){
  rib_sc_OUM_hunting_boot_opt_pou[[i]] <- c(rib_sc_OUM_hunting_boot[[i]][,16])}

rib_sc_OUM_hunting_boot_opt_pur <- list()
for(i in 1:length(rib_sc_OUM_hunting_boot)){
  rib_sc_OUM_hunting_boot_opt_pur[[i]] <- c(rib_sc_OUM_hunting_boot[[i]][,17])}

rib_sc_OUM_hunting_boot_opt_fos <- list()
for(i in 1:length(rib_sc_OUM_hunting_boot)){
  rib_sc_OUM_hunting_boot_opt_fos[[i]] <- c(rib_sc_OUM_hunting_boot[[i]][,18])}

mean_rib_sc_OUM_hunting_boot_opt <- rbind(
  mean(unlist(rib_sc_OUM_hunting_boot_opt_am)),
  mean(unlist(rib_sc_OUM_hunting_boot_opt_aq)),
  mean(unlist(rib_sc_OUM_hunting_boot_opt_occ)),
  mean(unlist(rib_sc_OUM_hunting_boot_opt_pou)),
  mean(unlist(rib_sc_OUM_hunting_boot_opt_pur)),
  mean(unlist(rib_sc_OUM_hunting_boot_opt_fos)))

CI_rib_sc_OUM_hunting_boot_opt <- rbind(
  quantile(unlist(rib_sc_OUM_hunting_boot_opt_am), probs=c(0.025, 0.975)),
  quantile(unlist(rib_sc_OUM_hunting_boot_opt_aq), probs=c(0.025, 0.975)),
  quantile(unlist(rib_sc_OUM_hunting_boot_opt_occ), probs=c(0.025, 0.975)),
  quantile(unlist(rib_sc_OUM_hunting_boot_opt_pou), probs=c(0.025, 0.975)),
  quantile(unlist(rib_sc_OUM_hunting_boot_opt_pur), probs=c(0.025, 0.975)),
  quantile(unlist(rib_sc_OUM_hunting_boot_opt_fos), probs=c(0.025, 0.975)))

rib_sc_OUM_hunting_boot_opt <- data.frame(mean_rib_sc_OUM_hunting_boot_opt,CI_rib_sc_OUM_hunting_boot_opt)
colnames(rib_sc_OUM_hunting_boot_opt) <- c("OUM_optima_boot", "L95", "U95")
rownames(rib_sc_OUM_hunting_boot_opt) <- c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial")
rib_sc_OUM_hunting_boot_opt
write.csv(rib_sc_OUM_hunting_boot_opt, file="rib_sc_OUM_hunting_boot_opt.csv")


col_hunting <- c("red", "blue", "black", "yellow","green","orange")

pdf("rib_sc_OUM_hunting_boot_line.pdf",width=5,height=7) 
plot(mean_rib_sc_OUM_hunting_optima$OUM_optima, ylim = c(min(mean_rib_sc_OUM_hunting_optima$OUM_optima-(rib_sc_OUM_hunting_boot_opt$OUM_optima_boot-rib_sc_OUM_hunting_boot_opt$L95)), max(rib_sc_OUM_hunting_boot_opt$U95-rib_sc_OUM_hunting_boot_opt$OUM_optima_boot+mean_rib_sc_OUM_hunting_optima$OUM_optima)),  ylab = "rib_sc", xaxt = "n", las=1, cex.axis=2)
axis(1, at=c(1:6), labels = c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial"), las=2, cex.axis=2)
plotCI(mean_rib_sc_OUM_hunting_optima$OUM_optima,y=NULL, uiw=rib_sc_OUM_hunting_boot_opt$U95-rib_sc_OUM_hunting_boot_opt$OUM_optima_boot, liw=rib_sc_OUM_hunting_boot_opt$OUM_optima_boot-rib_sc_OUM_hunting_boot_opt$L95, err="y", pch=20, slty=3, col=col_hunting, scol = col_hunting, add=TRUE, cex=3, lwd=3)
dev.off()

rib_sc_OUM_hunting_boot_opt_am_den <- density(unlist(rib_sc_OUM_hunting_boot_opt_am))
rib_sc_OUM_hunting_boot_opt_aq_den <- density(unlist(rib_sc_OUM_hunting_boot_opt_aq))
rib_sc_OUM_hunting_boot_opt_occ_den <- density(unlist(rib_sc_OUM_hunting_boot_opt_occ))
rib_sc_OUM_hunting_boot_opt_pou_den <- density(unlist(rib_sc_OUM_hunting_boot_opt_pou))
rib_sc_OUM_hunting_boot_opt_pur_den <- density(unlist(rib_sc_OUM_hunting_boot_opt_pur))
rib_sc_OUM_hunting_boot_opt_fos_den <- density(unlist(rib_sc_OUM_hunting_boot_opt_fos))

pdf("rib_sc_OUM_hunting_boot_hist.pdf",width=8,height=7) 
plot(range(rib_sc_OUM_hunting_boot_opt_occ_den$x, rib_sc_OUM_hunting_boot_opt_pur_den$x), range(rib_sc_OUM_hunting_boot_opt_fos_den$y, rib_sc_OUM_hunting_boot_opt_pou_den$y), type = "n", xlab = "rib_sc_optima", ylab = "Density", main = "am=red; aq=blue; occ=black; pou=yel; pur=gre; fos=ora")
polygon(rib_sc_OUM_hunting_boot_opt_am_den, col = "red")
polygon(rib_sc_OUM_hunting_boot_opt_aq_den, col = "blue")
polygon(rib_sc_OUM_hunting_boot_opt_occ_den, col = "black")
polygon(rib_sc_OUM_hunting_boot_opt_pou_den, col = "yellow")
polygon(rib_sc_OUM_hunting_boot_opt_pur_den, col = "green")
polygon(rib_sc_OUM_hunting_boot_opt_fos_den, col = "orange")
dev.off()
