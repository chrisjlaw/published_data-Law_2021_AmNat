library(OUwie)
library(plotrix)

options(scipen = 999)

load("Rdata/ERecologydata_0914.RData")
load("Rdata/simmap/simmap_hunting_1000_sam500.Rdata")

#### Best Model - OUM_hunting ####
load("Rdata/OUwie_models/AEI_L_OUM_hunting.Rdata")

# alpha constant
AEI_L_OUM_hunting_alpha <- matrix(,length(simmap_hunting_1000_sam500),1,dimnames=list(c(1: length(simmap_hunting_1000_sam500)),c("AEI_L_OUM_hunting_alpha")))
for(i in 1:length(simmap_hunting_1000_sam500)){
  AEI_L_OUM_hunting_alpha[i,1] <- AEI_L_OUM_hunting[[i]]$solution[1,1]
}
mean(AEI_L_OUM_hunting_alpha)
log(2)/mean(AEI_L_OUM_hunting_alpha)

# sigma constant
AEI_L_OUM_hunting_sigma <- matrix(,length(simmap_hunting_1000_sam500),1,dimnames=list(c(1: length(simmap_hunting_1000_sam500)),c("AEI_L_OUM_hunting_sigma")))
for(i in 1:length(simmap_hunting_1000_sam500)){
  AEI_L_OUM_hunting_sigma[i,1] <- AEI_L_OUM_hunting[[i]]$solution[2,1]
}
mean(AEI_L_OUM_hunting_sigma)

# optima regimes 
AEI_L_OUM_hunting_optima <- matrix(,length(simmap_hunting_1000_sam500),6, dimnames=list(c(1: length(simmap_hunting_1000_sam500)), c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial")))
for(i in 1:length(simmap_hunting_1000_sam500)){
  AEI_L_OUM_hunting_optima[i,1] <- AEI_L_OUM_hunting[[i]]$theta[1,1]
  AEI_L_OUM_hunting_optima[i,2] <- AEI_L_OUM_hunting[[i]]$theta[2,1]
  AEI_L_OUM_hunting_optima[i,3] <- AEI_L_OUM_hunting[[i]]$theta[3,1]
  AEI_L_OUM_hunting_optima[i,4] <- AEI_L_OUM_hunting[[i]]$theta[4,1]
  AEI_L_OUM_hunting_optima[i,5] <- AEI_L_OUM_hunting[[i]]$theta[5,1]
  AEI_L_OUM_hunting_optima[i,6] <- AEI_L_OUM_hunting[[i]]$theta[6,1]
}
AEI_L_OUM_hunting_optima

mean_AEI_L_OUM_hunting_optima <- rbind(
  mean(AEI_L_OUM_hunting_optima[,1]),
  mean(AEI_L_OUM_hunting_optima[,2]),
  mean(AEI_L_OUM_hunting_optima[,3]),
  mean(AEI_L_OUM_hunting_optima[,4]),
  mean(AEI_L_OUM_hunting_optima[,5]),
  mean(AEI_L_OUM_hunting_optima[,6]))

CI_AEI_L_OUM_hunting_optima <- rbind(
  quantile(AEI_L_OUM_hunting_optima[,1], probs=c(0.025, 0.975)),
  quantile(AEI_L_OUM_hunting_optima[,2], probs=c(0.025, 0.975)),
  quantile(AEI_L_OUM_hunting_optima[,3], probs=c(0.025, 0.975)),
  quantile(AEI_L_OUM_hunting_optima[,4], probs=c(0.025, 0.975)),
  quantile(AEI_L_OUM_hunting_optima[,5], probs=c(0.025, 0.975)),
  quantile(AEI_L_OUM_hunting_optima[,6], probs=c(0.025, 0.975)))

rownames(mean_AEI_L_OUM_hunting_optima) <- c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial")

mean_AEI_L_OUM_hunting_optima <- data.frame(mean_AEI_L_OUM_hunting_optima,CI_AEI_L_OUM_hunting_optima)
colnames(mean_AEI_L_OUM_hunting_optima) <- c("OUM_optima", "L95", "U95")
mean_AEI_L_OUM_hunting_optima
write.csv(mean_AEI_L_OUM_hunting_optima, file="mean_AEI_L_OUM_hunting_optima.csv")

col_hunting <- c("red", "blue", "black", "yellow","green","orange")

pdf("AEI_L_OUM_hunting_opt_line.pdf",width=5,height=7) 
plot(mean_AEI_L_OUM_hunting_optima$OUM_optima, ylim = c(min(mean_AEI_L_OUM_hunting_optima), max(mean_AEI_L_OUM_hunting_optima)),  ylab = "AEI_L",  xaxt = "n", las=1, cex.axis=2)
axis(1, at=c(1:6), labels = c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial"), las=2, cex.axis=2)
plotCI(mean_AEI_L_OUM_hunting_optima$OUM_optima,y=NULL, uiw=mean_AEI_L_OUM_hunting_optima$U95-mean_AEI_L_OUM_hunting_optima$OUM_optima, liw=mean_AEI_L_OUM_hunting_optima$OUM_optima-mean_AEI_L_OUM_hunting_optima$L95, err="y", pch=20, slty=3, col=col_hunting, scol = col_hunting, add=TRUE, cex=3, lwd=3)
dev.off()

AEI_L_OUM_hunting_opt_am <- density(AEI_L_OUM_hunting_optima[,1])
AEI_L_OUM_hunting_opt_aq <- density(AEI_L_OUM_hunting_optima[,2])
AEI_L_OUM_hunting_opt_occ <- density(AEI_L_OUM_hunting_optima[,3])
AEI_L_OUM_hunting_opt_pou <- density(AEI_L_OUM_hunting_optima[,4])
AEI_L_OUM_hunting_opt_pur <- density(AEI_L_OUM_hunting_optima[,5])
AEI_L_OUM_hunting_opt_fos <- density(AEI_L_OUM_hunting_optima[,6])

pdf("AEI_L_OUM_hunting_opt_hist.pdf",width=8,height=7) 
plot(range(AEI_L_OUM_hunting_opt_pou$x, AEI_L_OUM_hunting_opt_pur$x), range(AEI_L_OUM_hunting_opt_aq$y, AEI_L_OUM_hunting_opt_pur$y), type = "n", xlab = "AEI_L_optima",
     ylab = "Density", main = "aq=blue; car=red; fru=yellow; herb=gre; omni=org; ins=black")
polygon(AEI_L_OUM_hunting_opt_am, col = "red")
polygon(AEI_L_OUM_hunting_opt_aq, col = "blue")
polygon(AEI_L_OUM_hunting_opt_occ, col = "black")
polygon(AEI_L_OUM_hunting_opt_pou, col = "yellow")
polygon(AEI_L_OUM_hunting_opt_pur, col = "green")
polygon(AEI_L_OUM_hunting_opt_fos, col = "orange")
dev.off()


#### Bootstrapping for AEI_L_OUM_hunting model #####
simmap_hunting_1000_sam500_10 <- sample(simmap_hunting_1000_sam500[1:100],10)
AEI_L_OUM_hunting_boot <- list(length=length(simmap_hunting_1000_sam500_10))
for(i in 1:length(simmap_hunting_1000_sam500_10)){
  AEI_L_OUM_hunting_boot[[i]] <- OUwie.boot(simmap_hunting_1000_sam500_10[[i]], AEI_L_hunting, model="OUM",simmap.tree=TRUE, root.station=TRUE, nboot=100, mserr="none",
                                            alpha=c(mean(AEI_L_OUM_hunting_alpha),
                                                    mean(AEI_L_OUM_hunting_alpha),
                                                    mean(AEI_L_OUM_hunting_alpha),
                                                    mean(AEI_L_OUM_hunting_alpha),
                                                    mean(AEI_L_OUM_hunting_alpha),
                                                    mean(AEI_L_OUM_hunting_alpha)),
                                            sigma.sq=c(mean(AEI_L_OUM_hunting_sigma),
                                                       mean(AEI_L_OUM_hunting_sigma),
                                                       mean(AEI_L_OUM_hunting_sigma),
                                                       mean(AEI_L_OUM_hunting_sigma),
                                                       mean(AEI_L_OUM_hunting_sigma),
                                                       mean(AEI_L_OUM_hunting_sigma)),
                                            theta=c(mean(AEI_L_OUM_hunting_optima[,1]), 
                                                    mean(AEI_L_OUM_hunting_optima[,2]), 
                                                    mean(AEI_L_OUM_hunting_optima[,3]), 
                                                    mean(AEI_L_OUM_hunting_optima[,4]), 
                                                    mean(AEI_L_OUM_hunting_optima[,5]), 
                                                    mean(AEI_L_OUM_hunting_optima[,6])), 
                                            theta0=mean(AEI_L_OUM_hunting_optima[,4]))
  print(i)
}
save(AEI_L_OUM_hunting_boot, file="AEI_L_OUM_hunting_boot.Rdata")



### AEI_L_OUM_hunting optima ###
AEI_L_OUM_hunting_boot_opt_am <- list()
for(i in 1:length(AEI_L_OUM_hunting_boot)){
  AEI_L_OUM_hunting_boot_opt_am[[i]] <- c(AEI_L_OUM_hunting_boot[[i]][,13])}

AEI_L_OUM_hunting_boot_opt_aq <- list()
for(i in 1:length(AEI_L_OUM_hunting_boot)){
  AEI_L_OUM_hunting_boot_opt_aq[[i]] <- c(AEI_L_OUM_hunting_boot[[i]][,14])}

AEI_L_OUM_hunting_boot_opt_occ <- list()
for(i in 1:length(AEI_L_OUM_hunting_boot)){
  AEI_L_OUM_hunting_boot_opt_occ[[i]] <- c(AEI_L_OUM_hunting_boot[[i]][,15])}

AEI_L_OUM_hunting_boot_opt_pou <- list()
for(i in 1:length(AEI_L_OUM_hunting_boot)){
  AEI_L_OUM_hunting_boot_opt_pou[[i]] <- c(AEI_L_OUM_hunting_boot[[i]][,16])}

AEI_L_OUM_hunting_boot_opt_pur <- list()
for(i in 1:length(AEI_L_OUM_hunting_boot)){
  AEI_L_OUM_hunting_boot_opt_pur[[i]] <- c(AEI_L_OUM_hunting_boot[[i]][,17])}

AEI_L_OUM_hunting_boot_opt_fos <- list()
for(i in 1:length(AEI_L_OUM_hunting_boot)){
  AEI_L_OUM_hunting_boot_opt_fos[[i]] <- c(AEI_L_OUM_hunting_boot[[i]][,18])}

mean_AEI_L_OUM_hunting_boot_opt <- rbind(
  mean(unlist(AEI_L_OUM_hunting_boot_opt_am)),
  mean(unlist(AEI_L_OUM_hunting_boot_opt_aq)),
  mean(unlist(AEI_L_OUM_hunting_boot_opt_occ)),
  mean(unlist(AEI_L_OUM_hunting_boot_opt_pou)),
  mean(unlist(AEI_L_OUM_hunting_boot_opt_pur)),
  mean(unlist(AEI_L_OUM_hunting_boot_opt_fos)))

CI_AEI_L_OUM_hunting_boot_opt <- rbind(
  quantile(unlist(AEI_L_OUM_hunting_boot_opt_am), probs=c(0.025, 0.975)),
  quantile(unlist(AEI_L_OUM_hunting_boot_opt_aq), probs=c(0.025, 0.975)),
  quantile(unlist(AEI_L_OUM_hunting_boot_opt_occ), probs=c(0.025, 0.975)),
  quantile(unlist(AEI_L_OUM_hunting_boot_opt_pou), probs=c(0.025, 0.975)),
  quantile(unlist(AEI_L_OUM_hunting_boot_opt_pur), probs=c(0.025, 0.975)),
  quantile(unlist(AEI_L_OUM_hunting_boot_opt_fos), probs=c(0.025, 0.975)))

AEI_L_OUM_hunting_boot_opt <- data.frame(mean_AEI_L_OUM_hunting_boot_opt,CI_AEI_L_OUM_hunting_boot_opt)
colnames(AEI_L_OUM_hunting_boot_opt) <- c("OUM_optima_boot", "L95", "U95")
rownames(AEI_L_OUM_hunting_boot_opt) <- c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial")
AEI_L_OUM_hunting_boot_opt
write.csv(AEI_L_OUM_hunting_boot_opt, file="AEI_L_OUM_hunting_boot_opt.csv")


col_hunting <- c("red", "blue", "black", "yellow","green","orange")

pdf("AEI_L_OUM_hunting_boot_line.pdf",width=5,height=7) 
plot(mean_AEI_L_OUM_hunting_optima$OUM_optima, ylim = c(0,2.5),   ylab = "AEI_L", xaxt = "n", las=1, cex.axis=2)
axis(1, at=c(1:6), labels = c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial"), las=2, cex.axis=2)
plotCI(mean_AEI_L_OUM_hunting_optima$OUM_optima,y=NULL, uiw=AEI_L_OUM_hunting_boot_opt$U95-AEI_L_OUM_hunting_boot_opt$OUM_optima_boot, liw=AEI_L_OUM_hunting_boot_opt$OUM_optima_boot-AEI_L_OUM_hunting_boot_opt$L95, err="y", pch=20, slty=3, col=col_hunting, scol = col_hunting, add=TRUE, cex=3, lwd=3)
dev.off()

AEI_L_OUM_hunting_boot_opt_am_den <- density(unlist(AEI_L_OUM_hunting_boot_opt_am))
AEI_L_OUM_hunting_boot_opt_aq_den <- density(unlist(AEI_L_OUM_hunting_boot_opt_aq))
AEI_L_OUM_hunting_boot_opt_occ_den <- density(unlist(AEI_L_OUM_hunting_boot_opt_occ))
AEI_L_OUM_hunting_boot_opt_pou_den <- density(unlist(AEI_L_OUM_hunting_boot_opt_pou))
AEI_L_OUM_hunting_boot_opt_pur_den <- density(unlist(AEI_L_OUM_hunting_boot_opt_pur))
AEI_L_OUM_hunting_boot_opt_fos_den <- density(unlist(AEI_L_OUM_hunting_boot_opt_fos))

pdf("AEI_L_OUM_hunting_boot_hist.pdf",width=8,height=7) 
plot(range(AEI_L_OUM_hunting_boot_opt_fos_den$x, AEI_L_OUM_hunting_boot_opt_pou_den$x), range(AEI_L_OUM_hunting_boot_opt_fos_den$y, AEI_L_OUM_hunting_boot_opt_pou_den$y), type = "n", xlab = "AEI_L_optima", ylab = "Density", main = "am=red; aq=blue; occ=black; pou=yel; pur=gre; fos=ora")
polygon(AEI_L_OUM_hunting_boot_opt_am_den, col = "red")
polygon(AEI_L_OUM_hunting_boot_opt_aq_den, col = "blue")
polygon(AEI_L_OUM_hunting_boot_opt_occ_den, col = "black")
polygon(AEI_L_OUM_hunting_boot_opt_pou_den, col = "yellow")
polygon(AEI_L_OUM_hunting_boot_opt_pur_den, col = "green")
polygon(AEI_L_OUM_hunting_boot_opt_fos_den, col = "orange")
dev.off()




load("Rdata/simmap/simmap_locomotion_1000_sam500.Rdata")

#### second best Model - OUM_locomotion ####
load("Rdata/OUwie_models/OUwie_500/AEI_L_OUM_locomotion.Rdata")

# alpha constant
AEI_L_OUM_locomotion_alpha <- matrix(,length(simmap_locomotion_1000_sam500),1,dimnames=list(c(1: length(simmap_locomotion_1000_sam500)),c("AEI_L_OUM_locomotion_alpha")))
for(i in 1:length(simmap_locomotion_1000_sam500)){
  AEI_L_OUM_locomotion_alpha[i,1] <- AEI_L_OUM_locomotion[[i]]$solution[1,1]
}
mean(AEI_L_OUM_locomotion_alpha)
log(2)/mean(AEI_L_OUM_locomotion_alpha)

# sigma constant
AEI_L_OUM_locomotion_sigma <- matrix(,length(simmap_locomotion_1000_sam500),1,dimnames=list(c(1: length(simmap_locomotion_1000_sam500)),c("AEI_L_OUM_locomotion_sigma")))
for(i in 1:length(simmap_locomotion_1000_sam500)){
  AEI_L_OUM_locomotion_sigma[i,1] <- AEI_L_OUM_locomotion[[i]]$solution[2,1]
}
mean(AEI_L_OUM_locomotion_sigma)

# optima regimes 
AEI_L_OUM_locomotion_optima <- matrix(,length(simmap_locomotion_1000_sam500),6, dimnames=list(c(1: length(simmap_locomotion_1000_sam500)), c("aquatic", "arboreal", "semi_aquatic", "semi_arboreal", "semi_fossorial", "terrestrial")))
for(i in 1:length(simmap_locomotion_1000_sam500)){
  AEI_L_OUM_locomotion_optima[i,1] <- AEI_L_OUM_locomotion[[i]]$theta[1,1]
  AEI_L_OUM_locomotion_optima[i,2] <- AEI_L_OUM_locomotion[[i]]$theta[2,1]
  AEI_L_OUM_locomotion_optima[i,3] <- AEI_L_OUM_locomotion[[i]]$theta[3,1]
  AEI_L_OUM_locomotion_optima[i,4] <- AEI_L_OUM_locomotion[[i]]$theta[4,1]
  AEI_L_OUM_locomotion_optima[i,5] <- AEI_L_OUM_locomotion[[i]]$theta[5,1]
  AEI_L_OUM_locomotion_optima[i,6] <- AEI_L_OUM_locomotion[[i]]$theta[6,1]
}
AEI_L_OUM_locomotion_optima

mean_AEI_L_OUM_locomotion_optima <- rbind(
  mean(AEI_L_OUM_locomotion_optima[,1]),
  mean(AEI_L_OUM_locomotion_optima[,2]),
  mean(AEI_L_OUM_locomotion_optima[,3]),
  mean(AEI_L_OUM_locomotion_optima[,4]),
  mean(AEI_L_OUM_locomotion_optima[,5]),
  mean(AEI_L_OUM_locomotion_optima[,6]))

CI_AEI_L_OUM_locomotion_optima <- rbind(
  quantile(AEI_L_OUM_locomotion_optima[,1], probs=c(0.025, 0.975)),
  quantile(AEI_L_OUM_locomotion_optima[,2], probs=c(0.025, 0.975)),
  quantile(AEI_L_OUM_locomotion_optima[,3], probs=c(0.025, 0.975)),
  quantile(AEI_L_OUM_locomotion_optima[,4], probs=c(0.025, 0.975)),
  quantile(AEI_L_OUM_locomotion_optima[,5], probs=c(0.025, 0.975)),
  quantile(AEI_L_OUM_locomotion_optima[,6], probs=c(0.025, 0.975)))

rownames(mean_AEI_L_OUM_locomotion_optima) <- c("aquatic", "arboreal", "semi_aquatic", "semi_arboreal", "semi_fossorial", "terrestrial")

mean_AEI_L_OUM_locomotion_optima <- data.frame(mean_AEI_L_OUM_locomotion_optima,CI_AEI_L_OUM_locomotion_optima)
colnames(mean_AEI_L_OUM_locomotion_optima) <- c("OUM_optima", "L95", "U95")
mean_AEI_L_OUM_locomotion_optima
write.csv(mean_AEI_L_OUM_locomotion_optima, file="mean_AEI_L_OUM_locomotion_optima.csv")

col_locomotion <- c("blue", "green", "lightblue", "yellow", "orange", "black")

pdf("AEI_L_OUM_locomotion_opt_line.pdf",width=5,height=7) 
plot(mean_AEI_L_OUM_locomotion_optima$OUM_optima, ylim = c(min(mean_AEI_L_OUM_locomotion_optima), max(mean_AEI_L_OUM_locomotion_optima)),  ylab = "AEI_L",  xaxt = "n", las=1, cex.axis=2)
axis(1, at=c(1:6), labels = c("aquatic", "arboreal", "semi_aquatic", "semi_arboreal", "semi_fossorial", "terrestrial"), las=2, cex.axis=2)
plotCI(mean_AEI_L_OUM_locomotion_optima$OUM_optima,y=NULL, uiw=mean_AEI_L_OUM_locomotion_optima$U95-mean_AEI_L_OUM_locomotion_optima$OUM_optima, liw=mean_AEI_L_OUM_locomotion_optima$OUM_optima-mean_AEI_L_OUM_locomotion_optima$L95, err="y", pch=20, slty=3, col=col_locomotion, scol = col_locomotion, add=TRUE, cex=3, lwd=3)
dev.off()

AEI_L_OUM_locomotion_opt_aq <- density(AEI_L_OUM_locomotion_optima[,1])
AEI_L_OUM_locomotion_opt_arb <- density(AEI_L_OUM_locomotion_optima[,2])
AEI_L_OUM_locomotion_opt_saq <- density(AEI_L_OUM_locomotion_optima[,3])
AEI_L_OUM_locomotion_opt_sar <- density(AEI_L_OUM_locomotion_optima[,4])
AEI_L_OUM_locomotion_opt_fos <- density(AEI_L_OUM_locomotion_optima[,5])
AEI_L_OUM_locomotion_opt_ter <- density(AEI_L_OUM_locomotion_optima[,6])

pdf("AEI_L_OUM_locomotion_opt_hist.pdf",width=8,height=7) 
plot(range(AEI_L_OUM_locomotion_opt_sar$x, AEI_L_OUM_locomotion_opt_fos$x), range(AEI_L_OUM_locomotion_opt_arb$y, AEI_L_OUM_locomotion_opt_fos$y), type = "n", xlab = "AEI_L_optima",
     ylab = "Density", main = "aq=blue; car=red; fru=yellow; herb=gre; omni=org; ins=black")
polygon(AEI_L_OUM_locomotion_opt_aq, col = "red")
polygon(AEI_L_OUM_locomotion_opt_arb, col = "blue")
polygon(AEI_L_OUM_locomotion_opt_saq, col = "black")
polygon(AEI_L_OUM_locomotion_opt_sar, col = "yellow")
polygon(AEI_L_OUM_locomotion_opt_fos, col = "green")
polygon(AEI_L_OUM_locomotion_opt_ter, col = "orange")
dev.off()


#### Bootstrapping for AEI_L_OUM_locomotion model #####
simmap_locomotion_1000_sam500_10 <- sample(simmap_locomotion_1000_sam500[1:100],10)
AEI_L_OUM_locomotion_boot <- list(length=length(simmap_locomotion_1000_sam500_10))
for(i in 1:length(simmap_locomotion_1000_sam500_10)){
  AEI_L_OUM_locomotion_boot[[i]] <- OUwie.boot(simmap_locomotion_1000_sam500_10[[i]], AEI_L_locomotion, model="OUM",simmap.tree=TRUE, root.station=TRUE, nboot=100, mserr="none",
                                               alpha=c(mean(AEI_L_OUM_locomotion_alpha),
                                                       mean(AEI_L_OUM_locomotion_alpha),
                                                       mean(AEI_L_OUM_locomotion_alpha),
                                                       mean(AEI_L_OUM_locomotion_alpha),
                                                       mean(AEI_L_OUM_locomotion_alpha),
                                                       mean(AEI_L_OUM_locomotion_alpha)),
                                               sigma.sq=c(mean(AEI_L_OUM_locomotion_sigma),
                                                          mean(AEI_L_OUM_locomotion_sigma),
                                                          mean(AEI_L_OUM_locomotion_sigma),
                                                          mean(AEI_L_OUM_locomotion_sigma),
                                                          mean(AEI_L_OUM_locomotion_sigma),
                                                          mean(AEI_L_OUM_locomotion_sigma)),
                                               theta=c(mean(AEI_L_OUM_locomotion_optima[,1]), 
                                                       mean(AEI_L_OUM_locomotion_optima[,2]), 
                                                       mean(AEI_L_OUM_locomotion_optima[,3]), 
                                                       mean(AEI_L_OUM_locomotion_optima[,4]), 
                                                       mean(AEI_L_OUM_locomotion_optima[,5]), 
                                                       mean(AEI_L_OUM_locomotion_optima[,6])), 
                                               theta0=mean(AEI_L_OUM_locomotion_optima[,4]))
  print(i)
}
save(AEI_L_OUM_locomotion_boot, file="AEI_L_OUM_locomotion_boot.Rdata")



### AEI_L_OUM_locomotion optima ###
AEI_L_OUM_locomotion_boot_opt_aq <- list()
for(i in 1:length(AEI_L_OUM_locomotion_boot)){
  AEI_L_OUM_locomotion_boot_opt_aq[[i]] <- c(AEI_L_OUM_locomotion_boot[[i]][,13])}

AEI_L_OUM_locomotion_boot_opt_arb <- list()
for(i in 1:length(AEI_L_OUM_locomotion_boot)){
  AEI_L_OUM_locomotion_boot_opt_arb[[i]] <- c(AEI_L_OUM_locomotion_boot[[i]][,14])}

AEI_L_OUM_locomotion_boot_opt_saq <- list()
for(i in 1:length(AEI_L_OUM_locomotion_boot)){
  AEI_L_OUM_locomotion_boot_opt_saq[[i]] <- c(AEI_L_OUM_locomotion_boot[[i]][,15])}

AEI_L_OUM_locomotion_boot_opt_sar <- list()
for(i in 1:length(AEI_L_OUM_locomotion_boot)){
  AEI_L_OUM_locomotion_boot_opt_sar[[i]] <- c(AEI_L_OUM_locomotion_boot[[i]][,16])}

AEI_L_OUM_locomotion_boot_opt_fos <- list()
for(i in 1:length(AEI_L_OUM_locomotion_boot)){
  AEI_L_OUM_locomotion_boot_opt_fos[[i]] <- c(AEI_L_OUM_locomotion_boot[[i]][,17])}

AEI_L_OUM_locomotion_boot_opt_ter <- list()
for(i in 1:length(AEI_L_OUM_locomotion_boot)){
  AEI_L_OUM_locomotion_boot_opt_ter[[i]] <- c(AEI_L_OUM_locomotion_boot[[i]][,18])}

mean_AEI_L_OUM_locomotion_boot_opt <- rbind(
  mean(unlist(AEI_L_OUM_locomotion_boot_opt_aq)),
  mean(unlist(AEI_L_OUM_locomotion_boot_opt_arb)),
  mean(unlist(AEI_L_OUM_locomotion_boot_opt_saq)),
  mean(unlist(AEI_L_OUM_locomotion_boot_opt_sar)),
  mean(unlist(AEI_L_OUM_locomotion_boot_opt_fos)),
  mean(unlist(AEI_L_OUM_locomotion_boot_opt_ter)))

CI_AEI_L_OUM_locomotion_boot_opt <- rbind(
  quantile(unlist(AEI_L_OUM_locomotion_boot_opt_aq), probs=c(0.025, 0.975)),
  quantile(unlist(AEI_L_OUM_locomotion_boot_opt_arb), probs=c(0.025, 0.975)),
  quantile(unlist(AEI_L_OUM_locomotion_boot_opt_saq), probs=c(0.025, 0.975)),
  quantile(unlist(AEI_L_OUM_locomotion_boot_opt_sar), probs=c(0.025, 0.975)),
  quantile(unlist(AEI_L_OUM_locomotion_boot_opt_fos), probs=c(0.025, 0.975)),
  quantile(unlist(AEI_L_OUM_locomotion_boot_opt_ter), probs=c(0.025, 0.975)))

AEI_L_OUM_locomotion_boot_opt <- data.frame(mean_AEI_L_OUM_locomotion_boot_opt,CI_AEI_L_OUM_locomotion_boot_opt)
colnames(AEI_L_OUM_locomotion_boot_opt) <- c("OUM_optima_boot", "L95", "U95")
rownames(AEI_L_OUM_locomotion_boot_opt) <- c("aquatic", "arboreal", "semi_aquatic", "semi_arboreal", "semi_fossorial", "terrestrial")
AEI_L_OUM_locomotion_boot_opt
write.csv(AEI_L_OUM_locomotion_boot_opt, file="AEI_L_OUM_locomotion_boot_opt.csv")


col_locomotion <- c("blue", "green", "lightblue", "yellow", "orange", "black")

pdf("AEI_L_OUM_locomotion_boot_line.pdf",width=5,height=7) 
plot(mean_AEI_L_OUM_locomotion_optima$OUM_optima, ylim = c(min(mean_AEI_L_OUM_locomotion_optima$OUM_optima-(AEI_L_OUM_locomotion_boot_opt$OUM_optima_boot-AEI_L_OUM_locomotion_boot_opt$L95)), max(AEI_L_OUM_locomotion_boot_opt$U95-AEI_L_OUM_locomotion_boot_opt$OUM_optima_boot+mean_AEI_L_OUM_locomotion_optima$OUM_optima)),  ylab = "AEI_L", xaxt = "n", las=1, cex.axis=2)
axis(1, at=c(1:6), labels = c("aquatic", "arboreal", "semi_aquatic", "semi_arboreal", "semi_fossorial", "terrestrial"), las=2, cex.axis=2)
plotCI(mean_AEI_L_OUM_locomotion_optima$OUM_optima,y=NULL, uiw=AEI_L_OUM_locomotion_boot_opt$U95-AEI_L_OUM_locomotion_boot_opt$OUM_optima_boot, liw=AEI_L_OUM_locomotion_boot_opt$OUM_optima_boot-AEI_L_OUM_locomotion_boot_opt$L95, err="y", pch=20, slty=3, col=col_locomotion, scol = col_locomotion, add=TRUE, cex=3, lwd=3)
dev.off()

AEI_L_OUM_locomotion_boot_opt_aq_den <- density(unlist(AEI_L_OUM_locomotion_boot_opt_aq))
AEI_L_OUM_locomotion_boot_opt_arb_den <- density(unlist(AEI_L_OUM_locomotion_boot_opt_arb))
AEI_L_OUM_locomotion_boot_opt_saq_den <- density(unlist(AEI_L_OUM_locomotion_boot_opt_saq))
AEI_L_OUM_locomotion_boot_opt_sar_den <- density(unlist(AEI_L_OUM_locomotion_boot_opt_sar))
AEI_L_OUM_locomotion_boot_opt_fos_den <- density(unlist(AEI_L_OUM_locomotion_boot_opt_fos))
AEI_L_OUM_locomotion_boot_opt_ter_den <- density(unlist(AEI_L_OUM_locomotion_boot_opt_ter))

pdf("AEI_L_OUM_locomotion_boot_hist.pdf",width=8,height=7) 
plot(range(AEI_L_OUM_locomotion_boot_opt_saq_den$x, AEI_L_OUM_locomotion_boot_opt_fos_den$x), range(AEI_L_OUM_locomotion_boot_opt_ter_den$y, AEI_L_OUM_locomotion_boot_opt_sar_den$y), type = "n", xlab = "AEI_L_optima", ylab = "Density", main = "am=red; aq=blue; occ=black; pou=yel; pur=gre; fos=ora")
polygon(AEI_L_OUM_locomotion_boot_opt_aq_den, col = "red")
polygon(AEI_L_OUM_locomotion_boot_opt_arb_den, col = "blue")
polygon(AEI_L_OUM_locomotion_boot_opt_saq_den, col = "black")
polygon(AEI_L_OUM_locomotion_boot_opt_sar_den, col = "yellow")
polygon(AEI_L_OUM_locomotion_boot_opt_fos_den, col = "green")
polygon(AEI_L_OUM_locomotion_boot_opt_ter_den, col = "orange")
dev.off()