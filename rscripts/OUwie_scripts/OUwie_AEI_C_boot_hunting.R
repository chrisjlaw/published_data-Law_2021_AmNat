library(OUwie)
library(plotrix)

options(scipen = 999)

load("Rdata/ERecologydata_0504.RData")
load("Rdata/simmap/simmap_hunting_1000_sam500.Rdata")

#### Best Model - OUM_hunting ####
load("Rdata/OUwie_models/AEI_C_OUM_hunting.Rdata")

# alpha constant
AEI_C_OUM_hunting_alpha <- matrix(,length(simmap_hunting_1000_sam500),1,dimnames=list(c(1: length(simmap_hunting_1000_sam500)),c("AEI_C_OUM_hunting_alpha")))
for(i in 1:length(simmap_hunting_1000_sam500)){
  AEI_C_OUM_hunting_alpha[i,1] <- AEI_C_OUM_hunting[[i]]$solution[1,1]
}
mean(AEI_C_OUM_hunting_alpha)
log(2)/mean(AEI_C_OUM_hunting_alpha)

# sigma constant
AEI_C_OUM_hunting_sigma <- matrix(,length(simmap_hunting_1000_sam500),1,dimnames=list(c(1: length(simmap_hunting_1000_sam500)),c("AEI_C_OUM_hunting_sigma")))
for(i in 1:length(simmap_hunting_1000_sam500)){
  AEI_C_OUM_hunting_sigma[i,1] <- AEI_C_OUM_hunting[[i]]$solution[2,1]
}
mean(AEI_C_OUM_hunting_sigma)

# optima regimes 
AEI_C_OUM_hunting_optima <- matrix(,length(simmap_hunting_1000_sam500),6, dimnames=list(c(1: length(simmap_hunting_1000_sam500)), c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial")))
for(i in 1:length(simmap_hunting_1000_sam500)){
  AEI_C_OUM_hunting_optima[i,1] <- AEI_C_OUM_hunting[[i]]$theta[1,1]
  AEI_C_OUM_hunting_optima[i,2] <- AEI_C_OUM_hunting[[i]]$theta[2,1]
  AEI_C_OUM_hunting_optima[i,3] <- AEI_C_OUM_hunting[[i]]$theta[3,1]
  AEI_C_OUM_hunting_optima[i,4] <- AEI_C_OUM_hunting[[i]]$theta[4,1]
  AEI_C_OUM_hunting_optima[i,5] <- AEI_C_OUM_hunting[[i]]$theta[5,1]
  AEI_C_OUM_hunting_optima[i,6] <- AEI_C_OUM_hunting[[i]]$theta[6,1]
}
AEI_C_OUM_hunting_optima

mean_AEI_C_OUM_hunting_optima <- rbind(
  mean(AEI_C_OUM_hunting_optima[,1]),
  mean(AEI_C_OUM_hunting_optima[,2]),
  mean(AEI_C_OUM_hunting_optima[,3]),
  mean(AEI_C_OUM_hunting_optima[,4]),
  mean(AEI_C_OUM_hunting_optima[,5]),
  mean(AEI_C_OUM_hunting_optima[,6]))

CI_AEI_C_OUM_hunting_optima <- rbind(
  quantile(AEI_C_OUM_hunting_optima[,1], probs=c(0.025, 0.975)),
  quantile(AEI_C_OUM_hunting_optima[,2], probs=c(0.025, 0.975)),
  quantile(AEI_C_OUM_hunting_optima[,3], probs=c(0.025, 0.975)),
  quantile(AEI_C_OUM_hunting_optima[,4], probs=c(0.025, 0.975)),
  quantile(AEI_C_OUM_hunting_optima[,5], probs=c(0.025, 0.975)),
  quantile(AEI_C_OUM_hunting_optima[,6], probs=c(0.025, 0.975)))

rownames(mean_AEI_C_OUM_hunting_optima) <- c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial")

mean_AEI_C_OUM_hunting_optima <- data.frame(mean_AEI_C_OUM_hunting_optima,CI_AEI_C_OUM_hunting_optima)
colnames(mean_AEI_C_OUM_hunting_optima) <- c("OUM_optima", "L95", "U95")
mean_AEI_C_OUM_hunting_optima
write.csv(mean_AEI_C_OUM_hunting_optima, file="mean_AEI_C_OUM_hunting_optima.csv")

col_hunting <- c("red", "blue", "black", "yellow","green","orange")

pdf("AEI_C_OUM_hunting_opt_line.pdf",width=5,height=7) 
plot(mean_AEI_C_OUM_hunting_optima$OUM_optima, ylim = c(min(mean_AEI_C_OUM_hunting_optima), max(mean_AEI_C_OUM_hunting_optima)),  ylab = "AEI_C",  xaxt = "n", las=1, cex.axis=2)
axis(1, at=c(1:6), labels = c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial"), las=2, cex.axis=2)
plotCI(mean_AEI_C_OUM_hunting_optima$OUM_optima,y=NULL, uiw=mean_AEI_C_OUM_hunting_optima$U95-mean_AEI_C_OUM_hunting_optima$OUM_optima, liw=mean_AEI_C_OUM_hunting_optima$OUM_optima-mean_AEI_C_OUM_hunting_optima$L95, err="y", pch=20, slty=3, col=col_hunting, scol = col_hunting, add=TRUE, cex=3, lwd=3)
dev.off()

AEI_C_OUM_hunting_opt_am <- density(AEI_C_OUM_hunting_optima[,1])
AEI_C_OUM_hunting_opt_aq <- density(AEI_C_OUM_hunting_optima[,2])
AEI_C_OUM_hunting_opt_occ <- density(AEI_C_OUM_hunting_optima[,3])
AEI_C_OUM_hunting_opt_pou <- density(AEI_C_OUM_hunting_optima[,4])
AEI_C_OUM_hunting_opt_pur <- density(AEI_C_OUM_hunting_optima[,5])
AEI_C_OUM_hunting_opt_fos <- density(AEI_C_OUM_hunting_optima[,6])

pdf("AEI_C_OUM_hunting_opt_hist.pdf",width=8,height=7) 
plot(range(AEI_C_OUM_hunting_opt_pou$x, AEI_C_OUM_hunting_opt_pur$x), range(AEI_C_OUM_hunting_opt_aq$y, AEI_C_OUM_hunting_opt_pur$y), type = "n", xlab = "AEI_C_optima",
     ylab = "Density", main = "aq=blue; car=red; fru=yellow; herb=gre; omni=org; ins=black")
polygon(AEI_C_OUM_hunting_opt_am, col = "red")
polygon(AEI_C_OUM_hunting_opt_aq, col = "blue")
polygon(AEI_C_OUM_hunting_opt_occ, col = "black")
polygon(AEI_C_OUM_hunting_opt_pou, col = "yellow")
polygon(AEI_C_OUM_hunting_opt_pur, col = "green")
polygon(AEI_C_OUM_hunting_opt_fos, col = "orange")
dev.off()


#### Bootstrapping for AEI_C_OUM_hunting model #####
simmap_hunting_1000_sam500_10 <- sample(simmap_hunting_1000_sam500[1:100],10)
AEI_C_OUM_hunting_boot <- list(length=length(simmap_hunting_1000_sam500_10))
for(i in 1:length(simmap_hunting_1000_sam500_10)){
  AEI_C_OUM_hunting_boot[[i]] <- OUwie.boot(simmap_hunting_1000_sam500_10[[i]], AEI_C_hunting, model="OUM",simmap.tree=TRUE, root.station=TRUE, nboot=100, mserr="none",
                                            alpha=c(mean(AEI_C_OUM_hunting_alpha),
                                                    mean(AEI_C_OUM_hunting_alpha),
                                                    mean(AEI_C_OUM_hunting_alpha),
                                                    mean(AEI_C_OUM_hunting_alpha),
                                                    mean(AEI_C_OUM_hunting_alpha),
                                                    mean(AEI_C_OUM_hunting_alpha)),
                                            sigma.sq=c(mean(AEI_C_OUM_hunting_sigma),
                                                       mean(AEI_C_OUM_hunting_sigma),
                                                       mean(AEI_C_OUM_hunting_sigma),
                                                       mean(AEI_C_OUM_hunting_sigma),
                                                       mean(AEI_C_OUM_hunting_sigma),
                                                       mean(AEI_C_OUM_hunting_sigma)),
                                            theta=c(mean(AEI_C_OUM_hunting_optima[,1]), 
                                                    mean(AEI_C_OUM_hunting_optima[,2]), 
                                                    mean(AEI_C_OUM_hunting_optima[,3]), 
                                                    mean(AEI_C_OUM_hunting_optima[,4]), 
                                                    mean(AEI_C_OUM_hunting_optima[,5]), 
                                                    mean(AEI_C_OUM_hunting_optima[,6])), 
                                            theta0=mean(AEI_C_OUM_hunting_optima[,4]))
  print(i)
}
save(AEI_C_OUM_hunting_boot, file="AEI_C_OUM_hunting_boot.Rdata")


### AEI_C_OUM_hunting optima ###
AEI_C_OUM_hunting_boot_opt_am <- list()
for(i in 1:length(AEI_C_OUM_hunting_boot)){
  AEI_C_OUM_hunting_boot_opt_am[[i]] <- c(AEI_C_OUM_hunting_boot[[i]][,13])}

AEI_C_OUM_hunting_boot_opt_aq <- list()
for(i in 1:length(AEI_C_OUM_hunting_boot)){
  AEI_C_OUM_hunting_boot_opt_aq[[i]] <- c(AEI_C_OUM_hunting_boot[[i]][,14])}

AEI_C_OUM_hunting_boot_opt_occ <- list()
for(i in 1:length(AEI_C_OUM_hunting_boot)){
  AEI_C_OUM_hunting_boot_opt_occ[[i]] <- c(AEI_C_OUM_hunting_boot[[i]][,15])}

AEI_C_OUM_hunting_boot_opt_pou <- list()
for(i in 1:length(AEI_C_OUM_hunting_boot)){
  AEI_C_OUM_hunting_boot_opt_pou[[i]] <- c(AEI_C_OUM_hunting_boot[[i]][,16])}

AEI_C_OUM_hunting_boot_opt_pur <- list()
for(i in 1:length(AEI_C_OUM_hunting_boot)){
  AEI_C_OUM_hunting_boot_opt_pur[[i]] <- c(AEI_C_OUM_hunting_boot[[i]][,17])}

AEI_C_OUM_hunting_boot_opt_fos <- list()
for(i in 1:length(AEI_C_OUM_hunting_boot)){
  AEI_C_OUM_hunting_boot_opt_fos[[i]] <- c(AEI_C_OUM_hunting_boot[[i]][,18])}

mean_AEI_C_OUM_hunting_boot_opt <- rbind(
  mean(unlist(AEI_C_OUM_hunting_boot_opt_am)),
  mean(unlist(AEI_C_OUM_hunting_boot_opt_aq)),
  mean(unlist(AEI_C_OUM_hunting_boot_opt_occ)),
  mean(unlist(AEI_C_OUM_hunting_boot_opt_pou)),
  mean(unlist(AEI_C_OUM_hunting_boot_opt_pur)),
  mean(unlist(AEI_C_OUM_hunting_boot_opt_fos)))

CI_AEI_C_OUM_hunting_boot_opt <- rbind(
  quantile(unlist(AEI_C_OUM_hunting_boot_opt_am), probs=c(0.025, 0.975)),
  quantile(unlist(AEI_C_OUM_hunting_boot_opt_aq), probs=c(0.025, 0.975)),
  quantile(unlist(AEI_C_OUM_hunting_boot_opt_occ), probs=c(0.025, 0.975)),
  quantile(unlist(AEI_C_OUM_hunting_boot_opt_pou), probs=c(0.025, 0.975)),
  quantile(unlist(AEI_C_OUM_hunting_boot_opt_pur), probs=c(0.025, 0.975)),
  quantile(unlist(AEI_C_OUM_hunting_boot_opt_fos), probs=c(0.025, 0.975)))

AEI_C_OUM_hunting_boot_opt <- data.frame(mean_AEI_C_OUM_hunting_boot_opt,CI_AEI_C_OUM_hunting_boot_opt)
colnames(AEI_C_OUM_hunting_boot_opt) <- c("OUM_optima_boot", "L95", "U95")
rownames(AEI_C_OUM_hunting_boot_opt) <- c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial")
AEI_C_OUM_hunting_boot_opt
write.csv(AEI_C_OUM_hunting_boot_opt, file="AEI_C_OUM_hunting_boot_opt.csv")


col_hunting <- c("red", "blue", "black", "yellow","green","orange")

pdf("AEI_C_OUM_hunting_boot_line.pdf",width=5,height=7) 
plot(mean_AEI_C_OUM_hunting_optima$OUM_optima, ylim = c(min(mean_AEI_C_OUM_hunting_optima$OUM_optima-(AEI_C_OUM_hunting_boot_opt$OUM_optima_boot-AEI_C_OUM_hunting_boot_opt$L95)), max(AEI_C_OUM_hunting_boot_opt$U95-AEI_C_OUM_hunting_boot_opt$OUM_optima_boot+mean_AEI_C_OUM_hunting_optima$OUM_optima)),  ylab = "AEI_C", xaxt = "n", las=1, cex.axis=2)
axis(1, at=c(1:6), labels = c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial"), las=2, cex.axis=2)
plotCI(mean_AEI_C_OUM_hunting_optima$OUM_optima,y=NULL, uiw=AEI_C_OUM_hunting_boot_opt$U95-AEI_C_OUM_hunting_boot_opt$OUM_optima_boot, liw=AEI_C_OUM_hunting_boot_opt$OUM_optima_boot-AEI_C_OUM_hunting_boot_opt$L95, err="y", pch=20, slty=3, col=col_hunting, scol = col_hunting, add=TRUE, cex=3, lwd=3)
dev.off()

AEI_C_OUM_hunting_boot_opt_am_den <- density(unlist(AEI_C_OUM_hunting_boot_opt_am))
AEI_C_OUM_hunting_boot_opt_aq_den <- density(unlist(AEI_C_OUM_hunting_boot_opt_aq))
AEI_C_OUM_hunting_boot_opt_occ_den <- density(unlist(AEI_C_OUM_hunting_boot_opt_occ))
AEI_C_OUM_hunting_boot_opt_pou_den <- density(unlist(AEI_C_OUM_hunting_boot_opt_pou))
AEI_C_OUM_hunting_boot_opt_pur_den <- density(unlist(AEI_C_OUM_hunting_boot_opt_pur))
AEI_C_OUM_hunting_boot_opt_fos_den <- density(unlist(AEI_C_OUM_hunting_boot_opt_fos))

pdf("AEI_C_OUM_hunting_boot_hist.pdf",width=8,height=7) 
plot(range(AEI_C_OUM_hunting_boot_opt_occ_den$x, AEI_C_OUM_hunting_boot_opt_pur_den$x), range(AEI_C_OUM_hunting_boot_opt_fos_den$y, AEI_C_OUM_hunting_boot_opt_pou_den$y), type = "n", xlab = "AEI_C_optima", ylab = "Density", main = "am=red; aq=blue; occ=black; pou=yel; pur=gre; fos=ora")
polygon(AEI_C_OUM_hunting_boot_opt_am_den, col = "red")
polygon(AEI_C_OUM_hunting_boot_opt_aq_den, col = "blue")
polygon(AEI_C_OUM_hunting_boot_opt_occ_den, col = "black")
polygon(AEI_C_OUM_hunting_boot_opt_pou_den, col = "yellow")
polygon(AEI_C_OUM_hunting_boot_opt_pur_den, col = "green")
polygon(AEI_C_OUM_hunting_boot_opt_fos_den, col = "orange")
dev.off()