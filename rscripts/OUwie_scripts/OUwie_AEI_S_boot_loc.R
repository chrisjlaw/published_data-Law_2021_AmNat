library(OUwie)
library(plotrix)

options(scipen = 999)

load("Rdata/ERecologydata_0914.RData")
load("Rdata/simmap/simmap_locomotion_1000_sam500.Rdata")

#### second best Model - OUM_locomotion ####
load("Rdata/OUwie_models/OUwie_500/AEI_S_OUM_locomotion.Rdata")

# alpha constant
AEI_S_OUM_locomotion_alpha <- matrix(,length(simmap_locomotion_1000_sam500),1,dimnames=list(c(1: length(simmap_locomotion_1000_sam500)),c("AEI_S_OUM_locomotion_alpha")))
for(i in 1:length(simmap_locomotion_1000_sam500)){
  AEI_S_OUM_locomotion_alpha[i,1] <- AEI_S_OUM_locomotion[[i]]$solution[1,1]
}
mean(AEI_S_OUM_locomotion_alpha)
log(2)/mean(AEI_S_OUM_locomotion_alpha)

# sigma constant
AEI_S_OUM_locomotion_sigma <- matrix(,length(simmap_locomotion_1000_sam500),1,dimnames=list(c(1: length(simmap_locomotion_1000_sam500)),c("AEI_S_OUM_locomotion_sigma")))
for(i in 1:length(simmap_locomotion_1000_sam500)){
  AEI_S_OUM_locomotion_sigma[i,1] <- AEI_S_OUM_locomotion[[i]]$solution[2,1]
}
mean(AEI_S_OUM_locomotion_sigma)

# optima regimes 
AEI_S_OUM_locomotion_optima <- matrix(,length(simmap_locomotion_1000_sam500),6, dimnames=list(c(1: length(simmap_locomotion_1000_sam500)), c("aquatic", "arboreal", "semi_aquatic", "semi_arboreal", "semi_fossorial", "terrestrial")))
for(i in 1:length(simmap_locomotion_1000_sam500)){
  AEI_S_OUM_locomotion_optima[i,1] <- AEI_S_OUM_locomotion[[i]]$theta[1,1]
  AEI_S_OUM_locomotion_optima[i,2] <- AEI_S_OUM_locomotion[[i]]$theta[2,1]
  AEI_S_OUM_locomotion_optima[i,3] <- AEI_S_OUM_locomotion[[i]]$theta[3,1]
  AEI_S_OUM_locomotion_optima[i,4] <- AEI_S_OUM_locomotion[[i]]$theta[4,1]
  AEI_S_OUM_locomotion_optima[i,5] <- AEI_S_OUM_locomotion[[i]]$theta[5,1]
  AEI_S_OUM_locomotion_optima[i,6] <- AEI_S_OUM_locomotion[[i]]$theta[6,1]
}
AEI_S_OUM_locomotion_optima

mean_AEI_S_OUM_locomotion_optima <- rbind(
  mean(AEI_S_OUM_locomotion_optima[,1]),
  mean(AEI_S_OUM_locomotion_optima[,2]),
  mean(AEI_S_OUM_locomotion_optima[,3]),
  mean(AEI_S_OUM_locomotion_optima[,4]),
  mean(AEI_S_OUM_locomotion_optima[,5]),
  mean(AEI_S_OUM_locomotion_optima[,6]))

CI_AEI_S_OUM_locomotion_optima <- rbind(
  quantile(AEI_S_OUM_locomotion_optima[,1], probs=c(0.025, 0.975)),
  quantile(AEI_S_OUM_locomotion_optima[,2], probs=c(0.025, 0.975)),
  quantile(AEI_S_OUM_locomotion_optima[,3], probs=c(0.025, 0.975)),
  quantile(AEI_S_OUM_locomotion_optima[,4], probs=c(0.025, 0.975)),
  quantile(AEI_S_OUM_locomotion_optima[,5], probs=c(0.025, 0.975)),
  quantile(AEI_S_OUM_locomotion_optima[,6], probs=c(0.025, 0.975)))

rownames(mean_AEI_S_OUM_locomotion_optima) <- c("aquatic", "arboreal", "semi_aquatic", "semi_arboreal", "semi_fossorial", "terrestrial")

mean_AEI_S_OUM_locomotion_optima <- data.frame(mean_AEI_S_OUM_locomotion_optima,CI_AEI_S_OUM_locomotion_optima)
colnames(mean_AEI_S_OUM_locomotion_optima) <- c("OUM_optima", "L95", "U95")
mean_AEI_S_OUM_locomotion_optima
write.csv(mean_AEI_S_OUM_locomotion_optima, file="mean_AEI_S_OUM_locomotion_optima.csv")

col_locomotion <- c("blue", "green", "lightblue", "yellow", "orange", "black")

pdf("AEI_S_OUM_locomotion_opt_line.pdf",width=5,height=7) 
plot(mean_AEI_S_OUM_locomotion_optima$OUM_optima, ylim = c(min(mean_AEI_S_OUM_locomotion_optima), max(mean_AEI_S_OUM_locomotion_optima)),  ylab = "AEI_S",  xaxt = "n", las=1, cex.axis=2)
axis(1, at=c(1:6), labels = c("aquatic", "arboreal", "semi_aquatic", "semi_arboreal", "semi_fossorial", "terrestrial"), las=2, cex.axis=2)
plotCI(mean_AEI_S_OUM_locomotion_optima$OUM_optima,y=NULL, uiw=mean_AEI_S_OUM_locomotion_optima$U95-mean_AEI_S_OUM_locomotion_optima$OUM_optima, liw=mean_AEI_S_OUM_locomotion_optima$OUM_optima-mean_AEI_S_OUM_locomotion_optima$L95, err="y", pch=20, slty=3, col=col_locomotion, scol = col_locomotion, add=TRUE, cex=3, lwd=3)
dev.off()

AEI_S_OUM_locomotion_opt_aq <- density(AEI_S_OUM_locomotion_optima[,1])
AEI_S_OUM_locomotion_opt_arb <- density(AEI_S_OUM_locomotion_optima[,2])
AEI_S_OUM_locomotion_opt_saq <- density(AEI_S_OUM_locomotion_optima[,3])
AEI_S_OUM_locomotion_opt_sar <- density(AEI_S_OUM_locomotion_optima[,4])
AEI_S_OUM_locomotion_opt_fos <- density(AEI_S_OUM_locomotion_optima[,5])
AEI_S_OUM_locomotion_opt_ter <- density(AEI_S_OUM_locomotion_optima[,6])

pdf("AEI_S_OUM_locomotion_opt_hist.pdf",width=8,height=7) 
plot(range(AEI_S_OUM_locomotion_opt_sar$x, AEI_S_OUM_locomotion_opt_fos$x), range(AEI_S_OUM_locomotion_opt_arb$y, AEI_S_OUM_locomotion_opt_fos$y), type = "n", xlab = "AEI_S_optima",
     ylab = "Density", main = "aq=blue; car=red; fru=yellow; herb=gre; omni=org; ins=black")
polygon(AEI_S_OUM_locomotion_opt_aq, col = "red")
polygon(AEI_S_OUM_locomotion_opt_arb, col = "blue")
polygon(AEI_S_OUM_locomotion_opt_saq, col = "black")
polygon(AEI_S_OUM_locomotion_opt_sar, col = "yellow")
polygon(AEI_S_OUM_locomotion_opt_fos, col = "green")
polygon(AEI_S_OUM_locomotion_opt_ter, col = "orange")
dev.off()


#### Bootstrapping for AEI_S_OUM_locomotion model #####
simmap_locomotion_1000_sam500_10 <- sample(simmap_locomotion_1000_sam500[1:100],10)
AEI_S_OUM_locomotion_boot <- list(length=length(simmap_locomotion_1000_sam500_10))
for(i in 1:length(simmap_locomotion_1000_sam500_10)){
  AEI_S_OUM_locomotion_boot[[i]] <- OUwie.boot(simmap_locomotion_1000_sam500_10[[i]], AEI_S_locomotion, model="OUM",simmap.tree=TRUE, root.station=TRUE, nboot=100, mserr="none",
                                               alpha=c(mean(AEI_S_OUM_locomotion_alpha),
                                                       mean(AEI_S_OUM_locomotion_alpha),
                                                       mean(AEI_S_OUM_locomotion_alpha),
                                                       mean(AEI_S_OUM_locomotion_alpha),
                                                       mean(AEI_S_OUM_locomotion_alpha),
                                                       mean(AEI_S_OUM_locomotion_alpha)),
                                               sigma.sq=c(mean(AEI_S_OUM_locomotion_sigma),
                                                          mean(AEI_S_OUM_locomotion_sigma),
                                                          mean(AEI_S_OUM_locomotion_sigma),
                                                          mean(AEI_S_OUM_locomotion_sigma),
                                                          mean(AEI_S_OUM_locomotion_sigma),
                                                          mean(AEI_S_OUM_locomotion_sigma)),
                                               theta=c(mean(AEI_S_OUM_locomotion_optima[,1]), 
                                                       mean(AEI_S_OUM_locomotion_optima[,2]), 
                                                       mean(AEI_S_OUM_locomotion_optima[,3]), 
                                                       mean(AEI_S_OUM_locomotion_optima[,4]), 
                                                       mean(AEI_S_OUM_locomotion_optima[,5]), 
                                                       mean(AEI_S_OUM_locomotion_optima[,6])), 
                                               theta0=mean(AEI_S_OUM_locomotion_optima[,4]))
  print(i)
}
save(AEI_S_OUM_locomotion_boot, file="AEI_S_OUM_locomotion_boot.Rdata")



### AEI_S_OUM_locomotion optima ###
AEI_S_OUM_locomotion_boot_opt_aq <- list()
for(i in 1:length(AEI_S_OUM_locomotion_boot)){
  AEI_S_OUM_locomotion_boot_opt_aq[[i]] <- c(AEI_S_OUM_locomotion_boot[[i]][,13])}

AEI_S_OUM_locomotion_boot_opt_arb <- list()
for(i in 1:length(AEI_S_OUM_locomotion_boot)){
  AEI_S_OUM_locomotion_boot_opt_arb[[i]] <- c(AEI_S_OUM_locomotion_boot[[i]][,14])}

AEI_S_OUM_locomotion_boot_opt_saq <- list()
for(i in 1:length(AEI_S_OUM_locomotion_boot)){
  AEI_S_OUM_locomotion_boot_opt_saq[[i]] <- c(AEI_S_OUM_locomotion_boot[[i]][,15])}

AEI_S_OUM_locomotion_boot_opt_sar <- list()
for(i in 1:length(AEI_S_OUM_locomotion_boot)){
  AEI_S_OUM_locomotion_boot_opt_sar[[i]] <- c(AEI_S_OUM_locomotion_boot[[i]][,16])}

AEI_S_OUM_locomotion_boot_opt_fos <- list()
for(i in 1:length(AEI_S_OUM_locomotion_boot)){
  AEI_S_OUM_locomotion_boot_opt_fos[[i]] <- c(AEI_S_OUM_locomotion_boot[[i]][,17])}

AEI_S_OUM_locomotion_boot_opt_ter <- list()
for(i in 1:length(AEI_S_OUM_locomotion_boot)){
  AEI_S_OUM_locomotion_boot_opt_ter[[i]] <- c(AEI_S_OUM_locomotion_boot[[i]][,18])}

mean_AEI_S_OUM_locomotion_boot_opt <- rbind(
  mean(unlist(AEI_S_OUM_locomotion_boot_opt_aq)),
  mean(unlist(AEI_S_OUM_locomotion_boot_opt_arb)),
  mean(unlist(AEI_S_OUM_locomotion_boot_opt_saq)),
  mean(unlist(AEI_S_OUM_locomotion_boot_opt_sar)),
  mean(unlist(AEI_S_OUM_locomotion_boot_opt_fos)),
  mean(unlist(AEI_S_OUM_locomotion_boot_opt_ter)))

CI_AEI_S_OUM_locomotion_boot_opt <- rbind(
  quantile(unlist(AEI_S_OUM_locomotion_boot_opt_aq), probs=c(0.025, 0.975)),
  quantile(unlist(AEI_S_OUM_locomotion_boot_opt_arb), probs=c(0.025, 0.975)),
  quantile(unlist(AEI_S_OUM_locomotion_boot_opt_saq), probs=c(0.025, 0.975)),
  quantile(unlist(AEI_S_OUM_locomotion_boot_opt_sar), probs=c(0.025, 0.975)),
  quantile(unlist(AEI_S_OUM_locomotion_boot_opt_fos), probs=c(0.025, 0.975)),
  quantile(unlist(AEI_S_OUM_locomotion_boot_opt_ter), probs=c(0.025, 0.975)))

AEI_S_OUM_locomotion_boot_opt <- data.frame(mean_AEI_S_OUM_locomotion_boot_opt,CI_AEI_S_OUM_locomotion_boot_opt)
colnames(AEI_S_OUM_locomotion_boot_opt) <- c("OUM_optima_boot", "L95", "U95")
rownames(AEI_S_OUM_locomotion_boot_opt) <- c("aquatic", "arboreal", "semi_aquatic", "semi_arboreal", "semi_fossorial", "terrestrial")
AEI_S_OUM_locomotion_boot_opt
write.csv(AEI_S_OUM_locomotion_boot_opt, file="AEI_S_OUM_locomotion_boot_opt.csv")


col_locomotion <- c("red", "blue", "black", "yellow","green","orange")

pdf("AEI_S_OUM_locomotion_boot_line.pdf",width=5,height=7) 
plot(mean_AEI_S_OUM_locomotion_optima$OUM_optima, ylim = c(min(mean_AEI_S_OUM_locomotion_optima$OUM_optima-(AEI_S_OUM_locomotion_boot_opt$OUM_optima_boot-AEI_S_OUM_locomotion_boot_opt$L95)), max(AEI_S_OUM_locomotion_boot_opt$U95-AEI_S_OUM_locomotion_boot_opt$OUM_optima_boot+mean_AEI_S_OUM_locomotion_optima$OUM_optima)),  ylab = "AEI_S", xaxt = "n", las=1, cex.axis=2)
axis(1, at=c(1:6), labels = c("aquatic", "arboreal", "semi_aquatic", "semi_arboreal", "semi_fossorial", "terrestrial"), las=2, cex.axis=2)
plotCI(mean_AEI_S_OUM_locomotion_optima$OUM_optima,y=NULL, uiw=AEI_S_OUM_locomotion_boot_opt$U95-AEI_S_OUM_locomotion_boot_opt$OUM_optima_boot, liw=AEI_S_OUM_locomotion_boot_opt$OUM_optima_boot-AEI_S_OUM_locomotion_boot_opt$L95, err="y", pch=20, slty=3, col=col_locomotion, scol = col_locomotion, add=TRUE, cex=3, lwd=3)
dev.off()

AEI_S_OUM_locomotion_boot_opt_aq_den <- density(unlist(AEI_S_OUM_locomotion_boot_opt_aq))
AEI_S_OUM_locomotion_boot_opt_arb_den <- density(unlist(AEI_S_OUM_locomotion_boot_opt_arb))
AEI_S_OUM_locomotion_boot_opt_saq_den <- density(unlist(AEI_S_OUM_locomotion_boot_opt_saq))
AEI_S_OUM_locomotion_boot_opt_sar_den <- density(unlist(AEI_S_OUM_locomotion_boot_opt_sar))
AEI_S_OUM_locomotion_boot_opt_fos_den <- density(unlist(AEI_S_OUM_locomotion_boot_opt_fos))
AEI_S_OUM_locomotion_boot_opt_ter_den <- density(unlist(AEI_S_OUM_locomotion_boot_opt_ter))

pdf("AEI_S_OUM_locomotion_boot_hist.pdf",width=8,height=7) 
plot(range(AEI_S_OUM_locomotion_boot_opt_saq_den$x, AEI_S_OUM_locomotion_boot_opt_fos_den$x), range(AEI_S_OUM_locomotion_boot_opt_ter_den$y, AEI_S_OUM_locomotion_boot_opt_sar_den$y), type = "n", xlab = "AEI_S_optima", ylab = "Density", main = "am=red; aq=blue; occ=black; pou=yel; pur=gre; fos=ora")
polygon(AEI_S_OUM_locomotion_boot_opt_aq_den, col = "red")
polygon(AEI_S_OUM_locomotion_boot_opt_arb_den, col = "blue")
polygon(AEI_S_OUM_locomotion_boot_opt_saq_den, col = "black")
polygon(AEI_S_OUM_locomotion_boot_opt_sar_den, col = "yellow")
polygon(AEI_S_OUM_locomotion_boot_opt_fos_den, col = "green")
polygon(AEI_S_OUM_locomotion_boot_opt_ter_den, col = "orange")
dev.off()