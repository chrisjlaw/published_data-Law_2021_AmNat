library(OUwie)
library(plotrix)

options(scipen = 999)

load("Rdata/ERecologydata_0504.RData")
load("Rdata/simmap/simmap_hunting_1000_sam500.Rdata")

#### Best Model - OUM_hunting ####
load("Rdata/OUwie_models/headER_OUM_hunting.Rdata")

# alpha constant
headER_OUM_hunting_alpha <- matrix(,length(simmap_hunting_1000_sam500),1,dimnames=list(c(1: length(simmap_hunting_1000_sam500)),c("headER_OUM_hunting_alpha")))
for(i in 1:length(simmap_hunting_1000_sam500)){
  headER_OUM_hunting_alpha[i,1] <- headER_OUM_hunting[[i]]$solution[1,1]
}
mean(headER_OUM_hunting_alpha)
log(2)/mean(headER_OUM_hunting_alpha)

# sigma constant
headER_OUM_hunting_sigma <- matrix(,length(simmap_hunting_1000_sam500),1,dimnames=list(c(1: length(simmap_hunting_1000_sam500)),c("headER_OUM_hunting_sigma")))
for(i in 1:length(simmap_hunting_1000_sam500)){
  headER_OUM_hunting_sigma[i,1] <- headER_OUM_hunting[[i]]$solution[2,1]
}
mean(headER_OUM_hunting_sigma)

# optima regimes 
headER_OUM_hunting_optima <- matrix(,length(simmap_hunting_1000_sam500),6, dimnames=list(c(1: length(simmap_hunting_1000_sam500)), c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial")))
for(i in 1:length(simmap_hunting_1000_sam500)){
  headER_OUM_hunting_optima[i,1] <- headER_OUM_hunting[[i]]$theta[1,1]
  headER_OUM_hunting_optima[i,2] <- headER_OUM_hunting[[i]]$theta[2,1]
  headER_OUM_hunting_optima[i,3] <- headER_OUM_hunting[[i]]$theta[3,1]
  headER_OUM_hunting_optima[i,4] <- headER_OUM_hunting[[i]]$theta[4,1]
  headER_OUM_hunting_optima[i,5] <- headER_OUM_hunting[[i]]$theta[5,1]
  headER_OUM_hunting_optima[i,6] <- headER_OUM_hunting[[i]]$theta[6,1]
}
headER_OUM_hunting_optima

mean_headER_OUM_hunting_optima <- rbind(
  mean(headER_OUM_hunting_optima[,1]),
  mean(headER_OUM_hunting_optima[,2]),
  mean(headER_OUM_hunting_optima[,3]),
  mean(headER_OUM_hunting_optima[,4]),
  mean(headER_OUM_hunting_optima[,5]),
  mean(headER_OUM_hunting_optima[,6]))

CI_headER_OUM_hunting_optima <- rbind(
  quantile(headER_OUM_hunting_optima[,1], probs=c(0.025, 0.975)),
  quantile(headER_OUM_hunting_optima[,2], probs=c(0.025, 0.975)),
  quantile(headER_OUM_hunting_optima[,3], probs=c(0.025, 0.975)),
  quantile(headER_OUM_hunting_optima[,4], probs=c(0.025, 0.975)),
  quantile(headER_OUM_hunting_optima[,5], probs=c(0.025, 0.975)),
  quantile(headER_OUM_hunting_optima[,6], probs=c(0.025, 0.975)))

rownames(mean_headER_OUM_hunting_optima) <- c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial")

mean_headER_OUM_hunting_optima <- data.frame(mean_headER_OUM_hunting_optima,CI_headER_OUM_hunting_optima)
colnames(mean_headER_OUM_hunting_optima) <- c("OUM_optima", "L95", "U95")
mean_headER_OUM_hunting_optima
write.csv(mean_headER_OUM_hunting_optima, file="mean_headER_OUM_hunting_optima.csv")

col_hunting <- c("red", "blue", "black", "yellow","green","orange")

pdf("headER_OUM_hunting_opt_line.pdf",width=5,height=7) 
plot(mean_headER_OUM_hunting_optima$OUM_optima, ylim = c(min(mean_headER_OUM_hunting_optima), max(mean_headER_OUM_hunting_optima)),  ylab = "headER",  xaxt = "n", las=1, cex.axis=2)
axis(1, at=c(1:6), labels = c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial"), las=2, cex.axis=2)
plotCI(mean_headER_OUM_hunting_optima$OUM_optima,y=NULL, uiw=mean_headER_OUM_hunting_optima$U95-mean_headER_OUM_hunting_optima$OUM_optima, liw=mean_headER_OUM_hunting_optima$OUM_optima-mean_headER_OUM_hunting_optima$L95, err="y", pch=20, slty=3, col=col_hunting, scol = col_hunting, add=TRUE, cex=3, lwd=3)
dev.off()

headER_OUM_hunting_opt_am <- density(headER_OUM_hunting_optima[,1])
headER_OUM_hunting_opt_aq <- density(headER_OUM_hunting_optima[,2])
headER_OUM_hunting_opt_occ <- density(headER_OUM_hunting_optima[,3])
headER_OUM_hunting_opt_pou <- density(headER_OUM_hunting_optima[,4])
headER_OUM_hunting_opt_pur <- density(headER_OUM_hunting_optima[,5])
headER_OUM_hunting_opt_fos <- density(headER_OUM_hunting_optima[,6])

pdf("headER_OUM_hunting_opt_hist.pdf",width=8,height=7) 
plot(range(headER_OUM_hunting_opt_pou$x, headER_OUM_hunting_opt_pur$x), range(headER_OUM_hunting_opt_aq$y, headER_OUM_hunting_opt_pur$y), type = "n", xlab = "headER_optima",
     ylab = "Density", main = "aq=blue; car=red; fru=yellow; herb=gre; omni=org; ins=black")
polygon(headER_OUM_hunting_opt_am, col = "red")
polygon(headER_OUM_hunting_opt_aq, col = "blue")
polygon(headER_OUM_hunting_opt_occ, col = "black")
polygon(headER_OUM_hunting_opt_pou, col = "yellow")
polygon(headER_OUM_hunting_opt_pur, col = "green")
polygon(headER_OUM_hunting_opt_fos, col = "orange")
dev.off()


#### Bootstrapping for headER_OUM_hunting model #####
simmap_hunting_1000_sam500_10 <- sample(simmap_hunting_1000_sam500[1:100],10)
headER_OUM_hunting_boot <- list(length=length(simmap_hunting_1000_sam500_10))
for(i in 1:length(simmap_hunting_1000_sam500_10)){
  headER_OUM_hunting_boot[[i]] <- OUwie.boot(simmap_hunting_1000_sam500_10[[i]], headER_hunting, model="OUM",simmap.tree=TRUE, root.station=TRUE, nboot=100, mserr="none",
                                             alpha=c(mean(headER_OUM_hunting_alpha),
                                                     mean(headER_OUM_hunting_alpha),
                                                     mean(headER_OUM_hunting_alpha),
                                                     mean(headER_OUM_hunting_alpha),
                                                     mean(headER_OUM_hunting_alpha),
                                                     mean(headER_OUM_hunting_alpha)),
                                             sigma.sq=c(mean(headER_OUM_hunting_sigma),
                                                        mean(headER_OUM_hunting_sigma),
                                                        mean(headER_OUM_hunting_sigma),
                                                        mean(headER_OUM_hunting_sigma),
                                                        mean(headER_OUM_hunting_sigma),
                                                        mean(headER_OUM_hunting_sigma)),
                                             theta=c(mean(headER_OUM_hunting_optima[,1]), 
                                                     mean(headER_OUM_hunting_optima[,2]), 
                                                     mean(headER_OUM_hunting_optima[,3]), 
                                                     mean(headER_OUM_hunting_optima[,4]), 
                                                     mean(headER_OUM_hunting_optima[,5]), 
                                                     mean(headER_OUM_hunting_optima[,6])), 
                                             theta0=mean(headER_OUM_hunting_optima[,4]))
  print(i)
}
save(headER_OUM_hunting_boot, file="headER_OUM_hunting_boot.Rdata")



### headER_OUM_hunting optima ###
headER_OUM_hunting_boot_opt_am <- list()
for(i in 1:length(headER_OUM_hunting_boot)){
  headER_OUM_hunting_boot_opt_am[[i]] <- c(headER_OUM_hunting_boot[[i]][,13])}

headER_OUM_hunting_boot_opt_aq <- list()
for(i in 1:length(headER_OUM_hunting_boot)){
  headER_OUM_hunting_boot_opt_aq[[i]] <- c(headER_OUM_hunting_boot[[i]][,14])}

headER_OUM_hunting_boot_opt_occ <- list()
for(i in 1:length(headER_OUM_hunting_boot)){
  headER_OUM_hunting_boot_opt_occ[[i]] <- c(headER_OUM_hunting_boot[[i]][,15])}

headER_OUM_hunting_boot_opt_pou <- list()
for(i in 1:length(headER_OUM_hunting_boot)){
  headER_OUM_hunting_boot_opt_pou[[i]] <- c(headER_OUM_hunting_boot[[i]][,16])}

headER_OUM_hunting_boot_opt_pur <- list()
for(i in 1:length(headER_OUM_hunting_boot)){
  headER_OUM_hunting_boot_opt_pur[[i]] <- c(headER_OUM_hunting_boot[[i]][,17])}

headER_OUM_hunting_boot_opt_fos <- list()
for(i in 1:length(headER_OUM_hunting_boot)){
  headER_OUM_hunting_boot_opt_fos[[i]] <- c(headER_OUM_hunting_boot[[i]][,18])}

mean_headER_OUM_hunting_boot_opt <- rbind(
  mean(unlist(headER_OUM_hunting_boot_opt_am)),
  mean(unlist(headER_OUM_hunting_boot_opt_aq)),
  mean(unlist(headER_OUM_hunting_boot_opt_occ)),
  mean(unlist(headER_OUM_hunting_boot_opt_pou)),
  mean(unlist(headER_OUM_hunting_boot_opt_pur)),
  mean(unlist(headER_OUM_hunting_boot_opt_fos)))

CI_headER_OUM_hunting_boot_opt <- rbind(
  quantile(unlist(headER_OUM_hunting_boot_opt_am), probs=c(0.025, 0.975)),
  quantile(unlist(headER_OUM_hunting_boot_opt_aq), probs=c(0.025, 0.975)),
  quantile(unlist(headER_OUM_hunting_boot_opt_occ), probs=c(0.025, 0.975)),
  quantile(unlist(headER_OUM_hunting_boot_opt_pou), probs=c(0.025, 0.975)),
  quantile(unlist(headER_OUM_hunting_boot_opt_pur), probs=c(0.025, 0.975)),
  quantile(unlist(headER_OUM_hunting_boot_opt_fos), probs=c(0.025, 0.975)))

headER_OUM_hunting_boot_opt <- data.frame(mean_headER_OUM_hunting_boot_opt,CI_headER_OUM_hunting_boot_opt)
colnames(headER_OUM_hunting_boot_opt) <- c("OUM_optima_boot", "L95", "U95")
rownames(headER_OUM_hunting_boot_opt) <- c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial")
headER_OUM_hunting_boot_opt
write.csv(headER_OUM_hunting_boot_opt, file="headER_OUM_hunting_boot_opt.csv")


col_hunting <- c("red", "blue", "black", "yellow","green","orange")

pdf("headER_OUM_hunting_boot_line.pdf",width=5,height=7) 
plot(mean_headER_OUM_hunting_optima$OUM_optima, ylim = c(min(mean_headER_OUM_hunting_optima$OUM_optima-(headER_OUM_hunting_boot_opt$OUM_optima_boot-headER_OUM_hunting_boot_opt$L95)), max(headER_OUM_hunting_boot_opt$U95-headER_OUM_hunting_boot_opt$OUM_optima_boot+mean_headER_OUM_hunting_optima$OUM_optima)),  ylab = "headER", xaxt = "n", las=1, cex.axis=2)
axis(1, at=c(1:6), labels = c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial"), las=2, cex.axis=2)
plotCI(mean_headER_OUM_hunting_optima$OUM_optima,y=NULL, uiw=headER_OUM_hunting_boot_opt$U95-headER_OUM_hunting_boot_opt$OUM_optima_boot, liw=headER_OUM_hunting_boot_opt$OUM_optima_boot-headER_OUM_hunting_boot_opt$L95, err="y", pch=20, slty=3, col=col_hunting, scol = col_hunting, add=TRUE, cex=3, lwd=3)
dev.off()

headER_OUM_hunting_boot_opt_am_den <- density(unlist(headER_OUM_hunting_boot_opt_am))
headER_OUM_hunting_boot_opt_aq_den <- density(unlist(headER_OUM_hunting_boot_opt_aq))
headER_OUM_hunting_boot_opt_occ_den <- density(unlist(headER_OUM_hunting_boot_opt_occ))
headER_OUM_hunting_boot_opt_pou_den <- density(unlist(headER_OUM_hunting_boot_opt_pou))
headER_OUM_hunting_boot_opt_pur_den <- density(unlist(headER_OUM_hunting_boot_opt_pur))
headER_OUM_hunting_boot_opt_fos_den <- density(unlist(headER_OUM_hunting_boot_opt_fos))

pdf("headER_OUM_hunting_boot_hist.pdf",width=8,height=7) 
plot(range(headER_OUM_hunting_boot_opt_occ_den$x, headER_OUM_hunting_boot_opt_pur_den$x), range(headER_OUM_hunting_boot_opt_fos_den$y, headER_OUM_hunting_boot_opt_pou_den$y), type = "n", xlab = "headER_optima", ylab = "Density", main = "am=red; aq=blue; occ=black; pou=yel; pur=gre; fos=ora")
polygon(headER_OUM_hunting_boot_opt_am_den, col = "red")
polygon(headER_OUM_hunting_boot_opt_aq_den, col = "blue")
polygon(headER_OUM_hunting_boot_opt_occ_den, col = "black")
polygon(headER_OUM_hunting_boot_opt_pou_den, col = "yellow")
polygon(headER_OUM_hunting_boot_opt_pur_den, col = "green")
polygon(headER_OUM_hunting_boot_opt_fos_den, col = "orange")
dev.off()
