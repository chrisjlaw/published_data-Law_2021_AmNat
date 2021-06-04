library(OUwie)
library(plotrix)

options(scipen = 999)

load("Rdata/ERecologydata_1225.RData")
load("Rdata/simmap/simmap_hunting_1000_sam500.Rdata")

#### Best Model - OUM_hunting ####
load("Rdata/OUwie_models/OUwie_500_sc/hbER_sc_OUM_hunting.Rdata")

lnhbER_sc_all <- phyl.resid(tree_prune, lnsize_all, lnhbER_all, method = "lambda")$resid
colnames(lnhbER_sc_all) <- "lnhbER_sc"
lnhbER_sc_all<-as.numeric(as.character(lnhbER_sc_all))
names(lnhbER_sc_all) <- rownames(data_mean_pr)

hbER_sc_diet <- data.frame(species_all, diet, lnhbER_sc_all)
hbER_sc_locomotion <- data.frame(species_all, locomotion, lnhbER_sc_all)
hbER_sc_hunting <- data.frame(species_all, hunting, lnhbER_sc_all)

# alpha constant
hbER_sc_OUM_hunting_alpha <- matrix(,length(simmap_hunting_1000_sam500),1,dimnames=list(c(1: length(simmap_hunting_1000_sam500)),c("hbER_sc_OUM_hunting_alpha")))
for(i in 1:length(simmap_hunting_1000_sam500)){
  hbER_sc_OUM_hunting_alpha[i,1] <- hbER_sc_OUM_hunting[[i]]$solution[1,1]
}
mean(hbER_sc_OUM_hunting_alpha)
log(2)/mean(hbER_sc_OUM_hunting_alpha)

# sigma constant
hbER_sc_OUM_hunting_sigma <- matrix(,length(simmap_hunting_1000_sam500),1,dimnames=list(c(1: length(simmap_hunting_1000_sam500)),c("hbER_sc_OUM_hunting_sigma")))
for(i in 1:length(simmap_hunting_1000_sam500)){
  hbER_sc_OUM_hunting_sigma[i,1] <- hbER_sc_OUM_hunting[[i]]$solution[2,1]
}
mean(hbER_sc_OUM_hunting_sigma)

# optima regimes 
hbER_sc_OUM_hunting_optima <- matrix(,length(simmap_hunting_1000_sam500),6, dimnames=list(c(1: length(simmap_hunting_1000_sam500)), c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial")))
for(i in 1:length(simmap_hunting_1000_sam500)){
  hbER_sc_OUM_hunting_optima[i,1] <- hbER_sc_OUM_hunting[[i]]$theta[1,1]
  hbER_sc_OUM_hunting_optima[i,2] <- hbER_sc_OUM_hunting[[i]]$theta[2,1]
  hbER_sc_OUM_hunting_optima[i,3] <- hbER_sc_OUM_hunting[[i]]$theta[3,1]
  hbER_sc_OUM_hunting_optima[i,4] <- hbER_sc_OUM_hunting[[i]]$theta[4,1]
  hbER_sc_OUM_hunting_optima[i,5] <- hbER_sc_OUM_hunting[[i]]$theta[5,1]
  hbER_sc_OUM_hunting_optima[i,6] <- hbER_sc_OUM_hunting[[i]]$theta[6,1]
}
hbER_sc_OUM_hunting_optima

mean_hbER_sc_OUM_hunting_optima <- rbind(
  mean(hbER_sc_OUM_hunting_optima[,1]),
  mean(hbER_sc_OUM_hunting_optima[,2]),
  mean(hbER_sc_OUM_hunting_optima[,3]),
  mean(hbER_sc_OUM_hunting_optima[,4]),
  mean(hbER_sc_OUM_hunting_optima[,5]),
  mean(hbER_sc_OUM_hunting_optima[,6]))

CI_hbER_sc_OUM_hunting_optima <- rbind(
  quantile(hbER_sc_OUM_hunting_optima[,1], probs=c(0.025, 0.975)),
  quantile(hbER_sc_OUM_hunting_optima[,2], probs=c(0.025, 0.975)),
  quantile(hbER_sc_OUM_hunting_optima[,3], probs=c(0.025, 0.975)),
  quantile(hbER_sc_OUM_hunting_optima[,4], probs=c(0.025, 0.975)),
  quantile(hbER_sc_OUM_hunting_optima[,5], probs=c(0.025, 0.975)),
  quantile(hbER_sc_OUM_hunting_optima[,6], probs=c(0.025, 0.975)))

rownames(mean_hbER_sc_OUM_hunting_optima) <- c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial")

mean_hbER_sc_OUM_hunting_optima <- data.frame(mean_hbER_sc_OUM_hunting_optima,CI_hbER_sc_OUM_hunting_optima)
colnames(mean_hbER_sc_OUM_hunting_optima) <- c("OUM_optima", "L95", "U95")
mean_hbER_sc_OUM_hunting_optima
write.csv(mean_hbER_sc_OUM_hunting_optima, file="mean_hbER_sc_OUM_hunting_optima.csv")

col_hunting <- c("red", "blue", "black", "yellow","green","orange")

pdf("hbER_sc_OUM_hunting_opt_line.pdf",width=5,height=7) 
plot(mean_hbER_sc_OUM_hunting_optima$OUM_optima, ylim = c(min(mean_hbER_sc_OUM_hunting_optima), max(mean_hbER_sc_OUM_hunting_optima)),  ylab = "hbER_sc",  xaxt = "n", las=1, cex.axis=2)
axis(1, at=c(1:6), labels = c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial"), las=2, cex.axis=2)
plotCI(mean_hbER_sc_OUM_hunting_optima$OUM_optima,y=NULL, uiw=mean_hbER_sc_OUM_hunting_optima$U95-mean_hbER_sc_OUM_hunting_optima$OUM_optima, liw=mean_hbER_sc_OUM_hunting_optima$OUM_optima-mean_hbER_sc_OUM_hunting_optima$L95, err="y", pch=20, slty=3, col=col_hunting, scol = col_hunting, add=TRUE, cex=3, lwd=3)
dev.off()

hbER_sc_OUM_hunting_opt_am <- density(hbER_sc_OUM_hunting_optima[,1])
hbER_sc_OUM_hunting_opt_aq <- density(hbER_sc_OUM_hunting_optima[,2])
hbER_sc_OUM_hunting_opt_occ <- density(hbER_sc_OUM_hunting_optima[,3])
hbER_sc_OUM_hunting_opt_pou <- density(hbER_sc_OUM_hunting_optima[,4])
hbER_sc_OUM_hunting_opt_pur <- density(hbER_sc_OUM_hunting_optima[,5])
hbER_sc_OUM_hunting_opt_fos <- density(hbER_sc_OUM_hunting_optima[,6])

pdf("hbER_sc_OUM_hunting_opt_hist.pdf",width=8,height=7) 
plot(range(hbER_sc_OUM_hunting_opt_pou$x, hbER_sc_OUM_hunting_opt_aq$x), range(hbER_sc_OUM_hunting_opt_aq$y, hbER_sc_OUM_hunting_opt_pur$y), type = "n", xlab = "hbER_sc_optima",
     ylab = "Density", main = "am=red; aq=blue; occ=black; pou=yel; pur=gre; fos=ora")
polygon(hbER_sc_OUM_hunting_opt_am, col = "red")
polygon(hbER_sc_OUM_hunting_opt_aq, col = "blue")
polygon(hbER_sc_OUM_hunting_opt_occ, col = "black")
polygon(hbER_sc_OUM_hunting_opt_pou, col = "yellow")
polygon(hbER_sc_OUM_hunting_opt_pur, col = "green")
polygon(hbER_sc_OUM_hunting_opt_fos, col = "orange")
dev.off()


#### Bootstrapping for hbER_sc_OUM_hunting model #####
simmap_hunting_1000_sam500_10 <- sample(simmap_hunting_1000_sam500[1:100],10)
hbER_sc_OUM_hunting_boot <- list(length=length(simmap_hunting_1000_sam500_10))
for(i in 1:length(simmap_hunting_1000_sam500_10)){
  hbER_sc_OUM_hunting_boot[[i]] <- OUwie.boot(simmap_hunting_1000_sam500_10[[i]], hbER_sc_hunting, model="OUM",simmap.tree=TRUE, root.station=TRUE, nboot=100, mserr="none",
                                           alpha=c(mean(hbER_sc_OUM_hunting_alpha),
                                                   mean(hbER_sc_OUM_hunting_alpha),
                                                   mean(hbER_sc_OUM_hunting_alpha),
                                                   mean(hbER_sc_OUM_hunting_alpha),
                                                   mean(hbER_sc_OUM_hunting_alpha),
                                                   mean(hbER_sc_OUM_hunting_alpha)),
                                           sigma.sq=c(mean(hbER_sc_OUM_hunting_sigma),
                                                      mean(hbER_sc_OUM_hunting_sigma),
                                                      mean(hbER_sc_OUM_hunting_sigma),
                                                      mean(hbER_sc_OUM_hunting_sigma),
                                                      mean(hbER_sc_OUM_hunting_sigma),
                                                      mean(hbER_sc_OUM_hunting_sigma)),
                                           theta=c(mean(hbER_sc_OUM_hunting_optima[,1]), 
                                                   mean(hbER_sc_OUM_hunting_optima[,2]), 
                                                   mean(hbER_sc_OUM_hunting_optima[,3]), 
                                                   mean(hbER_sc_OUM_hunting_optima[,4]), 
                                                   mean(hbER_sc_OUM_hunting_optima[,5]), 
                                                   mean(hbER_sc_OUM_hunting_optima[,6])), 
                                           theta0=mean(hbER_sc_OUM_hunting_optima[,4]))
  print(i)
}
save(hbER_sc_OUM_hunting_boot, file="hbER_sc_OUM_hunting_boot.Rdata")


### hbER_sc_OUM_hunting optima ###
hbER_sc_OUM_hunting_boot_opt_am <- list()
for(i in 1:length(hbER_sc_OUM_hunting_boot)){
  hbER_sc_OUM_hunting_boot_opt_am[[i]] <- c(hbER_sc_OUM_hunting_boot[[i]][,13])}

hbER_sc_OUM_hunting_boot_opt_aq <- list()
for(i in 1:length(hbER_sc_OUM_hunting_boot)){
  hbER_sc_OUM_hunting_boot_opt_aq[[i]] <- c(hbER_sc_OUM_hunting_boot[[i]][,14])}

hbER_sc_OUM_hunting_boot_opt_occ <- list()
for(i in 1:length(hbER_sc_OUM_hunting_boot)){
  hbER_sc_OUM_hunting_boot_opt_occ[[i]] <- c(hbER_sc_OUM_hunting_boot[[i]][,15])}

hbER_sc_OUM_hunting_boot_opt_pou <- list()
for(i in 1:length(hbER_sc_OUM_hunting_boot)){
  hbER_sc_OUM_hunting_boot_opt_pou[[i]] <- c(hbER_sc_OUM_hunting_boot[[i]][,16])}

hbER_sc_OUM_hunting_boot_opt_pur <- list()
for(i in 1:length(hbER_sc_OUM_hunting_boot)){
  hbER_sc_OUM_hunting_boot_opt_pur[[i]] <- c(hbER_sc_OUM_hunting_boot[[i]][,17])}

hbER_sc_OUM_hunting_boot_opt_fos <- list()
for(i in 1:length(hbER_sc_OUM_hunting_boot)){
  hbER_sc_OUM_hunting_boot_opt_fos[[i]] <- c(hbER_sc_OUM_hunting_boot[[i]][,18])}

mean_hbER_sc_OUM_hunting_boot_opt <- rbind(
  mean(unlist(hbER_sc_OUM_hunting_boot_opt_am)),
  mean(unlist(hbER_sc_OUM_hunting_boot_opt_aq)),
  mean(unlist(hbER_sc_OUM_hunting_boot_opt_occ)),
  mean(unlist(hbER_sc_OUM_hunting_boot_opt_pou)),
  mean(unlist(hbER_sc_OUM_hunting_boot_opt_pur)),
  mean(unlist(hbER_sc_OUM_hunting_boot_opt_fos)))

CI_hbER_sc_OUM_hunting_boot_opt <- rbind(
  quantile(unlist(hbER_sc_OUM_hunting_boot_opt_am), probs=c(0.025, 0.975)),
  quantile(unlist(hbER_sc_OUM_hunting_boot_opt_aq), probs=c(0.025, 0.975)),
  quantile(unlist(hbER_sc_OUM_hunting_boot_opt_occ), probs=c(0.025, 0.975)),
  quantile(unlist(hbER_sc_OUM_hunting_boot_opt_pou), probs=c(0.025, 0.975)),
  quantile(unlist(hbER_sc_OUM_hunting_boot_opt_pur), probs=c(0.025, 0.975)),
  quantile(unlist(hbER_sc_OUM_hunting_boot_opt_fos), probs=c(0.025, 0.975)))

hbER_sc_OUM_hunting_boot_opt <- data.frame(mean_hbER_sc_OUM_hunting_boot_opt,CI_hbER_sc_OUM_hunting_boot_opt)
colnames(hbER_sc_OUM_hunting_boot_opt) <- c("OUM_optima_boot", "L95", "U95")
rownames(hbER_sc_OUM_hunting_boot_opt) <- c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial")
hbER_sc_OUM_hunting_boot_opt
write.csv(hbER_sc_OUM_hunting_boot_opt, file="hbER_sc_OUM_hunting_boot_opt.csv")


col_hunting <- c("red", "blue", "black", "yellow","green","orange")

pdf("hbER_sc_OUM_hunting_boot_line.pdf",width=5,height=7) 
plot(mean_hbER_sc_OUM_hunting_optima$OUM_optima, ylim = c(-1.8, 0.11),  ylab = "hbER_sc", xaxt = "n", las=1, cex.axis=2)
axis(1, at=c(1:6), labels = c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial"), las=2, cex.axis=2)
plotCI(mean_hbER_sc_OUM_hunting_optima$OUM_optima,y=NULL, uiw=hbER_sc_OUM_hunting_boot_opt$U95-hbER_sc_OUM_hunting_boot_opt$OUM_optima_boot, liw=hbER_sc_OUM_hunting_boot_opt$OUM_optima_boot-hbER_sc_OUM_hunting_boot_opt$L95, err="y", pch=20, slty=3, col=col_hunting, scol = col_hunting, add=TRUE, cex=3, lwd=3)
dev.off()

hbER_sc_OUM_hunting_boot_opt_am_den <- density(unlist(hbER_sc_OUM_hunting_boot_opt_am))
hbER_sc_OUM_hunting_boot_opt_aq_den <- density(unlist(hbER_sc_OUM_hunting_boot_opt_aq))
hbER_sc_OUM_hunting_boot_opt_occ_den <- density(unlist(hbER_sc_OUM_hunting_boot_opt_occ))
hbER_sc_OUM_hunting_boot_opt_pou_den <- density(unlist(hbER_sc_OUM_hunting_boot_opt_pou))
hbER_sc_OUM_hunting_boot_opt_pur_den <- density(unlist(hbER_sc_OUM_hunting_boot_opt_pur))
hbER_sc_OUM_hunting_boot_opt_fos_den <- density(unlist(hbER_sc_OUM_hunting_boot_opt_fos))

pdf("hbER_sc_OUM_hunting_boot_hist.pdf",width=8,height=7) 
plot(range(hbER_sc_OUM_hunting_boot_opt_occ_den$x, hbER_sc_OUM_hunting_boot_opt_pur_den$x), range(hbER_sc_OUM_hunting_boot_opt_fos_den$y, hbER_sc_OUM_hunting_boot_opt_pou_den$y), type = "n", xlab = "hbER_sc_optima", ylab = "Density", main = "am=red; aq=blue; occ=black; pou=yel; pur=gre; fos=ora")
polygon(hbER_sc_OUM_hunting_boot_opt_am_den, col = "red")
polygon(hbER_sc_OUM_hunting_boot_opt_aq_den, col = "blue")
polygon(hbER_sc_OUM_hunting_boot_opt_occ_den, col = "black")
polygon(hbER_sc_OUM_hunting_boot_opt_pou_den, col = "yellow")
polygon(hbER_sc_OUM_hunting_boot_opt_pur_den, col = "green")
polygon(hbER_sc_OUM_hunting_boot_opt_fos_den, col = "orange")
dev.off()