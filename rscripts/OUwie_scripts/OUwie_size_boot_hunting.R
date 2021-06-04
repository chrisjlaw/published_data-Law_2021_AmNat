library(OUwie)
library(plotrix)

options(scipen = 999)

load("Rdata/ERecologydata_0914.RData")
load("Rdata/simmap/simmap_hunting_1000_sam500.Rdata")

#### Best Model - OUM_hunting ####
load("Rdata/OUwie_models/size_OUM_hunting.Rdata")

# alpha constant
size_OUM_hunting_alpha <- matrix(,length(simmap_hunting_1000_sam500),1,dimnames=list(c(1: length(simmap_hunting_1000_sam500)),c("size_OUM_hunting_alpha")))
for(i in 1:length(simmap_hunting_1000_sam500)){
  size_OUM_hunting_alpha[i,1] <- size_OUM_hunting[[i]]$solution[1,1]
}
mean(size_OUM_hunting_alpha)
log(2)/mean(size_OUM_hunting_alpha)

# sigma constant
size_OUM_hunting_sigma <- matrix(,length(simmap_hunting_1000_sam500),1,dimnames=list(c(1: length(simmap_hunting_1000_sam500)),c("size_OUM_hunting_sigma")))
for(i in 1:length(simmap_hunting_1000_sam500)){
  size_OUM_hunting_sigma[i,1] <- size_OUM_hunting[[i]]$solution[2,1]
}
mean(size_OUM_hunting_sigma)

# optima regimes 
size_OUM_hunting_optima <- matrix(,length(simmap_hunting_1000_sam500),6, dimnames=list(c(1: length(simmap_hunting_1000_sam500)), c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial")))
for(i in 1:length(simmap_hunting_1000_sam500)){
  size_OUM_hunting_optima[i,1] <- size_OUM_hunting[[i]]$theta[1,1]
  size_OUM_hunting_optima[i,2] <- size_OUM_hunting[[i]]$theta[2,1]
  size_OUM_hunting_optima[i,3] <- size_OUM_hunting[[i]]$theta[3,1]
  size_OUM_hunting_optima[i,4] <- size_OUM_hunting[[i]]$theta[4,1]
  size_OUM_hunting_optima[i,5] <- size_OUM_hunting[[i]]$theta[5,1]
  size_OUM_hunting_optima[i,6] <- size_OUM_hunting[[i]]$theta[6,1]
}
size_OUM_hunting_optima

mean_size_OUM_hunting_optima <- rbind(
  mean(size_OUM_hunting_optima[,1]),
  mean(size_OUM_hunting_optima[,2]),
  mean(size_OUM_hunting_optima[,3]),
  mean(size_OUM_hunting_optima[,4]),
  mean(size_OUM_hunting_optima[,5]),
  mean(size_OUM_hunting_optima[,6]))

CI_size_OUM_hunting_optima <- rbind(
  quantile(size_OUM_hunting_optima[,1], probs=c(0.025, 0.975)),
  quantile(size_OUM_hunting_optima[,2], probs=c(0.025, 0.975)),
  quantile(size_OUM_hunting_optima[,3], probs=c(0.025, 0.975)),
  quantile(size_OUM_hunting_optima[,4], probs=c(0.025, 0.975)),
  quantile(size_OUM_hunting_optima[,5], probs=c(0.025, 0.975)),
  quantile(size_OUM_hunting_optima[,6], probs=c(0.025, 0.975)))

rownames(mean_size_OUM_hunting_optima) <- c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial")

mean_size_OUM_hunting_optima <- data.frame(mean_size_OUM_hunting_optima,CI_size_OUM_hunting_optima)
colnames(mean_size_OUM_hunting_optima) <- c("OUM_optima", "L95", "U95")
mean_size_OUM_hunting_optima
write.csv(mean_size_OUM_hunting_optima, file="mean_size_OUM_hunting_optima.csv")

col_hunting <- c("red", "blue", "black", "yellow","green","orange")

pdf("size_OUM_hunting_opt_line.pdf",width=5,height=7) 
plot(mean_size_OUM_hunting_optima$OUM_optima, ylim = c(min(mean_size_OUM_hunting_optima), max(mean_size_OUM_hunting_optima)),  ylab = "size",  xaxt = "n", las=1, cex.axis=2)
axis(1, at=c(1:6), labels = c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial"), las=2, cex.axis=2)
plotCI(mean_size_OUM_hunting_optima$OUM_optima,y=NULL, uiw=mean_size_OUM_hunting_optima$U95-mean_size_OUM_hunting_optima$OUM_optima, liw=mean_size_OUM_hunting_optima$OUM_optima-mean_size_OUM_hunting_optima$L95, err="y", pch=20, slty=3, col=col_hunting, scol = col_hunting, add=TRUE, cex=3, lwd=3)
dev.off()


#### Bootstrapping for size_OUM_hunting model #####
simmap_hunting_1000_sam500_10 <- sample(simmap_hunting_1000_sam500[1:100],10)
size_OUM_hunting_boot <- list(length=length(simmap_hunting_1000_sam500_10))
for(i in 1:length(simmap_hunting_1000_sam500_10)){
  size_OUM_hunting_boot[[i]] <- OUwie.boot(simmap_hunting_1000_sam500_10[[i]], size_hunting, model="OUM",simmap.tree=TRUE, root.station=TRUE, nboot=100, mserr="none",
                                           alpha=c(mean(size_OUM_hunting_alpha),
                                                   mean(size_OUM_hunting_alpha),
                                                   mean(size_OUM_hunting_alpha),
                                                   mean(size_OUM_hunting_alpha),
                                                   mean(size_OUM_hunting_alpha),
                                                   mean(size_OUM_hunting_alpha)),
                                           sigma.sq=c(mean(size_OUM_hunting_sigma),
                                                      mean(size_OUM_hunting_sigma),
                                                      mean(size_OUM_hunting_sigma),
                                                      mean(size_OUM_hunting_sigma),
                                                      mean(size_OUM_hunting_sigma),
                                                      mean(size_OUM_hunting_sigma)),
                                           theta=c(mean(size_OUM_hunting_optima[,1]), 
                                                   mean(size_OUM_hunting_optima[,2]), 
                                                   mean(size_OUM_hunting_optima[,3]), 
                                                   mean(size_OUM_hunting_optima[,4]), 
                                                   mean(size_OUM_hunting_optima[,5]), 
                                                   mean(size_OUM_hunting_optima[,6])), 
                                           theta0=mean(size_OUM_hunting_optima[,4]))
  print(i)
}
save(size_OUM_hunting_boot, file="size_OUM_hunting_boot.Rdata")



### size_OUM_hunting optima ###
size_OUM_hunting_boot_opt_am <- list()
for(i in 1:length(size_OUM_hunting_boot)){
  size_OUM_hunting_boot_opt_am[[i]] <- c(size_OUM_hunting_boot[[i]][,13])}

size_OUM_hunting_boot_opt_aq <- list()
for(i in 1:length(size_OUM_hunting_boot)){
  size_OUM_hunting_boot_opt_aq[[i]] <- c(size_OUM_hunting_boot[[i]][,14])}

size_OUM_hunting_boot_opt_occ <- list()
for(i in 1:length(size_OUM_hunting_boot)){
  size_OUM_hunting_boot_opt_occ[[i]] <- c(size_OUM_hunting_boot[[i]][,15])}

size_OUM_hunting_boot_opt_pou <- list()
for(i in 1:length(size_OUM_hunting_boot)){
  size_OUM_hunting_boot_opt_pou[[i]] <- c(size_OUM_hunting_boot[[i]][,16])}

size_OUM_hunting_boot_opt_pur <- list()
for(i in 1:length(size_OUM_hunting_boot)){
  size_OUM_hunting_boot_opt_pur[[i]] <- c(size_OUM_hunting_boot[[i]][,17])}

size_OUM_hunting_boot_opt_fos <- list()
for(i in 1:length(size_OUM_hunting_boot)){
  size_OUM_hunting_boot_opt_fos[[i]] <- c(size_OUM_hunting_boot[[i]][,18])}

mean_size_OUM_hunting_boot_opt <- rbind(
  mean(unlist(size_OUM_hunting_boot_opt_am)),
  mean(unlist(size_OUM_hunting_boot_opt_aq)),
  mean(unlist(size_OUM_hunting_boot_opt_occ)),
  mean(unlist(size_OUM_hunting_boot_opt_pou)),
  mean(unlist(size_OUM_hunting_boot_opt_pur)),
  mean(unlist(size_OUM_hunting_boot_opt_fos)))

CI_size_OUM_hunting_boot_opt <- rbind(
  quantile(unlist(size_OUM_hunting_boot_opt_am), probs=c(0.025, 0.975)),
  quantile(unlist(size_OUM_hunting_boot_opt_aq), probs=c(0.025, 0.975)),
  quantile(unlist(size_OUM_hunting_boot_opt_occ), probs=c(0.025, 0.975)),
  quantile(unlist(size_OUM_hunting_boot_opt_pou), probs=c(0.025, 0.975)),
  quantile(unlist(size_OUM_hunting_boot_opt_pur), probs=c(0.025, 0.975)),
  quantile(unlist(size_OUM_hunting_boot_opt_fos), probs=c(0.025, 0.975)))

size_OUM_hunting_boot_opt <- data.frame(mean_size_OUM_hunting_boot_opt,CI_size_OUM_hunting_boot_opt)
colnames(size_OUM_hunting_boot_opt) <- c("OUM_optima_boot", "L95", "U95")
rownames(size_OUM_hunting_boot_opt) <- c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial")
size_OUM_hunting_boot_opt
write.csv(size_OUM_hunting_boot_opt, file="size_OUM_hunting_boot_opt.csv")


col_hunting <- c("red", "blue", "black", "yellow","green","orange")

pdf("size_OUM_hunting_boot_line.pdf",width=5,height=7) 
plot(mean_size_OUM_hunting_optima$OUM_optima, ylim = c(min(mean_size_OUM_hunting_optima$OUM_optima-(size_OUM_hunting_boot_opt$OUM_optima_boot-size_OUM_hunting_boot_opt$L95)), max(size_OUM_hunting_boot_opt$U95-size_OUM_hunting_boot_opt$OUM_optima_boot+mean_size_OUM_hunting_optima$OUM_optima)),  ylab = "size", xaxt = "n", las=1, cex.axis=2)
axis(1, at=c(1:6), labels = c("ambush", "aquatic", "occasional","pounce", "pursuit", "semi_fossorial"), las=2, cex.axis=2)
plotCI(mean_size_OUM_hunting_optima$OUM_optima,y=NULL, uiw=size_OUM_hunting_boot_opt$U95-size_OUM_hunting_boot_opt$OUM_optima_boot, liw=size_OUM_hunting_boot_opt$OUM_optima_boot-size_OUM_hunting_boot_opt$L95, err="y", pch=20, slty=3, col=col_hunting, scol = col_hunting, add=TRUE, cex=3, lwd=3)
dev.off()

size_OUM_hunting_boot_opt_am_den <- density(unlist(size_OUM_hunting_boot_opt_am))
size_OUM_hunting_boot_opt_aq_den <- density(unlist(size_OUM_hunting_boot_opt_aq))
size_OUM_hunting_boot_opt_occ_den <- density(unlist(size_OUM_hunting_boot_opt_occ))
size_OUM_hunting_boot_opt_pou_den <- density(unlist(size_OUM_hunting_boot_opt_pou))
size_OUM_hunting_boot_opt_pur_den <- density(unlist(size_OUM_hunting_boot_opt_pur))
size_OUM_hunting_boot_opt_fos_den <- density(unlist(size_OUM_hunting_boot_opt_fos))

pdf("size_OUM_hunting_boot_hist.pdf",width=8,height=7) 
plot(range(size_OUM_hunting_boot_opt_occ_den$x, size_OUM_hunting_boot_opt_aq_den$x), range(size_OUM_hunting_boot_opt_fos_den$y, size_OUM_hunting_boot_opt_pou_den$y), type = "n", xlab = "size_optima", ylab = "Density", main = "am=red; aq=blue; occ=black; pou=yel; pur=gre; fos=ora")
polygon(size_OUM_hunting_boot_opt_am_den, col = "red")
polygon(size_OUM_hunting_boot_opt_aq_den, col = "blue")
polygon(size_OUM_hunting_boot_opt_occ_den, col = "black")
polygon(size_OUM_hunting_boot_opt_pou_den, col = "yellow")
polygon(size_OUM_hunting_boot_opt_pur_den, col = "green")
polygon(size_OUM_hunting_boot_opt_fos_den, col = "orange")
dev.off()
