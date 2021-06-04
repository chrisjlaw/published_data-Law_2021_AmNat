library(bayou)
load("Rdata/ERecologydata_0914.RData")


###size - bayou ###
{lnsize_prior <- make.prior(tree_prune, dists=list(dalpha="dhalfcauchy", dsig2="dhalfcauchy",dsb="dsb", dk="cdpois", dtheta="dnorm"), param=list(dalpha=list(scale=1), dsig2=list(scale=1), dk=list(lambda=15, kmax=200), dsb=list(bmax=1,prob=1), dtheta=list(mean=mean(lnsize_all), sd=sd(lnsize_all))),plot.prior=TRUE)}

#run1
lnsize_fit1 <- bayou.makeMCMC(tree_prune, lnsize_all, SE=size_se_all, model="OU", prior=lnsize_prior, new.dir="Rdata/bayou/Pteronura/bayou_size/", outname="lnsize_fit1", plot.freq=200000, ticker.freq=50000)
lnsize_fit1$run(1500000)
lnsize_chain1<-lnsize_fit1$load()
save(lnsize_chain1, file="bayou_lnsize_chain1.2.Rdata")
lnsize_chain1 <- set.burnin(lnsize_chain1, 0.3)
plot(lnsize_chain1)
lnsize_out1 <- summary(lnsize_chain1)
write.csv(lnsize_out1$branch.posteriors, file="lnsize_branch.posteriors.csv")
write.csv(lnsize_out1$statistics, file="lnsize_statistics.csv")

#run2
lnsize_fit2 <- bayou.makeMCMC(tree_prune, lnsize_all, SE=size_se_all, model="OU", prior=lnsize_prior, new.dir="Rdata/bayou/Pteronura/bayou_size/", outname="lnsize_fit2", plot.freq=200000, ticker.freq=50000)
lnsize_fit2$run(1500000)
lnsize_chain2<-lnsize_fit2$load()
save(lnsize_chain2, file="bayou_lnsize_chain2.Rdata")
lnsize_chain2 <- set.burnin(lnsize_chain2, 0.3)
plot(lnsize_chain2)
lnsize_out2 <- summary(lnsize_chain2)
write.csv(lnsize_out2$branch.posteriors, file="lnsize_branch.posteriors.csv")
write.csv(lnsize_out2$statistics, file="lnsize_statistics.csv")

load("Rdata/bayou/Pteronura/bayou_lnsize_chain1.2.Rdata")
pdf(width=5, height =7, file="bayou_size_shifts.pdf")
plotSimmap.mcmc(lnsize_chain1, burnin=0.3, lwd=5, edge.type="regimes", pal=colorRampPalette(c("darkred", "white", "blue")), show.tip.label=TRUE, cex = .25, pp.labels = TRUE, pp.cutoff = 0.5)
dev.off()

pdf(width=5, height =7, file="bayou_size_opt.pdf")
plotBranchHeatMap(tree_prune, lnsize_chain1, "theta", burnin = 0.3, pal = colorRampPalette(c("darkred", "yellow", "blue")), cex=.25, lwd =10)
dev.off()

load("Rdata/bayou/Pteronura/bayou_lnsize_chain2.2.Rdata")
plotSimmap.mcmc(lnsize_chain2, burnin=0.3, lwd=5, edge.type="regimes", pal=colorRampPalette(c("darkred", "white", "blue")), show.tip.label=TRUE, cex = .25, pp.labels = TRUE, pp.cutoff = 0.5)
plotBranchHeatMap(tree_prune, lnsize_chain2, "theta", burnin = 0.3, pal = colorRampPalette(c("darkred", "white", "blue")), cex=.25, lwd =10)

#Convergence check
lnsize_RlnL <- gelman.R("lnL", chain1=lnsize_chain1, chain2=lnsize_chain2, plot=TRUE, type="n", ylim=c(0.9, 2))
lnsize_Ralpha <- gelman.R("alpha", chain1=lnsize_chain1, chain2=lnsize_chain2, plot=TRUE, type="n", ylim=c(0.9, 2))

lnsize_L1 <- Lposterior(lnsize_chain1,tree_prune, burnin=0.3)
lnsize_L2 <- Lposterior(lnsize_chain2,tree_prune, burnin=0.3)
plot(lnsize_L1$pp,lnsize_L2$pp, xlim=c(0,.3), ylim=c(0,.3), xlab="lnsize_Chain 1", ylab="lnsize_Chain 2")
curve(1*x, add=TRUE, lty=2)


###hbER - bayou ###
{lnhbER_prior <- make.prior(tree_prune, dists=list(dalpha="dhalfcauchy", dsig2="dhalfcauchy",dsb="dsb", dk="cdpois", dtheta="dnorm"), param=list(dalpha=list(scale=1), dsig2=list(scale=1), dk=list(lambda=15, kmax=200), dsb=list(bmax=1,prob=1), dtheta=list(mean=mean(lnhbER_all), sd=sd(lnhbER_all))),plot.prior=TRUE)}

#run1
lnhbER_fit1 <- bayou.makeMCMC(tree_prune, lnhbER_all, SE=hbER_se_all, model="OU", prior=lnhbER_prior, new.dir="Rdata/bayou/Pteronura/bayou_hbER/", outname="lnhbER_fit1", plot.freq=200000, ticker.freq=50000)
lnhbER_fit1$run(1500000)
lnhbER_chain1<-lnhbER_fit1$load()
save(lnhbER_chain1, file="bayou_lnhbER_chain1.2.Rdata")
lnhbER_chain1 <- set.burnin(lnhbER_chain1, 0.3)
plot(lnhbER_chain1)
lnhbER_out1 <- summary(lnhbER_chain1)
write.csv(lnhbER_out1$branch.posteriors, file="lnhbER_branch.posteriors.csv")
write.csv(lnhbER_out1$statistics, file="lnhbER_statistics.csv")

test <- bayou2OUwie(lnhbER_out1$branch.posteriors, tree_prune, lnhbER_all)

#run2
lnhbER_fit2 <- bayou.makeMCMC(tree_prune, lnhbER_all, SE=hbER_se_all, model="OU", prior=lnhbER_prior, new.dir="Rdata/bayou/Pteronura/bayou_hbER/", outname="lnhbER_fit2", plot.freq=200000, ticker.freq=50000)
lnhbER_fit2$run(1500000)
lnhbER_chain2<-lnhbER_fit2$load()
save(lnhbER_chain2, file="bayou_lnhbER_chain2.Rdata")
lnhbER_chain2 <- set.burnin(lnhbER_chain2, 0.3)
plot(lnhbER_chain2)
lnhbER_out2 <- summary(lnhbER_chain2)
write.csv(lnhbER_out2$branch.posteriors, file="lnhbER_branch.posteriors.csv")
write.csv(lnhbER_out2$statistics, file="lnhbER_statistics.csv")

load("Rdata/bayou/Pteronura/bayou_lnhbER_chain1.2.Rdata")
pdf(width=5, height =7, file="bayou_hbER_shifts.pdf")
plotSimmap.mcmc(lnhbER_chain1, burnin=0.3, lwd=5, edge.type="regimes", pal=colorRampPalette(c("darkred", "white", "blue")), show.tip.label=TRUE, cex = .25, pp.labels = TRUE, pp.cutoff = 0.5)
dev.off()

pdf(width=5, height =7, file="bayou_hbER_opt.pdf")
plotBranchHeatMap(tree_prune, lnhbER_chain1, "theta", burnin = 0.3, pal = colorRampPalette(c("darkred", "yellow", "blue")), cex=.25, lwd =10)
dev.off()

load("Rdata/bayou/Pteronura/bayou_lnhbER_chain2.2.Rdata")
plotSimmap.mcmc(lnhbER_chain2, burnin=0.3, lwd=5, edge.type="regimes", pal=colorRampPalette(c("darkred", "white", "blue")), show.tip.label=TRUE, cex = .25, pp.labels = TRUE, pp.cutoff = 0.5)
plotBranchHeatMap(tree_prune, lnhbER_chain2, "theta", burnin = 0.3, pal = colorRampPalette(c("darkred", "white", "blue")), cex=.25, lwd =10)

#Convergence check
lnhbER_RlnL <- gelman.R("lnL", chain1=lnhbER_chain1, chain2=lnhbER_chain2, plot=TRUE, type="n", ylim=c(0.9, 2))
lnhbER_Ralpha <- gelman.R("alpha", chain1=lnhbER_chain1, chain2=lnhbER_chain2, plot=TRUE, type="n", ylim=c(0.9, 2))

lnhbER_L1 <- Lposterior(lnhbER_chain1,tree_prune, burnin=0.3)
lnhbER_L2 <- Lposterior(lnhbER_chain2,tree_prune, burnin=0.3)
plot(lnhbER_L1$pp,lnhbER_L2$pp, xlim=c(0,.3), ylim=c(0,.3), xlab="lnhbER_Chain 1", ylab="lnhbER_Chain 2")
curve(1*x, add=TRUE, lty=2)


###headER - bayou ###
{lnheadER_prior <- make.prior(tree_prune, dists=list(dalpha="dhalfcauchy", dsig2="dhalfcauchy",dsb="dsb", dk="cdpois", dtheta="dnorm"), param=list(dalpha=list(scale=1), dsig2=list(scale=1), dk=list(lambda=15, kmax=200), dsb=list(bmax=1,prob=1), dtheta=list(mean=mean(lnheadER_all), sd=sd(lnheadER_all))),plot.prior=TRUE)}

#run1
lnheadER_fit1 <- bayou.makeMCMC(tree_prune, lnheadER_all, SE=headER_se_all, model="OU", prior=lnheadER_prior, new.dir="Rdata/bayou/Pteronura/bayou_headER/", outname="lnheadER_fit1", plot.freq=200000, ticker.freq=50000)
lnheadER_fit1$run(1500000)
lnheadER_chain1<-lnheadER_fit1$load()
save(lnheadER_chain1, file="bayou_lnheadER_chain1.Rdata")
lnheadER_chain1 <- set.burnin(lnheadER_chain1, 0.3)
plot(lnheadER_chain1)
lnheadER_out1 <- summary(lnheadER_chain1)
write.csv(lnheadER_out1$branch.posteriors, file="lnheadER_branch.posteriors.csv")
write.csv(lnheadER_out1$statistics, file="lnheadER_statistics.csv")

pdf(width=5, height =7, file="bayou_lnheadER_shifts.pdf")
plotSimmap.mcmc(lnheadER_chain1, burnin=0.3, lwd=5, edge.type="regimes", pal=colorRampPalette(c("darkred", "white", "blue")), show.tip.label=TRUE, cex = .25, pp.labels = TRUE, pp.cutoff = 0.5)
dev.off()
pdf(width=5, height =7, file="bayou_lnheadER_opt.pdf")
plotBranchHeatMap(tree_prune, lnheadER_chain1, "theta", burnin = 0.3, pal = colorRampPalette(c("darkred", "yellow", "blue")), cex=.25, lwd =10)
dev.off()

#run2
lnheadER_fit2 <- bayou.makeMCMC(tree_prune, lnheadER_all, SE=headER_se_all, model="OU", prior=lnheadER_prior, new.dir="Rdata/bayou/Pteronura/bayou_headER/", outname="lnheadER_fit2", plot.freq=200000, ticker.freq=50000)
lnheadER_fit2$run(1500000)
lnheadER_chain2<-lnheadER_fit2$load()
save(lnheadER_chain2, file="bayou_lnheadER_chain2.Rdata")
lnheadER_chain2 <- set.burnin(lnheadER_chain2, 0.3)
plot(lnheadER_chain2)
lnheadER_out2 <- summary(lnheadER_chain2)
write.csv(lnheadER_out2$branch.posteriors, file="lnheadER_branch.posteriors.csv")
write.csv(lnheadER_out2$statistics, file="lnheadER_statistics.csv")

plotSimmap.mcmc(lnheadER_chain2, burnin=0.3, lwd=5, edge.type="regimes", pal=colorRampPalette(c("darkred", "white", "blue")), show.tip.label=TRUE, cex = .25, pp.labels = TRUE, pp.cutoff = 0.5)
plotBranchHeatMap(tree_prune, lnheadER_chain2, "theta", burnin = 0.3, pal = colorRampPalette(c("darkred", "yellow", "blue")), cex=.25, lwd =10)

#run3
lnheadER_fit3 <- bayou.makeMCMC(tree_prune, lnheadER_all, SE=headER_se_all, model="OU", prior=lnheadER_prior, new.dir="Rdata/bayou/Pteronura/bayou_headER/", outname="lnheadER_fit3", plot.freq=200000, ticker.freq=50000)
lnheadER_fit3$run(3000000)
lnheadER_chain3<-lnheadER_fit3$load()
save(lnheadER_chain3, file="bayou_lnheadER_chain3.2.Rdata")
lnheadER_chain3 <- set.burnin(lnheadER_chain3, 0.3)
plot(lnheadER_chain3)
lnheadER_out1 <- summary(lnheadER_chain3)
write.csv(lnheadER_out1$branch.posteriors, file="lnheadER_branch.posteriors.csv")
write.csv(lnheadER_out1$statistics, file="lnheadER_statistics.csv")

pdf(width=5, height =7, file="bayou_lnheadER_shifts.pdf")
plotSimmap.mcmc(lnheadER_chain3, burnin=0.3, lwd=5, edge.type="regimes", pal=colorRampPalette(c("darkred", "white", "blue")), show.tip.label=TRUE, cex = .25, pp.labels = TRUE, pp.cutoff = 0.5)
dev.off()
pdf(width=5, height =7, file="bayou_lnheadER_opt.pdf")
plotBranchHeatMap(tree_prune, lnheadER_chain3, "theta", burnin = 0.3, pal = colorRampPalette(c("darkred", "yellow", "blue")), cex=.25, lwd =10)
dev.off()

#Convergence check
lnheadER_RlnL <- gelman.R("lnL", chain1=lnheadER_chain1, chain2=lnheadER_chain2, plot=TRUE, type="n", ylim=c(0.9, 2))
lnheadER_Ralpha <- gelman.R("alpha", chain1=lnheadER_chain1, chain2=lnheadER_chain2, plot=TRUE, type="n", ylim=c(0.9, 2))

lnheadER_L1 <- Lposterior(lnheadER_chain1,tree_prune, burnin=0.3)
lnheadER_L2 <- Lposterior(lnheadER_chain2,tree_prune, burnin=0.3)
lnheadER_L3 <- Lposterior(lnheadER_chain3,tree_prune, burnin=0.3)
plot(lnheadER_L3$pp,lnheadER_L2$pp, xlim=c(0,.3), ylim=c(0,.3), xlab="lnheadER_Chain 3", ylab="lnheadER_Chain 2")
curve(1*x, add=TRUE, lty=2)

###AEI_C - bayou ###
{lnAEI_C_prior <- make.prior(tree_prune, dists=list(dalpha="dhalfcauchy", dsig2="dhalfcauchy",dsb="dsb", dk="cdpois", dtheta="dnorm"), param=list(dalpha=list(scale=1), dsig2=list(scale=1), dk=list(lambda=15, kmax=200), dsb=list(bmax=1,prob=1), dtheta=list(mean=mean(lnAEI_C_all), sd=sd(lnAEI_C_all))),plot.prior=TRUE)}

#run1
lnAEI_C_fit1 <- bayou.makeMCMC(tree_prune, lnAEI_C_all, SE=AEI_C_se_all, model="OU", prior=lnAEI_C_prior, new.dir="Rdata/bayou/Pteronura/bayou_AEI_C/", outname="lnAEI_C_fit1", plot.freq=200000, ticker.freq=50000)
lnAEI_C_fit1$run(1500000)
lnAEI_C_chain1<-lnAEI_C_fit1$load()
save(lnAEI_C_chain1, file="bayou_lnAEI_C_chain1.Rdata")
lnAEI_C_chain1 <- set.burnin(lnAEI_C_chain1, 0.3)
plot(lnAEI_C_chain1)
lnAEI_C_out1 <- summary(lnAEI_C_chain1)
write.csv(lnAEI_C_out1$branch.posteriors, file="lnAEI_C_branch.posteriors.csv")
write.csv(lnAEI_C_out1$statistics, file="lnAEI_C_statistics.csv")

pdf(width=5, height =7, file="bayou_lnAEI_C_shifts.pdf")
plotSimmap.mcmc(lnAEI_C_chain1, burnin=0.3, lwd=5, edge.type="regimes", pal=colorRampPalette(c("darkred", "white", "blue")), show.tip.label=TRUE, cex = .25, pp.labels = TRUE, pp.cutoff = 0.5)
dev.off()
pdf(width=5, height =7, file="bayou_lnAEI_C_opt.pdf")
plotBranchHeatMap(tree_prune, lnAEI_C_chain1, "theta", burnin = 0.3, pal = colorRampPalette(c("darkred", "yellow", "blue")), cex=.25, lwd =10)
dev.off()

#run2
lnAEI_C_fit2 <- bayou.makeMCMC(tree_prune, lnAEI_C_all, SE=AEI_C_se_all, model="OU", 
                               prior=lnAEI_C_prior, new.dir="Rdata/bayou/Pteronura/bayou_AEI_C2/", outname="lnAEI_C_fit2", plot.freq=200000, ticker.freq=50000)
lnAEI_C_fit2$run(1500000)
lnAEI_C_chain2<-lnAEI_C_fit2$load()
save(lnAEI_C_chain2, file="bayou_lnAEI_C_chain2.Rdata")
lnAEI_C_chain2 <- set.burnin(lnAEI_C_chain2, 0.3)
plot(lnAEI_C_chain2)
lnAEI_C_out2 <- summary(lnAEI_C_chain2)
write.csv(lnAEI_C_out2$branch.posteriors, file="lnAEI_C_branch.posteriors.csv")
write.csv(lnAEI_C_out2$statistics, file="lnAEI_C_statistics.csv")

plotSimmap.mcmc(lnAEI_C_chain2, burnin=0.3, lwd=5, edge.type="regimes", pal=colorRampPalette(c("darkred", "white", "blue")), show.tip.label=TRUE, cex = .25, pp.labels = TRUE, pp.cutoff = 0.5)
plotBranchHeatMap(tree_prune, lnAEI_C_chain2, "theta", burnin = 0.3, pal = colorRampPalette(c("darkred", "white", "blue")), cex=.25, lwd =10)

#Convergence check
lnAEI_C_RlnL <- gelman.R("lnL", chain1=lnAEI_C_chain1, chain2=lnAEI_C_chain2, plot=TRUE, type="n", ylim=c(0.9, 2))
lnAEI_C_Ralpha <- gelman.R("alpha", chain1=lnAEI_C_chain1, chain2=lnAEI_C_chain2, plot=TRUE, type="n", ylim=c(0.9, 2))

lnAEI_C_L1 <- Lposterior(lnAEI_C_chain1,tree_prune, burnin=0.3)
lnAEI_C_L2 <- Lposterior(lnAEI_C_chain2,tree_prune, burnin=0.3)
plot(lnAEI_C_L1$pp,lnAEI_C_L2$pp, xlim=c(0,.3), ylim=c(0,.3), xlab="lnAEI_C_Chain 1", ylab="lnAEI_C_Chain 2")
curve(1*x, add=TRUE, lty=2)

###AEI_T - bayou ###
{lnAEI_T_prior <- make.prior(tree_prune, dists=list(dalpha="dhalfcauchy", dsig2="dhalfcauchy",dsb="dsb", dk="cdpois", dtheta="dnorm"), param=list(dalpha=list(scale=1), dsig2=list(scale=1), dk=list(lambda=15, kmax=200), dsb=list(bmax=1,prob=1), dtheta=list(mean=mean(lnAEI_T_all), sd=sd(lnAEI_T_all))),plot.prior=TRUE)}

#run1
lnAEI_T_fit1 <- bayou.makeMCMC(tree_prune, lnAEI_T_all, SE=AEI_T_se_all, model="OU", prior=lnAEI_T_prior, new.dir="Rdata/bayou/Pteronura/bayou_AEI_T/", outname="lnAEI_T_fit1", plot.freq=200000, ticker.freq=50000)
lnAEI_T_fit1$run(3000000)
lnAEI_T_chain1<-lnAEI_T_fit1$load()
save(lnAEI_T_chain1, file="bayou_lnAEI_T_chain1.2.Rdata")
lnAEI_T_chain1 <- set.burnin(lnAEI_T_chain1, 0.3)
plot(lnAEI_T_chain1)
lnAEI_T_out1 <- summary(lnAEI_T_chain1)
write.csv(lnAEI_T_out1$branch.posteriors, file="lnAEI_T_branch.posteriors.csv")
write.csv(lnAEI_T_out1$statistics, file="lnAEI_T_statistics.csv")

pdf(width=5, height =7, file="bayou_lnAEI_T_shifts.pdf")
plotSimmap.mcmc(lnAEI_T_chain1, burnin=0.3, lwd=5, edge.type="regimes", pal=colorRampPalette(c("darkred", "white", "blue")), show.tip.label=TRUE, cex = .25, pp.labels = TRUE, pp.cutoff = 0.5)
dev.off()
pdf(width=5, height =7, file="bayou_lnAEI_T_opt.pdf")
plotBranchHeatMap(tree_prune, lnAEI_T_chain1, "theta", burnin = 0.3, pal = colorRampPalette(c("darkred", "yellow", "blue")), cex=.25, lwd =10)
dev.off()

#run2
lnAEI_T_fit2 <- bayou.makeMCMC(tree_prune, lnAEI_T_all, SE=AEI_T_se_all, model="OU", prior=lnAEI_T_prior, new.dir="Rdata/bayou/Pteronura/bayou_AEI_T/", outname="lnAEI_T_fit2", plot.freq=200000, ticker.freq=50000)
lnAEI_T_fit2$run(3000000)
lnAEI_T_chain2<-lnAEI_T_fit2$load()
save(lnAEI_T_chain2, file="bayou_lnAEI_T_chain2.2.Rdata")
lnAEI_T_chain2 <- set.burnin(lnAEI_T_chain2, 0.3)
plot(lnAEI_T_chain2)
lnAEI_T_out2 <- summary(lnAEI_T_chain2)
write.csv(lnAEI_T_out2$branch.posteriors, file="lnAEI_T_branch.posteriors.csv")
write.csv(lnAEI_T_out2$statistics, file="lnAEI_T_statistics.csv")

plotSimmap.mcmc(lnAEI_T_chain2, burnin=0.3, lwd=5, edge.type="regimes", pal=colorRampPalette(c("darkred", "white", "blue")), show.tip.label=TRUE, cex = .25, pp.labels = TRUE, pp.cutoff = 0.5)
plotBranchHeatMap(tree_prune, lnAEI_T_chain2, "theta", burnin = 0.3, pal = colorRampPalette(c("darkred", "white", "blue")), cex=.25, lwd =10)

#Convergence check
lnAEI_T_RlnL <- gelman.R("lnL", chain1=lnAEI_T_chain1, chain2=lnAEI_T_chain2, plot=TRUE, type="n", ylim=c(0.9, 2))
lnAEI_T_Ralpha <- gelman.R("alpha", chain1=lnAEI_T_chain1, chain2=lnAEI_T_chain2, plot=TRUE, type="n", ylim=c(0.9, 2))

lnAEI_T_L1 <- Lposterior(lnAEI_T_chain1,tree_prune, burnin=0.3)
lnAEI_T_L2 <- Lposterior(lnAEI_T_chain2,tree_prune, burnin=0.3)
plot(lnAEI_T_L1$pp,lnAEI_T_L2$pp, xlim=c(0,.3), ylim=c(0,.3), xlab="lnAEI_T_Chain 1", ylab="lnAEI_T_Chain 2")
curve(1*x, add=TRUE, lty=2)

###AEI_L - bayou ###
{lnAEI_L_prior <- make.prior(tree_prune, dists=list(dalpha="dhalfcauchy", dsig2="dhalfcauchy",dsb="dsb", dk="cdpois", dtheta="dnorm"), param=list(dalpha=list(scale=1), dsig2=list(scale=1), dk=list(lambda=15, kmax=200), dsb=list(bmax=1,prob=1), dtheta=list(mean=mean(lnAEI_L_all), sd=sd(lnAEI_L_all))),plot.prior=TRUE)}

#run1
lnAEI_L_fit1 <- bayou.makeMCMC(tree_prune, lnAEI_L_all, SE=AEI_L_se_all, model="OU", prior=lnAEI_L_prior, new.dir="Rdata/bayou/Pteronura/bayou_AEI_L/", outname="lnAEI_L_fit1", plot.freq=200000, ticker.freq=50000)
lnAEI_L_fit1$run(1500000)
lnAEI_L_chain1<-lnAEI_L_fit1$load()
save(lnAEI_L_chain1, file="bayou_lnAEI_L_chain1.Rdata")
lnAEI_L_chain1 <- set.burnin(lnAEI_L_chain1, 0.3)
plot(lnAEI_L_chain1)
lnAEI_L_out1 <- summary(lnAEI_L_chain1)
write.csv(lnAEI_L_out1$branch.posteriors, file="lnAEI_L_branch.posteriors.csv")
write.csv(lnAEI_L_out1$statistics, file="lnAEI_L_statistics.csv")

pdf(width=5, height =7, file="bayou_lnAEI_L_shifts.pdf")
plotSimmap.mcmc(lnAEI_L_chain1, burnin=0.3, lwd=5, edge.type="regimes", pal=colorRampPalette(c("darkred", "white", "blue")), show.tip.label=TRUE, cex = .25, pp.labels = TRUE, pp.cutoff = 0.5)
dev.off()
pdf(width=5, height =7, file="bayou_lnAEI_L_opt.pdf")
plotBranchHeatMap(tree_prune, lnAEI_L_chain1, "theta", burnin = 0.3, pal = colorRampPalette(c("darkred", "yellow", "blue")), cex=.25, lwd =10)
dev.off()

#run2
lnAEI_L_fit2 <- bayou.makeMCMC(tree_prune, lnAEI_L_all, SE=AEI_L_se_all, model="OU", prior=lnAEI_L_prior, new.dir="Rdata/bayou/Pteronura/bayou_AEI_L/", outname="lnAEI_L_fit2", plot.freq=200000, ticker.freq=50000)
lnAEI_L_fit2$run(1500000)
lnAEI_L_chain2<-lnAEI_L_fit2$load()
save(lnAEI_L_chain2, file="bayou_lnAEI_L_chain2.Rdata")
lnAEI_L_chain2 <- set.burnin(lnAEI_L_chain2, 0.3)
plot(lnAEI_L_chain2)
lnAEI_L_out2 <- summary(lnAEI_L_chain2)
write.csv(lnAEI_L_out2$branch.posteriors, file="lnAEI_L_branch.posteriors.csv")
write.csv(lnAEI_L_out2$statistics, file="lnAEI_L_statistics.csv")

plotSimmap.mcmc(lnAEI_L_chain2, burnin=0.3, lwd=5, edge.type="regimes", pal=colorRampPalette(c("darkred", "white", "blue")), show.tip.label=TRUE, cex = .25, pp.labels = TRUE, pp.cutoff = 0.5)
plotBranchHeatMap(tree_prune, lnAEI_L_chain2, "theta", burnin = 0.3, pal = colorRampPalette(c("darkred", "white", "blue")), cex=.25, lwd =10)

#Convergence check
lnAEI_L_RlnL <- gelman.R("lnL", chain1=lnAEI_L_chain1, chain2=lnAEI_L_chain2, plot=TRUE, type="n", ylim=c(0.9, 2))
lnAEI_L_Ralpha <- gelman.R("alpha", chain1=lnAEI_L_chain1, chain2=lnAEI_L_chain2, plot=TRUE, type="n", ylim=c(0.9, 2))

lnAEI_L_L1 <- Lposterior(lnAEI_L_chain1,tree_prune, burnin=0.3)
lnAEI_L_L2 <- Lposterior(lnAEI_L_chain2,tree_prune, burnin=0.3)
plot(lnAEI_L_L1$pp,lnAEI_L_L2$pp, xlim=c(0,.3), ylim=c(0,.3), xlab="lnAEI_L_Chain 1", ylab="lnAEI_L_Chain 2")
curve(1*x, add=TRUE, lty=2)

###AEI_S - bayou ###
{lnAEI_S_prior <- make.prior(tree_prune, dists=list(dalpha="dhalfcauchy", dsig2="dhalfcauchy",dsb="dsb", dk="cdpois", dtheta="dnorm"), param=list(dalpha=list(scale=1), dsig2=list(scale=1), dk=list(lambda=15, kmax=200), dsb=list(bmax=1,prob=1), dtheta=list(mean=mean(lnAEI_S_all), sd=sd(lnAEI_S_all))),plot.prior=TRUE)}

#run1
lnAEI_S_fit1 <- bayou.makeMCMC(tree_prune, lnAEI_S_all, SE=AEI_S_se_all, model="OU", prior=lnAEI_S_prior, new.dir="Rdata/bayou/Pteronura/bayou_AEI_S/", outname="lnAEI_S_fit1", plot.freq=200000, ticker.freq=50000)
lnAEI_S_fit1$run(1500000)
lnAEI_S_chain1<-lnAEI_S_fit1$load()
save(lnAEI_S_chain1, file="bayou_lnAEI_S_chain1.Rdata")
lnAEI_S_chain1 <- set.burnin(lnAEI_S_chain1, 0.3)
plot(lnAEI_S_chain1)
lnAEI_S_out1 <- summary(lnAEI_S_chain1)
write.csv(lnAEI_S_out1$branch.posteriors, file="lnAEI_S_branch.posteriors.csv")
write.csv(lnAEI_S_out1$statistics, file="lnAEI_S_statistics.csv")

pdf(width=5, height =7, file="bayou_lnAEI_S_shifts.pdf")
plotSimmap.mcmc(lnAEI_S_chain1, burnin=0.3, lwd=5, edge.type="regimes", pal=colorRampPalette(c("darkred", "white", "blue")), show.tip.label=TRUE, cex = .25, pp.labels = TRUE, pp.cutoff = 0.5)
dev.off()
pdf(width=5, height =7, file="bayou_lnAEI_S_opt.pdf")
plotBranchHeatMap(tree_prune, lnAEI_S_chain1, "theta", burnin = 0.3, pal = colorRampPalette(c("darkred", "yellow", "blue")), cex=.25, lwd =10)
dev.off()

#run2
lnAEI_S_fit2 <- bayou.makeMCMC(tree_prune, lnAEI_S_all, SE=AEI_S_se_all, model="OU", prior=lnAEI_S_prior, new.dir="Rdata/bayou/Pteronura/bayou_AEI_S/", outname="lnAEI_S_fit2", plot.freq=200000, ticker.freq=50000)
lnAEI_S_fit2$run(1500000)
lnAEI_S_chain2<-lnAEI_S_fit2$load()
save(lnAEI_S_chain2, file="bayou_lnAEI_S_chain2.Rdata")
lnAEI_S_chain2 <- set.burnin(lnAEI_S_chain2, 0.3)
plot(lnAEI_S_chain2)
lnAEI_S_out2 <- summary(lnAEI_S_chain2)
write.csv(lnAEI_S_out2$branch.posteriors, file="lnAEI_S_branch.posteriors.csv")
write.csv(lnAEI_S_out2$statistics, file="lnAEI_S_statistics.csv")

plotSimmap.mcmc(lnAEI_S_chain2, burnin=0.3, lwd=5, edge.type="regimes", pal=colorRampPalette(c("darkred", "white", "blue")), show.tip.label=TRUE, cex = .25, pp.labels = TRUE, pp.cutoff = 0.5)
plotBranchHeatMap(tree_prune, lnAEI_S_chain2, "theta", burnin = 0.3, pal = colorRampPalette(c("darkred", "white", "blue")), cex=.25, lwd =10)

#Convergence check
lnAEI_S_RlnL <- gelman.R("lnL", chain1=lnAEI_S_chain1, chain2=lnAEI_S_chain2, plot=TRUE, type="n", ylim=c(0.9, 2))
lnAEI_S_Ralpha <- gelman.R("alpha", chain1=lnAEI_S_chain1, chain2=lnAEI_S_chain2, plot=TRUE, type="n", ylim=c(0.9, 2))

lnAEI_S_L1 <- Lposterior(lnAEI_S_chain1,tree_prune, burnin=0.3)
lnAEI_S_L2 <- Lposterior(lnAEI_S_chain2,tree_prune, burnin=0.3)
plot(lnAEI_S_L1$pp,lnAEI_S_L2$pp, xlim=c(0,.3), ylim=c(0,.3), xlab="lnAEI_S_Chain 1", ylab="lnAEI_S_Chain 2")
curve(1*x, add=TRUE, lty=2)

###rib_sc - bayou ###
{lnrib_sc_prior <- make.prior(tree_prune, dists=list(dalpha="dhalfcauchy", dsig2="dhalfcauchy",dsb="dsb", dk="cdpois", dtheta="dnorm"), param=list(dalpha=list(scale=1), dsig2=list(scale=1), dk=list(lambda=15, kmax=200), dsb=list(bmax=1,prob=1), dtheta=list(mean=mean(lnrib_sc_all), sd=sd(lnrib_sc_all))),plot.prior=TRUE)}

#run1
lnrib_sc_fit1 <- bayou.makeMCMC(tree_prune, lnrib_sc_all,  model="OU", prior=lnrib_sc_prior, new.dir="Rdata/bayou/Pteronura/bayou_rib_sc/", outname="lnrib_sc_fit1", plot.freq=200000, ticker.freq=50000)
lnrib_sc_fit1$run(1500000)
lnrib_sc_chain1<-lnrib_sc_fit1$load()
save(lnrib_sc_chain1, file="bayou_lnrib_sc_chain1.Rdata")
lnrib_sc_chain1 <- set.burnin(lnrib_sc_chain1, 0.3)
plot(lnrib_sc_chain1)
lnrib_sc_out1 <- summary(lnrib_sc_chain1)
write.csv(lnrib_sc_out1$branch.posteriors, file="lnrib_sc_branch.posteriors.csv")
write.csv(lnrib_sc_out1$statistics, file="lnrib_sc_statistics.csv")

pdf(width=5, height =7, file="bayou_lnrib_sc_shifts.pdf")
plotSimmap.mcmc(lnrib_sc_chain1, burnin=0.3, lwd=5, edge.type="regimes", pal=colorRampPalette(c("darkred", "white", "blue")), show.tip.label=TRUE, cex = .25, pp.labels = TRUE, pp.cutoff = 0.5)
dev.off()
pdf(width=5, height =7, file="bayou_lnrib_sc_opt.pdf")
plotBranchHeatMap(tree_prune, lnrib_sc_chain1, "theta", burnin = 0.3, pal = colorRampPalette(c("darkred", "yellow", "blue")), cex=.25, lwd =10)
dev.off()

#run2
lnrib_sc_fit2 <- bayou.makeMCMC(tree_prune, lnrib_sc_all,  model="OU", prior=lnrib_sc_prior, new.dir="Rdata/bayou/Pteronura/bayou_rib_sc/", outname="lnrib_sc_fit2", plot.freq=200000, ticker.freq=50000)
lnrib_sc_fit2$run(1500000)
lnrib_sc_chain2<-lnrib_sc_fit2$load()
save(lnrib_sc_chain2, file="bayou_lnrib_sc_chain2.Rdata")
lnrib_sc_chain2 <- set.burnin(lnrib_sc_chain2, 0.3)
plot(lnrib_sc_chain2)
lnrib_sc_out2 <- summary(lnrib_sc_chain2)
write.csv(lnrib_sc_out2$branch.posteriors, file="lnrib_sc_branch.posteriors.csv")
write.csv(lnrib_sc_out2$statistics, file="lnrib_sc_statistics.csv")

plotSimmap.mcmc(lnrib_sc_chain2, burnin=0.3, lwd=5, edge.type="regimes", pal=colorRampPalette(c("darkred", "white", "blue")), show.tip.label=TRUE, cex = .25, pp.labels = TRUE, pp.cutoff = 0.5)
plotBranchHeatMap(tree_prune, lnrib_sc_chain2, "theta", burnin = 0.3, pal = colorRampPalette(c("darkred", "white", "blue")), cex=.25, lwd =10)

#Convergence check
lnrib_sc_RlnL <- gelman.R("lnL", chain1=lnrib_sc_chain1, chain2=lnrib_sc_chain2, plot=TRUE, type="n", ylim=c(0.9, 2))
lnrib_sc_Ralpha <- gelman.R("alpha", chain1=lnrib_sc_chain1, chain2=lnrib_sc_chain2, plot=TRUE, type="n", ylim=c(0.9, 2))

lnrib_sc_L1 <- Lposterior(lnrib_sc_chain1,tree_prune, burnin=0.3)
lnrib_sc_L2 <- Lposterior(lnrib_sc_chain2,tree_prune, burnin=0.3)
plot(lnrib_sc_L1$pp,lnrib_sc_L2$pp, xlim=c(0,.3), ylim=c(0,.3), xlab="lnrib_sc_Chain 1", ylab="lnrib_sc_Chain 2")
curve(1*x, add=TRUE, lty=2)

