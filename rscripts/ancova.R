library(RRPP)
library(geiger)

load("Rdata/ERecologydata_1225.RData")

locomotion_rrpp <- lm.rrpp(lnhbER_all ~ lnsize_all*locomotion, iter = 999, Cov = vcv.phylo(tree_prune), SS.type = "II")
summary(locomotion_rrpp)
anova(locomotion_rrpp)
locomotion_PW <- pairwise(locomotion_rrpp, groups = locomotion, covariate = lnsize_all)
summary(locomotion_PW, test.type = "VC", cor.type = 'dist', show.vectors = TRUE)

pdf("hbER_size_locomotion.pdf", width=5, height=5)
plot(lnhbER_all ~ lnsize_all, pch = c(25, 21, 22, 23, 24, 25)[factor(locomotion)], bg = col_locomotion[factor(locomotion)], las =1, cex = 1.75, cex.axis=2)
abline(a = 0.2325751, b = 0.2457965, col= "blue") #aquatic
abline(a = 0.2325751+2.9996845, b = 0.2457965-0.5896003, col = "green") #arboreal
abline(a = 0.2325751+2.4766316, b = 0.2457965-0.4592911, col = "lightblue") #semi-aquatic
abline(a = 0.2325751+3.1442320, b = 0.2457965-0.6129740, col = "yellow") #semi-arboreal
abline(a = 0.2325751+2.5181637, b = 0.2457965-0.4945138, col = "orange") #semi-fossorial 
abline(a = 0.2325751+2.7196149, b = 0.2457965-0.5239083, col = "black") #terrestrial
dev.off()

hunting_rrpp <- lm.rrpp(lnhbER_all ~ lnsize_all*hunting, iter = 999, Cov = vcv.phylo(tree_prune), SS.type = "II")
summary(hunting_rrpp)
anova(hunting_rrpp)
hunting_PW <- pairwise(hunting_rrpp, groups = hunting, covariate = lnsize_all)
summary(hunting_PW, test.type = "VC",  cor.type = 'dist', show.vectors = TRUE)

pdf("hbER_size_hunting.pdf", width=5, height=5)
plot(lnhbER_all ~ lnsize_all, pch = c(21, 21, 22, 23, 24, 25)[factor(hunting)], bg = col_hunting[factor(hunting)], las =1, cex = 1.75, cex.axis=2)
abline(a = 2.50586418, b = -0.16755685, col= "red") #ambush
abline(a = 2.50586418-0.67897209, b = -0.16755685+0.12973975, col = "blue") #aquatic
abline(a = 2.50586418+0.88305192, b = -0.16755685-0.22373666, col = "black") #occasional
abline(a = 2.50586418+0.52032345, b = -0.16755685-0.11939477, col = "yellow") #pounce
abline(a = 2.50586418+0.61658723, b = -0.16755685-0.15660449, col = "green") #pursuit
abline(a = 2.50586418-0.07009599, b = -0.16755685-0.01352888, col = "orange") #semi-fossorial 
dev.off()

diet_rrpp <- lm.rrpp(lnhbER_all ~ lnsize_all*diet, iter = 999, Cov = vcv.phylo(tree_prune), SS.type = "II")
summary(diet_rrpp)
anova(diet_rrpp)
diet_PW <- pairwise(diet_rrpp, groups = diet, covariate = lnsize_all)
summary(diet_PW, test.type = "VC", cor.type = 'dist', show.vectors = TRUE)

pdf("hbER_size_diet.pdf", width=5, height=5)
plot(lnhbER_all ~ lnsize_all, pch = c( 21, 22, 23, 24, 25)[factor(diet)], bg = col_diet[factor(diet)], las =1, cex = 1.75, cex.axis=2)
abline(a = 1.8268921, b = -0.0378171, col= "blue") #aquatic
abline(a = 1.8268921+1.0843708, b = -0.0378171-0.2223058, col = "red") #carnivore
abline(a = 1.8268921+1.5841167, b = -0.0378171-0.3516410, col = "green") #herbivore
abline(a = 1.8268921+1.3080944, b = -0.0378171-0.3046187, col = "black") #insectivore
abline(a = 1.8268921+1.4162460, b = -0.0378171-0.3127880, col = "orange") #omnivore
dev.off()

