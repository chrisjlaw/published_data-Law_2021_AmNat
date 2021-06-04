boxplot(lnsize_all~locomotion)
boxplot(lnsize_all~diet)
boxplot(lnsize_all~hunting)

col_locomotion_vp <- c("blue", "green", "lightblue", "yellow","orange","black")
col_hunting_vp <- c("red", "blue", "black", "yellow","green","orange")
col_diet_vp <- c("blue", "red", "green","black","orange")


size_vp_locomotion <- ggplot(data_mean_pr, aes(x=fct_relevel(locomotion, "arboreal", "semi_arboreal", "terrestrial", "semi_fossorial", "semi_aquatic", "aquatic"), y=size_all, fill = locomotion)) + 
  coord_trans(y='log') +
  scale_y_continuous(name="ln size (cm)", limits=c(50, 500), breaks = scales::pretty_breaks(n = 10)) +
  geom_violin(trim=TRUE) + 
  scale_fill_manual(values=col_locomotion_vp) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
  geom_hline(yintercept=mean(size_all)) + 
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x=element_text(color = "black", size = 15, angle = 45, hjust = .5, vjust = .5),
        axis.text.y=element_text(color = "black", size = 15),
        axis.title.y = element_text(size = 18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black"),
        panel.background = element_blank(),
        legend.position="none") 
size_vp_locomotion
pdf("size_vp_locomotion.pdf", width=4, height =5)
size_vp_locomotion
dev.off()

size_vp_hunting <- ggplot(data_mean_pr, aes(x=fct_relevel(hunting, "pursuit", "pounce", "ambush", "occasional", "semi_fossorial", "aquatic"), y=size_all, fill = hunting)) +
  coord_trans(y='log') +
  scale_y_continuous(name="ln size (cm)", limits=c(50, 500), breaks = scales::pretty_breaks(n = 10)) +
  geom_violin(trim=TRUE) + 
  scale_fill_manual(values=col_hunting_vp) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
  geom_hline(yintercept=mean(size_all)) + 
  ylab("size") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x=element_text(color = "black", size = 15, angle = 45, hjust = .5, vjust = .5),
        axis.text.y=element_text(color = "black", size = 15),
        axis.title.y = element_text(size = 18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black"),
        panel.background = element_blank(),
        legend.position="none") 
size_vp_hunting
pdf("size_vp_hunting.pdf", width=4, height =5)
size_vp_hunting
dev.off()

size_vp_diet <- ggplot(data_mean_pr, aes(x=fct_relevel(diet, "carnivore", "omnivore", "insectivore", "herbivore", "aquatic"), y=size_all, fill = diet)) + 
  coord_trans(y='log') +
  scale_y_continuous(name="ln size (cm)", limits=c(50, 500), breaks = scales::pretty_breaks(n = 10)) +
  geom_violin(trim=TRUE) + 
  scale_fill_manual(values=col_diet_vp) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
  geom_hline(yintercept=mean(size_all)) + 
  ylab("size") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x=element_text(color = "black", size = 15, angle = 45, hjust = .5, vjust = .5),
        axis.text.y=element_text(color = "black", size = 15),
        axis.title.y = element_text(size = 18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black"),
        panel.background = element_blank(),
        legend.position="none") 
size_vp_diet
pdf("size_vp_diet.pdf", width=4, height =5)
size_vp_diet
dev.off()                     
                        

#### hbER ####
hbER_vp_locomotion <- ggplot(data_mean_pr, aes(x=fct_relevel(locomotion, "arboreal", "semi_arboreal", "terrestrial", "semi_fossorial", "semi_aquatic", "aquatic"), y=hbER_all, fill = locomotion)) + 
  geom_violin(trim=TRUE) + 
  coord_trans(y='log') +
  scale_y_continuous(name="ln hbER", limits=c(4, 8.5), breaks = scales::pretty_breaks(n = 9)) +
  scale_fill_manual(values=col_locomotion_vp) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
  geom_hline(yintercept=mean(hbER_all)) + 
  ylab("hbER") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x=element_text(color = "black", size = 15, angle = 45, hjust = .5, vjust = .5),
        axis.text.y=element_text(color = "black", size = 15),
        axis.title.y = element_text(size = 18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black"),
        panel.background = element_blank(),
        legend.position="none") 
hbER_vp_locomotion
pdf("hbER_vp_locomotion.pdf", width=4, height =5)
hbER_vp_locomotion
dev.off()

hbER_vp_hunting <- ggplot(data_mean_pr, aes(x=fct_relevel(hunting, "pursuit", "pounce", "ambush", "occasional", "semi_fossorial", "aquatic"), y=hbER_all, fill = hunting)) +
  geom_violin(trim=TRUE) + 
  coord_trans(y='log') +
  scale_y_continuous(name="ln hbER", limits=c(4, 8.5), breaks = scales::pretty_breaks(n = 9)) +
  scale_fill_manual(values=col_hunting_vp) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
  geom_hline(yintercept=mean(hbER_all)) + 
  ylab("hbER") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x=element_text(color = "black", size = 15, angle = 45, hjust = .5, vjust = .5),
        axis.text.y=element_text(color = "black", size = 15),
        axis.title.y = element_text(size = 18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black"),
        panel.background = element_blank(),
        legend.position="none") 
hbER_vp_hunting
pdf("hbER_vp_hunting.pdf", width=4, height =5)
hbER_vp_hunting
dev.off()

hbER_vp_diet <- ggplot(data_mean_pr, aes(x=fct_relevel(diet, "carnivore", "omnivore", "insectivore", "herbivore", "aquatic"), y=hbER_all, fill = diet)) + 
  geom_violin(trim=TRUE) + 
  coord_trans(y='log') +
  scale_y_continuous(name="ln hbER", limits=c(4, 8.5), breaks = scales::pretty_breaks(n = 9)) +
  scale_fill_manual(values=col_diet_vp) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
  geom_hline(yintercept=mean(hbER_all)) + 
  ylab("hbER") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x=element_text(color = "black", size = 15, angle = 45, hjust = .5, vjust = .5),
        axis.text.y=element_text(color = "black", size = 15),
        axis.title.y = element_text(size = 18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black"),
        panel.background = element_blank(),
        legend.position="none") 
hbER_vp_diet
pdf("hbER_vp_diet.pdf", width=5, height =5)
hbER_vp_diet
dev.off()

#### hbER_sc ####
contMap(tree_prune, lnhbER_sc_all)

hbER_sc_vp_locomotion <- ggplot(data_mean_pr, aes(x=fct_relevel(locomotion, "arboreal", "semi_arboreal", "terrestrial", "semi_fossorial", "semi_aquatic", "aquatic"), y=lnhbER_sc_all, fill = locomotion)) + 
  geom_violin(trim=TRUE) + 
  scale_y_continuous(name="size corrected ln hbER", limits=c(-0.4, 0.5), breaks = scales::pretty_breaks(n = 9)) +
  scale_fill_manual(values=col_locomotion_vp) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
  geom_hline(yintercept=mean(lnhbER_sc_all)) + 
  ylab("hbER_sc") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x=element_text(color = "black", size = 15, angle = 45, hjust = .5, vjust = .5),
        axis.text.y=element_text(color = "black", size = 15),
        axis.title.y = element_text(size = 18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black"),
        panel.background = element_blank(),
        legend.position="none") 
hbER_sc_vp_locomotion
pdf("hbER_sc_vp_locomotion.pdf", width=4, height =5)
hbER_sc_vp_locomotion
dev.off()

hbER_sc_vp_hunting <- ggplot(data_mean_pr, aes(x=fct_relevel(hunting, "pursuit", "pounce", "ambush", "occasional", "semi_fossorial", "aquatic"), y=lnhbER_sc_all, fill = hunting)) +
  geom_violin(trim=TRUE) + 
  scale_y_continuous(name="size corrected ln hbER", limits=c(-0.4, 0.5), breaks = scales::pretty_breaks(n = 9)) +
  scale_fill_manual(values=col_hunting_vp) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
  geom_hline(yintercept=mean(lnhbER_sc_all)) + 
  ylab("hbER_sc") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x=element_text(color = "black", size = 15, angle = 45, hjust = .5, vjust = .5),
        axis.text.y=element_text(color = "black", size = 15),
        axis.title.y = element_text(size = 18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black"),
        panel.background = element_blank(),
        legend.position="none") 
hbER_sc_vp_hunting
pdf("hbER_sc_vp_hunting.pdf", width=4, height =5)
hbER_sc_vp_hunting
dev.off()

hbER_sc_vp_diet <- ggplot(data_mean_pr, aes(x=fct_relevel(diet, "carnivore", "omnivore", "insectivore", "herbivore", "aquatic"), y=lnhbER_sc_all, fill = diet)) + 
  geom_violin(trim=TRUE) + 
  scale_y_continuous(name="size corrected ln hbER", limits=c(-0.4, 0.5), breaks = scales::pretty_breaks(n = 9)) +
  scale_fill_manual(values=col_diet_vp) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
  geom_hline(yintercept=mean(lnhbER_sc_all)) + 
  ylab("hbER_sc") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x=element_text(color = "black", size = 15, angle = 45, hjust = .5, vjust = .5),
        axis.text.y=element_text(color = "black", size = 15),
        axis.title.y = element_text(size = 18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black"),
        panel.background = element_blank(),
        legend.position="none") 
hbER_sc_vp_diet
pdf("hbER_sc_vp_diet.pdf", width=5, height =5)
hbER_sc_vp_diet
dev.off()


