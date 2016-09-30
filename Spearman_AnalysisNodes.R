library(ggplot2)
library(classInt)


setwd("~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/")

#########################################################
# Comparisons:
#
# 1. PhyML Optimzed vs RAxML
#
# 2. PhyML NonOptimized vs RAxML
#
# 3. PhyML Optimized vs PhyML NonOptimized
#
#########################################################

#
## Comparison 1
#

comparison1 <- read.csv("PhyOp-RaxML", header = T)

head(comparison1, 4L) 

# Correlation

spearman <- cor.test(comparison1$InternalTree1,comparison1$InternalTree2, method = "spearman")
spearman$estimate

# Classification with intervals

brks <- classIntervals(comparison1$InternalTree1,n=5,style = "quantile")
brks <- brks$brks

Class <- findInterval(comparison1$InternalTree1,brks)

# Graphic

p <- ggplot() + geom_point(aes(x=log(comparison1$InternalTree1[-which(Class%in%c(5,6))]),
                               y=log(comparison1$InternalTree2[-which(Class%in%c(5,6))])))+
  #geom_smooth(aes(x=bl.data$BL.raxml[which(bl.data$BL.raxml<max(bl.data$BL.raxml))],
  #               y=bl.data$BL.phyml[which(bl.data$BL.phyml<max(bl.data$BL.phyml))]))+
  ylab("log PhyML-Optimize Branch Length (Nodes)")+ xlab("log RAxML Branch Length (Nodes)")+
  geom_abline(intercept=0,slope=spearman$estimate)+
  geom_label(label=paste("rho",round((spearman$estimate),digits=4),sep = ": "),
             aes(x=-16,y=-3),
             size=12)+
  #geom_label(label=paste("p-value",pearson$p.value,sep = ": "),
  #           aes(x=0.05,y=0.045),
  #           size=12)+
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 20,),
        axis.title.x = element_text(size = 20,))

p

png(filename = "~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/Nodes_PhyOpRaX.png",
    width = 2800,height=1900,res = 300)
p
dev.off()

##################################################################################
##################################################################################

#
## Comparison 2
#

comparison2 <- read.csv("PhyNonOp-RaxML", header = T)

head(comparison1, 4L) 

# Correlation

spearman <- cor.test(comparison2$InternalTree1,comparison2$InternalTree2, method = "spearman")
spearman$estimate

# Classification with intervals

brks <- classIntervals(comparison2$InternalTree2,n=5,style = "quantile")
brks <- brks$brks

Class <- findInterval(comparison2$InternalTree1,brks)

# Graphic

p <- ggplot() + geom_point(aes(x=log(comparison2$InternalTree1[-which(Class%in%c(5,6))]),
                               y=log(comparison2$InternalTree2[-which(Class%in%c(5,6))])))+
  #geom_smooth(aes(x=bl.data$BL.raxml[which(bl.data$BL.raxml<max(bl.data$BL.raxml))],
  #               y=bl.data$BL.phyml[which(bl.data$BL.phyml<max(bl.data$BL.phyml))]))+
  ylab("log PhyML-NonOptimize Branch Length (Nodes)")+ xlab("log RAxML Branch Length (Nodes)")+
  geom_abline(intercept=0,slope=spearman$estimate)+
  geom_label(label=paste("rho",round((spearman$estimate),digits=4),sep = ": "),
             aes(x=-16,y=-3),
             size=12)+
  #geom_label(label=paste("p-value",pearson$p.value,sep = ": "),
  #           aes(x=0.05,y=0.045),
  #           size=12)+
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 20,),
        axis.title.x = element_text(size = 20,))

p

png(filename = "~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/Nodes_PhyNonOpRaX.png",
    width = 2800,height=1900,res = 300)
p
dev.off()

#######################################################################################
#######################################################################################

#
## Comparison 3
#

comparison3 <- read.csv("PhyNonOp-PhyOP", header = T)

head(comparison3, 4L) 

# Correlation

spearman <- cor.test(comparison3$InternalTree1,comparison3$InternalTree2, method = "spearman")
spearman$estimate

# Classification with intervals

brks <- classIntervals(comparison3$InternalTree1,n=5,style = "quantile")
brks <- brks$brks

Class <- findInterval(comparison3$InternalTree1,brks)

# Graphic

p <- ggplot() + geom_point(aes(x=log(comparison3$InternalTree1[-which(Class%in%c(5,6))]),
                               y=log(comparison3$InternalTree2[-which(Class%in%c(5,6))])))+
  #geom_smooth(aes(x=bl.data$BL.raxml[which(bl.data$BL.raxml<max(bl.data$BL.raxml))],
  #               y=bl.data$BL.phyml[which(bl.data$BL.phyml<max(bl.data$BL.phyml))]))+
  ylab("log PhyML-Optimize Branch Length (Nodes)")+ xlab("log PhyML-NonOptimize Branch Length (Nodes)")+
  geom_abline(intercept=0,slope=spearman$estimate)+
  geom_label(label=paste("rho",round((spearman$estimate),digits=4),sep = ": "),
             aes(x=-16,y=-3),
             size=12)+
  #geom_label(label=paste("p-value",pearson$p.value,sep = ": "),
  #           aes(x=0.05,y=0.045),
  #           size=12)+
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 20,),
        axis.title.x = element_text(size = 20,))

p

png(filename = "~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/Nodes_PhyOpPhyNonOp.png",
    width = 2800,height=1900,res = 300)
p
dev.off()
