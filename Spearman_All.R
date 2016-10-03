library(ggplot2)
library(classInt)


setwd("~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/")

terminals <- read.csv("BL.data-Results")

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

nodes <- read.csv("PhyOp-RaxML")
col2 <- as.matrix(rep("N", length(nodes$InternalTree1)))

terminals2 <- as.matrix(cbind(terminals$BL.phyml.Op,terminals$BL.raxml))
head(terminals2)
col1 <- as.matrix(rep("T", length(terminals2[,1])))

col <- rbind(col1,col2)

end <- rbind(terminals2,as.matrix(nodes))
end <- as.data.frame(end)

end <- cbind(end, col)


head(end, 5L)

# Graphic

spearman <- cor.test(end$InternalTree1,end$InternalTree2, method = "spearman")
spearman$estimate

# Classification with intervals

brks <- classIntervals(end$InternalTree2,n=5,style = "quantile")
brks <- brks$brks

Class <- findInterval(end$InternalTree1,brks)

p <- ggplot() + geom_point(aes(x=log(end$InternalTree1[-which(Class%in%c(5,6))]),
                               y=log(end$InternalTree2[-which(Class%in%c(5,6))]),
                               colour=end$col[-which(Class%in%c(5,6))]))+
  #geom_smooth(aes(x=bl.data$BL.raxml[which(bl.data$BL.raxml<max(bl.data$BL.raxml))],
  #               y=bl.data$BL.phyml[which(bl.data$BL.phyml<max(bl.data$BL.phyml))]))+
  ylab("log PhyML-Optimize Branch Length")+ xlab("log RAxML Branch Length")+
  geom_abline(intercept=0,slope=spearman$estimate)+
  geom_label(label=paste("rho",round((spearman$estimate),digits=4),sep = ": "),
             aes(x=-17,y=0),
             size=12)+
  #geom_label(label=paste("p-value",pearson$p.value,sep = ": "),
  #           aes(x=0.05,y=0.045),
  #           size=12)+
  scale_colour_discrete(name="Branch Lengths",
                        labels=c("Nodes", "Terminals"))+
  ggtitle("C")+
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 20,),
        axis.title.x = element_text(size = 20,),
        plot.title=element_text(size=40),
        legend.position=c(.17,.75),
        legend.title = element_text(size=25),
        legend.text = element_text(size = 25),
        legend.key.size = unit(1, "cm"))


p

png(filename = "~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/Comparison1_All.png",
    width = 2800,height=1900,res = 300)
p
dev.off()


##################################################################################
##################################################################################
