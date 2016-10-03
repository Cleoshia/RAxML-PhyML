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

comparison1 <- read.csv("PhyOP-Rax.dist")
comparison2 <- read.csv("PhyNON-Rax.dist")
comparison3 <- read.csv("PhyOP-PhyNon.dist")

g <- ggplot()

g <- g+  
  
  geom_line(aes(x=1:length(comparison1$branch.score.difference),
                y=(comparison1$symmetric.difference)+1,colour="red"),size=1)+
  geom_line(aes(x=1:length(comparison1$branch.score.difference),
                y=(comparison2$symmetric.difference)+.5,colour="blue"),size=1)+
  geom_line(aes(x=1:length(comparison1$symmetric.difference),
                y=comparison3$symmetric.difference,colour="green"),size=1)+
  
  geom_point(aes(x=1:length(comparison1$branch.score.difference),
                 y=(comparison1$symmetric.difference)+1,colour="red"), size=2)+
  geom_point(aes(x=1:length(comparison1$branch.score.difference),
                 y=(comparison2$symmetric.difference)+.5,colour="blue"), size=2)+
  geom_point(aes(x=1:length(comparison1$symmetric.difference),
                 y=comparison3$symmetric.difference,colour="green"), size=2)+
  scale_colour_discrete(name="Comparisons", 
                      labels=c("Comparison1","Comparison2","Comparison3"))+
  scale_x_discrete(limits=as.factor(1:50),expand = c(.01, .01))+
  xlab("Phylogenies")+ylab("Robinson Fould Distance")+ggtitle("D")+
  
  theme(axis.text = element_text(size = 10),
        axis.title.y = element_text(size = 20,),
        axis.title.x = element_text(size = 20,),
        legend.position = c(.47,.8 ),
        legend.background = element_rect(fill="transparent"),
        legend.key = element_rect(fill="transparent"),
        legend.title = element_text(colour="black", size=28, face="bold"),
        legend.text = element_text(colour="black", size = 25, face = "bold"),
        legend.key.size = unit(1, "cm"),
        plot.title=element_text(size=40))

g

png(filename = "~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/RF-Distance.png",
    width = 4000,height=1900,res = 300)
g
dev.off()

  
########################################################################################

g <- ggplot()

g <- g+  
  
  geom_line(aes(x=1:length(comparison1$branch.score.difference),
                y=log((comparison1$branch.score.difference)+1),colour="red"),size=1)+
  geom_line(aes(x=1:length(comparison1$branch.score.difference),
                y=log((comparison2$branch.score.difference)+.5),colour="blue"),size=1)+
  geom_line(aes(x=1:length(comparison1$symmetric.difference),
                y=log(comparison3$branch.score.difference),colour="green"),size=1)+
  
  geom_point(aes(x=1:length(comparison1$branch.score.difference),
                 y=log((comparison1$branch.score.difference)+1),colour="red"), size=2)+
  geom_point(aes(x=1:length(comparison1$branch.score.difference),
                 y=log((comparison2$branch.score.difference)+.5),colour="blue"), size=2)+
  geom_point(aes(x=1:length(comparison1$symmetric.difference),
                 y=log(comparison3$branch.score.difference),colour="green"), size=2)+
  
  scale_colour_discrete(name="Comparisons", 
                        labels=c("Comparison1","Comparison2","Comparison3"))+
  scale_x_discrete(limits=as.factor(1:50),expand = c(.01, .01))+
  
  xlab("Phylogenies")+ylab("log Branch Score Difference")+ggtitle("E")+
  
  theme(axis.text = element_text(size = 10),
        axis.title.y = element_text(size = 20,),
        axis.title.x = element_text(size = 20,),
        legend.position = c(.47,.8 ),
        legend.background = element_rect(fill="transparent"),
        legend.key = element_rect(fill="transparent"),
        legend.title = element_text(colour="black", size=28, face="bold"),
        legend.text = element_text(colour="black", size = 25, face = "bold"),
        legend.key.size = unit(1, "cm"),
        plot.title=element_text(size=40))

g

png(filename = "~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/BS-Distance.png",
    width = 4000,height=1900,res = 300)
g
dev.off()

  
