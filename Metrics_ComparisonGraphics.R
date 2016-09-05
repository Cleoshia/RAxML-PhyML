library(ggplot2)

setwd("~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/")

Pop.R <- read.csv("PhyOP-Rax.dist", header = T)
Pnp.R <- read.csv("PhyNON-Rax.dist", header = T)
Pop.Pnp <- read.csv("PhyOP-PhyNon.dist", header = T)


RF.d <- rbind(as.matrix(Pop.R$symmetric.difference),
              as.matrix(Pnp.R$symmetric.difference),
              as.matrix(Pop.Pnp$symmetric.difference))

RF.d <- cbind(RF.d,
        as.matrix(c(rep("1",length(Pop.Pnp$Tree)),rep("1.1",length(Pop.Pnp$Tree)),rep("1.2",length(Pop.Pnp$Tree)))),
        as.matrix(c(rep("PhyML.Op-RAxML",length(Pop.Pnp$Tree)),rep("PhyML.NonOp-RAxML",length(Pop.Pnp$Tree)),rep("PhyML.Op-PhyML.NonOP",length(Pop.Pnp$Tree)))),
        as.matrix(c(rep(1:length(Pop.Pnp$Tree),3))))

RF.d


RF <- ggplot()+ geom_bar(aes(x=as.factor(RF.d[,4]),y=as.numeric(RF.d[,1]),fill=RF.d[,3]),stat="identity", position = "dodge")+
  ylab("Robinson-Foulds Distance")+ xlab("Phylogeny")+
  theme(axis.text = element_text(size = 10),
        axis.title.y = element_text(size = 20,),
        axis.title.x = element_text(size = 20,),
        legend.position = c(.48,.8 ),
        legend.background = element_rect(fill="transparent"),
        legend.key = element_rect(fill="transparent"),
        legend.title = element_text(colour="black", size=20, face="bold"),
        legend.text = element_text(colour="black", size = 18, face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_line(colour = "gray97"),
        panel.border = element_blank())+
  scale_x_discrete(limits=as.factor(1:50),expand = c(.01, .01))+
  scale_fill_discrete(name="Comparisons", 
                      labels=c("PhyML.NonOp vs RAxML","PhyML.NonOp vs PhyML.NonOp","PhyML.Op vs RAxML"))+
  scale_y_continuous(expand = c(0,0))


RF

png(filename = "~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/RF-Distance.png",
    width = 4000,height=1900,res = 300)
RF
dev.off()

########################################################################33

RF.d2 <- rbind(as.matrix(Pop.R$branch.score.difference),
              as.matrix(Pnp.R$branch.score.difference),
              as.matrix(Pop.Pnp$branch.score.difference))

RF.d2 <- cbind(RF.d2,
              as.matrix(c(rep("1",length(Pop.Pnp$Tree)),rep("1.1",length(Pop.Pnp$Tree)),rep("1.2",length(Pop.Pnp$Tree)))),
              as.matrix(c(rep("PhyML.Op-RAxML",length(Pop.Pnp$Tree)),rep("PhyML.NonOp-RAxML",length(Pop.Pnp$Tree)),rep("PhyML.Op-PhyML.NonOP",length(Pop.Pnp$Tree)))),
              as.matrix(c(rep(1:length(Pop.Pnp$Tree),3))))

RF.d2 <- RF.d2[-which(as.numeric(RF.d2[,1])>5),]

BS <- ggplot()+ geom_bar(aes(x=as.factor(RF.d2[,4]),y=as.numeric(RF.d2[,1]),fill=RF.d2[,3]),stat="identity", position = "dodge")+
  ylab("Branch Score Difference")+ xlab("Phylogeny")+
  theme(axis.text = element_text(size = 10),
        axis.title.y = element_text(size = 20,),
        axis.title.x = element_text(size = 20,),
        legend.position = c(.48,.9 ),
        legend.background = element_rect(fill="transparent"),
        legend.key = element_rect(fill="transparent"),
        legend.title = element_text(colour="black", size=20, face="bold"),
        legend.text = element_text(colour="black", size = 18, face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_line(colour = "gray97"),
        panel.border = element_blank())+
  scale_x_discrete(limits=as.factor(1:50),expand = c(.01, .01))+
  scale_fill_discrete(name="Comparisons", 
                      labels=c("PhyML.NonOp vs RAxML","PhyML.NonOp vs PhyML.NonOp","PhyML.Op vs RAxML"))+
  scale_y_continuous(expand = c(0,0))
  

BS

png(filename = "~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/BS-Distance.png",
    width = 4000,height=1900,res = 300)
BS
dev.off()
