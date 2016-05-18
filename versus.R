library(phangorn)
library(ape)
library(phytools)
library(ggplot2)

setwd("~/Documentos/Omar/Tesis/Taxa/Trees/")

## Read phyml trees

dir.tree <- dir()[grep("_tree",dir())]
#dir.tree <- dir.tree[-grep("~",dir.tree)]

# Creat a empty list where we will put the trees.
multi.phylo.phyml <- list()

for (i in 1:length(dir.tree)){
  tree <- read.tree(dir.tree[i]) # Read each tree and...
  #plot.phylo(tree)
  multi.phylo.phyml[[i]] <- tree # Put inside the list, at the enda we create a multiphylo object.
}

names(multi.phylo.phyml) <- dir.tree

dead.pool <- c()
bl <- c()


## Extract all species terminals

for (i in 1:length(multi.phylo.phyml)){
  
  tax <- multi.phylo.phyml[[i]]$tip.label
  dead.pool <- c(dead.pool,tax)
  eg <- which(multi.phylo.phyml[[i]]$edge[,2]%in%1:length(multi.phylo.phyml[[i]]$tip.label))
  aa <- multi.phylo.phyml[[i]]$edge.length[eg]
  
  bl <- c(bl,aa)
  
}

##########################################################################
#### RAxML


setwd("~/Documentos/Omar/Tesis/Taxa/raxml/All.trees/")

## Read phyml trees

dir.tree <- dir()[grep("_bestTree",dir())]
#dir.tree <- dir.tree[-grep("~",dir.tree)]

# Creat a empty list where we will put the trees.
multi.phylo.raxml <- list()

for (i in 1:length(dir.tree)){
  tree <- read.tree(dir.tree[i]) # Read each tree and...
  #plot.phylo(tree)
  multi.phylo.raxml[[i]] <- tree # Put inside the list, at the enda we create a multiphylo object.
}

names(multi.phylo.raxml) <- dir.tree

dead.pool.r <- c()
bl.r <- c()


## Extract all species terminals

for (i in 1:length(multi.phylo.raxml)){
  
  tax <- multi.phylo.raxml[[i]]$tip.label
  dead.pool.r <- c(dead.pool.r,tax)
  eg <- which(multi.phylo.raxml[[i]]$edge[,2]%in%1:length(multi.phylo.raxml[[i]]$tip.label))
  aa <- multi.phylo.raxml[[i]]$edge.length[eg]
  
  bl.r <- c(bl,aa)
  
}


dead.pool.r <- dead.pool.r[which(dead.pool.r%in%dead.pool)]
bl.r <- bl.r[which(dead.pool.r%in%dead.pool)]

dead.pool <- dead.pool[which(dead.pool%in%dead.pool.r)]
bl <- bl[which(dead.pool%in%dead.pool.r)]

bl.data <- data.frame(Sp=dead.pool,
                      BL.phyml=bl,
                      BL.raxml=rep(0,length(bl)))



for(i in 1:length(bl.data$Sp)){
  
 bl.data$BL.raxml[i] <- bl.r[which(dead.pool.r%in%bl.data$Sp[i])]
    
}

pearson <- cor.test(y=bl.data$BL.phyml[which(bl.data$BL.phyml<max(bl.data$BL.phyml))],
         x=bl.data$BL.raxml[which(bl.data$BL.raxml<max(bl.data$BL.raxml))])


pearson$estimate

p <- ggplot() + geom_point(aes(x=bl.data$BL.raxml[which(bl.data$BL.raxml<max(bl.data$BL.raxml))],
                   y=bl.data$BL.phyml[which(bl.data$BL.phyml<max(bl.data$BL.phyml))]))+
  #geom_smooth(aes(x=bl.data$BL.raxml[which(bl.data$BL.raxml<max(bl.data$BL.raxml))],
  #               y=bl.data$BL.phyml[which(bl.data$BL.phyml<max(bl.data$BL.phyml))]))+
  ylab("PhyML Branch Length")+ xlab("RAxML Branch Length")+
  geom_abline(intercept=0,slope=pearson$estimate)+
  geom_label(label=paste("R²",round((pearson$estimate^2),digits=2),sep = ": "),aes(x=0.2,y=0.6),
             size=12)+
  geom_label(label=paste("R",round(pearson$estimate,digits=2),sep = ": "),aes(x=0.2,y=0.5),
             size=12)+
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 20,),
        axis.title.x = element_text(size = 20,))

p

png(filename = "~/Documentos/Omar/Tesis/RaxmlvPhyml.png",width = 2800,height=1900,res = 300)
p
dev.off()

bl.data$Sp[which(bl.data$BL.phyml==max(bl.data$BL.phyml))]
bl.data$Sp[which(bl.data$BL.raxml==max(bl.data$BL.raxml))]
