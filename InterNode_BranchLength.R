library(ape)
library(phytools)
library(phangorn)

##################################################################################################################
##################################################################################################################
#                                                                                                                #
#                                    PhyML Non-Optimize vs PhyML Optimize                                        #
#                                                                                                                #
##################################################################################################################
##################################################################################################################

setwd("~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/PhyML-GTR-NonOp_trees")

## Read phyml trees
dir.tree <- dir()[grep(".tree",dir())]
#dir.tree <- dir.tree[-grep("~",dir.tree)]

# Creat a empty list where we will put the trees.
multi.phylo.phyml <- list()

for (i in 1:length(dir.tree)){
  tree <- read.tree(dir.tree[i]) # Read each tree and...
  #plot.phylo(tree)
  multi.phylo.phyml[[i]] <- tree # Put inside the list, at the enda we create a multiphylo object.
}
##################################################################################################################
##################################################################################################################
setwd("~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/Phyml-GTR-Op_trees/")

## Read phyml trees
dir.tree <- dir()[grep(".tree",dir())]
#dir.tree <- dir.tree[-grep("~",dir.tree)]

# Creat a empty list where we will put the trees.
multi.phylo.phyml2 <- list()

for (i in 1:length(dir.tree)){
  tree <- read.tree(dir.tree[i]) # Read each tree and...
  #plot.phylo(tree)
  multi.phylo.phyml2[[i]] <- tree # Put inside the list, at the enda we create a multiphylo object.
}
###############################################################################################################
###############################################################################################################

## Empty vector where the internal branch lengths will put

InternalTree1 <- c()
InternalTree2 <- c()

for (i in 1:length(multi.phylo.phyml)) {
  
  tree1 <- multi.phylo.phyml[[i]]
  tree2 <- multi.phylo.phyml2[[i]]
  
  if((all.equal.phylo(tree1,tree2,use.edge.length = F)==T)){
  
  eg1 <- which(tree1$edge[,2]%in%1:length(tree1$tip.label))
  eg2 <- which(tree2$edge[,2]%in%1:length(tree2$tip.label))
  
  bl1 <- tree1$edge.length[-eg1]
  bl2 <- tree2$edge.length[-eg2]
  
  InternalTree1 <- c(InternalTree1, bl1)
  InternalTree2 <- c(InternalTree2, bl2)
    
  }else{
    
    treeN1 <- makeNodeLabel(tree1, "number")
    treeN2 <- makeNodeLabel(tree1, "number")
    
    nodos1 <- treeN1$node.label
    nodos2 <- treeN2$node.label
    
    comunes <- (nodos2 %in% nodos1)
    
    nodosC <- treeN1$node.label[which(comunes==T)]
    
    for(j in 1:length(nodosC)){
      
      tt1 <- extract.clade(treeN1, nodosC[j])
      tt2 <- extract.clade(treeN2, nodosC[j])
      
      eg1 <- which(tt1$edge[,2]%in%1:length(tt1$tip.label))
      eg2 <- which(tt2$edge[,2]%in%1:length(tt2$tip.label))
      
      bl1 <- treeN1$edge.length[-eg1]
      bl2 <- treeN2$edge.length[-eg2]
      
      InternalTree1 <- c(InternalTree1, bl1)
      InternalTree2 <- c(InternalTree2, bl2)
      
    }
    
  }

}

length(InternalTree2)
length(InternalTree1)

InternalEnd <- cbind(as.data.frame(InternalTree1),as.data.frame(InternalTree2))
head(InternalEnd,3L)

setwd("~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/")

write.table(InternalEnd,"PhyNonOp-PhyOP", sep = ",", col.names = T, row.names = F, quote = F)

##################################################################################################################
##################################################################################################################
#                                                                                                                #
#                                        PhyML Non-Optimize vs RaxML                                             #
#                                                                                                                #
##################################################################################################################
##################################################################################################################

setwd("~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/PhyML-GTR-NonOp_trees")

## Read phyml trees
dir.tree <- dir()[grep(".tree",dir())]
#dir.tree <- dir.tree[-grep("~",dir.tree)]

# Creat a empty list where we will put the trees.
multi.phylo.phyml <- list()

for (i in 1:length(dir.tree)){
  tree <- read.tree(dir.tree[i]) # Read each tree and...
  #plot.phylo(tree)
  multi.phylo.phyml[[i]] <- tree # Put inside the list, at the enda we create a multiphylo object.
}
##################################################################################################################
##################################################################################################################
setwd("~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/RaxML-GTR_trees/")

## Read phyml trees
dir.tree <- dir()[grep(".tree",dir())]
#dir.tree <- dir.tree[-grep("~",dir.tree)]

# Creat a empty list where we will put the trees.
multi.phylo.phyml2 <- list()

for (i in 1:length(dir.tree)){
  tree <- read.tree(dir.tree[i]) # Read each tree and...
  #plot.phylo(tree)
  multi.phylo.phyml2[[i]] <- tree # Put inside the list, at the enda we create a multiphylo object.
}
###############################################################################################################
###############################################################################################################

## Empty vector where the internal branch lengths will put

InternalTree1 <- c()
InternalTree2 <- c()

for (i in 1:length(multi.phylo.phyml)) {
  
  tree1 <- multi.phylo.phyml[[i]]
  tree2 <- multi.phylo.phyml2[[i]]
  
  if((all.equal.phylo(tree1,tree2,use.edge.length = F)==T)){
    
    eg1 <- which(tree1$edge[,2]%in%1:length(tree1$tip.label))
    eg2 <- which(tree2$edge[,2]%in%1:length(tree2$tip.label))
    
    bl1 <- tree1$edge.length[-eg1]
    bl2 <- tree2$edge.length[-eg2]
    
    InternalTree1 <- c(InternalTree1, bl1)
    InternalTree2 <- c(InternalTree2, bl2)
    
  }else{
    
    treeN1 <- makeNodeLabel(tree1, "number")
    treeN2 <- makeNodeLabel(tree1, "number")
    
    nodos1 <- treeN1$node.label
    nodos2 <- treeN2$node.label
    
    comunes <- (nodos2 %in% nodos1)
    
    nodosC <- treeN1$node.label[which(comunes==T)]
    
    for(j in 1:length(nodosC)){
      
      tt1 <- extract.clade(treeN1, nodosC[j])
      tt2 <- extract.clade(treeN2, nodosC[j])
      
      eg1 <- which(tt1$edge[,2]%in%1:length(tt1$tip.label))
      eg2 <- which(tt2$edge[,2]%in%1:length(tt2$tip.label))
      
      bl1 <- treeN1$edge.length[-eg1]
      bl2 <- treeN2$edge.length[-eg2]
      
      InternalTree1 <- c(InternalTree1, bl1)
      InternalTree2 <- c(InternalTree2, bl2)
      
    }
    
  }
  
}

length(InternalTree2)
length(InternalTree1)

InternalEnd <- cbind(as.data.frame(InternalTree1),as.data.frame(InternalTree2))
head(InternalEnd,3L)

setwd("~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/")

write.table(InternalEnd,"PhyNonOp-RaxML", sep = ",", col.names = T, row.names = F, quote = F)


##################################################################################################################
##################################################################################################################
#                                                                                                                #
#                                            PhyML Optimize vs RAxML                                             #
#                                                                                                                #
##################################################################################################################
##################################################################################################################

setwd("~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/Phyml-GTR-Op_trees/")

## Read phyml trees
dir.tree <- dir()[grep(".tree",dir())]
#dir.tree <- dir.tree[-grep("~",dir.tree)]

# Creat a empty list where we will put the trees.
multi.phylo.phyml <- list()

for (i in 1:length(dir.tree)){
  tree <- read.tree(dir.tree[i]) # Read each tree and...
  #plot.phylo(tree)
  multi.phylo.phyml[[i]] <- tree # Put inside the list, at the enda we create a multiphylo object.
}
##################################################################################################################
##################################################################################################################
setwd("~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/RaxML-GTR_trees/")

## Read phyml trees
dir.tree <- dir()[grep(".tree",dir())]
#dir.tree <- dir.tree[-grep("~",dir.tree)]

# Creat a empty list where we will put the trees.
multi.phylo.phyml2 <- list()

for (i in 1:length(dir.tree)){
  tree <- read.tree(dir.tree[i]) # Read each tree and...
  #plot.phylo(tree)
  multi.phylo.phyml2[[i]] <- tree # Put inside the list, at the enda we create a multiphylo object.
}
###############################################################################################################
###############################################################################################################

## Empty vector where the internal branch lengths will put

InternalTree1 <- c()
InternalTree2 <- c()

for (i in 1:length(multi.phylo.phyml)) {
  
  tree1 <- multi.phylo.phyml[[i]]
  tree2 <- multi.phylo.phyml2[[i]]
  
  if((all.equal.phylo(tree1,tree2,use.edge.length = F)==T)){
    
    eg1 <- which(tree1$edge[,2]%in%1:length(tree1$tip.label))
    eg2 <- which(tree2$edge[,2]%in%1:length(tree2$tip.label))
    
    bl1 <- tree1$edge.length[-eg1]
    bl2 <- tree2$edge.length[-eg2]
    
    InternalTree1 <- c(InternalTree1, bl1)
    InternalTree2 <- c(InternalTree2, bl2)
    
  }else{
    
    treeN1 <- makeNodeLabel(tree1, "number")
    treeN2 <- makeNodeLabel(tree1, "number")
    
    nodos1 <- treeN1$node.label
    nodos2 <- treeN2$node.label
    
    comunes <- (nodos2 %in% nodos1)
    
    nodosC <- treeN1$node.label[which(comunes==T)]
    
    for(j in 1:length(nodosC)){
      
      tt1 <- extract.clade(treeN1, nodosC[j])
      tt2 <- extract.clade(treeN2, nodosC[j])
      
      eg1 <- which(tt1$edge[,2]%in%1:length(tt1$tip.label))
      eg2 <- which(tt2$edge[,2]%in%1:length(tt2$tip.label))
      
      bl1 <- treeN1$edge.length[-eg1]
      bl2 <- treeN2$edge.length[-eg2]
      
      InternalTree1 <- c(InternalTree1, bl1)
      InternalTree2 <- c(InternalTree2, bl2)
      
    }
    
  }
  
}

length(InternalTree2)
length(InternalTree1)

InternalEnd <- cbind(as.data.frame(InternalTree1),as.data.frame(InternalTree2))
head(InternalEnd,3L)

setwd("~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/")

write.table(InternalEnd,"PhyOp-RaxML", sep = ",", col.names = T, row.names = F, quote = F)
