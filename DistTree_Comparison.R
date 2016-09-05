library(ape)
library(phytools)
library(phangorn)


setwd("~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/PhyML-GTR-NonOp_trees/")

dir.tree <- dir()
dir.tree <- dir.tree[-grep("~", dir.tree)]
dir.tree

PhymlOp <- list()
PhymlNon <- list()
Raxml <- list()

for (i in 1:length(dir.tree)){
  
  setwd("~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/Phyml-GTR-Op_trees/")
  
  tree <- read.tree(dir.tree[i]) # Read each tree and...
  print(dir.tree[i])
  #plot.phylo(tree)
  PhymlOp[[i]] <- tree # Put inside the list, at the enda we create a multiphylo object.

###########################################################################################
  setwd("~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/PhyML-GTR-NonOp_trees/")
  print(dir.tree[i])
  tree <- read.tree(dir.tree[i]) # Read each tree and...
  #plot.phylo(tree)
  PhymlNon[[i]] <- tree # Put inside the list, at the enda we create a multiphylo object.
  
############################################################################################  
  setwd("~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/RaxML-GTR_trees/")
  print(dir.tree[i])
  tree <- read.tree(dir.tree[i]) # Read each tree and...
  #plot.phylo(tree)
  Raxml[[i]] <- tree # Put inside the list, at the enda we create a multiphylo object.
  
}

###################################################################
#               METRICS- PHYML OPTIMIZE VS RAXML                  #
###################################################################

PhyOpRax.out <- matrix(0,ncol = 4, nrow = length(dir.tree))

PhyOpRax.out

for (i in 1:length(dir.tree)){
  
  tree1 <- PhymlOp[[i]]
  tree2 <- Raxml[[i]]
  
  dist.r <- treedist(tree1,tree2)
  
  PhyOpRax.out[i,] <- dist.r
  
}

PhyOpRax.out <- cbind(dir.tree,PhyOpRax.out)
colnames(PhyOpRax.out) <- c("Tree",names(dist.r))
PhyOpRax.out

setwd("~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/")

write.table(PhyOpRax.out, "PhyOP-Rax.dist", quote = F, col.names = T, row.names = F, sep = ",")

###################################################################
#           METRICS- PHYML NON-OPTIMIZE VS RAXML                  #
###################################################################

PhyNonRax.out <- matrix(0,ncol = 4, nrow = length(dir.tree))

PhyNonRax.out

for (i in 1:length(dir.tree)){
  
  tree1 <- PhymlNon[[i]]
  tree2 <- Raxml[[i]]
  
  dist.r <- treedist(tree1,tree2)
  
  PhyNonRax.out[i,] <- dist.r
  
}

PhyNonRax.out <- cbind(dir.tree,PhyNonRax.out)
colnames(PhyNonRax.out) <- c("Tree",names(dist.r))
PhyNonRax.out

setwd("~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/")

write.table(PhyNonRax.out, "PhyNON-Rax.dist", quote = F, col.names = T, row.names = F, sep = ",")

###################################################################
#        METRICS- PHYML NON-OPTIMIZE VS PHYML OPTIMIZE            #
###################################################################

PhyOpPhyNon.out <- matrix(0,ncol = 4, nrow = length(dir.tree))

PhyOpPhyNon.out

for (i in 1:length(dir.tree)){
  
  tree1 <- PhymlOp[[i]]
  tree2 <- PhymlNon[[i]]
  
  dist.r <- treedist(tree1,tree2)
  
  PhyOpPhyNon.out[i,] <- dist.r
  
}

PhyOpPhyNon.out <- cbind(dir.tree,PhyOpPhyNon.out)
colnames(PhyOpPhyNon.out) <- c("Tree",names(dist.r))
PhyOpPhyNon.out

setwd("~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/")

write.table(PhyOpPhyNon.out, "PhyOP-PhyNon.dist", quote = F, col.names = T, row.names = F, sep = ",")
