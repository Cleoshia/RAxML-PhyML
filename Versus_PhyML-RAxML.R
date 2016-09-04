## Autor
# Leon-Alvarado, Omar Daniel.
# leon.alvarado12@gmail.com

## License
# The follow script was created under the GNU/GPLv2. license.
# http://www.gnu.org/licenses/old-licenses/gpl-2.0-standalone.html

## Title
# RAxML Branch Length vs PhyML Branch Length 

## Description
# This script organize the branch length of each species in both phylogenies (RAxML and PhyML),
# and for each treatment, Phyml-Optimize, Phyml-NonOptimize and RAxML
# The final outcome, is data frame, with 4 columns.
# 1: Species names
# 2: Phyml-NonOptimize Branch Length
# 3: Phyml-Optimize Branch Length
# 4: RAxML Branch Length

## Load the libraries

library(phangorn)
library(ape)
library(phytools)
library(classInt)
library(ggplot2)

#########################################################################################
#########################################################################################
#                          NON-OPTIMIZE BRANCH LENGTH PHYML                             #
#########################################################################################
#########################################################################################

## First, the Phyml-Nonoptimize trees

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

## Assign names to each phylogeny
names(multi.phylo.phyml) <- dir.tree
# Create two empty vectors, one for the species and second for branch length values
dead.pool <- c()
bl <- c()


## Extract all species terminals

for (i in 1:length(multi.phylo.phyml)){
  
  tax <- multi.phylo.phyml[[i]]$tip.label ## Extrac the tip labels or taxa names
  dead.pool <- c(dead.pool,tax) ## Species from phylogenies
  
  # Here, each tip label area equivalent to a number from 1 to length of tip labels
  # Those number will be find in the colum 2 of the edge data
  
  eg <- which(multi.phylo.phyml[[i]]$edge[,2]%in%1:length(multi.phylo.phyml[[i]]$tip.label))
  
  # And given the position in eg, the branch length of each taxa will be extracted
  
  aa <- multi.phylo.phyml[[i]]$edge.length[eg] # Branch length
  
  # And then, assigned to the bl vector
  
  bl <- c(bl,aa) ## Brach length
  
}


##########################################################################
##########################################################################
#                   OPTIMIZE BRANCH LENGTH PHYML TREES                   #
##########################################################################
##########################################################################


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

## Assign names to each phylogeny
names(multi.phylo.phyml2) <- dir.tree
# Create two empty vectors, one for the species and second for branch length values
dead.pool.p <- c()
bl.p <- c()


## Extract all species terminals

for (i in 1:length(multi.phylo.phyml2)){
  
  tax <- multi.phylo.phyml2[[i]]$tip.label ## Extrac the tip labels or taxa names
  dead.pool.p <- c(dead.pool.p,tax) ## Species from phylogenies
  
  # Here, each tip label area equivalent to a number from 1 to length of tip labels
  # Those number will be find in the colum 2 of the edge data
  
  eg <- which(multi.phylo.phyml2[[i]]$edge[,2]%in%1:length(multi.phylo.phyml2[[i]]$tip.label))
  
  # And given the position in eg, the branch length of each taxa will be extracted
  
  aa <- multi.phylo.phyml2[[i]]$edge.length[eg] # Branch length
  
  # And then, assigned to the bl vector
  
  bl.p <- c(bl.p,aa) ## Brach length
  
}


##########################################################################
##########################################################################
#                             RAxML TREES                                #
##########################################################################
##########################################################################


setwd("~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/RaxML-GTR_trees/")

## Read phyml trees
dir.tree <- dir()[grep(".tree",dir())]
#dir.tree <- dir.tree[-grep("~",dir.tree)]

# Creat a empty list where we will put the trees.
multi.phylo.raxml <- list()

for (i in 1:length(dir.tree)){
  tree <- read.tree(dir.tree[i]) # Read each tree and...
  #plot.phylo(tree)
  multi.phylo.raxml[[i]] <- tree # Put inside the list, at the enda we create a multiphylo object.
}

## Assign names to each phylogeny
names(multi.phylo.raxml) <- dir.tree

## Create two empty vectors, one for species and second for branch length values
dead.pool.r <- c()
bl.r <- c()

## Extract all species terminals

for (i in 1:length(multi.phylo.raxml)){
  
  tax <- multi.phylo.raxml[[i]]$tip.label
  dead.pool.r <- c(dead.pool.r,tax) # Species
  eg <- which(multi.phylo.raxml[[i]]$edge[,2]%in%1:length(multi.phylo.raxml[[i]]$tip.label))
  aa <- multi.phylo.raxml[[i]]$edge.length[eg] #Branch length
  
  bl.r <- c(bl.r,aa) #Branch length
  
}

# Check if some species have different names

which((dead.pool%in%dead.pool.r)==F)
which((dead.pool%in%dead.pool.p)==F)
which((dead.pool.p%in%dead.pool.r)==F)


#############################################################################
#                          CREATE THE DATA OUTCOME                          #
#############################################################################

# Create a data frame with all data 
bl.data <- data.frame(Sp=dead.pool,
                      BL.phyml.NonOp=bl,
                      BL.phyml.Op=rep(0,length(bl)), # BL for Optimize Phyml is empty
                      BL.raxml=rep(0,length(bl))) # BL for RAxML is empty


bl.data 

# Now find the species names from bl.data in dead.pool.r, and assing its BL value

for(i in 1:length(bl.data$Sp)){
  
  tt <- bl.r[which(dead.pool.r%in%bl.data$Sp[i])]
  
  bl.data$BL.raxml[i] <- tt
  
  tt2 <- bl.p[which(dead.pool.p%in%bl.data$Sp[i])]
  
  bl.data$BL.phyml.Op[i] <- tt2
  
}

## Check the results

bl.data


########################################################################
#                         SAVE THE RESULTS                             #
########################################################################

setwd("~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/")

write.table(bl.data, "BL.data-Results", quote = F, col.names = T, row.names = F, sep = ",")


###################################################################################################################
