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
# Then correlates the RAxML branch length against PhyML Branch Length of each species
# The corrwlation method is pearson
#
# The final outcome, is graphic with the Pearson estimate and squared Pearson estimate

## Load the libraries

library(phangorn)
library(ape)
library(phytools)
library(classInt)
library(ggplot2)

setwd("~/Documentos/Omar/Projects/PhyML-RAxML/PhyML_Trees/")

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
  
  tax <- multi.phylo.phyml[[i]]$tip.label
  dead.pool <- c(dead.pool,tax) ## Species from phylogenies
  eg <- which(multi.phylo.phyml[[i]]$edge[,2]%in%1:length(multi.phylo.phyml[[i]]$tip.label))
  aa <- multi.phylo.phyml[[i]]$edge.length[eg] # Branch length
  
  bl <- c(bl,aa) ## Brach length
  
}

##########################################################################
#### RAxML


setwd("~/Documentos/Omar/Projects/PhyML-RAxML/RAxML_Trees/")

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
  
  bl.r <- c(bl,aa) #Branch length
  
}

# Some species names differs
out <- which((dead.pool%in%dead.pool.r)==F)

# Create a data frame with all data 
bl.data <- data.frame(Sp=dead.pool[-out],
                      BL.phyml=bl[-out],
                      BL.raxml=rep(0,length(bl[-out]))) # BL for RAxML is empty


bl.data 

# Now find the species names from bl.data in dead.pool.r, and assing its BL value

for(i in 1:length(bl.data$Sp)){
  
  tt <- bl.r[which(dead.pool.r%in%bl.data$Sp[i])]
  
  bl.data$BL.raxml[i] <- tt
  
}


## Made the quatiles range for RAxML and PhyML
brks.p <- classIntervals(bl.data$BL.phyml,n=5,style = "quantile")
brks.r <- classIntervals(bl.data$BL.raxml,n=5,style = "quantile")

brks.p <- brks.p$brks
brks.r <- brks.r$brks

brks.p
brks.r
## Classification given quantiles intervals.
Class.Phy <- findInterval(bl.data$BL.phyml,brks.p) # Classify each BL value in brks categories
Class.Rax <- findInterval(bl.data$BL.raxml,brks.r) # Classify each BL value in brks categories

## Do the Pearson correlation (PhyML vs RAxML)
pearson <- cor.test(y=bl.data$BL.phyml[which(bl.data$BL.phyml<max(bl.data$BL.phyml))],
                    x=bl.data$BL.raxml[which(bl.data$BL.raxml<max(bl.data$BL.raxml))])

## Pearson's R
pearson$estimate
## Pearson's squared R
(pearson$estimate)^2

## Make the graphic
p <- ggplot() + geom_point(aes(x=bl.data$BL.raxml[-which(Class.Rax%in%c(5,6))],
                               y=bl.data$BL.phyml[-which(Class.Phy%in%c(5,6))]))+
  #geom_smooth(aes(x=bl.data$BL.raxml[which(bl.data$BL.raxml<max(bl.data$BL.raxml))],
  #               y=bl.data$BL.phyml[which(bl.data$BL.phyml<max(bl.data$BL.phyml))]))+
  ylab("PhyML Branch Length")+ xlab("RAxML Branch Length")+
  geom_abline(intercept=0,slope=pearson$estimate)+
  geom_label(label=paste("R²",round((pearson$estimate^2),digits=4),sep = ": "),
             aes(x=0.07,y=0.06),
             size=12)+
  geom_label(label=paste("R",round(pearson$estimate,digits=2),sep = ": "),
             aes(x=0.07,y=0.05),
             size=12)+
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 20,),
        axis.title.x = element_text(size = 20,))

p

# Save the png file
png(filename = "~/Documentos/Omar/Projects/PhyML-RAxML/June6.png",
    width = 2800,height=1900,res = 300)
p
dev.off()

# Find the species with the highs BL for PhyML and RAxML
bl.data$Sp[which(bl.data$BL.phyml==max(bl.data$BL.phyml))]
bl.data$Sp[which(bl.data$BL.raxml==max(bl.data$BL.raxml))]

# Something interesting, the species differs form PhyML to RAxML
