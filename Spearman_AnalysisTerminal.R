library(ggplot2)
library(classInt)

###################################################################################################################

setwd("~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/")

bl.data <- read.csv("BL.data-Results", header = T)


## Made the quatiles range for RAxML and PhyML
brks.p <- classIntervals(bl.data$BL.phyml.NonOp,n=5,style = "quantile")
brks.r <- classIntervals(bl.data$BL.raxml,n=5,style = "quantile")
brks.p2 <- classIntervals(bl.data$BL.phyml.Op,n=5,style = "quantile")


brks.p <- brks.p$brks
brks.r <- brks.r$brks
brks.p2 <-brks.p2$brks 

brks.p
brks.r
brks.p2
## Classification given quantiles intervals.
Class.Phy <- findInterval(bl.data$BL.phyml.NonOp,brks.p) # Classify each BL value in brks categories
Class.Rax <- findInterval(bl.data$BL.raxml,brks.r) # Classify each BL value in brks categories
Class.Phy2 <- findInterval(bl.data$BL.phyml.Op,brks.p2)


#####################################################################################
#                            PHYML NON-OPTIMIZE VS RAXML                            #
#####################################################################################

## Do the Pearson correlation (PhyMLNonOp vs RAxML)
pearson <- cor.test(y=bl.data$BL.phyml.NonOp,
                    x=bl.data$BL.raxml,method = "spearman")

## Pearson's R
pearson$estimate
## Pearson's squared R
pearson$p.value

## Make the graphic
p <- ggplot() + geom_point(aes(x=bl.data$BL.raxml[-which(Class.Rax%in%c(5,6))],
                               y=bl.data$BL.phyml.NonOp[-which(Class.Phy%in%c(5,6))]))+
  #geom_smooth(aes(x=bl.data$BL.raxml[which(bl.data$BL.raxml<max(bl.data$BL.raxml))],
  #               y=bl.data$BL.phyml[which(bl.data$BL.phyml<max(bl.data$BL.phyml))]))+
  ylab("PhyML-NonOptimize Branch Length (Terminals)")+ xlab("RAxML Branch Length (Terminals)")+
  geom_abline(intercept=0,slope=pearson$estimate)+
  geom_label(label=paste("rho",round((pearson$estimate),digits=4),sep = ": "),
             aes(x=0.05,y=0.05),
             size=12)+
  #geom_label(label=paste("p-value",pearson$p.value,sep = ": "),
  #           aes(x=0.05,y=0.045),
  #           size=12)+
  ggtitle("B")+
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 20,),
        axis.title.x = element_text(size = 20,),
        plot.title =  element_text(size=40))
  

p

# Save the png file
png(filename = "~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/NonOptimizeVsRaxml-Spearman.png",
    width = 2800,height=1900,res = 300)
p
dev.off()

# Find the species with the highs BL for PhyML and RAxML
bl.data$Sp[which(bl.data$BL.phyml.NonOp==max(bl.data$BL.phyml.NonOp))]
bl.data$Sp[which(bl.data$BL.raxml==max(bl.data$BL.raxml))]

# Something interesting, the species differs form PhyML to RAxML



#####################################################################################
#                            PHYML OPTIMIZE VS RAXML                                #
#####################################################################################

## Do the Pearson correlation (PhyMLNonOp vs RAxML)
pearson <- cor.test(y=bl.data$BL.phyml.Op,
                    x=bl.data$BL.raxml,method = "spearman")

## Pearson's R
pearson$estimate
## Pearson's squared R
pearson$p.value

## Make the graphic
p <- ggplot() + geom_point(aes(x=bl.data$BL.raxml[-which(Class.Rax%in%c(5,6))],
                               y=bl.data$BL.phyml.Op[-which(Class.Phy2%in%c(5,6))]))+
  #geom_smooth(aes(x=bl.data$BL.raxml[which(bl.data$BL.raxml<max(bl.data$BL.raxml))],
  #               y=bl.data$BL.phyml[which(bl.data$BL.phyml<max(bl.data$BL.phyml))]))+
  ylab("PhyML-Optimize Branch Length (Terminals)")+ xlab("RAxML Branch Length (Terminals)")+
  geom_abline(intercept=0,slope=pearson$estimate)+
  geom_label(label=paste("rho",round(pearson$estimate,digits=4),sep = ": "),
             aes(x=0.05,y=0.05),
             size=12)+
  #geom_label(label=paste("p-value",pearson$p.value,sep = ": "),
  #           aes(x=0.05,y=0.045),
  #           size=12)+
  ggtitle("A")+
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 20,),
        axis.title.x = element_text(size = 20,),
        plot.title = element_text(size=40))

p

# Save the png file
png(filename = "~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/OptimizeVsRaxml-Spearman.png",
    width = 2800,height=1900,res = 300)
p
dev.off()

# Find the species with the highs BL for PhyML and RAxML
bl.data$Sp[which(bl.data$BL.phyml.Op==max(bl.data$BL.phyml.Op))]
bl.data$Sp[which(bl.data$BL.raxml==max(bl.data$BL.raxml))]

# Something interesting, the species differs form PhyML to RAxML


#####################################################################################
#                        PHYML NON-OPTIMIZE VS PHYML-OPTIMIZE                       #
#####################################################################################


## Do the Pearson correlation (PhyMLNonOp vs RAxML)
pearson <- cor.test(y=bl.data$BL.phyml.NonOp,
                    x=bl.data$BL.phyml.Op, method = "spearman")

## Pearson's R
pearson$estimate
## Pearson's squared R
pearson$p.value

## Make the graphic
p <- ggplot() + geom_point(aes(x=bl.data$BL.phyml.Op[-which(Class.Phy2%in%c(5,6))],
                               y=bl.data$BL.phyml.NonOp[-which(Class.Phy%in%c(5,6))]))+
  #geom_smooth(aes(x=bl.data$BL.raxml[which(bl.data$BL.raxml<max(bl.data$BL.raxml))],
  #               y=bl.data$BL.phyml[which(bl.data$BL.phyml<max(bl.data$BL.phyml))]))+
  ylab("PhyML Non-Optimize Branch Length (Terminals)")+ xlab("PhyML Optimize Branch Length (Terminals)")+
  geom_abline(intercept=0,slope=pearson$estimate)+
  geom_label(label=paste("rho",round(pearson$estimate,digits=4),sep = ": "),
             aes(x=0.048,y=0.05),
             size=12)+
  #geom_label(label=paste("p-value",pearson$p.value,sep = ": "),
  #           aes(x=0.048,y=0.045),
  #           size=12)+
  ggtitle("C")+
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 20,),
        axis.title.x = element_text(size = 20,),
        plot.title = element_text(size = 40))

p

# Save the png file
png(filename = "~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/OptimizeVsNonOptimize-Spearman.png",
    width = 2800,height=1900,res = 300)
p
dev.off()

