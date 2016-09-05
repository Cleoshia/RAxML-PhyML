library(ggplot2)

################################################################################
#                               NORMALITY TEST                                 #
################################################################################

setwd("~/Documentos/Omar/Projects/PhyML-RAxML/Comparisons/")

bl.data <- read.csv("BL.data-Results", header = T)

head(bl.data,5L)

######################
# PHYML NON-OPTIMIZE # 
######################

#Scott's normal reference rule
h <- (3.5*sd(bl.data$BL.phyml.NonOp)) / (length(bl.data$BL.phyml.NonOp)^(1/3))
h
# Ceiling Function
k <- diff(range(bl.data$BL.phyml.NonOp)) / h
k

## Shapiro-Wilks Test

sw <- shapiro.test(bl.data$BL.phyml.NonOp)

if(sw$p.value<.05){pval <- "< 0.05"}else{pval <- sw$p.value}

pval

# Plot the histogram
hist <- ggplot()+geom_histogram(aes(bl.data$BL.phyml.NonOp),bins = k, binwidth = h)+
  xlab("PhyML Non-Optimize Branch Length")+
  geom_label(label="Shapiro-Wilk",
             aes(x=1.0,y=600),
             size=12)+
  geom_label(label=paste("p-value",pval,sep = ": "),
             aes(x=1.0,y=500),
             size=12)+
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 20,),
        axis.title.x = element_text(size = 20,))

hist


######################
#  PHYML OPTIMIZE    # 
######################

#Scott's normal reference rule
h <- (3.5*sd(bl.data$BL.phyml.Op)) / (length(bl.data$BL.phyml.Op)^(1/3))
h
# Ceiling Function
k <- diff(range(bl.data$BL.phyml.Op)) / h
k

## Shapiro-Wilks Test

sw <- shapiro.test(bl.data$BL.phyml.Op)

if(sw$p.value<.05){pval <- "< 0.05"}else{pval <- sw$p.value}

pval

# Plot the histogram
hist <- ggplot()+geom_histogram(aes(bl.data$BL.phyml.Op),bins = k, binwidth = h)+
  xlab("PhyML Optimize Branch Length")+
  geom_label(label="Shapiro-Wilk",
             aes(x=60,y=700),
             size=12)+
  geom_label(label=paste("p-value",pval,sep = ": "),
             aes(x=60,y=500),
             size=12)+
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 20,),
        axis.title.x = element_text(size = 20,))

hist

######################
#       RAXML        # 
######################

#Scott's normal reference rule
h <- (3.5*sd(bl.data$BL.raxml)) / (length(bl.data$BL.raxml)^(1/3))
h
# Ceiling Function
k <- diff(range(bl.data$BL.raxml)) / h
k

## Shapiro-Wilks Test

sw <- shapiro.test(bl.data$BL.raxml)

if(sw$p.value<.05){pval <- "< 0.05"}else{pval <- sw$p.value}

pval

# Plot the histogram
hist <- ggplot()+geom_histogram(aes(bl.data$BL.raxml),bins = k, binwidth = h)+
  xlab("RAxML Branch Length")+
  geom_label(label="Shapiro-Wilk",
             aes(x=20,y=700),
             size=12)+
  geom_label(label=paste("p-value",pval,sep = ": "),
             aes(x=20,y=500),
             size=12)+
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 20,),
        axis.title.x = element_text(size = 20,))

hist
