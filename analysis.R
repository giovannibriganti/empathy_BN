# BN analysis empathy & self worth

# Packages

library(stats)
library(qgraph)
library(readr)
library(bootnet)
library(dplyr)
library(corpcor)
library(bnlearn)
library(psych)
library(ggplot2)
library(mgm)
library(igraph)
library(EGAnet)
library(BGGM)
library(NetworkComparisonTest)


#################################
# Empathy data preparation

empathy <- read_delim("empathy.csv", delim = ";", 
                      escape_double = FALSE, trim_ws = TRUE)

# column names
colnames(empathy) <- c(1:28)


# names
names<- c("1FS", "2EC", "3PT_R", "4EC_R", "5FS", "6PD", "7FS_R", 
          "8PT","9EC", "10PD", "11PT", "12FS_R", "13PD_R", "14EC_R", "15PT_R", 
          "16FS", "17PD", "18EC_R", "19PD_R", "20EC", "21PT", "22EC", "23FS", 
          "24PD", "25PT", "26FS", "27PD", "28PT")

# groups
gr <- list(c(1, 5, 7, 12, 16, 23, 26), c(3, 8, 11, 15, 21, 25, 28),
           c(2, 4, 9, 14, 18, 20, 22), c(6, 10, 13, 17, 19, 24, 27))

# full network
network1 <- estimateNetwork(empathy, 
                            default="EBICglasso", 
                            corMethod = "cor", 
                            corArgs =
                              list(method = "spearman", 
                                   use = "pairwise.complete.obs"), 
                            threshold=TRUE)

# getting bootstrapped centrality values
boot1 <- bootnet(network1, ncores=7, nboots=2000)                
boot4 <- plot(boot1, "strength", order="sample", labels=TRUE) 
pdf("bootcentEm.pdf", width=10, height=7)
plot(boot4) 
dev.off()

# items 23, 14, 24, 8, 19, 4, 12, 26, 10, 9 are extracted in the emShort csv file

emShort <- read_delim("emShort.csv", delim = ";", 
                      escape_double = FALSE, trim_ws = TRUE)
data <- emShort
BST <- boot.strength(data, 
                     R = 1000, 
                     algorithm = "hc", 
                     debug = TRUE)  
BST[BST$strength > 0.85 & BST$direction > 0.5, ]
avgnet1 <- averaged.network(BST, 
                            threshold = 0.85)
avgnet1
bnlearn::score(avgnet1, data = data)
astr1 <- arc.strength(avgnet1, data, "bic-g")   ## compute edge strengths


longnamesEm <- c("Sometimes I don't feel very sorry for other people when they are having problems. (Reversed)",
"I try to look at everybody's side of a disagreement before I make a decision.",
"When I see someone being taken advantage of, I feel kind of protective towards them.",
"I sometimes feel helpless when I am in the middle of a very emotional situation.",
"Becoming extremely involved in a good book or movie is somewhat rare for me. (Reversed)",
"Other people's misfortunes do not usually disturb me a great deal. (Reversed)",
"I am usually pretty effective in dealing with emergencies. (Reversed)",
"When I watch a good movie, I can very easily put myself in the place of a leading character.",
"I tend to lose control during emergencies.",
"When I am reading an interesting story or novel, I imagine how I would feel if the events in the story were happening to me.")



pdf("hcEMPlot.pdf", width=10, height=6)
strength.plot(avgnet1, astr1)
dev.off()

#########################################
# Self worth

selfworth <- read_delim("selfworth.csv", 
                        delim = ";", 
                        escape_double = FALSE, 
                        trim_ws = TRUE)

factornames <- c("FS", "C", "A", "GL", "AC", "V", "OA")
longnames <- c("Family Support", "Competition", "Appearance", 
               "God's Love", "Academic competition", "Virtue", 
               "Other's Approval")

data3 <- selfworth
colnames(data3) <- c("FS", "C", "A", "GL", "AC", "V", "OA")

BST <- boot.strength(data3, 
                     R = 1000, 
                     algorithm = "hc", 
                     debug = TRUE)  
BST[BST$strength > 0.85 & BST$direction > 0.5, ]
avgnet1 <- averaged.network(BST, 
                            threshold = 0.85)
avgnet1
bnlearn::score(avgnet1, data = data3)
astr1 <- arc.strength(avgnet1, data3, "bic-g")   ## compute edge strengths
pdf("hcEMPlot.pdf", width=10, height=6)
strength.plot(avgnet1, astr1)
dev.off()




# EM 4

BST <- boot.strength(em4, 
                     R = 1000, 
                     algorithm = "hc", 
                     debug = TRUE)  
BST[BST$strength > 0.85 & BST$direction > 0.5, ]
avgnet1 <- averaged.network(BST, 
                            threshold = 0.85)
avgnet1
bnlearn::score(avgnet1, data = em4)
astr1 <- arc.strength(avgnet1, em4, "bic-g")   ## compute edge strengths
pdf("hcEMshortPlot.pdf", width=10, height=6)
strength.plot(avgnet1, astr1)
dev.off()

