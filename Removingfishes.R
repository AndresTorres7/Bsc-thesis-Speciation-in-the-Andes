library(readxl)
library(ape)

setwd("C:/Users/Andres Torres/Documents/Biología/AndesGMSpeciationInfering/1CandidatesClades/Results")


a<-read_xlsx("FinalGenusList.xlsx")

out <-c("Ophiuroidea", "Actinopterygii")

sinfish <- a[!a$class %in% out,]
write.csv(sinfish,"pecesremoved.csv", row.names = F)

setwd("C:/Users/Andres Torres/Documents/Biología/AndesGMSpeciationInfering/3GMSInferring/trees/time-tree-searcher-main/enlaces")

tress<-list.files(pattern = "_arbol")
tress1<- gsub(pattern = "_arbol", x = tress, replacement = "")
tress1<-gsub(r"{\s*\([^\)]+\)}","",tress1)

downtrees<- data.frame(realname = tress, genusname = tress1)

trees <- downtrees[downtrees$genusname %in% sinfish$genus,]

order<-match(sinfish$genus, trees$genusname)

newtrees <- trees[order,]

sinfish$tree <- newtrees$realname

n<- character()

for(s in 1:nrow(sinfish)) {
  arbol <- read.tree(sinfish$tree[s])
  m<-arbol$tip.label
  n <- c(n,m) 
  
  
}

write.table(n,"allspecies.txt")
write.csv(n,"allspecies.csv", row.names = F)


prueba <- read.csv("allspecies.csv")


##----------------------------------- testing arc for diglossa-----------------

setwd("C:/Users/Andres Torres/Desktop/Diglossa")

library(abc)
library(caret)
library(DDD)
library(gridExtra)
library(ggplot2)
library(MASS)
library(phytools)
library(RPANDA)
library(RRphylo)
library(stringi)
library(stringr)
library(tidyverse)

scripts <- c("rangeDispersal.R", "nicheEvolution.R","speciateAllopatric.R","speciateSympatric.R",
             "speciateParapatric.R","speciateDispersal.R","seedSpecies.R","environmentalChange.R",
             "nicheRecenter.R","DREaD.R","generateSummaryStatistics.R", "helperFunctions.R","findSisters.R","summary_statsitics_functions.R")
for (i in 1:length(scripts)){
  scripts[[i]] <- paste("C:/Users/Andres Torres/Documents/Biología/diversificationTest/Code/AuxiliaryScripts/DREAD/",scripts[[i]],sep="")
}
lapply(scripts, source)
rangeDispersal <- disperseRange

required.packages <- (c("raster","gstat", "SpaDES", "ape","phytools","geiger","phyloclim","ggplot2","gridExtra","moments",
                        "apTreeshape","parallel", "doSNOW", "rgeos","knitr","data.table", "fossil", "ENMTools"))
lapply(required.packages, require, character.only=T)

load("C:/Users/Andres Torres/Documents/Biología/diversificationTest/Code/AuxiliaryFiles/spp_rasters.rda")
All.species.rasters <- rasters
sp.rasters <- All.species.rasters

setwd("C:/Users/Andres Torres/Documents/Biología/diversificationTest/Code/AuxiliaryFiles")
vtree <- read.tree("Tree.tre")
vtree.ingroup <- drop.tip(vtree,c("borneensis","crocodilurus"))#Tree of varanids, excluding outgroups

australian.tree <- extract.clade(vtree.ingroup,node=getMRCA(vtree.ingroup,c("tristis","varius")))
plot(australian.tree)

clade.trees <- list(australian=australian.tree)

# do i need rho? clade.rho?





#Get summary statistics of each clade for DREaD


df <- data.frame(clade = NA, ntips = NA, crown.age=NA,
                 RO0=NA, RO50=NA, RO75=NA, RO90=NA, RO100=NA, ROmean=NA, ROslope=NA, ROintercept=NA, ROskew=NA, ROkurtosis=NA,
                 ASYMmean=NA, ASYMslope=NA, ASYMintercept=NA,
                 BIMOD50=NA, BIMOD75=NA, BIMOD90=NA, BIMOD100=NA,
                 RSskew=NA, RSmean=NA, RSsd=NA, CI=NA, Beta=NA, Gamma=NA, SI=NA,
                 lambda.x=NA, lambda.y=NA, TD=NA, TOmean=NA, TOsd=NA)

setwd("C:/Users/Andres Torres/Documents/Biología/diversificationTest/Code")
for (w in 1:length(clade.trees)){
  phy = clade.trees[[w]]
  Clade= names(clade.trees)[w]
  
  root.age.true <- max(findMRCA(phy, type=c("height")))
  phy$edge.length<-phy$edge.length/root.age.true
  root.age<-1
  
  # get sister species
  sisters <- findSisters(phy, solve.polytomies=T)
  
  # find divergence times
  sisters.split.time <- sisterSpeciesSplitTimes(phy, sisters, root.age=1)
  
  #get sister species range overlap
  RO_ss <- sisterSpeciesOverlap(sp.rasters, sisters)
  
  #get sister species range asymmetry
  Asym_ss <- sisterSpeciesRangeAsymmetry(sp.rasters, sisters)
  
  # get sister species range distances
  print(paste("running range distance ... takes a min"))
  RD_ss <- sisterSpeciesRangeDistances(sp.rasters, sisters, aggregate_size = 2)
  
  # get sister-species-outgroup overlap metrics
  TO <- outgroupOverlap(sp.rasters, sisters, sisters.overlap = RO_ss)
  
  #! first batch of summary statistics - mean and SD
  range.overlap.av <- mean(RO_ss, na.rm=T)
  range.overlap.sd <- sd(RO_ss, na.rm=T)
  
  range.asymmetry.av <- mean(Asym_ss, na.rm=T)
  range.asymmetry.sd <- sd(Asym_ss, na.rm=T)
  
  range.distance.av <- mean(RD_ss, na.rm=T)
  range.distance.sd <- sd(RD_ss, na.rm=T)
  
  TO.av <- mean(TO, na.rm=T)
  TO.sd <- sd(TO, na.rm=T)
  
  #! second batch of summary statistics - linear models
  
  lm.asym    <-     lm(Asym_ss ~ sisters.split.time)
  lm.overlap <-  lm(RO_ss ~ sisters.split.time)
  lm.distance <- lm(RD_ss ~ sisters.split.time)
  
  range.overlap.slope <- lm.overlap$coefficients[[2]]
  range.overlap.intercept <- lm.overlap$coefficients[[1]]
  
  range.distance.slope <- lm.distance$coefficients[[2]]
  range.distance.intercept <- lm.distance$coefficients[[1]]
  
  range.asymmetry.slope<-lm.asym$coefficients[[2]]
  range.asymmetry.intercept<-lm.asym$coefficients[[1]]
  
  #! third batch of summary statistics - species range sizes
  
  species.range.sizes <- rangeSize(sp.rasters)
  species.range.sizes.stand <- species.range.sizes/max(species.range.sizes)
  
  range.size.skew <- skewness(species.range.sizes.stand)
  range.size.av <- mean(species.range.sizes.stand, na.rm=T)
  range.size.sd <- sd(species.range.sizes.stand, na.rm=T)
  
  #! fourth batch of summary statistics - Phylogenetic balance and treeshape metrics
  
  phy_treeshape <- as.treeshape(phy)
  beta <- maxlik.betasplit(phy_treeshape, up = 10, remove.outgroup = FALSE, confidence.interval = "none", conf.level = 0.95, size.bootstrap = 100)$max_lik
  collessI <- colless(phy_treeshape)
  sackinI <- sackin(phy_treeshape)
  ltt <- ltt(phy, plot=FALSE)
  gamma <-ltt$gamma
  
  #! fifth batch of summary statistics - further overlap summary metrics
  
  symp0 <-   length(which(RO_ss == 0))/   length(RO_ss)
  symp50 <-  length(which(RO_ss >= 0.5))/ length(RO_ss)
  symp75 <-  length(which(RO_ss >= 0.75))/length(RO_ss)
  symp90 <-  length(which(RO_ss >= 0.9))/ length(RO_ss)
  symp100 <- length(which(RO_ss == 1))/   length(RO_ss)
  
  range.overlap.kurt<- kurtosis(RO_ss)
  range.overlap.skew <- skewness(RO_ss)
  
  #! sixth batch of summary statistics - biomodality
  sisterpairs <- length(sisters)
  
  bimodality100 <-((symp0*sisterpairs) *  (symp100*sisterpairs)) / ((sisterpairs/2)*(sisterpairs/2))
  bimodality90  <-((symp0*sisterpairs) *  (symp90*sisterpairs)) / ((sisterpairs/2)*(sisterpairs/2))
  bimodality75  <-((symp0*sisterpairs) *  (symp75*sisterpairs)) / ((sisterpairs/2)*(sisterpairs/2))
  bimodality50  <-((symp0*sisterpairs) *  (symp50*sisterpairs)) / ((sisterpairs/2)*(sisterpairs/2))
  
  #! seventh batch of summary statistics - ARC
  print(paste("running ARC ... takes a min"))
  ARC <- calculateARC(phy, sp.rasters)
  
  ARCint <- ARC$coefficients[[1,1]]#What's this?
  ARCslope <- ARC$coefficients[[1,2]]#What's this?
  
  # put it together
  
  tmp_df <- data.frame(clade         = Clade,
                       ntips         = length(phy$tip.label), 
                       ROmean        = range.overlap.av,
                       ROsd          = range.overlap.sd,
                       ROslope       = range.overlap.slope, 
                       ROintercept   = range.overlap.intercept, 
                       ROskew        = range.overlap.skew, 
                       ROkurtosis    = range.overlap.kurt,
                       RO0           = symp0, 
                       RO50          = symp50, 
                       RO75          = symp75, 
                       RO90          = symp90, 
                       RO100         = symp100,
                       TOmean        = TO.av,
                       TOsd          = TO.sd, 
                       ASYMmean      = range.asymmetry.av, 
                       ASYMsd        = range.asymmetry.sd,
                       ASYMslope     = range.asymmetry.slope, 
                       ASYMintercept = range.asymmetry.intercept,
                       RDmean        = range.distance.av, 
                       RDsd          = range.distance.sd, 
                       RDintercept   = range.distance.intercept, 
                       RDslope       = range.distance.slope,
                       BIMOD50       = bimodality50, 
                       BIMODE75      = bimodality75, 
                       BIMOD90       = bimodality90, 
                       BIMOD100      = bimodality100,
                       RSskew        = range.size.skew, 
                       RSmean        = range.size.av, 
                       RSsd          = range.size.sd, 
                       CI            = collessI, 
                       Beta          = beta, 
                       Gamma         = gamma, 
                       SI            = sackinI,
                       ARCslope      = ARCslope, 
                       ARCint        = ARCint) 
  
  write.csv(tmp_df,file=paste("Empirical_data_",Clade,".csv",sep=""))
}


dat <- read.csv("./AuxiliaryFiles/simulation_results.csv",stringsAsFactors = F)
variabs <- c("asymintercept","ROintercept","RSmean",#These are the 14 statistics used in model selection
             "ROmean", "TOmean", "SI", "RDintercept", "ARCslope",
             "RO90", "RSsd","RO100", "RDsd", "ARCint","asymslope" )
dat <- dat[!grepl("EXTINCT", dat[,14]),]#Delete simulations that resulted in extinction
for (i in 1:ncol(dat)){
  dat[,i] <- gsub("NaN",NA,dat[,i])
}
dat <- na.omit(dat)
dat[,14:45] <- as.numeric(as.character(unlist(dat[,14:45])))
df <- data.frame(dat$geo.mode,dat[,which(colnames(dat) %in% variabs)])
colnames(df)[[1]] <- "geo.mode"
models <- as.character(df[,1])
df <- df[,2:length(df)]

#Model selection to infer the predominant geographic mode of speciation: ABC

clades <- names(clade.trees)
all.res.list <- list()
for (i in 1:length(clades)){
  clade <- clades[i]
  
  emp.dat <- read.csv(paste("./Empirical_data_",clade,".csv",sep=""),stringsAsFactors = F)
  
  colnames(emp.dat)[[which(colnames(emp.dat)=="ASYMslope")]] <- "asymslope"
  colnames(emp.dat)[[which(colnames(emp.dat)=="ASYMintercept")]] <- "asymintercept"
  
  emp.dat <- emp.dat[,which(colnames(emp.dat) %in% variabs)]
  
  emp.dat <- emp.dat[,(colnames(df))]
  
  ###
  
  emp.log <- postpr(emp.dat, models, df, tol=.05, method="mnlogistic")
  emp.neu <- postpr(emp.dat, models, df, tol=.05, method="neuralnet")
  
  table.tmp <- cbind(emp.log$pred,emp.neu$pred)
  all.res.list[[i]] <- table.tmp
  names(all.res.list)[i] <- names(clades)[i]
}

all.res.table <- cbind(all.res.list[[1]])#,all.res.list[[2]],all.res.list[[3]])
colnames(all.res.table) <- rep(clades,each=2)
colnames(all.res.table) <- paste(colnames(all.res.table),rep(c("log","neu"),1),sep="_")
all.res.table <- cbind(rownames(all.res.table),as.data.frame(all.res.table))
colnames(all.res.table)[1] <- "Mode"
all.res.table[,1] <- as.character(all.res.table[,1])
write.csv(all.res.table,"ABCresults.csv",row.names = F)

#Model selection to infer the predominant geographic mode of speciation: LDA

df.lda <- cbind(df,models)
lda.res <- lda(models ~ asymintercept + ROintercept + RSmean +
                 ROmean + TOmean + SI + RDintercept + ARCslope +
                 RO90 + RSsd + RO100 + RDsd + ARCint + asymslope, df.lda)
emp.table <- data.frame(matrix(ncol=ncol(emp.dat),nrow=length(clades)))
for (i in 1:length(clades)){
  clade <- clades[i]
  emp.dat <- read.csv(paste("./Empirical_data_",clade,".csv",sep=""),stringsAsFactors = F)
  emp.table[i,] <- emp.dat
}
colnames(emp.table) <- colnames(emp.dat)
colnames(emp.table)[[which(colnames(emp.table)=="ASYMslope")]] <- "asymslope"
colnames(emp.table)[[which(colnames(emp.table)=="ASYMintercept")]] <- "asymintercept"
emp.table <- emp.table[,which(colnames(emp.table) %in% variabs)]
emp.table <- emp.table[,(colnames(df))]
emp.table$models <- rep("allopatric",nrow(emp.table))
predictions <- lda.res %>% predict(emp.table)
lda.plot <- cbind(df.lda, predict(lda.res)$x)

ggplot(lda.plot, aes(LD1, LD2)) +
  geom_point(aes(color = models)) +
  stat_ellipse(aes(color = models),level=0.95) +
  theme_bw() +
  geom_point(data= p, color = "black")
  #geom_point(data=as.data.frame(predictions$x[,1:2]),color=c("blue","red","yellow"))#Colors for Australian, Malesian, and Papuan clade, in that order
li<-predictions$x[,1:2]
u<-as.data.frame(li)
p<-as.data.frame(t(u))






igual <- read_xlsx("menorAUno.xlsx")

sinfish[sinfish$genus %in% igual$Genus,6]
