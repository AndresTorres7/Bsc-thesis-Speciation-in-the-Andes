library(gstat)
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
library(rgbif)
library(readxl)


genero <- ("Anolis")

### real 
setwd("C:/Users/Andres Torres/Desktop/Vainas")


#Prepare data for DREaD analyses
#Code for DREaD analyses was mostly written by Alexander Skeels

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
load(paste(genero, "_rasters.rda", sep = ""))
All.species.rasters <- k
sp.rasters <- All.species.rasters
names(sp.rasters) <- gsub(" ", "_", names(sp.rasters))

for( f in 1:length(sp.rasters)) {
  
  origin(sp.rasters[[f]])<-origin(sp.rasters[[1]])
  
}
setwd("C:/Users/Andres Torres/Documents/Biología/AndesGMSpeciationInfering/3GMSInferring/trees/time-tree-searcher-main/enlaces")
vtree <- read.tree(list.files(pattern = genero))

setwd("C:/Users/Andres Torres/Documents")
pa<-read_xlsx("losanolis.xlsx")
pa<-gsub(" ", "_", pa$Name)

vtree<-drop.tip(vtree, tip = c("Anolis_kreutzi", "Anolis_amplisquamosus", "Anolis_purpurgularis", "Anolis_loveridgei", "Anolis_yoroensis",
                                   "Anolis_amplisquamosus", "Anolis_kreutzi", "Anolis_alvarezdeltoroi", "Anolis_rodriguezii", "Anolis_cusuco",
                                "Anolis_pseudokemptoni", "Anolis_pijolense", "Anolis_marsupialis", "Anolis_magnaphallus", "Anolis_townsendi",
                               "Anolis_medemi", "Anolis_gracilipes", "Anolis_compressicauda", "Anolis_ibanezi", "Anolis_scypheus", 
                               "Anolis_cryptolimifrons", "Anolis_wermuthi", "Anolis_aquaticus", pa))
                               


#vtree<-drop.tip(vtree, tip = c("Liolaemus_chehuachekenk", "Liolaemus_chillanensis", "Liolaemus_curicensis"))
vtree1<-vtree
### the  phylo names in raster

namesKept<- vtree$tip.label[!vtree[[5]]%in% names(sp.rasters)] 
vtree<-drop.tip(vtree,namesKept)

vtree.ingroup <- vtree

clade.trees <- list(Anolis = vtree) ########################





#test
#Code for DREaD analyses was mostly written by Alexander Skeels

#setwd("C:/Users/Andres Torres/Desktop")


#Prepare data for DREaD analyses
#Code for DREaD analyses was mostly written by Alexander Skeels

#scripts <- c("rangeDispersal.R", "nicheEvolution.R","speciateAllopatric.R","speciateSympatric.R",
#"speciateParapatric.R","speciateDispersal.R","seedSpecies.R","environmentalChange.R",
 #            "nicheRecenter.R","DREaD.R","generateSummaryStatistics.R", "helperFunctions.R","findSisters.R","summary_statsitics_functions.R")
#for (i in 1:length(scripts)){
 # scripts[[i]] <- paste("C:/Users/Andres Torres/Documents/Biología/diversificationTest/Code/AuxiliaryScripts/DREAD/",scripts[[i]],sep="")
#}
#lapply(scripts, source)
#rangeDispersal <- disperseRange
#required.packages <- (c("raster","gstat", "SpaDES", "ape","phytools","geiger","phyloclim","ggplot2","gridExtra","moments",
  #                      "apTreeshape","parallel", "doSNOW", "rgeos","knitr","data.table", "fossil", "ENMTools"))

#lapply(required.packages, require, character.only=T)
#load("spp_rasters.rda")

#All.species.rasters <- rasters
#sp.rasters <- All.species.rasters

#vtree <- read.tree("Tree.tre")
#vtree.ingroup <- drop.tip(vtree,c("borneensis","crocodilurus"))#Tree of varanids, excluding outgroups




#Get summary statistics of each clade for DREaD
# TOmean=NA, TOsd=NA


df <- data.frame(clade = NA, ntips = NA, crown.age=NA,
                 RO0=NA, RO50=NA, RO75=NA, RO90=NA, RO100=NA, ROmean=NA, ROslope=NA, ROintercept=NA, ROskew=NA, ROkurtosis=NA,
                 ASYMmean=NA, ASYMslope=NA, ASYMintercept=NA,
                 BIMOD50=NA, BIMOD75=NA, BIMOD90=NA, BIMOD100=NA,
                 RSskew=NA, RSmean=NA, RSsd=NA, CI=NA, Beta=NA, Gamma=NA, SI=NA,
                 lambda.x=NA, lambda.y=NA, TD=NA )

#w<- 1
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
  #TO <- outgroupOverlap(sp.rasters, sisters, sisters.overlap = RO_ss)
  
  #! first batch of summary statistics - mean and SD
  range.overlap.av <- mean(RO_ss, na.rm=T)
  range.overlap.sd <- sd(RO_ss, na.rm=T)
  
  range.asymmetry.av <- mean(Asym_ss, na.rm=T)
  range.asymmetry.sd <- sd(Asym_ss, na.rm=T)
  
  range.distance.av <- mean(RD_ss, na.rm=T)
  range.distance.sd <- sd(RD_ss, na.rm=T)
  
  #TO.av <- mean(TO, na.rm=T)
  #TO.sd <- sd(TO, na.rm=T)
  
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
  
  
  setwd("C:/Users/Andres Torres/Documents/Biología/AndesGMSpeciationInfering/3GMSInferring/results")
  dir.create(Clade)
  setwd(paste("C:/Users/Andres Torres/Documents/Biología/AndesGMSpeciationInfering/3GMSInferring/results", Clade, sep = "/"))
  write.csv(tmp_df,file=paste("Empirical_data_",Clade,".csv",sep=""))
}

setwd("C:/Users/Andres Torres/Documents/Biología/diversificationTest/Code") #/AuxiliaryScripts/DREAD
#setwd("C:/Users/Andres Torres/Desktop/Vainas")

#Load and prepare DREaD simulation results

dat <- read.csv("./AuxiliaryFiles/simulation_results.csv",stringsAsFactors = F)
variabs <- c("asymintercept","ROintercept","RSmean",#These are the 14 statistics used in model selection
             "ROmean", "SI", "RDintercept", "ARCslope",
             "RO90", "RSsd","RO100", "RDsd", "ARCint" ,"asymslope" ) #, 
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


setwd(paste("C:/Users/Andres Torres/Documents/Biología/AndesGMSpeciationInfering/3GMSInferring/results", Clade, sep = "/"))

for (i in 1:length(clades)){
  clade <- clades[i]
  
  emp.dat <- read.csv(paste("./Empirical_data_",Clade,".csv",sep=""),stringsAsFactors = F)
  
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


setwd(paste("C:/Users/Andres Torres/Documents/Biología/AndesGMSpeciationInfering/3GMSInferring/results", Clade, sep = "/"))
write.csv(all.res.table,paste(genero, "ABCresults.csv", sep = "_"),row.names = F)

all.res.table 



#Model selection to infer the predominant geographic mode of speciation: LDA

df.lda <- cbind(df,models)
lda.res <- lda(models ~ asymintercept + ROintercept + RSmean +
                 ROmean + SI + RDintercept + 
                 RO90 + RSsd + RO100 + RDsd + asymslope, df.lda)
emp.table <- data.frame(matrix(ncol=ncol(emp.dat),nrow=length(clades))) # ARCslope + ARCint +
for (i in 1:length(clades)){
  
  
  clade <- clades[i]
  emp.dat <- read.csv(paste("./Empirical_data_",clade,".csv",sep=""),stringsAsFactors = F)
  colnames(emp.dat)[[which(colnames(emp.dat)=="ASYMslope")]] <- "asymslope"
  colnames(emp.dat)[[which(colnames(emp.dat)=="ASYMintercept")]] <- "asymintercept"
  
  emp.dat <- emp.dat[,which(colnames(emp.dat) %in% variabs)]
  
  emp.dat <- emp.dat[,(colnames(df))]
  emp.table[i,] <- emp.dat
}
colnames(emp.table) <- colnames(emp.dat)
#colnames(emp.table)[[which(colnames(emp.table)=="ASYMslope")]] <- "asymslope"
#colnames(emp.table)[[which(colnames(emp.table)=="ASYMintercept")]] <- "asymintercept"
emp.table <- emp.table[,which(colnames(emp.table) %in% variabs)]
emp.table <- emp.table[,(colnames(df))]
emp.table$models <- rep("allopatric",nrow(emp.table))
predictions <- lda.res %>% predict(emp.table)
lda.plot <- cbind(df.lda, predict(lda.res)$x)

write.table(predictions, paste(genero, "LDA_results.txt", sep = "_"))
write.csv(lda.plot, "backgroundpoints.csv", row.names = F)

### saving data

###########CHANGE THE NAMEEEES



a<-data.table(Reg = 1:15, 
              value = c(all.res.table[,2], all.res.table[,3], c(predictions$posterior)), ########
              Mode = c(all.res.table$Mode, all.res.table$Mode, all.res.table$Mode), 
              model = c(rep( "mnL", times = 5), rep( "NN", times = 5), rep( "LDA", times = 5) )
)

all.res.table
a
write.csv(a, paste(Clade,"Allvalues.csv",sep = "_"), row.names = F)

pdf(paste(clade, "ABC.pdf"))

ggplot(a, aes(fill=Mode, y=value, x=model)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle(genero)

dev.off()


li<-predictions$x[,1:2]
u<-as.data.frame(li)
p<-as.data.frame(t(u))

pdf(paste(genero, "LDAgraphic.pdf", sep = "_"))
ggplot(lda.plot, aes(LD1, LD2)) +
  geom_point(aes(color = models)) +
  stat_ellipse(aes(color = models),level=0.95) +   
  theme_bw() +
  geom_point(data= p, color = "black") +
  ggtitle(genero)
dev.off()

### quality metadata
setwd("C:/Users/Andres Torres/Documents/Biología/AndesGMSpeciationInfering/1CandidatesClades/Results")
cladoss<-read_xlsx("Clados_candidatos_con_mas_de_doce_spp.xlsx")
thekey <- as.numeric(cladoss[cladoss$genus==genero,2])

x<-name_lookup(higherTaxonKey = thekey, limit = 10000 , hl = T, status = "ACCEPTED") #search spp names of the genus
x<-x[["data"]][,c("species","rank","taxonomicStatus","synonym","speciesKey")]  # Select the wanted columns
x<-x[x$synonym=="FALSE",-4]
x<-x[x$taxonomicStatus=="ACCEPTED",-3]
x<-x[x$rank=="SPECIES",-2]
x<-na.omit(x)
x<-x %>% distinct(speciesKey, .keep_all=T)
x<-nrow(x)

phylonum<- Ntip(vtree1)
phyyrasnum <- phylonum - length(namesKept)
rate1 <- phylonum/x
rate2 <- phyyrasnum/x

calidad<- data.frame(sppinPhylo = phylonum, sppinPhyloYRaster = phyyrasnum,
                     totalsppaccepted = x , timetreeRate = rate1, realRate = rate2 )

calidad
setwd(paste("C:/Users/Andres Torres/Documents/Biología/AndesGMSpeciationInfering/3GMSInferring/results", Clade, sep = "/"))
write.table(namesKept, "speciesdeletedfromtphylo.txt")
write.csv(calidad, paste(genero, "calidadphylo.csv", sep = "_"), row.names = F)



print("fin")




