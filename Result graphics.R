library(ggplot2)
library(readxl)
library(ggpubr)
library(tidyverse)
library(dplyr)
 
mafecolors<- c("#E57373","#ffff56", "#64DD17", "#6eb8f5", "#CE93D8")


setwd("C:/Users/thbio/Documents/Biología/AndesGMSpeciationInfering/3GMSInferring/results")

spp<-dir()
frame <- data.frame(Gneus = spp, x.LD1 = NA, x.LD2 = NA, x.LD3 = NA, x.LD4 = NA)


for(i in 1:length(spp)){
  setwd(paste("C:/Users/thbio/Documents/Biología/AndesGMSpeciationInfering/3GMSInferring/results", spp[i], sep= "/"))
  a<-as.data.frame(read.table(list.files(pattern = "_LDA_results.txt")))
  a<- a[1, c (7,8,9,10)]
  frame[i, -1] <- a
  
}

predictions <- frame 
  lda.plot <- read.csv("backgroundpoints.csv")
names(predictions)[2:3]  <- c("LD1", "LD2")
  predictions
  
  
  setwd("C:/Users/Andres Torres/Documents/Biología/AndesGMSpeciationInfering/3GMSInferring/graphics")
  
  pdf("GeneralLDA.pdf", width = 10, height = 6)
  ggplot(lda.plot, aes(LD1, LD2)) +
  geom_point(aes(color = models), size = 0.6) +
  stat_ellipse(aes(color = models),level=0.95, linewidth = 2) +   
  theme_bw() +
 geom_point(data= predictions[,2:3], color = "black") +
  ggtitle("LDA") +
    xlim(-8, 8) +
    ylim(-7.5,5)
dev.off()
  
png("GeneralLDA.png")
ggplot(lda.plot, aes(LD1, LD2)) +
  geom_point(aes(color = models), size = 0.6) +
  stat_ellipse(aes(color = models),level=0.95, linewidth = 2) +   
  theme_bw() +
  geom_point(data= predictions[,2:3], color = "black") +
  ggtitle("LDA")
dev.off()


#color test

png("GeneralLDA.png")
ggplot(lda.plot, aes(LD1, LD2)) +
  geom_point(aes(color = models), size = 0.6) +
  scale_color_manual(values = c("#E57373","#ffff56", "#64DD17", "#6eb8f5", "#CE93D8"))
  stat_ellipse(aes(color = models),level=0.95, linewidth = 2) +   
  theme_bw() +
  geom_point(data= predictions[,2:3], color = "black") +
  ggtitle("LDA")
dev.off()

  
####Column bars  
  figuras <- data.frame(Reg= NA, value=NA, Mode = NA, model = NA, Genus = NA)
  
  for(i in 1:length(spp)){
    setwd(paste("C:/Users/Andres Torres/Documents/Biología/AndesGMSpeciationInfering/3GMSInferring/results", spp[i], sep= "/"))
    a1<-read.csv(list.files(pattern = "Allvalues.csv"))
    a1$Genus <- spp[i]
    figuras <- rbind(figuras,a1)
  
    
    
  }
  
  #names(figuras)<- spp
 figuras<-figuras[-1,]

 
 setwd(paste("C:/Users/Andres Torres/Documents/Biología/AndesGMSpeciationInfering/1CandidatesClades/results"))
 
finalgenus<- read_xlsx("FinalGenusList.xlsx")

finalgenus<-finalgenus[finalgenus$genus%in%spp,]


#plants

plants <- finalgenus[finalgenus$kingdom=="Plantae",]

plants2 <-figuras[figuras$Genus%in%plants$genus,]
names(plants)[6]<-"Genus"
plants2<-merge(plants2, plants, by = "Genus")
plants2 <- plants2[order(plants2$order),]

planisub<- subset(plants2, class %in% names(table(plants2$class)))
planisub$Genus <-as.factor(planisub$Genus)

for( o in 1:length(names(table(plants2$class)))) {
  
  assign(paste("planisub", o, sep = ""), planisub[planisub$class == names(table(plants2$class))[o],]  )
  assign(paste("pplot", o, sep = ""),  ggplot(get(paste("planisub", o, sep="")), aes(fill=Mode, y=value, x=model)) + 
           geom_bar(position="stack", stat="identity") +
           facet_wrap(~Genus, ncol = 10) +
          # theme(legend.position = "bottom") +
           xlab("") +
           ylab(names(table(plants2$class))[o]) +
           theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black")) 
  )
  
  
}

megaplot<-ggarrange(pplot1, pplot2, pplot3, nrow = 3)
megaplot


#fungi

fungi <- finalgenus[finalgenus$kingdom=="Fungi",]

fungi2 <-figuras[figuras$Genus%in%fungi$genus,]
names(fungi)[6]<-"Genus"
fungi2<-merge(fungi2, fungi, by = "Genus")
fungi2 <- fungi2[order(fungi2$order),]  


#animals
animals<- finalgenus[finalgenus$kingdom=="Animalia",]

animals2 <-figuras[figuras$Genus%in%animals$genus,]
names(animals)[6]<-"Genus"
animals2<-merge(animals2, animals, by = "Genus")
animals2 <- animals2[order(animals2$order),]

anisub<- subset(animals2, class %in% names(table(animals2$class)))
anisub$Genus <-as.factor(anisub$Genus)

for( o in 1:length(names(table(animals2$class)))) {
  
  assign(paste("anisub", o, sep = ""), anisub[anisub$class == names(table(animals2$class))[o],]  )
  assign(paste("plot", o, sep = ""),  ggplot(get(paste("anisub", o, sep="")), aes(fill=Mode, y=value, x=model)) + 
           geom_bar(position="stack", stat="identity") +
           facet_wrap(~Genus, nrow = 1) +
           xlab("")+
           ylab(names(table(animals2$class))[o]) +
           theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black")) 
                 )

     
}

megaplot<-ggarrange(plot1, plot2, plot3, plot4, plot5, nrow = 5)
megaplot




#animals

setwd("C:/Users/Andres Torres/Documents/Biología/AndesGMSpeciationInfering/3GMSInferring/graphics")




pdf("AnimalLDAgraphics.pdf")

ggplot(animals2, aes(fill=Mode, y=value, x=model)) + 
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~Genus) +
  theme(legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Probabilidad posterior de modos geográficos de especiación en animales") +
  scale_fill_manual(values = c("#E57373","#ffff56", "#64DD17", "#6eb8f5", "#CE93D8"))

  dev.off()

#fungi

setwd("C:/Users/Andres Torres/Documents/Biología/AndesGMSpeciationInfering/3GMSInferring/graphics")

pdf("Fungibarsgraphics.pdf")

ggplot(fungi2, aes(fill=Mode, y=value, x=model)) + 
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~Genus) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
  

dev.off()
#plants

pdf("PLantLDAgraphics.pdf")

ggplot(plants2, aes(fill=Mode, y=value, x=model)) + 
  geom_bar(position="stack", stat="identity") +
  facet_wrap(order~Genus) +
  theme(legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Probabilidad posterior de modos geográficos de especiación en  plantas")

dev.off()


#animalsperorder

pdf("animalsperorderLDAgraphics.pdf")


ggplot(anuros, aes(fill=Mode, y=value, x=model)) + 
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~Genus, nrow = 2, ncol = 4) +
  theme(legend.position = "bottom") +
  ggtitle("Anuros")



ggplot(mamif, aes(fill=Mode, y=value, x=model)) + 
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~Genus, nrow = 2, ncol = 4) +
  theme(legend.position = "bottom") +
  ggtitle("Mamíferos")




ggplot(insects, aes(fill=Mode, y=value, x=model)) + 
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~Genus, nrow = 2, ncol = 4) +
  theme(legend.position = "bottom") +
  ggtitle("Insectos")




ggplot(birds, aes(fill=Mode, y=value, x=model)) + 
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~Genus, nrow = 2, ncol = 5) +
  theme(legend.position = "bottom") +
  ggtitle("Aves")




ggplot(reptil, aes(fill=Mode, y=value, x=model)) + 
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~Genus, nrow = 2, ncol = 4) +
  theme(legend.position = "bottom") +
  ggtitle("Reptiles")

    
  dev.off()



#plants

plants2 <-figuras[figuras$Genus%in%plants$genus,]

ggplot(plants2, aes(fill=Mode, y=value, x=model)) + 
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~Genus) +
  ggtitle(names(plants2)[2])


#fungi

fungi2 <-figuras[figuras$Genus%in%fungi$genus,]

ggplot(fungi2, aes(fill=Mode, y=value, x=model)) + 
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~Genus) +
  ggtitle(names(fungi2)[2])








############________-------------------------- Data table and results--------------------######################


# Percentage of every geographic mode 


maximosLDA <- data.frame(Reg = NA, value = NA, Mode = NA, model = NA, Genus = NA)
maximosmnL <- data.frame(Reg = NA, value = NA, Mode = NA, model = NA, Genus = NA)
maximosNN <- data.frame(Reg = NA, value = NA, Mode = NA, model = NA, Genus = NA)


for(i in 1:length(spp)){
  setwd(paste("C:/Users/Andres Torres/Documents/Biología/AndesGMSpeciationInfering/3GMSInferring/results", spp[i], sep= "/"))
  a1<-read.csv(list.files(pattern = "Allvalues.csv"))
  a1$Genus <- spp[i]
  #summarydata$Group <- a1$Genus
  
  LDA <- a1[a1$model=="LDA",]
  mnL <- a1[a1$model=="mnL",]
  NN <- a1[a1$model=="NN",]
  
  
  maximosLDA[i,]<- LDA[LDA$value ==max(LDA$value),] 
  maximosmnL[i,]<- mnL[mnL$value ==max(mnL$value),]
  maximosNN[i,] <-  NN[NN$value ==max(NN$value),]

  
}


maximospermodel<- rbind(maximosLDA,maximosmnL,maximosNN)

setwd("C:/Users/Andres Torres/Documents/Biología/AndesGMSpeciationInfering/3GMSInferring/results2")
write.csv(maximospermodel, "selectedModePerModel.csv", row.names = F)

modes <- c("allopatric", "dispersal" , "mixed", "parapatric", "sympatric" )

modos <- data.frame (Modes = modes)

# conteo en todos 

allxLDA<-as.data.frame(table(maximosLDA$Mode))
allxmnL<-as.data.frame(table(maximosmnL$Mode))
allxNN<-as.data.frame(table(maximosNN$Mode))

names(allxLDA)<- c("Modes", "Value")
names(allxmnL)<- c("Modes", "Value")
names(allxNN)<- c("Modes", "Value")

# conteo en animales

animaxLDA<- maximosLDA[maximosLDA$Genus%in%animals$Genus,]
animaxmnL<- maximosmnL[maximosmnL$Genus%in%animals$Genus,]
animaxNN<- maximosNN[maximosNN$Genus%in%animals$Genus,]

anixLDA <- as.data.frame(table(animaxLDA$Mode))
         names(anixLDA)<- c("Modes", "Value")
anixmnL<-as.data.frame(table(animaxmnL$Mode))
        names(anixmnL)<- c("Modes", "Value")
anixNN <-as.data.frame(table(animaxNN$Mode))
       names(anixNN)<-c("Modes", "Value")


# conteo en plantas

plantxLDA<- maximosLDA[maximosLDA$Genus%in%plants$Genus,]
plantxmnL<- maximosmnL[maximosmnL$Genus%in%plants$Genus,]
plantxNN<- maximosNN[maximosNN$Genus%in%plants$Genus,]

plaxLDA <- as.data.frame(table(plantxLDA$Mode))
names(plaxLDA)<- c("Modes", "Value")
plaxmnL <- as.data.frame(table(plantxmnL$Mode))
names(plaxmnL)<- c("Modes", "Value")
plaxNN <- as.data.frame(table(plantxNN$Mode))
names(plaxNN)<- c("Modes", "Value")



# conteo fungi

fungixLDA<- maximosLDA[maximosLDA$Genus%in%fungi$Genus,]
fungixmnL<- maximosmnL[maximosmnL$Genus%in%fungi$Genus,]
fungixNN<- maximosNN[maximosNN$Genus%in%fungi$Genus,]

table(fungixLDA$Mode)
table(fungixmnL$Mode)
table(fungixNN$Mode)

# datos para table

#LDA

generaltableLDA <- merge(modos, allxLDA,  by = "Modes", all = T )
generaltableLDA <- merge(generaltableLDA, anixLDA,  by = "Modes", all = T )
generaltableLDA <- merge(generaltableLDA, plaxLDA,  by = "Modes", all = T )

names(generaltableLDA)[-1] <- c("All", "Animals", "Plants")
generaltableLDA[is.na(generaltableLDA)] = 0

generaltableLDA$Fungi <- c(0,0,0,1,1)

# mnL

generaltablemnL <- merge(modos, allxmnL,  by = "Modes", all = T )
generaltablemnL <- merge(generaltablemnL, anixmnL,  by = "Modes", all = T )
generaltablemnL <- merge(generaltablemnL, plaxmnL,  by = "Modes", all = T )

names(generaltablemnL)[-1] <- c("All", "Animals", "Plants")
generaltablemnL[is.na(generaltablemnL)] = 0

generaltablemnL$Fungi <- c(0,0,0,1,1)


#NN

generaltableNN <- merge(modos, allxNN,  by = "Modes", all = T )
generaltableNN <- merge(generaltableNN, anixNN,  by = "Modes", all = T )
generaltableNN <- merge(generaltableNN, plaxNN,  by = "Modes", all = T )

names(generaltableNN)[-1] <- c("All", "Animals", "Plants")
generaltableNN[is.na(generaltableNN)] = 0

generaltableNN$Fungi <- c(0,0,0,1,1)


generaltableLDA$Model <- "LDA"
generaltablemnL$Model <- "mnL"
generaltableNN$Model <- "NN"

generaltable <- rbind(generaltableLDA, generaltablemnL, generaltableNN)

setwd("C:/Users/Andres Torres/Documents/Biología/AndesGMSpeciationInfering/3GMSInferring/results2")
write.csv(generaltable, "conteopormodo.csv", row.names = F)


tablaporcen <- generaltable
tablaporcen$All <- (tablaporcen$All/68)*100
tablaporcen$Animals <- (tablaporcen$Animals/25) * 100
tablaporcen$Plants <- (tablaporcen$Plants/41) * 100
tablaporcen$Fungi <- (tablaporcen$Fungi/2) *100










