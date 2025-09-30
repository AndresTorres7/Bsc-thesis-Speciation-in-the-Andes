library(ggplot2)
library(readxl)
library(ggpubr)
library(tidyverse)
library(dplyr)

newpalet <- c("#ef476f", "#ffd166", "#06d6a0", "#118ab2", "#073b4c")

setwd("C:/Users/thbio/Documents/Biología/AndesGMSpeciationInfering/3GMSInferring/results")

spp<-dir()


figuras <- data.frame(Reg= NA, value=NA, Mode = NA, model = NA, Genus = NA)

for(i in 1:1){
  setwd(paste("C:/Users/thbio/Documents/Biología/AndesGMSpeciationInfering/3GMSInferring/results", spp[i], sep= "/"))
  a1<-read.csv(list.files(pattern = "Allvalues.csv"))
  a1$Genus <- spp[i]
  figuras <- rbind(figuras,a1)
  
  
  
}



## Adelpha

ggplot(figuras , aes(fill=Mode, y=value, x=model)) +
geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = newpalet) +
  theme(panel.background = element_blank(), plot.margin = margin(5,.6,2,.6, "cm")) +
  xlab("")+
  ylab("")



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
