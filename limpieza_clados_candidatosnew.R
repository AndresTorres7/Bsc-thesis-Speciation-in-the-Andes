## Data donwloaded from https://doi.org/10.15468/dl.mbedcm
setwd("C:/Users/Flia. Ramos/Desktop/probando")


library("writexl")
library("readxl")
library("rgbif")
library("dplyr")



#reading raw data
rawdata<-read.csv("rawspecieslist.csv", sep = "\t")

# Selection of useful columns 
columns<-rawdata[,c("taxonRank", "kingdom", "genus", "genusKey", "order", "family")]


# Depuration of kingdoms
table(columns$kingdom)
threetaxa<-columns[columns$kingdom=="Animalia"|columns$kingdom=="Fungi"
           |columns$kingdom=="Plantae", ]
table(threetaxa$kingdom)

# Depuration of taxon rank / dont include "GENUS" because there are some genus with no species occurrences but only genus occurrences 
table(threetaxa$taxonRank)
d<-threetaxa[threetaxa$taxonRank=="SPECIES"|threetaxa$taxonRank=="SUBSPECIES", ]
d<-na.omit(d)
table(d$taxonRank)

# Removing repeated genus
y<-d[,c(3,4,5,6)] %>% distinct(genusKey, .keep_all = T) 

#Getting the number of occurences per genus
for (o in 1:nrow(y)) {
  
  y$numOccGenus[o]<-occ_count(taxonKey = y$genusKey[o], georeferenced = T)
  
}

# First approach to removing genus with less than 12 species and genus who doesn't 
#meet the requirement of having at least 12 species with at least 5 occurrences each one. (According to https://sci-hub.hkvisa.net/10.1111/j.1365-2699.2006.01594.x) (historial de juan, chrome, 8/01/2022 1:32 am para encontrar los otros artículos)

y<-y[y$numOccGenus>=60,]

write_xlsx(y,"listOfGenusWithOccurences125.xlsx")

## Getting the number of spp in gbif for every genus
m<-data.frame(y)


for (i in 1:nrow(y)) {

  z<-name_lookup(higherTaxonKey = y$genusKey[i], hl = T)  #search spp names of the genus
  z<-z[["data"]][,c("species","rank","taxonomicStatus","synonym","speciesKey")]  # Select the wanted columns
  z<-z[z$synonym=="FALSE",-4]
  z<-z[z$taxonomicStatus=="ACCEPTED",-3]
  z<-z[z$rank=="SPECIES"|z$rank=="SUBSPECIES",-2]
  z<-na.omit(z)
  z<-z %>% distinct(speciesKey, .keep_all=T)
  
  for (ii in 1:nrow(z)) {
    
    z$numPerSp[ii]<- occ_count(taxonKey = z$speciesKey[ii], georeferenced = T)  
    
  }
  
  z<-z[z$numPerSp>0,]
  
  m$numSpp[i]<-length(z$species)
  
  }


# Exporting dataframe of genus present in Andes

write_xlsx(m, "Clados_candidatos.xlsx")


numsp<-m[m$numSpp>=12,]

write_xlsx(numsp, "Clados_candidatos_con_mas_de_doce_spp.xlsx")

























