####  Calculating the number os spp in the Andes for every genus ----------------
library(readxl)
library(rgbif)
library(dplyr)


totalsp<- numeric()
existingsp<- numeric()
genero <- "Diglossa"
poli <- "MULTIPOLYGON(((-73.21289 10.29557,-74.59384 5.56196,-74.08702 8.88844,-75.07925 7.52501,
                        -76.60885 7.98634,-76.71673 4.23202,-79.37656 0.40091,-79.34288 -2.47184,
                        -80.84842 -4.11195,-74.91438 -15.13186,-70.25384 -18.05604,-71.61476 -34.46562,
                        -72.91156 -36.51196,-71.23404 -34.9772,-71.90981 -38.39411,-73.65395 -40.0169,
                        -73.58257 -41.00914,-72.51895 -40.03118,-72.68313 -42.15842,-73.51832 -45.19223,
                        -74.35352 -44.06437,-75.41 -46.45572,-73.77531 -46.46286,-75.58846 -49.68941,
                        -74.76754 -50.51033,-75.27437 -51.66674,-74.21075 -51.26699,-75.06022 -52.05936,
                        -74.39635 -53.13725,-71.45533 -54.97182,-68.28589 -55.30018,-66.25859 -54.672,
                        -71.29115 -53.93675,-71.88363 -52.05222,-72.75452 -51.82379,-71.66234 -50.803,
                        -72.77593 -50.27476,-71.8801 -49.87032,-72.79021 -49.43957,-72.18345 -49.58947,
                        -70.92746 -44.9298,-71.5398 -44.22498,-70.98301 -40.19179,-69.84801 -39.08534,
                        -70.46191 -38.84264,-69.64099 -35.50188,-67.82784 -31.59718,-68.43461 -29.66268,
                        -67.31471 -31.48038,-65.38652 -29.09161,-65.80054 -27.32843,-65.20806 -24.99418,
                        -63.07441 -20.20975,-63.51193 -17.73859,-65.37951 -16.94483,-68.15368 -13.77569,
                        -72.83185 -12.12605,-74.32913 -9.55544,-75.37311 -9.77514,-75.53148 -6.73456,
                        -77.59339 -4.72088,-78.09165 -2.19033,-77.14938 0.48657,-74.42252 3.09921,
                        -73.83052 2.19246,-73.57424 4.15898,-71.98476 7.2618,-69.21579 9.65327,-65.40193 10.10288,
                        -66.22885 10.65967,-70.54995 10.21233,-68.6559 10.92617,-70.43573 10.71677,
                        -69.3507 11.4782,-70.93066 10.43124,-71.11732 9.1106,-72.71323 8.41974,
                        -72.21003 11.07756,-73.21289 10.29557)))"

setwd("C:/Users/Andres Torres/Documents/Biología/AndesGMSpeciationInfering/1CandidatesClades/Results")
cladoss<-read_xlsx("Clados_candidatos_con_mas_de_doce_spp.xlsx")
thekey <- as.numeric(cladoss[cladoss$genus==genero,2])
thekey <- 7791043
###   Preparing general information of final genus


setwd("C:/Users/Andres Torres/Documents/Biología/AndesGMSpeciationInfering")
a<-read.csv("porcentaje.csv")
b<-read_xlsx("AditionalData.xlsx")
a$Edad_MYA <- b$`Edad (MYA)`
c<- read_xlsx("Actualspp.xlsx")

clados<- cladoss[cladoss$genus%in%a$Genus,c(1,2)]
#a$genusKey <- clados
colnames(clados)[1] <- "Genus"

merged <- merge(a,clados, by = "Genus"  )
merged$TotalAceptedSppinGbif <- NA

pa <- merged$genusKey[c(1:5)]

status = "ACCEPTED"

for(l in 96:119) {
  x<-name_lookup(higherTaxonKey = merged$genusKey[l], limit = 99998 , hl = T ) #search spp names of the genus
  x<-x[["data"]][,c("species","rank","taxonomicStatus","synonym","speciesKey", "genus")]  # Select the wanted columns
  x<-x[x$synonym=="FALSE",-4]
  x<-x[x$taxonomicStatus=="ACCEPTED",-3]
  x<-x[x$rank=="SPECIES",-2]
  x<-na.omit(x)
  x<-x %>% distinct(speciesKey, .keep_all=T)
  table(x$genus)
  merged$TotalAceptedSppinGbif[l]<- nrow(x)
  
  
}

library(writexl)
setwd("C:/Users/Andres Torres/Documents/Biología/AndesGMSpeciationInfering")
write_xlsx(merged, "GenuswitcCompletedAddData.xlsx")


for(o in 1:10){
  
  print(o)
  
}


setwd("C:/Users/Andres Torres/Documents/Biología/AndesGMSpeciationInfering/2PotentialDistriModelling/Results")
GenusList<-read_xlsx("generosListos.xlsx")


#Total sp in gbif
x<-name_lookup(higherTaxonKey = thekey, limit = 10000 , hl = T, status = "ACCEPTED") #search spp names of the genus
x<-x[["data"]][,c("species","rank","taxonomicStatus","synonym","speciesKey")]  # Select the wanted columns
x<-x[x$synonym=="FALSE",-4]
x<-x[x$taxonomicStatus=="ACCEPTED",-3]
x<-x[x$rank=="SPECIES",-2]
x<-na.omit(x)
x<-x %>% distinct(speciesKey, .keep_all=T)
numTotal<-nrow(x)

## Total spp in the andes
datos<-occ_data(taxonKey = thekey, hasCoordinate = T, geometry = poli, limit = 100000)
a<-datos[["data"]]
a<-a[["data"]][,c("species","rank","taxonomicStatus","synonym","speciesKey")]  # Select the wanted columns
a<-a[a$synonym=="FALSE",-4]
a<-a[a$taxonomicStatus=="ACCEPTED",-3]
a<-a[a$rank=="SPECIES",-2]
a<-na.omit(a)
Andessp<-a %>% distinct(speciesKey, .keep_all=T)

# Spp in my data

setwd(  paste("/datos/ramos.andres/Occurrence_data", genero, sep = "/")   )


pa<-occ_get(key = thekey, fields = c('scientificName'))



## Gbif spp in the Andes
setwd("C:/Users/Andres Torres/Documents/Biología/AndesGMSpeciationInfering")
#reading raw data
rawdata<-read.csv("rawspecieslist.csv", sep = "\t")

# Selection of useful columns 
columns<-rawdata[,c("species", "speciesKey", "taxonomicStatus","taxonRank", "kingdom", "genus", "genusKey", "order", "family")]


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
y<-d %>% distinct(speciesKey, .keep_all = T) 

#write.csv(y,"AllsppintheAndes.csv",row.names = F)


## number of Andes spp in gbif for 485 spp at least 1 occurrence

setwd("C:/Users/Andres Torres/Documents/Biología/AndesGMSpeciationInfering/2PotentialDistriModelling/Results")

listdegenus<-read_xlsx("generosListos.xlsx")


setwd("C:/Users/Andres Torres/Documents/Biología/AndesGMSpeciationInfering")

listdegenus<-read_xlsx("Actualspp.xlsx")

for(i in 1:nrow(listdegenus)) {

  sppinA<- y[y$genus==listdegenus$Genus[i],]
  
  listdegenus[i,6]<- nrow(sppinA)
    
}




a<-read.csv("numsppinAndes.csv")

a<-listdegenus[,c(1,6)]
a<-a[a$...6>10,]

names(a)[2]<-"numSpAndes"


write.csv(a, "numsppinAndes.csv", row.names = F)



### number of spp with actual data--------------
mainpath <- "/datos/ramos.andres/Occurrence_data"
andesspp<-read.csv("AllsppintheAndes.csv")

setwd("/datos/ramos.andres/Occurrence_data")
datos<- dir()

datos<-datos[-which(datos %in% c(list.files(pattern = ".zip")))]

final <- data.frame(Genus = datos, numspinmydata = NULL, sppinmydatayAndes = NULL)

for(s in 1:length(datos)) {
  
  
  setwd(paste(mainpath, datos[s], sep = "/"))

  spplist<-dir()
  spplist <- spplist[-which(spplist == "sppList.csv")] #Remove the csv file
  
  final$numspinmydata[s] <- length(spplist)
  
  andesymy<- spplist[spplist%in% andesspp$species] 
  
  final$sppinmydatayAndes[s] <- length(andesymy)
  
  
}

setwd("/datos/ramos.andres")
write.csv(final, "sppintheandesymydata.csv", row.names = F)




####   removing genus in my data -------------

toremove<- listdegenus[!listdegenus$Genus %in% a$Genus,c(1,6)]
#write.csv(toremove, "genustoremove.csv", row.names = F)

setwd("/datos/ramos.andres")
toremove<-read.csv("genustoremove.csv")
setwd("/datos/ramos.andres/Occurrence_data")

unlink(toremove$Genus, recursive = T, force = T)

################ Removing genus with no enough data---------------

setwd("C:/Users/Andres Torres/Documents/Biología/AndesGMSpeciationInfering")

a<-read.csv("sppintheandesymydata.csv")

quedan<- a[a$sppinmydatayAndes>11,]

quedan$PorcentajeAndesspPresentsp <-(quedan$sppinmydatayAndes/quedan$numspinmydata)*100

quedan<-quedan[quedan$PorcentajeAndesspPresentsp>=75,]

#write.csv(quedan, "porcentaje.csv", row.names = F)
#a<-read.csv("porcentaje.csv")


toremove<- a[!a$Genus %in% quedan$Genus,c(1,2)]
write.csv(toremove, "genustoremovenew.csv", row.names = F)

setwd("/datos/ramos.andres")
toremove<-read.csv("genustoremove.csv")
setwd("/datos/ramos.andres/Occurrence_data")

unlink(toremove$Genus, recursive = T, force = T)

library(writexl)
write_xlsx(TotalGenus, "TotalGenus.xlsx")





