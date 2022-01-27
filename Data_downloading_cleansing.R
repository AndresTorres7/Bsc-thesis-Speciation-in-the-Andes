
#-----------------------------DATA OCCURRENCE FROM GBIF-------------------------
# directory
setwd("C:/Users/juanp/Desktop/Tesis Andres")
basepath<-"C:/Users/juanp/Desktop/Tesis Andres"

#-------------------Downloading species occurrences-----------------------------

library(rgbif)
library(readxl)
library(writexl)

clados<-read_xlsx("Clados_candidatos_con_mas_de_doce_spp.xlsx")

dir.create("Occurrence_data")

for (v in 1:nrow(clados)) {
  
  setwd(paste(basepath,"Occurrence_data", sep= "/"))
  x<-name_lookup(higherTaxonKey = clados$genusKey[v], hl = T)  #search spp names of the genus
  x<-x[["data"]][,c("species","rank","taxonomicStatus","synonym","speciesKey")]  # Select the wanted columns
  x<-x[x$synonym=="FALSE",-4]
  x<-x[x$taxonomicStatus=="ACCEPTED",-3]
  x<-x[x$rank=="SPECIES"|x$rank=="SUBSPECIES",-2]
  x<-na.omit(x)
  x<-x %>% distinct(speciesKey, .keep_all=T)
  
  dir.create(clados$Genus[v])
  setwd(paste(getwd(),clados$Genus[v], sep= "/"))
  
  tablaGenus<-data.frame(x)

for (k in 1:nrow(x)) {
  setwd(paste("C:/Users/juanp/Desktop/Tesis Andres/Occurrence_data", clados$Genus[v], sep = "/"))
  dir.create(x$species[k])
  setwd(paste(getwd(),x$species[k], sep= "/"))
  ## Polígono de America hecho a mano para filtrar outliers localizados en los oceanos.
  datos<-occ_data(taxonKey = clados$speciesKey[k], hasCoordinate = T, geometry =  "POLYGON((-77.01855 10.14697,-84.14209 16.27075,-87.8335 16.552,  
                  -86.25586 22.54394,-95.25586 19.73144,-96.5918 27.39551,-87.87304 29.64551,-79.65527 25.12353,-80.70117 31.05176,-59.18555 46.87207,
                  -52.36523 46.73144,-51.67969 51.08203,-62.92969 62.05077,-75.30467 75.26953,-59.27343 69.36328,-47.46094 55.86328,-39.58593 62.89453,
                  -23.83594 67.39453,-8.64841 83.98828,-58.99219 84.83203,-93.58592 83.42578,-120.86719 79.76953,-131.44043 70.40478,-150.74121 71.14307,
                  -153.20215 58.979,-142.65527 59.40088,-130.7373 52.36963,-128.13574 44.67041,-121.03418 34.229,-111.43652 22.52197,-107.63965 23.04932,
                  -106.19824 22.17041,-106.19824 20.69385,-104.40527 18.05713,-94.73731 14.75244,-88.02246 12.57275,-86.61621 10.07666,-82.39746 6.45557,
                  -78.81152 7.26416,-78.1084 3.67822,-80.6748 1.42822,-82.0459 -5.35693,-76.28027 -15.16553,-70.97168 -19.80615,-72.30762 -28.31397,
                  -76.24512 -45.15381,-76.13965 -53.69678,-68.44043 -56.61475,-64.6084 -55.17334,-67.63184 -51.13037,-60.74121 -41.91943,-39.8584 -23.28662,
                  -38.03027 -13.75928,-34.72558 -10.17334,-34.26855 -5.42725,-49.10449 3.25635,-60.31933 11.09619,-71.67481 13.27588,-77.01855 10.14697))" ,limit = 100000 )
  
  write_xlsx(datos, paste(gsub(" ", "_", x$species[k]),"Raw_data.xlsx", sep = "_")) # rewrite so that species name is abbreviated (ex. Leopardus pardalis = L. pardalis)  
  
  spcolumns <- c("scientificName", "longitude", "latitude", "year") #look up for a way of filtering by cordinateUncertainity without error
  
  occ <- datos[, spcolumns]
  write.csv(occ, paste(x$species[k],"_GBIF.csv", sep = "_"), row.names = F)
  occ <- na.omit(occ)
  
  ## excluding older records
  occ <- occ[occ$year >= 1960, -4]
  
  ## excluding 0, 0 coordinates 
  occ <- occ[occ$longitude != 0 & occ$latitude != 0, ]
  
  ## excluding duplicates
  occ <- occ[!duplicated(paste(occ$longitude, occ$latitude)), ]
  
  ## changing col 1 name
  names(occ) [1] <- paste("species")
  
  ## changing species name
  occ$species <- gsub(" ", "_", x$species[k])
  
  ##save occurrences data 
  write.csv(occ, paste(gsub(" ", "_", x$species[k]) ,"clean.csv", sep= "_"), row.names = F)

  
  #-----------------------------Thinning------------------------------------------
  # spatial distance thinning
  
  library(ellipsenm)
  
  occt <- thin_data(occ, "longitude", "latitude", thin_distance = 20, save = T, #Change the Km based on environmental resolution 
                    name = "Occ_Thinning_20km")
  write.csv(occt, paste(gsub(" ", "_", x$species[k]) ,"thin.csv", sep= "_"), row.names = F)

  tablaGenus$numOcc[k] <- nrow(occt)  ## número de ourrencias por especie
  
}
  
  clados$numSpp2[v] <- sum(tablaGenus$numOcc) ## número de especies por género luego del segundo filtro
 
   write_xlsx(tablaGenus,"sppList")
  
}

clados<-clados[clados$numSpp2 >= 60,]
write_xlsx(clados, "segundoFiltroMasde12spp.xlsx")

## Segundo filtro de géneros por número de especies y cantidad de ocurrencias por especie (mínimo 5 registros por especie, mínimo 12 spp. por género)


