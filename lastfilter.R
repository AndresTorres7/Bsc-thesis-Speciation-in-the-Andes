#-----------------------------DATA OCCURRENCE FROM GBIF-------------------------
# directory
basepath<-"/datos/ramos.andres"

setwd("/datos/ramos.andres")


#-------------------Downloading species occurrences-----------------------------

library(rgbif)
library(readxl)
library(writexl)
library(dplyr)
library(ellipsenm)
library(doParallel)

## Poligono de America hecho a mano para filtrar outliers localizados en los oceanos.
poli <-"POLYGON((-77.01855 10.14697,-84.14209 16.27075,-87.8335 16.552,  
                  -86.25586 22.54394,-95.25586 19.73144,-96.5918 27.39551,-87.87304 29.64551,-79.65527 25.12353,-80.70117 31.05176,-59.18555 46.87207,
                  -52.36523 46.73144,-51.67969 51.08203,-62.92969 62.05077,-75.30467 75.26953,-59.27343 69.36328,-47.46094 55.86328,-39.58593 62.89453,
                  -23.83594 67.39453,-8.64841 83.98828,-58.99219 84.83203,-93.58592 83.42578,-120.86719 79.76953,-131.44043 70.40478,-150.74121 71.14307,
                  -153.20215 58.979,-142.65527 59.40088,-130.7373 52.36963,-128.13574 44.67041,-121.03418 34.229,-111.43652 22.52197,-107.63965 23.04932,
                  -106.19824 22.17041,-106.19824 20.69385,-104.40527 18.05713,-94.73731 14.75244,-88.02246 12.57275,-86.61621 10.07666,-82.39746 6.45557,
                  -78.81152 7.26416,-78.1084 3.67822,-80.6748 1.42822,-82.0459 -5.35693,-76.28027 -15.16553,-70.97168 -19.80615,-72.30762 -28.31397,
                  -76.24512 -45.15381,-76.13965 -53.69678,-68.44043 -56.61475,-64.6084 -55.17334,-67.63184 -51.13037,-60.74121 -41.91943,-39.8584 -23.28662,
                  -38.03027 -13.75928,-34.72558 -10.17334,-34.26855 -5.42725,-49.10449 3.25635,-60.31933 11.09619,-71.67481 13.27588,-77.01855 10.14697))"

clados<-read_xlsx("clados2filo.xlsx")

 clados$numSpp2 <- NA
 
  clados$numOccGenus2 <- NA
clados2<-clados

dir.create("Occurrence_data")

nCores<- 57
myCluster <-makeCluster(nCores)
registerDoParallel(myCluster)

## Se va a hacer por partes
#1:350  Se logró pero quedó incompleto
#350:700
#700:nrow(clados)

#Voy a comenzar a ir en grupos de 50

#350:400 incompleto

# Voy a dejarlo el findesemana completo en grupos de 40 todos los que faltan.

m2<-foreach(v= nrow(clados):1) %dopar% {
  
  library(rgbif)
  library(readxl)
  library(writexl)
  library(dplyr)
  library(ellipsenm)
  library(doParallel)

  setwd(paste(basepath,"Occurrence_data", sep= "/"))
  
  x<-name_lookup(higherTaxonKey = clados$genusKey[v], limit = 10000 , hl = T) #search spp names of the genus
  x<-x[["data"]][,c("species","rank","taxonomicStatus","synonym","speciesKey")]  # Select the wanted columns
  x<-x[x$synonym=="FALSE",-4]
  x<-x[x$taxonomicStatus=="ACCEPTED",-3]
  x<-x[x$rank=="SPECIES"|x$rank=="SUBSPECIES",-2]
  x<-na.omit(x)
  x<-x %>% distinct(speciesKey, .keep_all=T)
  
  dir.create(clados$genus[v])
  
  setwd(paste(basepath,"Occurrence_data",clados$genus[v], sep= "/"))
  
  tablaGenus<-data.frame(x)
  
  for (kk in 1:nrow(x)) {
    tablaGenus$NumperSp[kk]<- occ_count(taxonKey = tablaGenus$speciesKey[kk] , georeferenced = T) #Numpersp se calcula a partir de la función occ_count, no está filtrado 
  }
  
  tablaGenus<-tablaGenus[tablaGenus$NumperSp > 0,]
  
  
  for (k in 1:nrow(tablaGenus)) {
    setwd(paste(basepath, "Occurrence_data", clados$genus[v], sep = "/"))
    dir.create(tablaGenus$species[k])
    print("c3")
    setwd(paste(basepath,"Occurrence_data",clados$genus[v],tablaGenus$species[k], sep= "/"))
    print("c4")
    
    datos<-occ_data(taxonKey = tablaGenus$speciesKey[k], hasCoordinate = T, geometry = poli, limit = 100000)
    a<-datos[["data"]]
    
    
    if (is.null(a)) {
      setwd(paste(basepath, "Occurrence_data", clados$genus[v], sep = "/"))
      unlink(tablaGenus$species[k], recursive = T)
      tablaGenus$numOcc[k] <- 0
      
      
    } else { 
      
      a1<-apply(a, 2, as.character)
      name<- paste(substr(tablaGenus$species[k], 1,1),paste(gsub(".* ", "", tablaGenus$species[k]),"Raw_data.csv", sep = "_"), sep = "_"  )
      write.csv(a1, name, row.names = F) # rewrite so that species name is abbreviated (ex. Leopardus pardalis = L. pardalis)  
      
      spcolumns <- c("scientificName", "decimalLongitude", "decimalLatitude",  "basisOfRecord") #"year" #look up for a way of filtering by cordinateUncertainity
      
      occ <- a[, spcolumns]
      occ1<-apply(occ, 2, as.character)
      write.csv(occ1, paste(tablaGenus$species[k],"_GBIF.csv", sep = "_"), row.names = F)
      occ <- na.omit(occ)
      
      basesofrecord <- c("HUMAN_OBSERVATION", "LITERATURE", "LIVING_SPECIMEN", "MACHINE_OBSERVATION", 
                         "OBSERVATION", "PRESERVED_SPECIMEN", "OCCURRENCE")
      
      occ<- occ[occ$basisOfRecord %in% basesofrecord, -4 ] 
      
      
      ## excluding older records
      ##occ <- occ[occ$year >= 1960, -4]
      ## lo tuve que quitar porque algunas no tienen fecha
      
      
      ## excluding 0, 0 coordinates 
      occ <- occ[occ$decimalLongitude != 0 & occ$decimalLatitude != 0, ]
      
      ## excluding duplicates
      occ <- occ[!duplicated(paste(occ$decimalLongitude, occ$decimalLatitude)), ]
      
      ## changing col 1 name
      names(occ) [1] <- paste("species")
      names(occ)[2] <- paste("longitude")
      names(occ)[3] <- paste("latitude")
      ## changing species name
      occ$species <- gsub(" ", "_", tablaGenus$species[k])
      
      ##save occurrences data 
      occ1<-apply(occ, 2, as.character)
      write.csv(occ1, paste(gsub(" ", "_", tablaGenus$species[k]) ,"clean.csv", sep= "_"), row.names = F)
      
      if(nrow(occ)>4) {
        
        
        
        #-----------------------------Thinning------------------------------------------
        # spatial distance thinning
        
        
        occt <- thin_data(occ, "longitude", "latitude", thin_distance = 1, save = T, #Change the Km based on environmental resolution 
                          name = "Occ_Thinning_1km")
        occt1<-apply(occt, 2, as.character)
        write.csv(occt1, paste(gsub(" ", "_", tablaGenus$species[k]) ,"thin.csv", sep= "_"), row.names = F)
        
        tablaGenus$numOcc[k] <- nrow(occt)  ## n???mero de ourrencias por especie luego de limpieza
        
        # 50% split 
        
        all <- occt
        
        all <- unique(all)
        
        test<- all[j<-sample(nrow(all), round((nrow(all[,1])/2))), ]
        train <- all[-j,]
        
        write.csv(all, paste(gsub(" ", "_", tablaGenus$species[k]) ,"join.csv", sep= "_"), row.names = F)
        write.csv(train, paste(gsub(" ", "_", tablaGenus$species[k]) ,"train.csv", sep= "_"), row.names = F)
        write.csv(test, paste(gsub(" ", "_", tablaGenus$species[k]) ,"test.csv", sep= "_"), row.names = F)
      } else {
        
        tablaGenus$numOcc[k] <- nrow(occ)  ## n???mero de ourrencias por especie luego de limpieza
      }
      
    }
    
  }
  
  setwd(paste(basepath,"Occurrence_data", clados$genus[v], sep = "/"))
  
  tablaGenus<-tablaGenus[tablaGenus$numOcc >= 5,]    # Filtro por numero de ocurrencias por especie
  
  
  
  clados$numSpp2[v] <- nrow(tablaGenus) ## n???mero de especies por g???nero luego del segundo filtro
  
  clados$numOccGenus2[v] <- sum(tablaGenus$numOcc) ## número de ocurrencias por genero luego del segundo filtro
  
  # Filtro por numero de especies con ocurrencias
  
  if (nrow(tablaGenus) >= 12) {  
    
    write.csv(tablaGenus,"sppList.csv", row.names = F)
    clados2$genusKey[v] <- 0
  } else {
    
    setwd(paste(basepath,"Occurrence_data", sep = "/"))
   # clados2[v] <- clados[clados$genusKey==clados$genusKey[v],]
    unlink(clados$genus[v],recursive = T)
    
  }
  
  
  
  
}


#stopCluster(myCluster)
#setwd("/datos/ramos.andres/Occurrence_data")

#clados3<-clados[!clados$genusKey%in%clados2$genusKey,]

#write.csv(clados, "Cladoscompletosconnumeros.csv", row.names = F)
#write.csv(m2, "porsiFiltroMasde12spp.csv", row.names = F)

#write.csv(clados3, "segundoFiltroMasde12spp.csv", row.names = F)
## Segundo filtro de g???neros por n???mero de especies y cantidad de ocurrencias por especie (m???nimo 5 registros por especie, m???nimo 12 spp. por g???nero)

