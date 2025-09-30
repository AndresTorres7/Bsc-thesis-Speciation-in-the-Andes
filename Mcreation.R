

#setwd("C:/Users/Andres Torres/Desktop/Vainas pal curso")

#setwd("D:/Occurrence_data/Sturnira")

library(rgeos)
library(raster)
library(readxl)


#setwd("/datos/ramos.andres")
#genusList<- read_xlsx("igualAUno.xlsx")
setwd("C:/Users/Andres Torres/Documents/Biología/AndesGMSpeciationInfering/1CandidatesClades/Results")
naames <- read.csv("realallspecies.csv")

setwd("C:/Users/Andres Torres/Documents/Biología/AndesGMSpeciationInfering/2PotentialDistriModelling/Worldclim/Origin")

map1<- raster("PC1.tif")
map2<- raster("PC2.tif")
map3<- raster("PC3.tif")

crs(map1)<- "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs"
crs(map2)<- "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs"
crs(map3)<- "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs"

maps <- c(map1, map2, map3)





  
  setwd("D:/Occurrence_data/Zeltnera")
  
  spplist<-dir()
  spplist <- spplist[-which(spplist == "sppList.csv")] #Remove the csv file
  
 
  
  for (i in 1:length(spplist)) {
    # foreach( i = 1:length(spplist)) %dopar% {
    setwd(paste("D:/Occurrence_data/Zeltnera", spplist[i], sep = "/"))
    
    if(spplist[i] %in% naames$x) {
      
      #print(spplist[i])
      
      if (!file.exists("./M/Set_1/PC3.asc") | file.exists("tempi")){
        
        a<-dir()
        
        if( length(a)>6 ) {
          
          #creates unique filepath for temp directory
          setwd("C:/Users/Andres Torres/Desktop/Vainas pal curso")
          dir.create ("tempi", showWarnings = FALSE)
          #sets temp directory
          rasterOptions(tmpdir="C:/Users/Andres Torres/Desktop/Vainas pal curso/tempi")
          
          #------------------ building M ############
          setwd(paste("D:/Occurrence_data/Zeltnera", spplist[i], sep = "/"))
          dir.create("M")
          Occurencedata<- read.csv(list.files(pattern = "join.csv"))
          cordi <- Occurencedata[,c(2,3)]
          spData <- SpatialPointsDataFrame(cordi, Occurencedata)
          crs(spData) <- "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs"
          buferr<- gBuffer(spData, width = 2)
          
          mascas <- lapply(X = maps, FUN = mask, mask = buferr)
          #mascara1<- mask(map1, buferr)
          #mascara2<- mask(map2, buferr)
          #mascara3<- mask(map3, buferr)
          
          setwd(paste("D:/Occurrence_data/Zeltnera", spplist[i], "M", sep = "/")) 
          dir.create("Set_1")
          
          maskis <- lapply(mascas, crop, y = buferr)
          #maski1 <- crop(mascara1, buferr)
          #maski2 <- crop(mascara2, buferr)
          #maski3 <- crop(mascara3, buferr)
         
          setwd(paste("D:/Occurrence_data/Zeltnera", spplist[i], "M", "Set_1", sep = "/")) 
          writeRaster(maskis[[1]], "PC1.asc", format = "ascii", datatype = "INT4S", overwrite = T)
          writeRaster(maskis[[2]], "PC2.asc", format = "ascii", datatype = "INT4S", overwrite = T)
          writeRaster(maskis[[3]], "PC3.asc", format = "ascii", datatype = "INT4S", overwrite = T)
          
          setwd("C:/Users/Andres Torres/Desktop/Vainas pal curso")
          unlink("tempi", recursive = TRUE)
          
        } else {
          
          print(paste( spplist[i],"NoData", sep = "_"))
          
        }
        
      }
      
    }else {
      setwd(paste("D:/Occurrence_data/Yucca", spplist[i], sep = "/"))
      write.table(x = "noinphylo","noinphylo.txt")
    }
    
  } 

