library(raster)
library(readxl)

setwd("/datos/ramos.andres")
genusList<- read_xlsx("igualAUno.xlsx")
naames <- read.csv("allspeciessinraya.csv")
k <- list()


for(v in 1:nrow(genusList)) {
  
setwd(paste("/datos/ramos.andres/Occurrence_data", genusList$Genus[v], sep = "/"))  
  
  spplist<-dir()
  spplist <- spplist[-which(spplist == "sppList.csv")] #Remove the csv file
  
  fornames <- spplist
  
  for (i in 1:length(spplist)) {
    
    if (file.exists("Final_Models")) {
      
      setwd(paste("/datos/ramos.andres/Occurrence_data", genusList$Genus[v],"Final_Models", sep = "/"))  
      
      models<-dir()
      
      
      setwd(paste("/datos/ramos.andres/Occurrence_data", genusList$Genus[v],"Final_Models", models[length(models)], sep = "/"))
      
      model<- raster(list.files(pattern = "_avg.asc")) 
      maxent<- read.csv("maxentResults.csv")
      max<-maxent[nrow(maxent),29]
      
      bina <- a >= max
      k <- as.list(c(k, bina))
  
      
    } else {
      
      print("No final models")
      fornames<- fornames[-i]
      
    }
    
    
  }
  
  setwd(paste("/datos/ramos.andres/Occurrence_data", genusList$Genus[v], sep = "/"))
  names(k) <- fornames
  saveRDS(k, file = paste(genusList$Genus[v],"rasters.rda", sep = "_"))
  
}