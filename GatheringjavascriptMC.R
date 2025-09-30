
  library(readxl)
  
  setwd("/datos/ramos.andres/Occurrence_data")
  
  genusList <- "GenusReadyFor.xlsx"
  naames <- read.csv("realallspecies.csv")
  a<- character()
  
  
  
  for(z in 1:nrow(genusList)  ){ 
    
    setwd(paste("/datos/ramos.andres/Occurrence_data", genusList$Genus, sep = "/"))
    
    spplist<-dir()
    spplist <- spplist[-which(spplist == "sppList.csv")] #Remove the csv file
    
    for (i in 3:length(spplist)){
      
      
      if(spplist[i] %in% naames$x) {
        
        setwd(paste("/datos/ramos.andres/Occurrence_data", genusList$Genus, spplist[i], sep = "/"))
        
        
        if(file.exists("Candidate_models.sh")){
          
          b<-readLines("Candidate_models.sh")
          
          a<-c(a,b)
          
        }
        
        
        
        }
      
      
    }
    }
  setwd("/home/ramos.andres")
  writeLines(a, "models.sh")
