

library(writexl)


setwd("/datos/ramos.andres/Occurrence_data")

generos <- dir()

datos <- data.frame(generos = generos, M = NA, modelos_candidato = NA,
                    seleccion_modelo = NA, modelo_final = NA, raster = NA)

for(i in 1:length(generos)) {
  
  setwd(paste("/datos/ramos.andres/Occurrence_data", generos[i], sep = "/"))
  spp <- dir()
  spp <- spp[-which(spp == "sppList.csv")]
  
  if (file.exists(paste(generos[i], "rasters.rda", sep ="_"))){
    spp <- spp[-which(spp == list.files(pattern = "rasters.rda"))]
    datos$raster[i] <- "Done"
    datos$modelo_final[i] <- "Done"
    datos$seleccion_modelo[i] <- "Done"
    datos$modelos_candidato[i] <- "Done"
    datos$M[i] <- "Done"
  } else {
    
    setwd(paste("/datos/ramos.andres/Occurrence_data", generos[i], spp[length(spp)], sep = "/"))
   
    
    
    if (file.exists("Final_Models.sh")) {
      
      datos$raster[i] <- "Missing"
      datos$modelo_final[i] <- "Done"
      datos$seleccion_modelo[i] <- "Done"
      datos$modelos_candidato[i] <- "Done"
      datos$M[i] <- "Done"
    
    
    } else {
    
      if (file.exists("Calibration_results")) {
        
        datos$raster[i] <- "Missing"
        datos$modelo_final[i] <- "Missing"
        datos$seleccion_modelo[i] <- "Done"
        datos$modelos_candidato[i] <- "Done"
        datos$M[i] <- "Done"
        
        
      } else {
        
        if(file.exists("Candidate_Models")) {
          
          setwd(paste("/datos/ramos.andres/Occurrence_data", generos[i], spp[length(spp)], 
                      "Candidate_Models",sep = "/"))
          b <- dir()
          setwd(paste("/datos/ramos.andres/Occurrence_data", generos[i], spp[length(spp)], 
                      "Candidate_Models", b[length(b)],sep = "/"))
          c<- dir()
              if (length(c)!= 0){
                datos$raster[i] <- "Missing"
                datos$modelo_final[i] <- "Missing"
                datos$seleccion_modelo[i] <- "Missing"
                datos$modelos_candidato[i] <- "Done"
                datos$M[i] <- "Done"
                
              } else {
                
                datos$raster[i] <- "Missing"
                datos$modelo_final[i] <- "Missing"
                datos$seleccion_modelo[i] <- "Missing"
                datos$modelos_candidato[i] <- "Missing"
                datos$M[i] <- "Done"
              }
          
          
        } else {
          
          setwd(paste("/datos/ramos.andres/Occurrence_data", generos[i], spp[length(spp)], sep = "/"))
          
          if (!file.exists("./M/Set_1/PC3.asc") | file.exists("tempi")) {
          
          datos$raster[i] <- "Missing"
          datos$modelo_final[i] <- "Missing"
          datos$seleccion_modelo[i] <- "Missing"
          datos$modelos_candidato[i] <- "Missing"
          datos$M[i] <- "Missing"
          } else {
            
            datos$raster[i] <- "Missing"
            datos$modelo_final[i] <- "Missing"
            datos$seleccion_modelo[i] <- "Missing"
            datos$modelos_candidato[i] <- "Missing"
            datos$M[i] <- "Done"
            
          }
          
          
        }
        
        
        
        
      }
      
      
      
      
  }

    
     
}
}

setwd("/datos/ramos.andres")

write_xlsx(datos, "Tabla_progreso.xlsx")


