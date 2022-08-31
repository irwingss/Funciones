GBIF_down_view <-function(Especie, mapview=TRUE, seed=123,
                          occ_data_limit = 100000, separacion_puntos=10,
                          df_CRS=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")){
  
  # Paquetes
  if (!require("rgbif")) install.packages("rgbif") 
  if (!require("scrubr")) install.packages("scrubr") 
  if (!require("openxlsx")) install.packages("openxlsx") 
  if (!require("rlang")) install.packages("rlang")
  if (!require("mapview")) install.packages("mapview")
  if (!require("sf")) install.packages("sf")
  if (!require("rgdal")) install.packages("rgdal") 
  if (!require("stringr")) install.packages("stringr") 
  if (!require("tidyverse")) install.packages("tidyverse")
  if (!require("spThin")) install.packages("spThin")
  if (!require("pacman")) install.packages("pacman")
  paquetes <- c("rgbif", "scrubr", "openxlsx","rlang","mapview",
                      "sf", "rgdal", "stringr", "spThin", "tidyverse")
  pacman::p_load(char = paquetes)
  
  # ------------------------------------------------------------------- -
  # Descargar la data
  gbif_data <- occ_data(scientificName = Especie, 
                        hasCoordinate = TRUE, 
                        limit = occ_data_limit)
  # Extraer las ocurrencias
  occurrences <- gbif_data$data
  
  # Eliminar registros sin coordenadas
  occurrences <- occurrences %>%
    filter(!is.na(decimalLongitude) | !is.na(decimalLatitude))
  
  # Eliminar registros duplicados
  occurrences$code <-  paste(occurrences$name, occurrences$decimalLongitude, 
                             occurrences$decimalLatitude, sep = "_")
  occurrences <- occurrences[!duplicated(occurrences$code), ] 
  
  # Eliminar registros con coordenadas (0, 0)
  occurrences <- occurrences %>%  
    filter(decimalLongitude != 0 & decimalLatitude != 0) %>% 
    mutate(scientificName=Especie)
  
  # Guardar las ocurrencias
  DF <- apply(occurrences,2,as.character)
  write.csv(as.data.frame(DF), paste0(Especie,"_occ_full.csv"), 
            row.names = FALSE)
  
  
  # Reducción de puntos cercanos (Thining)
  suppressWarnings(thin(occurrences, lat.col = "decimalLatitude",
       long.col = "decimalLongitude", spec.col = "scientificName",
       thin.par = separacion_puntos, # 10 = 10 km de separación
       reps = 10, write.files = TRUE,
       max.files = 1, out.base = Especie,
       out.dir = getwd(), write.log.file = FALSE, 
       verbose = FALSE))
  
  
  # Separar la base en entrenamiento 75% y testeo 25%
  occ_thinn <- read.csv(paste0(Especie,"_thin1.csv"))
  occ_thinn$scientificName <- gsub(" ", "_", occ_thinn$scientificName)
  
  all <- unique(occ_thinn)
  all$check <- paste(all[,2], all[,3], sep = "_")
  train <- all[sample(nrow(all), round((length(all[,1])/4 *3))), ]
  test <- all[!all[,4] %in% train[,4], ]
  
  all$check <- NULL
  train$check <- NULL
  test$check <- NULL
  
  # ------------------------------------------------------------------- -
  # Creación de bases de datos en la carpeta de trabajo
  write.csv(all, paste0(Especie,"_thin1","_joint.csv"), row.names = FALSE)
  write.csv(train, paste0(Especie,"_thin1","_train.csv"), row.names = FALSE)
  write.csv(test, paste0(Especie,"_thin1","_test.csv"), row.names = FALSE)
  
  # ------------------------------------------------------------------- -
  # Mapa
  if (mapview == TRUE){
    data_temp <- occurrences %>% 
      select(scientificName, decimalLongitude,
             decimalLatitude, eventDate, country, 
             locality, elevation)
    
    coord_mapview <<- st_as_sf(data_temp,
                               coords = c("decimalLongitude", 
                                          "decimalLatitude"),
                               crs =  df_CRS)
    
    mapview::mapview(coord_mapview, layer.name = Especie)
    
    #En el mapa se muestran los puntos de presencia luego del proceso 
    # de limpieza de datos y de thinning (reducción de puntos cercanos 
    # en base al argumento separacion_puntos expresado en km [10km por defecto])
  }
  # FIN
}