GBIF_down_view <-function(especie, mapview=TRUE, seed=123){
  require(rgbif)
  require(scrubr)
  require(openxlsx)
  require(rlang)
  require(mapview)
  require(sf)
  require(rgdal)
  require(stringr)
  require(tidyverse)
  require(spThin)
  
  gbif_data <- occ_data(scientificName = especie, 
                        hasCoordinate = TRUE, 
                        limit = 20000)
  # myspecies_coords <- gbif_data$data %>% 
  #   dplyr::select(decimalLongitude, decimalLatitude, 
  #                 individualCount, occurrenceStatus,
  #                 coordinateUncertaintyInMeters,
  #                 institutionCode, references, 
  #                 basisOfRecord, stateProvince, 
  #                 year, month,day, eventDate)
  
  # absence_rows <- which(myspecies_coords$individualCount == 0 | myspecies_coords$occurrenceStatus %in% c("absent", "Absent", "ABSENT", "ausente", "Ausente", "AUSENTE"))
  # length(absence_rows)
  # if (length(absence_rows) > 0) {
  #   myspecies_coords <- myspecies_coords[-absence_rows, ]
  # }
  # myspecies_coords <- coord_incomplete(coord_imprecise(coord_impossible(coord_unlikely(myspecies_coords))))
  # myspecies_coords <- coord_uncertain(myspecies_coords, 
  #                                     coorduncertainityLimit = 5000)
  
  occurrences <- gbif_data$data
  # excluding records with no coordinates
  occurrences <- occurrences[!is.na(occurrences$decimalLongitude) | !is.na(occurrences$decimalLatitude), ]
  
  # excluding duplicates
  occurrences$code <-  paste(occurrences$name, occurrences$decimalLongitude, 
                             occurrences$decimalLatitude, sep = "_")
  
  # erasing duplicates
  occurrences <- occurrences[!duplicated(occurrences$code), 1:4] 
  
  # excluding records with (0, 0) coordinates
  occurrences <- occurrences[occurrences$decimalLongitude != 0 & occurrences$decimalLatitude != 0, 1:3]
  
  # saving the new set of occurrences 
  write.csv(occurrences, "data/cmex_clean.csv", row.names = FALSE)
  
  
  # thinning
  thin(occurrences, lat.col = "decimalLatitude",
       long.col = "decimalLongitude", spec.col = "name",
       thin.par = 10, reps = 10, write.files = TRUE,
       max.files = 1, out.base = "sp",
       write.log.file = FALSE, verbose = TRUE)
  
  
  # training and testing data splitting. randomly 75% for training and 25% for testing
  occ_thinn <- read.csv("sp_thin1.csv")
  occ_thinn$name <- gsub(" ", "_", occ_thinn$name)
  
  all <- unique(occ_thinn)
  
  all$check <- paste(all[,2], all[,3], sep = "_")
  train <- all[sample(nrow(all), round((length(all[,1])/4 *3))), ]
  test <- all[!all[,4] %in% train[,4], ]
  
  all$check <- NULL
  train$check <- NULL
  test$check <- NULL
  
  write.csv(all, "Sp_joint.csv", row.names = FALSE)
  write.csv(train, "Sp_train.csv", row.names = FALSE)
  write.csv(test, "Sp_test.csv", row.names = FALSE)
  
  especie_2 <- stringr::str_replace(especie," ","_")
  
  assign(especie_2, occurrences, envir=globalenv())
  
  occurrences %>% write.xlsx(paste(especie,"GBIF.xlsx"))
  
  occurrences |> write.csv("Sp_joint.csv")
  
  set.seed(seed)
  parti80 <- occurrences %>% sample_frac(0.8)
  parti20 <- anti_join(occurrences, parti80)
  
  parti80 |> write.csv("Sp_train.csv")
  parti20 |> write.csv("Sp_test.csv")
  
  if (mapview == TRUE){
    coord_mapview <<- st_as_sf(occurrences,
                               coords = c("decimalLongitude", 
                                          "decimalLatitude"),
                               crs =  CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
    
    mapview::mapview(coord_mapview, layer.name = especie_2)
    
  } else if (mapview == FALSE) {
    print("No mapa")
  }
}