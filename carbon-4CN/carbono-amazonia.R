
amaz <- "c:/Users/pedro/Downloads/carbonoamz_original_true/carbonoamz_original_true.csv"

csv <- data.table::fread(amaz)

###########################################################################################
require(colrow)
dataDir <- "c:/Users/pedro/Dropbox/colrow/"

country <- "Brazil"

mySimU <- colrow::getSimU(country, dataDir)

mraster <- raster::raster("C:/Users/pedro/Downloads/veg-preterita-carbono/amazonia_carbtotal_vegetacao_preterita tiif.tif")
mraster

mySimU <- sf::st_transform(mySimU, crs = raster::crs(mraster))

dim(mySimU)[1]

process <- function(mraster){
  lapply(1:11003, function(i){
    print(i)

    res <- raster::extract(mraster, mySimU[[1]])
    
    if(length(res) > 0){
        return(sum(res, na.rm = TRUE))
    }
    
    return(0)
  })
}

result_amaz <- process(mraster)


###########################################################################################


total <- 0
quant <- 0
forEachBlock(mraster, function(v, row, nrows){
  total <<- total + sum(v, na.rm = TRUE)
  quant <<- quant + length(which(!is.na(v)))})

quant
total * 6.25 / 1e6

#raster::extract(data, polygon)


sum(mraster)

head(csv)

sum(csv$ctotal4inv, na.rm = TRUE) / 1e6 # 9596.81 MtC

sum(csv$ctotal4inv, na.rm = TRUE) * 6.25 / 1e6 # 59980.08 MtC

csv <- csv %>%
  dplyr::mutate(newcagb = cagb * tot_area_true / 6.25 / 1e4) %>%
  dplyr::mutate(newcbgb = cbgb * tot_area_true / 6.25 / 1e4) %>%
  dplyr::mutate(finalctotal4inv = ctotal4inv * tot_area_true / 6.25 / 1e4)

sum(csv$finalctotal4inv, na.rm = TRUE) / 1e6 # 13949.74 MtC
sum(csv$newcagb, na.rm = TRUE) / 1e6 # 13949.74 MtC
sum(csv$newcbgb, na.rm = TRUE) / 1e6 # 13949.74 MtC

csv <- csv %>%
  #  dplyr::mutate(newcagb = cagb * tot_area_true) %>%
  #  dplyr::mutate(newcbgb = cbgb * tot_area_true) %>%
  dplyr::mutate(finalctotal4inv = ctotal4inv * tot_area_true / 1e4) #tC/ha * m => /1e4 => tC

sum(csv$finalctotal4inv, na.rm = TRUE) / 1e6 # 87185 MtC
# ==> RESULTADO MAURO: 57.000MtC


length(which(is.na(csv$tot_area_true))) / dim(csv)[1] # 0% eh NA
length(which(is.na(csv$ctotal4inv))) / dim(csv)[1] # 2.5% eh NA

dim(csv)[1] / 1e6


areas <- round(csv$tot_area_true / 6.25 / 1e4, 0)

table(areas)

tot_area <- sum(csv$tot_area_true) / 1e4 / 1e6 # 829 Mha
68226120 * 6.25 / 1e6 # 426 Mha

# dado real: 420 Mha

tot_area / (68226120 * 6.25)

# ==> Mauro: 68.226.120 pixels



570/139


# a conta que voce fez deu 1.516.771,177549419924617. deveria ter dado 25 * 6.25 * 1e4 = 1.562.500
tot = 2.911050000000000 # ctotal4inv
area = 1516771 # tot_area_t

area / 1e4 / 6.25 # 24.2 (count eh 25)

tot * area / 1e4 / 6.25 # 70.64

resultado = 72.776252031326294 # mauro




#csv <- csv %>%
#  dplyr::mutate(newcagb = cagb * inter_area) %>%
#  dplyr::mutate(newcbgb = cbgb * inter_area) %>%
#  dplyr::mutate(newctotal4inv = ctotal4inv * inter_area)

amaz_original <- c(sum(csv$newcagb, na.rm=T), sum(csv$newcbgb, na.rm=T), sum(csv$newctotal4inv, na.rm=T))

amaz_original <- amaz_original %>% units::set_units("tC") %>% units::set_units("MtC")

amaz_gerado <- c(0, 0, 0)

mtable <- data.frame(type = c("cagb", "cbgb", "tot_carb"), shp = amaz_original, gerado = amaz_gerado)

mtable$proporcao <- round(mtable$gerado / mtable$shp * 100, 2)

mtable

mtable$biome <- "amazonia"


result <- rbind(result, mtable)

result %>% dplyr::filter(type == "tot_carb") %>% select(type, biome, shp)
