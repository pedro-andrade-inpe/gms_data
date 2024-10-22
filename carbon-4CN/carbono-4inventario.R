# cagb =
# cbgb =
# ctotal4inv =

devtools::install_github("pedro-andrade-inpe/colrow")
require(colrow)
dataDir <- "c:/Users/Usuario/Dropbox/colrow/"

country <- "Brazil"


mdir <- "C:/Users/Usuario/Dropbox/pesquisa/2021/4inventario/veg-preterita-carbono/"

pantanal <- "Pantanal CarVeg/pantanal_carbono_vegetacao_preterita.shp"
caatinga <- "Caatinga CarbVeg/caatinga_carbono_vegetacao_preterita.shp"
pampa    <- "Pampa Carb Veg/pampa_carbono_vegetacao_preterita.shp"
mata     <- "Mata CarbVeg/mata_atlantica_carbono_vegetacao_preterita.shp"
cerrado  <- "Cerrado CarbVeg/cerrado_carbono_vegetacao_preterita.shp"

shp <- sf::read_sf(paste0(mdir, pantanal))

myCR <- colrow::getCR(country, dataDir)
myCR <- sf::st_transform(myCR, sf::st_crs(shp))

#amazonia <- "amazonia_carbono_compartimentos/amazonia_carbono_compartimentos.gpkg"

process <- function(shp){
  shp <- sf::read_sf(paste0(mdir, shp))
  shp$tot_area <- sf::st_area(shp)
  lapply(1:3001, function(i){
    print(i)
    inters <- sf::st_intersection(myCR[i,], shp)

    if(dim(inters)[1] > 0){
      result <- inters %>%
        dplyr::mutate(inter_area = sf::st_area(.)) %>%
        as.data.frame() %>%
        dplyr::select(ID, cagb, cbgb, ctotal4inv, tot_area, inter_area)

      return(result)
    }

    return(NULL)
  })
}

processCaatinga <- function(shp){
  shp <- sf::read_sf(paste0(mdir, shp))
  shp$tot_area <- sf::st_area(shp)
  lapply(1:3001, function(i){
    print(i)
    inters <- sf::st_intersection(myCR[i,], shp)

    if(dim(inters)[1] > 0){
      result <- inters %>%
        dplyr::mutate(inter_area = sf::st_area(.)) %>%
        as.data.frame() %>%
        dplyr::select(ID = id, ColRow, cagb = c_agb, cbgb = c_bgb, ctotal4inv = c_v_4i, tot_area, inter_area)

      return(result)
    }

    return(NULL)
  })
}

result_pantanal <- process(pantanal)
result_pampa <- process(pampa)
result_caatinga <- processCaatinga(caatinga)
result_cerrado <- process(cerrado)
result_mata <- process(mata)

pampa_df <- do.call(rbind, result_pampa)
write.csv(pampa_df, "pampa.csv")

pantanal_df <- do.call(rbind, result_pantanal)
write.csv(pantanal_df, "pantanal.csv")

caatinga_df <- do.call(rbind, result_caatinga)
write.csv(caatinga_df, "caatinga.csv")

cerrado_df <- do.call(rbind, result_cerrado)
write.csv(cerrado_df, "cerrado.csv")

mata_df <- do.call(rbind, result_mata)
write.csv(mata_df, "mata.csv")


