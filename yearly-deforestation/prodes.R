
devtools::install_github("pedro-andrade-inpe/colrow")
require(colrow)
dataDir <- "c:/Users/Usuario/Dropbox/colrow/"

country <- "Brazil"

myCR <- colrow::getCR(country, dataDir)

prodes <- sf::read_sf("C:/Users/Usuario/Dropbox/pesquisa/2024/aline/yearly_deforestation/yearly_deforestation.shp")

prodes <- sf::st_transform(prodes, sf::st_crs(myCR))

for(d in unique(prodes$class_name)){
  prodes_d <- prodes %>% dplyr::filter(class_name == d)
  sf::write_sf(prodes_d, paste0("prodes_", d, ".shp"))
}

read_prodes <- function(year)
  sf::read_sf(paste0("C:/Users/Usuario/Dropbox/pesquisa/2024/aline/yearly_deforestation/yearly/prodes_d", year, ".shp"))

sf::sf_use_s2(FALSE)

process <- function(shp){
  lapply(1:3001, function(i){
    print(i)
    inters <- sf::st_intersection(myCR[i, ], shp)
    
    if(dim(inters)[1] > 0){
      result <- inters %>%
        dplyr::mutate(inter_area = sf::st_area(.)) %>% 
        sf::st_drop_geometry() %>%
        dplyr::select(ID, inter_area) %>%
        as.data.frame()

      return(result)
    }
    
    return(NULL)
  })
}

writeGmsByPairs <- function(result, year){
  result <- result %>%
    dplyr::mutate(value = round(units::drop_units(mvalue), 6)) %>%
    dplyr::filter(value > 0) %>%
    dplyr::arrange(ID)
  
  res <- paste0("Brazil.", result$ID,  ".", year, "\t", result$value) %>%
    c("/", ";") %>%
    data.frame()
  
  colnames(res) <- paste0("PARAMETER ",
                          "DEFOR\n(COUNTRY,COLROW,ALLSCENYEAR) ",
                          " deforestation\n/")
  
  write.table(res, paste0("prodes-", year, ".gms"), row.names = FALSE, quote = FALSE)
}

for(year in 2021:2023){
  cat(paste0("===================================\nProcessing ", year, "\n"))
  result <- read_prodes(year) %>%
    process()

  final <- do.call(rbind, result) %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(mvalue = sum(inter_area))
  
  writeGmsByPairs(final, year)
}
