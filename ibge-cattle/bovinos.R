
br <- geobr::read_municipality()

devtools::install_github("pedro-andrade-inpe/colrow")
require(colrow)
dataDir <- "c:/Users/Usuario/Dropbox/colrow/"

country <- "Brazil"

myCR <- colrow::getCR(country, dataDir)

csv <- read.csv("C:/Users/Usuario/Dropbox/pesquisa/2024/aline/ibge-cabecas-de-gado-bovino/ibge-gado-por-municipio.csv",
                sep = ";", skip = 4)

csv <- csv[1:5567, ]
csv <- csv %>% dplyr::select(code_muni = Cód., value = Bovino.1) %>%
  dplyr::mutate(code_muni = as.numeric(code_muni), value = as.numeric(value))

bovinos <- dplyr::left_join(br, csv)

plot(bovinos[,"value"])

sf::st_crs(bovinos)
sf::st_crs(myCR)

myCR <- sf::st_transform(myCR, sf::st_crs(bovinos))

sf::sf_use_s2(FALSE)

process <- function(shp){
  lapply(1:5565, function(i){
    print(i)
    inters <- sf::st_intersection(myCR, bovinos[i,])

    if(dim(inters)[1] > 0){
      result <- inters %>%
        dplyr::mutate(inter_area = sf::st_area(.)) %>%
        dplyr::mutate(area = sf::st_area(!!bovinos[i,])) %>%
        as.data.frame() %>%
        dplyr::select(ID, code_muni, value, inter_area, area)

      return(result)
    }

    return(NULL)
  })
}

result <- process(bovinos)
final <- do.call(rbind, result) %>% 
  dplyr::mutate(prop = inter_area / area) %>% 
  dplyr::mutate(prop = units::drop_units(prop)) %>% 
  dplyr::mutate(mvalue = value * prop) %>% 
  dplyr::filter(!is.infinite(mvalue)) %>% 
  dplyr::group_by(ID) %>%
  dplyr::summarise(mvalue = sum(mvalue, na.rm=T))

# 0.994 => error of 0.6%
sum(final$mvalue) / sum(bovinos$value, na.rm = TRUE)

final


writeGmsByPairs <- function(result, name){
  result <- result %>%
    dplyr::mutate(value = round(mvalue, 6)) %>%
    dplyr::filter(value > 0) %>%
    dplyr::arrange(ID)
  
  res <- paste0("Brazil.", result$ID,  ".2020\t", result$value) %>%
    c("/", ";") %>%
    data.frame()
  
  colnames(res) <- paste0("PARAMETER ",
                          name,
                          "_BEEF\n(COUNTRY,COLROW,ALLSCENYEAR) ",
                          name,
                          " sourcing in head of cattle\n/")
  
  write.table(res, "cattle-herd.gms", row.names = FALSE, quote = FALSE)
}

writeGmsByPairs(final, "BEEF")
