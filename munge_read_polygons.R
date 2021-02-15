
require(rgdal)

read_shapefile <- function(.url, .layer){
  
  
  arquivo_temporario <- tempfile()
  download.file(.url, destfile = arquivo_temporario)
  
  x <- suppressWarnings(
    unz(arquivo_temporario, .layer) %>% 
      rgdal::readOGR(dsn = ".", layer = .layer)
  )
  
  sp::proj4string(x) <- suppressWarnings(sp::CRS("+init=epsg:31983"))
  
  x <- suppressWarnings(sp::spTransform(x, sp::CRS("+proj=longlat +datum=WGS84")))
  
  return(x)
}

bairros_bh <- read_shapefile('https://geoservicos.pbh.gov.br/geoserver/wfs?service=WFS&version=1.0.0&request=GetFeature&typeName=ide_bhgeo:BAIRRO&srsName=EPSG:31983&outputFormat=SHAPE-ZIP', 'BAIRRO')

bairros_bh@data <- bairros_bh@data %>% 
  dplyr::mutate(NOME = stringi::stri_trans_general(NOME, "latin-ascii")) %>% 
  dplyr::mutate(NOME = stringr::str_to_upper(NOME)) %>% 
  dplyr::mutate(NOME = stringr::str_replace_all(NOME, '[:punct:]', ' '))

save(bairros_bh)

# COLOQUE AQUI O CAMINHO COM O ARQUIVO DAS REGIONAIS BAIXADO NO SITE DA PREFEITURA DE BH
.path <- ""
regionais_bh <- raster::shapefile(.path)

sp::proj4string(regionais_bh) <- sp::CRS("+init=epsg:31983")

regionais_bh <- sp::spTransform(regionais_bh, sp::CRS("+proj=longlat +datum=WGS84"))

regionais_bh@data <- regionais_bh@data %>%
  dplyr::mutate(NOME = ifelse(NOME == 'CENTRO-SUL', 'CENTRO SUL', NOME))

save(regionais_bh)
