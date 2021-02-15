require(readr) 
require(tidyverse)
require(rvest)
require(sp)
require(leaflet)

load("D:/Projetos/acidentes_bh/shiny_acidentes_bh/bairros_bh.RData")
load("D:/Projetos/acidentes_bh/shiny_acidentes_bh/regionais_bh.RData")

formata_tabela <- function(x){
  
  x %>% 
    table() %>% 
    dplyr::tbl_df() %>% 
    setNames(nm = c("value", 'freq')) %>% 
    dplyr::mutate(value = stringr::str_c(value, ' (', freq, ')')) %>% 
    dplyr::pull(value) %>% 
    sort() %>% 
    stringr::str_c(., collapse = ', ')
  
}

urldata <- 'https://dados.pbh.gov.br'

tmp_lists <- stringr::str_c(urldata, 'dataset', sep = '/') %>% 
  xml2::read_html() %>% 
  rvest::html_nodes('a') %>%
  rvest::html_attr('href') %>% 
  na.omit() %>% 
  purrr::keep(.p = stringr::str_detect, pattern = 'transito\\-com\\-vitima') %>% 
  stringr::str_c(urldata, .) %>% 
  data.frame(link = ., stringsAsFactors = FALSE) %>% 
  dplyr::tbl_df() %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(refdata = stringr::str_remove(link, urldata)) %>% 
  dplyr::mutate(refdata = stringr::str_remove(refdata, 'dataset')) %>%
  dplyr::mutate(refdata = stringr::str_replace_all(refdata, '[:punct:]', ' ')) %>% 
  dplyr::mutate(refdata = stringr::str_trim(refdata)) %>% 
  dplyr::mutate(refdata = stringr::str_remove_all(refdata, 'relacao d[aeo][s|\\s+]')) %>% 
  dplyr::mutate(refdata = stringr::str_trim(refdata)) %>% 
  tidyr::separate(refdata, 'refdata', sep = '\\s+', extra = 'drop') %>% 
  plyr::adply(.margins = 1, .fun = function(item){
    
    item %>% 
      dplyr::pull(link) %>% 
      xml2::read_html() %>% 
      rvest::html_nodes('li') %>% 
      rvest::html_nodes('a') %>% 
      rvest::html_attr('href') %>% 
      purrr::keep(.p = stringr::str_detect, pattern = '\\.csv$') %>% 
      purrr::keep(.p = stringr::str_detect, pattern = '201[6-9].csv') %>% 
      purrr::keep(.p = stringr::str_detect, pattern = 'dicionario', negate = TRUE) %>% 
      data.frame(download_data = ., stringsAsFactors = FALSE) %>% 
      dplyr::tbl_df() %>% 
      dplyr::mutate(refdata = item[['refdata']]) %>% 
      dplyr::select(refdata, download_data)
    
  }, .progress = 'time') %>% 
  plyr::alply(.margins = 1, .fun = function(item){
    
    nomeia_tabelas <- function(ref, df){
      
      switch(ref,
             
             'ocorrencias' = df %>% 
               setNames(nm = c('num_boletim', 'data_hora', 'data_hora_inclusao', 
                               'cod_acidente', 'desc_acidente', 'cod_tempo', 'desc_tempo', 
                               'cod_pavimento', 'desc_pavimento', 'cod_regional', 'desc_regional', 
                               'origem_boletim', 'local_sinalizado', 'velocidade_permitida', 
                               'coordenadax', 'coordenaday', 'ind_hora_informada', 'ind_fatalidade',
                               'cod_ups', 'desc_ups', 'data_alteracao_smsa', 'cod_ups_antiga', 
                               'desc_ups_antiga')),
             
             'pessoas' = df %>% 
               setNames(nm = c('num_boletim', 'data_hora', 'num_envolvidos', 
                               'ind_condutor_envolvido', 'cod_severidade_acidente', 
                               'desc_severidade_acidente', 'sexo', 'ind_cinto_seguranca', 
                               'ind_embriaguez', 'idade', 'nascimento', 'categoria_habilitacao',
                               'desc_habilitacao', 'ind_declaracao_obito', 'cod_severidade_antiga',
                               'especie_veiculo', 'ind_pedestre', 'ind_passageiro')),
             
             'veiculos' = df %>% 
               setNames(nm = c('num_boletim', 'data_hora', 'seq_veiculos', 
                               'cod_categoria_veiculo', 'desc_categoria_veiculo', 
                               'cod_tipo_veiculo', 'desc_tipo_veiculo', 'cod_situacao',
                               'desc_situacao', 'cod_tipo_socorro', 'desc_tipo_socorro')),
             
             'logradouros' =  df %>% 
               setNames(nm = c('num_boletim', 'data_hora', 'cod_municipio', 
                               'municipio', 'sequencia_logradouros', 
                               'cod_logradouro', 'tipo_logradouro', 'nome_logradouro',
                               'tipo_logradouro_anterior', 'nome_logradouro_anterior',
                               'cod_bairro', 'nome_bairro', 'tipo_bairro', 'desc_tipo_bairro',
                               'num_imovel', 'num_imovel_proximo')) 
      )
      
    }
    
    tmp_downloaded <- item[['download_data']] %>% 
      data.table::fread(sep = ';') %>%  
      dplyr::tbl_df()
    
    nomeia_tabelas(ref = item[['refdata']], df = tmp_downloaded) %>% 
      dplyr::mutate(refdata = item[['refdata']])
    
  }, .progress = 'time')

tmp_ocorrencias <- tmp_lists %>% 
  purrr::keep(.p = function(i){
    
    i[['refdata']][1] == 'ocorrencias'
    
  }) %>% 
  dplyr::bind_rows() %>%
  dplyr::mutate_all(.funs = stringr::str_to_lower) %>% 
  dplyr::mutate_all(.funs = stringi::stri_trans_general, id = 'Latin-ASCII') %>% 
  dplyr::mutate_all(.funs = function(x){ifelse(x %in% c('nao informado', 'ni'), NA_character_, x)}) %>% 
  dplyr::mutate_at(.vars = c('data_hora'), .funs = lubridate::dmy_hm) %>% 
  dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 'sim', 1, 0)) %>% 
  dplyr::mutate(origem_boletim = ifelse(origem_boletim == 'pmmg', 'policia militar', origem_boletim)) %>% 
  dplyr::mutate(origem_boletim = ifelse(origem_boletim == 'corpo de bombei', 'corpo de bombeiros', origem_boletim)) %>% 
  dplyr::mutate(desc_acidente = ifelse(desc_acidente %in% c('abalroamento com vitima', 'colisao de veiculos com vitima'), "colisao de veiculos", desc_acidente)) %>% 
  dplyr::mutate(desc_acidente = ifelse(desc_acidente == 'atropelamento de animal com vitima', 'atropelamento de animal', desc_acidente)) %>% 
  dplyr::mutate(desc_acidente = ifelse(desc_acidente %in% c('atropelamento de pessoa com vitima fatal', 'atropelamento de pessoa sem vitima fatal'), 'atropelamento de pedestre', desc_acidente)) %>% 
  dplyr::mutate(desc_acidente = ifelse(desc_acidente %in% c('capotamento/tombamento com vitima', 'capotamento/tombamento sem vitima'), 'capotamento de veiculo', desc_acidente)) %>% 
  dplyr::mutate(desc_acidente = ifelse(desc_acidente == c('choque mecanico com vitima'), 'colisao de veiculo com objeto parado', desc_acidente)) %>% 
  dplyr::mutate(desc_acidente = ifelse(desc_acidente == c('outros com vitima'), 'outros tipos de acidentes', desc_acidente)) %>% 
  dplyr::mutate(desc_acidente = ifelse(desc_acidente %in% c('queda de pessoa de veiculo', 'queda de veiculo com vitima'), 'queda de pessoa de veiculo', desc_acidente)) %>% 
  dplyr::mutate(desc_acidente = ifelse(desc_acidente == c('queda e/ou vazamento de carga de veiculo c/ vitima'), 'vazamento de carga do veiculo', desc_acidente)) %>% 
  dplyr::select(num_boletim, data_hora, desc_acidente, desc_tempo, origem_boletim, 
                coordenadax, coordenaday)

tmp_pessoas <- tmp_lists %>% 
  purrr::keep(.p = function(i){
    
    i[['refdata']][1] == 'pessoas'
    
  }) %>% 
  dplyr::bind_rows() %>% 
  dplyr::mutate_all(.funs = stringr::str_to_lower) %>% 
  dplyr::mutate_all(.funs = stringi::stri_trans_general, id = 'Latin-ASCII') %>% 
  dplyr::mutate_all(.funs = function(x){ifelse(x  == 'nao informado', NA_character_, x)}) %>% 
  dplyr::mutate(ind_condutor = ifelse(ind_condutor_envolvido == 's', 1, NA_integer_)) %>% 
  dplyr::mutate(ind_condutor = ifelse(ind_condutor_envolvido == 'n', 0, ind_condutor)) %>%
  dplyr::mutate(ind_pedestre = ifelse(ind_pedestre == "", NA_integer_, ind_pedestre)) %>% 
  dplyr::mutate(ind_pedestre = ifelse(ind_pedestre == 's', 1, ind_pedestre)) %>% 
  dplyr::mutate(ind_pedestre = ifelse(ind_pedestre %in% c('n', '0'), 0, ind_pedestre)) %>%
  dplyr::mutate(ind_passageiro = ifelse(ind_passageiro == "", NA_integer_, ind_passageiro)) %>% 
  dplyr::mutate(ind_passageiro = ifelse(ind_passageiro == 's', 1, ind_passageiro)) %>% 
  dplyr::mutate(ind_passageiro = ifelse(ind_passageiro %in% c('n', '0'), 0, ind_passageiro)) %>%
  dplyr::mutate_at(.vars = c('ind_pedestre', 'ind_passageiro'), .funs = as.integer) %>% 
  dplyr::mutate(sexo = ifelse(sexo == '0', NA_character_, sexo)) %>%
  dplyr::mutate(idade = ifelse(idade == '0' & nascimento == '00/00/0000', NA_character_, idade)) %>% 
  dplyr::mutate(idade = as.integer(idade)) %>% 
  dplyr::mutate(nascimento = ifelse(nascimento == '00/00/0000', NA_character_, nascimento)) %>% 
  dplyr::mutate(categoria_habilitacao = ifelse(categoria_habilitacao %in% c('', 'n'), NA_character_, categoria_habilitacao)) %>% 
  dplyr::mutate(especie_veiculo = ifelse(especie_veiculo == '', NA_character_, especie_veiculo)) %>% 
  dplyr::mutate_at(.vars = c('nascimento'), .funs = lubridate::dmy) %>%
  dplyr::mutate_at(.vars = c('data_hora'), .funs = lubridate::dmy_hm) %>% 
  dplyr::rename(ind_embriagado = ind_embriaguez) %>% 
  dplyr::mutate(ind_embriagado = ifelse(ind_embriagado == 'sim', 1, ind_embriagado)) %>% 
  dplyr::mutate(ind_embriagado = ifelse(ind_embriagado == 'nao', 0, ind_embriagado)) %>% 
  dplyr::mutate(ind_cinto_seguranca = ifelse(ind_cinto_seguranca == 'sim', 1, ind_cinto_seguranca)) %>% 
  dplyr::mutate(ind_cinto_seguranca = ifelse(ind_cinto_seguranca == 'nao', 0, ind_cinto_seguranca)) %>% 
  dplyr::mutate(ind_envolvido_evasao = ifelse(is.na(idade) & is.na(sexo) & is.na(nascimento) & is.na(desc_severidade_acidente), 1, 0)) %>% 
  dplyr::group_by(num_boletim) %>% 
  dplyr::mutate(num_pessoas_envolvidas = dplyr::n()) %>% 
  dplyr::mutate(ind_fatalidade = sum(desc_severidade_acidente == 'fatal', na.rm = TRUE) > 0) %>% 
  dplyr::mutate(ind_fatalidade = as.integer(ind_fatalidade)) %>% 
  dplyr::mutate(ind_embriaguez = sum(ind_embriagado == 1, na.rm = TRUE) > 0) %>% 
  dplyr::mutate(ind_embriaguez = as.integer(ind_embriaguez)) %>% 
  dplyr::mutate(ind_pedestre_envolvido = sum(ind_pedestre == 1, na.rm = TRUE) > 0) %>% 
  dplyr::mutate(ind_pedestre_envolvido = as.integer(ind_pedestre_envolvido)) %>% 
  dplyr::mutate(ind_evasao = sum(ind_envolvido_evasao, na.rm = TRUE) > 0) %>% 
  dplyr::mutate(ind_evasao = as.integer(ind_evasao)) %>% 
  dplyr::mutate(ind_falta_cinto_seguranca_condutor = sum(ind_cinto_seguranca == 0 & ind_condutor == 1, na.rm = TRUE) > 0) %>% 
  dplyr::mutate(ind_falta_cinto_seguranca_condutor = as.integer(ind_falta_cinto_seguranca_condutor)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(num_boletim, data_hora, sexo, idade, nascimento, ind_condutor, 
                ind_pedestre, ind_passageiro, desc_severidade_acidente, 
                ind_cinto_seguranca, ind_embriagado, categoria_habilitacao, 
                num_pessoas_envolvidas, ind_fatalidade, ind_embriaguez, 
                ind_pedestre_envolvido, ind_falta_cinto_seguranca_condutor, ind_evasao)
  

tmp_veiculos <- tmp_lists %>% 
  purrr::keep(.p = function(i){
    
    i[['refdata']][1] == 'veiculos'
    
  }) %>% 
  dplyr::bind_rows() %>% 
  dplyr::mutate_all(.funs = stringr::str_to_lower) %>% 
  dplyr::mutate_all(.funs = stringi::stri_trans_general, id = 'Latin-ASCII') %>% 
  dplyr::mutate_all(.funs = function(x){ifelse(x  == 'nao informado', NA_character_, x)}) %>% 
  dplyr::mutate_all(.funs = function(x){ifelse(x  == '', NA_character_, x)}) %>% 
  dplyr::mutate(desc_tipo_veiculo = ifelse(desc_tipo_veiculo == 'taxi', 'automovel', desc_tipo_veiculo)) %>%
  dplyr::mutate(desc_tipo_veiculo = ifelse(is.na(desc_tipo_veiculo), 'nao informado', desc_tipo_veiculo)) %>% 
  dplyr::mutate_at(.vars = c('data_hora'), .funs = lubridate::dmy_hm) %>% 
  dplyr::group_by(num_boletim) %>% 
  dplyr::mutate(desc_veiculos = formata_tabela(desc_tipo_veiculo)) %>% 
  dplyr::mutate(num_veiculos_envolvidos = dplyr::n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(num_boletim, data_hora, seq_veiculos, desc_tipo_veiculo, 
                desc_categoria_veiculo, desc_situacao, num_veiculos_envolvidos, 
                desc_veiculos)

tmp_logradouros <- tmp_lists %>% 
  purrr::keep(.p = function(i){
    
    i[['refdata']][1] == 'logradouros'
    
  }) %>% 
  dplyr::bind_rows() %>% 
  dplyr::mutate_all(.funs = stringr::str_to_lower) %>% 
  dplyr::mutate_all(.funs = stringi::stri_trans_general, id = 'Latin-ASCII') %>% 
  dplyr::mutate_all(.funs = function(x){ifelse(x  %in% c('nao informado', 'ni'), NA_character_, x)}) %>% 
  dplyr::mutate_all(.funs = function(x){ifelse(x  == '', NA_character_, x)}) %>% 
  dplyr::mutate_at(.vars = c('data_hora'), .funs = lubridate::dmy_hm) %>%
  dplyr::mutate_at(.vars = c('tipo_logradouro', 'tipo_logradouro_anterior'),
                   .funs = function(x){
                     
                     ifelse(x == 'ni', NA_character_, x) %>% 
                       stringr::str_replace(pattern = 'acs', replacement = 'acesso') %>% 
                       stringr::str_replace(pattern = 'ala', replacement = 'alameda') %>% 
                       stringr::str_replace(pattern = 'ave', replacement = 'avenida') %>% 
                       stringr::str_replace(pattern = 'bec', replacement = 'beco') %>% 
                       stringr::str_replace(pattern = 'elp', replacement = 'rua') %>% 
                       stringr::str_replace(pattern = 'est', replacement = 'estrada') %>% 
                       stringr::str_replace(pattern = 'pca', replacement = 'praca') %>% 
                       stringr::str_replace(pattern = 'rdp', replacement = 'rua') %>% 
                       stringr::str_replace(pattern = 'rod', replacement = 'rodovia') %>% 
                       stringr::str_replace(pattern = 'tre', replacement = 'trevo') %>% 
                       stringr::str_replace(pattern = 'tri', replacement = 'trilha') %>% 
                       stringr::str_replace(pattern = 'trv', replacement = 'travessa') %>% 
                       stringr::str_replace(pattern = 'tun', replacement = 'tunel') %>% 
                       stringr::str_replace(pattern = 'vdp', replacement = 'beco') %>% 
                       stringr::str_replace(pattern = 'vdt', replacement = 'viaduto') 
                     
                   }) %>% 
  dplyr::mutate(logradouro = stringr::str_c(tipo_logradouro, ' ', nome_logradouro)) %>% 
  dplyr::mutate(nome_logradouro_anterior = ifelse(is.na(tipo_logradouro_anterior), 
                NA_character_, nome_logradouro_anterior)) %>% 
  dplyr::mutate(logradouro_anterior = stringr::str_c(tipo_logradouro_anterior, ' ', 
                                                     nome_logradouro_anterior)) %>% 
  dplyr::mutate_at(.vars = c('logradouro', 'logradouro_anterior'), .funs = function(x){
    
    stringr::str_remove_all(x, ' bh-rio') %>% 
    stringr::str_remove_all(' bh-brasilia') %>% 
      stringr::str_replace_all('trezentos e cinquenta e seis', '356') %>% 
      stringr::str_replace_all('zero quarenta', '040')
    
  }) %>% 
  dplyr::mutate(aux = stringr::str_detect(logradouro, ' x ')) %>% 
  group_split(aux) %>% 
  plyr::llply(.fun = function(item){
    
    if(unique(item[['aux']]) == 1){
      
      item %>% 
        dplyr::mutate(logradouro_anterior = NA_character_) %>% 
        tidyr::separate(logradouro, c('logradouro', 'logradouro_anterior'), sep = ' x ')
      
    } else{
      
      item
      
    }
    
  }) %>% 
  dplyr::bind_rows() %>% 
  dplyr::select(num_boletim, data_hora, logradouro, logradouro_anterior)

tmp_coordenadas <- tmp_ocorrencias %>% 
  dplyr::mutate(lat = coordenaday, 
                lng = coordenadax) %>% 
  dplyr::mutate_at(.vars = c('lat', 'lng'), 
                   .funs = function(x){ifelse(is.na(x), 0, as.numeric(x))})

sp::coordinates(tmp_coordenadas) <- ~lng + lat

sp::proj4string(tmp_coordenadas) <- sp::CRS("+init=epsg:31983")

tmp_coordenadas <- sp::spTransform(tmp_coordenadas, sp::CRS("+proj=longlat +datum=WGS84"))

tmp_bairros <- plyr::llply(seq_len(length(bairros_bh@data$NOME)), function(item){
  
  tmp_coordenadas[bairros_bh[item,],]@data$num_boletim %>% 
    data.frame(num_boletim = ., stringsAsFactors = FALSE) %>% 
    dplyr::mutate(bairro = bairros_bh[item,]@data$NOME)
  
}, .progress = 'time') %>% 
  dplyr::bind_rows() %>% 
  dplyr::tbl_df()

tmp_regionais <- plyr::llply(seq_len(length(regionais_bh@data$NOME)), function(item){
  
  tmp_coordenadas[regionais_bh[item,],]@data$num_boletim %>% 
    data.frame(num_boletim = ., stringsAsFactors = FALSE) %>% 
    dplyr::mutate(regional = regionais_bh[item,]@data$NOME)
  
}, .progress = 'time') %>% 
  dplyr::bind_rows() %>% 
  dplyr::tbl_df()

tmp_local <- tmp_logradouros %>% 
  dplyr::distinct(num_boletim, .keep_all = TRUE) %>% 
  dplyr::select(num_boletim, logradouro) %>% 
  dplyr::rename(logradouros = logradouro)

db_ocorrencias_transito_bh <- list()

db_ocorrencias_transito_bh[['ocorrencias']] <- tmp_ocorrencias %>% 
  dplyr::mutate(hora_acidente = lubridate::hour(data_hora)) %>% 
  dplyr::mutate(periodo = ifelse(hora_acidente %in% 6:11, 'manha', NA_character_)) %>% 
  dplyr::mutate(periodo = ifelse(hora_acidente %in% 12:17, 'tarde', periodo)) %>% 
  dplyr::mutate(periodo = ifelse(hora_acidente %in% 18:23, 'noite', periodo)) %>% 
  dplyr::mutate(periodo = ifelse(hora_acidente %in% 0:5, 'madrugada', periodo)) %>% 
  dplyr::bind_cols(data.frame(tmp_coordenadas@coords)) %>% 
  dplyr::mutate(lng = ifelse(coordenadax == 0, NA_real_, lng)) %>% 
  dplyr::mutate(lat = ifelse(coordenaday == 0, NA_real_, lat)) %>% 
  dplyr::select(-c(coordenadax, coordenaday)) %>% 
  dplyr::left_join(tmp_bairros, by = 'num_boletim') %>% 
  dplyr::left_join(tmp_regionais, by = 'num_boletim') %>% 
  dplyr::mutate(aux = ifelse(!is.na(lat) & !is.na(lng) & is.na(bairro), 1, 0)) %>% 
  (function(df){
    
    plyr::alply(df, .margins = 1, .fun = function(item, dfaux){
      
      if(item$aux == 1){
        
        tmp_poligonos <- dfaux %>% 
          dplyr::mutate(dist = ((lng - item$lng)^2) + ((lat - item$lat)^2)) %>% 
          dplyr::arrange(dist) %>% 
          dplyr::slice(1:100) 
        
        aux_bairro <- tmp_poligonos %>% 
          dplyr::pull(bairro) %>% 
          table() %>% 
          dplyr::tbl_df() %>% 
          setNames(nm = c('bairro', 'freq')) %>% 
          dplyr::arrange(desc(freq)) %>% 
          dplyr::slice(1) %>% 
          dplyr::pull(bairro)
        
        
        item %>% 
          dplyr::mutate(bairro = aux_bairro)
        
      } else{
        
        item
        
      }
      
    }, dfaux = df, .progress = 'time') %>% 
      dplyr::bind_rows()
    
  }) %>% 
  dplyr::mutate(aux = ifelse(!is.na(lat) & !is.na(lng) & is.na(regional), 1, 0)) %>% 
  (function(df){
    
    plyr::alply(df, .margins = 1, .fun = function(item, dfaux){
      
      if(item$aux == 1){
        
        tmp_poligonos <- dfaux %>% 
          dplyr::mutate(dist = ((lng - item$lng)^2) + ((lat - item$lat)^2)) %>% 
          dplyr::arrange(dist) %>% 
          dplyr::slice(1:100) 
        
        aux_regional <- tmp_poligonos %>% 
          dplyr::pull(regional) %>% 
          table() %>% 
          dplyr::tbl_df() %>% 
          setNames(nm = c('regional', 'freq')) %>% 
          dplyr::arrange(desc(freq)) %>% 
          dplyr::slice(1) %>% 
          dplyr::pull(regional)
        
        
        item %>% 
          dplyr::mutate(regional = aux_regional)
        
      } else{
        
        item
        
      }
      
    }, dfaux = df, .progress = 'time') %>% 
      dplyr::bind_rows()
    
  }) %>% 
  dplyr::select(-aux) %>% 
  dplyr::arrange(data_hora, num_boletim) %>% 
  dplyr::left_join(tmp_pessoas %>% dplyr::distinct(num_boletim, num_pessoas_envolvidas, ind_fatalidade, ind_embriaguez, ind_pedestre_envolvido, ind_falta_cinto_seguranca_condutor, ind_evasao), 
                   by = c('num_boletim')) %>% 
  dplyr::left_join(tmp_veiculos %>% dplyr::distinct(num_boletim, num_veiculos_envolvidos, desc_veiculos), 
                   by = c('num_boletim')) %>% 
  dplyr::left_join(tmp_local, by = c('num_boletim')) %>% 
  dplyr::arrange(data_hora, num_boletim)
  
db_ocorrencias_transito_bh[['pessoas']] <- tmp_pessoas %>% 
  dplyr::mutate_at(.vars = c('ind_cinto_seguranca', 'ind_embriagado'), .funs = as.integer) %>% 
  dplyr::select(num_boletim, data_hora, sexo, idade, nascimento, ind_condutor, 
                ind_pedestre, ind_passageiro, desc_severidade_acidente, 
                ind_cinto_seguranca, ind_embriagado, categoria_habilitacao)

db_ocorrencias_transito_bh[['veiculos']] <- tmp_veiculos %>% 
  dplyr::select(num_boletim, data_hora, seq_veiculos, desc_tipo_veiculo, 
                desc_categoria_veiculo, desc_situacao)

save(db_ocorrencias_transito_bh, file = "D:/Projetos/acidentes_bh/shiny_acidentes_bh/db_ocorrencias_transito_bh.RData")
