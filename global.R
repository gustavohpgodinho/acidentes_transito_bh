
require(shiny)
require(shinyWidgets)
require(leaflet)
require(ggplot2)
require(tidyverse)

load("D:/Projetos/acidentes_bh/shiny_acidentes_bh/lst_tables.RData")
load("D:/Projetos/acidentes_bh/shiny_acidentes_bh/db_ocorrencias_transito_bh.RData")
load("D:/Projetos/acidentes_bh/shiny_acidentes_bh/bairros_bh.RData")
load("D:/Projetos/acidentes_bh/shiny_acidentes_bh/regionais_bh.RData")

calcula_moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

seleciona_veiculos <- function(x){
  
  switch(x, 
         "Automoveis" = c("automovel", "kombi"), 
         "Bicicletas e Patinetes" = c("bicicleta", "patinete"), 
         "Caminhoes" = c("caminhao", "caminhao-trator"), 
         "Caminhonetes" = c("caminhonete", "camioneta"), 
         "Veiculos de tracao animal" = c("carroca", "charrete", "tracao"), 
         "Motocicletas" = c("ciclomotor", "motocicleta", "motoneta", "triciclo"), 
         "Onibus" = c("onibus", "microonibus"), 
         "Outros" = c("trator de rodas", "trator misto", "bonde", "carro de mao", 
                      "especial", "misto", "reboque e semi-reboque"))
  
}

muda_valor_sexo <- function(x){
  
  switch(x, 
         "Feminino" = "f", 
         "Masculino" = "m",
         "Nao informado" = 'NA'
         )
  
}

seleciona_periodo <- function(x){
  
  switch(x, 
         "Manha" = 6:11, 
         "Tarde" = 12:17, 
         "Noite" = 18:23, 
         "Madrugada" = 0:5)
  
}

muda_objsp <- function(.variavel, .valor, .divisao = 'bairro'){
  
  if(.divisao == 'bairro'){
    
    switch(.variavel,
           'Indicativo de pedestre envolvido' = db_ocorrencias_transito_bh$ocorrencias %>% 
             dplyr::group_by(bairro) %>% 
             dplyr::summarise(n = dplyr::n(), 
                              soma = sum(ind_pedestre_envolvido)) %>% 
             dplyr::mutate(perc = 100*soma/n) %>% 
             dplyr::right_join(bairros_bh@data, by = c('bairro' = 'NOME')) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(perc), '0%', paste0(round(perc, 2), '%'))) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(percnorm), '0%', percnorm)) %>% 
             dplyr::mutate_at(.vars = c('n', 'soma', 'perc'), .funs = function(x) ifelse(is.na(x), 0, x)) %>% 
             dplyr::mutate(legenda = stringr::str_c('<strong>Bairro: </strong>', bairro,
                                                    '<br><strong>Número total de acidentes: </strong>', n,
                                                    '<br><strong>Número de acidentes envolvendo pedestres: </strong>', soma,
                                                    '<br><strong>Percentual de acidentes envolvendo pedestres: </strong>', percnorm)),
           
           'Indicativo de embriaguez' = db_ocorrencias_transito_bh$ocorrencias %>% 
             dplyr::group_by(bairro) %>% 
             dplyr::summarise(n = dplyr::n(), 
                              soma = sum(ind_embriaguez)) %>% 
             dplyr::mutate(perc = 100*soma/n) %>% 
             dplyr::right_join(bairros_bh@data, by = c('bairro' = 'NOME')) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(perc), '0%', paste0(round(perc, 2), '%'))) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(percnorm), '0%', percnorm)) %>% 
             dplyr::mutate_at(.vars = c('n', 'soma', 'perc'), .funs = function(x) ifelse(is.na(x), 0, x)) %>%  
             dplyr::mutate_at(.vars = c('percnorm'), .funs = function(x) ifelse(is.na(x), '0%', x)) %>% 
             dplyr::mutate(legenda = stringr::str_c('<strong>Bairro: </strong>', bairro,
                                                    '<br><strong>Número total de acidentes: </strong>', n,
                                                    '<br><strong>Número de acidentes contendo sinais de embriaguez: </strong>', soma,
                                                    '<br><strong>Percentual de acidentes contendo sinais de embriaguez: </strong>', percnorm)),
           
           'Indicativo de acidentes com vitima fatal' = db_ocorrencias_transito_bh$ocorrencias %>% 
             dplyr::group_by(bairro) %>% 
             dplyr::summarise(n = dplyr::n(), 
                              soma = sum(ind_fatalidade)) %>% 
             dplyr::mutate(perc = 100*soma/n) %>% 
             dplyr::right_join(bairros_bh@data, by = c('bairro' = 'NOME')) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(perc), '0%', paste0(round(perc, 2), '%'))) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(percnorm), '0%', percnorm)) %>% 
             dplyr::mutate_at(.vars = c('n', 'soma', 'perc'), .funs = function(x) ifelse(is.na(x), 0, x)) %>% 
             dplyr::mutate(legenda = stringr::str_c('<strong>Bairro: </strong>', bairro,
                                                    '<br><strong>Número total de acidentes: </strong>', n,
                                                    '<br><strong>Número de acidentes com vítimas fatais: </strong>', soma,
                                                    '<br><strong>Percentual de acidentes com vítimas fatais: </strong>', percnorm)),
           
           'Tipo de acidente' = db_ocorrencias_transito_bh$ocorrencias %>% 
             dplyr::group_by(bairro) %>% 
             dplyr::summarise(n = dplyr::n(),
                              soma = sum(desc_acidente == stringr::str_to_lower(.valor))) %>% 
             dplyr::mutate(perc = 100*soma/n) %>% 
             dplyr::right_join(bairros_bh@data, by = c('bairro' = 'NOME')) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(perc), '0%', paste0(round(perc, 2), '%'))) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(percnorm), '0%', percnorm)) %>% 
             dplyr::mutate_at(.vars = c('n', 'soma', 'perc'), .funs = function(x) ifelse(is.na(x), 0, x)) %>% 
             dplyr::mutate(legenda = stringr::str_c('<strong>Bairro: </strong>', bairro,
                                                    '<br><strong>Número total de acidentes: </strong>', n,
                                                    '<br><strong>Número de ', .valor,': </strong>', soma,
                                                    '<br><strong>Percentual de ', .valor ,': </strong>', percnorm)),
           
           'Tipo de veiculo' = db_ocorrencias_transito_bh$veiculos %>% 
             dplyr::group_by(num_boletim) %>% 
             dplyr::summarise(ind = sum(desc_tipo_veiculo %in% seleciona_veiculos(.valor), na.rm = TRUE)) %>%
             dplyr::mutate(ind = (ind > 0)) %>% 
             dplyr::mutate(ind = as.integer(ind)) %>%
             dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
             dplyr::group_by(bairro) %>% 
             dplyr::summarise(n = dplyr::n(),
                              soma = sum(ind)) %>% 
             dplyr::mutate(perc = 100*soma/n) %>% 
             dplyr::right_join(bairros_bh@data, by = c('bairro' = 'NOME')) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(perc), '0%', paste0(round(perc, 2), '%'))) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(percnorm), '0%', percnorm)) %>% 
             dplyr::mutate_at(.vars = c('n', 'soma', 'perc'), .funs = function(x) ifelse(is.na(x), 0, x)) %>% 
             dplyr::mutate(legenda = stringr::str_c('<strong>Bairro: </strong>', bairro,
                                                    '<br><strong>Número total de acidentes: </strong>', n,
                                                    '<br><strong>Número de acidentes envolvendo ', .valor,': </strong>', soma,
                                                    '<br><strong>Percentual de acidentes envolvendo ', .valor ,': </strong>', percnorm)),
           
           'Periodo do dia' = db_ocorrencias_transito_bh$ocorrencias %>% 
             dplyr::group_by(bairro) %>% 
             dplyr::summarise(n = dplyr::n(),
                              soma = sum(periodo == stringr::str_to_lower(.valor))) %>% 
             dplyr::mutate(perc = 100*soma/n) %>% 
             dplyr::right_join(bairros_bh@data, by = c('bairro' = 'NOME')) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(perc), '0%', paste0(round(perc, 2), '%'))) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(percnorm), '0%', percnorm)) %>% 
             dplyr::mutate_at(.vars = c('n', 'soma', 'perc'), .funs = function(x) ifelse(is.na(x), 0, x)) %>% 
             dplyr::mutate(legenda = stringr::str_c('<strong>Bairro: </strong>', bairro,
                                                    '<br><strong>Número total de acidentes: </strong>', n,
                                                    '<br><strong>Número de acidentes de ', stringr::str_to_lower(.valor),': </strong>', soma,
                                                    '<br><strong>Percentual de acidente de ', stringr::str_to_lower(.valor) ,': </strong>', percnorm)),
           
           'Sexo do condutor' = db_ocorrencias_transito_bh$pessoas %>% 
             dplyr::mutate(sexo = ifelse(is.na(sexo), 'NA', sexo)) %>% 
             dplyr::group_by(num_boletim) %>% 
             dplyr::summarise(ind = sum(sexo == muda_valor_sexo(.valor) & ind_condutor == 1, na.rm = TRUE)) %>%
             dplyr::mutate(ind = (ind > 0)) %>% 
             dplyr::mutate(ind = as.integer(ind)) %>%
             dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
             dplyr::group_by(bairro) %>% 
             dplyr::summarise(n = dplyr::n(),
                              soma = sum(ind)) %>% 
             dplyr::mutate(perc = 100*soma/n) %>% 
             dplyr::right_join(bairros_bh@data, by = c('bairro' = 'NOME')) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(perc), '0%', paste0(round(perc, 2), '%'))) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(percnorm), '0%', percnorm)) %>% 
             dplyr::mutate_at(.vars = c('n', 'soma', 'perc'), .funs = function(x) ifelse(is.na(x), 0, x)) %>% 
             dplyr::mutate(legenda = stringr::str_c('<strong>Bairro: </strong>', bairro,
                                                    '<br><strong>Número total de acidentes: </strong>', n,
                                                    '<br><strong>Número de acidentes envolvendo condutores de sexo ', stringr::str_to_lower(.valor),': </strong>', soma,
                                                    '<br><strong>Percentual de acidentes envolvendo condutores de sexo ', stringr::str_to_lower(.valor) ,': </strong>', percnorm)),
           
           'Numero de veiculos envolvidos' = db_ocorrencias_transito_bh$ocorrencias %>% 
             dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'Mais que 4', num_veiculos_envolvidos)) %>% 
             dplyr::group_by(bairro) %>% 
             dplyr::summarise(n = dplyr::n(),
                              soma = sum(num_veiculos_envolvidos == .valor, na.rm = TRUE)) %>% 
             dplyr::mutate(perc = 100*soma/n) %>% 
             dplyr::right_join(bairros_bh@data, by = c('bairro' = 'NOME')) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(perc), '0%', paste0(round(perc, 2), '%'))) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(percnorm), '0%', percnorm)) %>% 
             dplyr::mutate_at(.vars = c('n', 'soma', 'perc'), .funs = function(x) ifelse(is.na(x), 0, x)) %>% 
             dplyr::mutate(legenda = stringr::str_c('<strong>Bairro: </strong>', bairro,
                                                    '<br><strong>Número total de acidentes: </strong>', n,
                                                    '<br><strong>Número de acidentes envolvendo ', stringr::str_to_lower(.valor), ' veículo(s): </strong>', soma,
                                                    '<br><strong>Percentual de acidentes envolvendo ', stringr::str_to_lower(.valor), ' veículo(s): </strong>', percnorm)),
           
           'Numero de pessoas envolvidas' = db_ocorrencias_transito_bh$ocorrencias %>% 
             dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'Mais que 4', num_veiculos_envolvidos)) %>% 
             dplyr::group_by(bairro) %>% 
             dplyr::summarise(n = dplyr::n(),
                              soma = sum(num_veiculos_envolvidos == .valor, na.rm = TRUE)) %>% 
             dplyr::mutate(perc = 100*soma/n) %>% 
             dplyr::right_join(bairros_bh@data, by = c('bairro' = 'NOME')) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(perc), '0%', paste0(round(perc, 2), '%'))) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(percnorm), '0%', percnorm)) %>% 
             dplyr::mutate_at(.vars = c('n', 'soma', 'perc'), .funs = function(x) ifelse(is.na(x), 0, x)) %>% 
             dplyr::mutate(legenda = stringr::str_c('<strong>Bairro: </strong>', bairro,
                                                    '<br><strong>Número total de acidentes: </strong>', n,
                                                    '<br><strong>Número de acidentes envolvendo ', stringr::str_to_lower(.valor),' pessoa(s): </strong>', soma,
                                                    '<br><strong>Percentual de acidentes envolvendo ', stringr::str_to_lower(.valor) ,' pessoa(s): </strong>', percnorm))
           
    )
    
    
  } else {
    
    switch(.variavel,
           'Indicativo de pedestre envolvido' = db_ocorrencias_transito_bh$ocorrencias %>% 
             dplyr::group_by(regional) %>% 
             dplyr::summarise(n = dplyr::n(), 
                              soma = sum(ind_pedestre_envolvido)) %>% 
             dplyr::mutate(perc = 100*soma/n) %>% 
             dplyr::right_join(regionais_bh@data, by = c('regional' = 'NOME')) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(perc), '0%', paste0(round(perc, 2), '%'))) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(percnorm), '0%', percnorm)) %>% 
             dplyr::mutate_at(.vars = c('n', 'soma', 'perc'), .funs = function(x) ifelse(is.na(x), 0, x)) %>% 
             dplyr::mutate(legenda = stringr::str_c('<strong>Regional: </strong>', regional,
                                                    '<br><strong>Número total de acidentes: </strong>', n,
                                                    '<br><strong>Número de acidentes envolvendo pedestres: </strong>', soma,
                                                    '<br><strong>Percentual de acidentes envolvendo pedestres: </strong>', percnorm)),
           
           'Indicativo de embriaguez' = db_ocorrencias_transito_bh$ocorrencias %>% 
             dplyr::group_by(regional) %>% 
             dplyr::summarise(n = dplyr::n(), 
                              soma = sum(ind_embriaguez)) %>% 
             dplyr::mutate(perc = 100*soma/n) %>% 
             dplyr::right_join(regionais_bh@data, by = c('regional' = 'NOME')) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(perc), '0%', paste0(round(perc, 2), '%'))) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(percnorm), '0%', percnorm)) %>% 
             dplyr::mutate_at(.vars = c('n', 'soma', 'perc'), .funs = function(x) ifelse(is.na(x), 0, x)) %>%  
             dplyr::mutate_at(.vars = c('percnorm'), .funs = function(x) ifelse(is.na(x), '0%', x)) %>% 
             dplyr::mutate(legenda = stringr::str_c('<strong>Regional: </strong>', regional,
                                                    '<br><strong>Número total de acidentes: </strong>', n,
                                                    '<br><strong>Número de acidentes contendo sinais de embriaguez: </strong>', soma,
                                                    '<br><strong>Percentual de acidentes contendo sinais de embriaguez: </strong>', percnorm)),
           
           'Indicativo de acidentes com vitima fatal' = db_ocorrencias_transito_bh$ocorrencias %>% 
             dplyr::group_by(regional) %>% 
             dplyr::summarise(n = dplyr::n(), 
                              soma = sum(ind_fatalidade)) %>% 
             dplyr::mutate(perc = 100*soma/n) %>% 
             dplyr::right_join(regionais_bh@data, by = c('regional' = 'NOME')) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(perc), '0%', paste0(round(perc, 2), '%'))) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(percnorm), '0%', percnorm)) %>% 
             dplyr::mutate_at(.vars = c('n', 'soma', 'perc'), .funs = function(x) ifelse(is.na(x), 0, x)) %>% 
             dplyr::mutate(legenda = stringr::str_c('<strong>Regional: </strong>', regional,
                                                    '<br><strong>Número total de acidentes: </strong>', n,
                                                    '<br><strong>Número de acidentes com vítimas fatais: </strong>', soma,
                                                    '<br><strong>Percentual de acidentes com vítimas fatais: </strong>', percnorm)),
           
           'Tipo de acidente' = db_ocorrencias_transito_bh$ocorrencias %>% 
             dplyr::group_by(regional) %>% 
             dplyr::summarise(n = dplyr::n(),
                              soma = sum(desc_acidente == stringr::str_to_lower(.valor))) %>% 
             dplyr::mutate(perc = 100*soma/n) %>% 
             dplyr::right_join(regionais_bh@data, by = c('regional' = 'NOME')) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(perc), '0%', paste0(round(perc, 2), '%'))) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(percnorm), '0%', percnorm)) %>% 
             dplyr::mutate_at(.vars = c('n', 'soma', 'perc'), .funs = function(x) ifelse(is.na(x), 0, x)) %>% 
             dplyr::mutate(legenda = stringr::str_c('<strong>Regional: </strong>', regional,
                                                    '<br><strong>Número total de acidentes: </strong>', n,
                                                    '<br><strong>Número de ', .valor,': </strong>', soma,
                                                    '<br><strong>Percentual de ', .valor ,': </strong>', percnorm)),
           
           'Tipo de veiculo' = db_ocorrencias_transito_bh$veiculos %>% 
             dplyr::group_by(num_boletim) %>% 
             dplyr::summarise(ind = sum(desc_tipo_veiculo %in% seleciona_veiculos(.valor), na.rm = TRUE)) %>%
             dplyr::mutate(ind = (ind > 0)) %>% 
             dplyr::mutate(ind = as.integer(ind)) %>%
             dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
             dplyr::group_by(regional) %>%  
             dplyr::summarise(n = dplyr::n(),
                              soma = sum(ind)) %>% 
             dplyr::mutate(perc = 100*soma/n) %>% 
             dplyr::right_join(regionais_bh@data, by = c('regional' = 'NOME')) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(perc), '0%', paste0(round(perc, 2), '%'))) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(percnorm), '0%', percnorm)) %>% 
             dplyr::mutate_at(.vars = c('n', 'soma', 'perc'), .funs = function(x) ifelse(is.na(x), 0, x)) %>% 
             dplyr::mutate(legenda = stringr::str_c('<strong>Regional: </strong>', regional,
                                                    '<br><strong>Número total de acidentes: </strong>', n,
                                                    '<br><strong>Número de acidentes envolvendo ', .valor,': </strong>', soma,
                                                    '<br><strong>Percentual de acidentes envolvendo ', .valor ,': </strong>', percnorm)),
           
           'Periodo do dia' = db_ocorrencias_transito_bh$ocorrencias %>% 
             dplyr::group_by(regional) %>% 
             dplyr::summarise(n = dplyr::n(),
                              soma = sum(periodo == stringr::str_to_lower(.valor))) %>% 
             dplyr::mutate(perc = 100*soma/n) %>% 
             dplyr::right_join(regionais_bh@data, by = c('regional' = 'NOME')) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(perc), '0%', paste0(round(perc, 2), '%'))) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(percnorm), '0%', percnorm)) %>% 
             dplyr::mutate_at(.vars = c('n', 'soma', 'perc'), .funs = function(x) ifelse(is.na(x), 0, x)) %>% 
             dplyr::mutate(legenda = stringr::str_c('<strong>Regional: </strong>', regional,
                                                    '<br><strong>Número total de acidentes: </strong>', n,
                                                    '<br><strong>Número de acidentes de ', stringr::str_to_lower(.valor),': </strong>', soma,
                                                    '<br><strong>Percentual de acidente de ', stringr::str_to_lower(.valor) ,': </strong>', percnorm)),
           
           'Sexo do condutor' = db_ocorrencias_transito_bh$pessoas %>% 
             dplyr::mutate(sexo = ifelse(is.na(sexo), 'NA', sexo)) %>% 
             dplyr::group_by(num_boletim) %>% 
             dplyr::summarise(ind = sum(sexo == muda_valor_sexo(.valor) & ind_condutor == 1, na.rm = TRUE)) %>%
             dplyr::mutate(ind = (ind > 0)) %>% 
             dplyr::mutate(ind = as.integer(ind)) %>%
             dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
             dplyr::group_by(regional) %>% 
             dplyr::summarise(n = dplyr::n(),
                              soma = sum(ind)) %>% 
             dplyr::mutate(perc = 100*soma/n) %>% 
             dplyr::right_join(regionais_bh@data, by = c('regional' = 'NOME')) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(perc), '0%', paste0(round(perc, 2), '%'))) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(percnorm), '0%', percnorm)) %>% 
             dplyr::mutate_at(.vars = c('n', 'soma', 'perc'), .funs = function(x) ifelse(is.na(x), 0, x)) %>% 
             dplyr::mutate(legenda = stringr::str_c('<strong>Regional: </strong>', regional,
                                                    '<br><strong>Número total de acidentes: </strong>', n,
                                                    '<br><strong>Número de acidentes envolvendo condutores de sexo ', stringr::str_to_lower(.valor),': </strong>', soma,
                                                    '<br><strong>Percentual de acidentes envolvendo condutores de sexo ', stringr::str_to_lower(.valor) ,': </strong>', percnorm)),
           
           'Numero de veiculos envolvidos' = db_ocorrencias_transito_bh$ocorrencias %>% 
             dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'Mais que 4', num_veiculos_envolvidos)) %>% 
             dplyr::group_by(regional) %>% 
             dplyr::summarise(n = dplyr::n(),
                              soma = sum(num_veiculos_envolvidos == .valor, na.rm = TRUE)) %>% 
             dplyr::mutate(perc = 100*soma/n) %>% 
             dplyr::right_join(regionais_bh@data, by = c('regional' = 'NOME')) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(perc), '0%', paste0(round(perc, 2), '%'))) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(percnorm), '0%', percnorm)) %>% 
             dplyr::mutate_at(.vars = c('n', 'soma', 'perc'), .funs = function(x) ifelse(is.na(x), 0, x)) %>% 
             dplyr::mutate(legenda = stringr::str_c('<strong>Regional: </strong>', regional,
                                                    '<br><strong>Número total de acidentes: </strong>', n,
                                                    '<br><strong>Número de acidentes envolvendo ', stringr::str_to_lower(.valor), ' veículo(s): </strong>', soma,
                                                    '<br><strong>Percentual de acidentes envolvendo ', stringr::str_to_lower(.valor) , ' veículo(s): </strong>', percnorm)),
           
           'Numero de pessoas envolvidas' = db_ocorrencias_transito_bh$ocorrencias %>% 
             dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'Mais que 4', num_veiculos_envolvidos)) %>% 
             dplyr::group_by(regional) %>% 
             dplyr::summarise(n = dplyr::n(),
                              soma = sum(num_veiculos_envolvidos == .valor, na.rm = TRUE)) %>% 
             dplyr::mutate(perc = 100*soma/n) %>% 
             dplyr::right_join(regionais_bh@data, by = c('regional' = 'NOME')) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(perc), '0%', paste0(round(perc, 2), '%'))) %>% 
             dplyr::mutate(percnorm = ifelse(is.na(percnorm), '0%', percnorm)) %>% 
             dplyr::mutate_at(.vars = c('n', 'soma', 'perc'), .funs = function(x) ifelse(is.na(x), 0, x)) %>% 
             dplyr::mutate(legenda = stringr::str_c('<strong>Regional: </strong>', regional,
                                                    '<br><strong>Número total de acidentes: </strong>', n,
                                                    '<br><strong>Número de acidentes envolvendo ', stringr::str_to_lower(.valor),' pessoa(s): </strong>', soma,
                                                    '<br><strong>Percentual de acidentes envolvendo ', stringr::str_to_lower(.valor) ,' pessoa(s): </strong>', percnorm))
           
    )
    
  }

}

cria_mapa_bairros <- function(.key, .value, .ind_percentual = FALSE, .corte_num_acidentes = 45){
  
  poligonos_bairros <- bairros_bh
  
  poligonos_bairros@data <- poligonos_bairros@data %>% 
    dplyr::left_join(muda_objsp(.key, .value), by = c('ID', 'CODIGO', 'NOME' = 'bairro', 'AREA_KM2', 'PERIMETR_M')) %>%
    dplyr::mutate(variavel = soma)
  
  if(.ind_percentual){
    
    poligonos_bairros@data <- poligonos_bairros@data %>% 
      dplyr::mutate(variavel = ifelse(n < as.integer(.corte_num_acidentes), 0, perc)) 

    pal <- leaflet::colorNumeric("Purples", range(poligonos_bairros@data$variavel))
    
    leaflet::leaflet() %>% 
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
      leaflet::addPolygons(data = poligonos_bairros, weight = 0.25, 
                           stroke = TRUE, smoothFactor = 0.01, color = 'black',
                           fillOpacity = 0.7, fillColor = ~pal(variavel),
                           popup = ~legenda, label = ~NOME)
    
  } else{
    
    pal <- leaflet::colorNumeric("Purples", log(range(poligonos_bairros@data$variavel)+1))
    
    leaflet::leaflet() %>% 
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
      leaflet::addPolygons(data = poligonos_bairros, weight = 0.25, 
                           stroke = TRUE, smoothFactor = 0.01, color = 'black',
                           fillOpacity = 0.7, fillColor = ~pal(log(variavel + 1)),
                           popup = ~legenda, label = ~NOME)
    
  } 

}

cria_mapa_regionais <- function(.key, .value, .ind_percentual = FALSE){
  
  poligonos_regionais <- regionais_bh
  
  poligonos_regionais@data <- poligonos_regionais@data %>% 
    dplyr::left_join(muda_objsp(.key, .value, .divisao = 'regional'), 
                     by = c('ID_REG', 'COD_REG', 'SIGLA', 'NOME' = 'regional', 'AREA_KM2', 'PERIMETR_M')) %>%
    dplyr::mutate(variavel = soma)
  
  if(.ind_percentual){
    
    poligonos_regionais@data <- poligonos_regionais@data %>% 
      dplyr::mutate(variavel = perc) 

    pal <- leaflet::colorNumeric("Purples", c(0, max(poligonos_regionais@data$variavel)))
    
    leaflet::leaflet() %>% 
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
      leaflet::addPolygons(data = poligonos_regionais, weight = 0.25, 
                           stroke = TRUE, smoothFactor = 0.01, color = 'black',
                           fillOpacity = 0.7, fillColor = ~pal(variavel),
                           popup = ~legenda, label = ~NOME)
    
  } else{
    
    pal <- leaflet::colorNumeric("Purples", log(c(1, max(poligonos_regionais@data$variavel)+1)))
    
    leaflet::leaflet() %>% 
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
      leaflet::addPolygons(data = poligonos_regionais, weight = 0.25, 
                           stroke = TRUE, smoothFactor = 0.01, color = 'black',
                           fillOpacity = 0.7, fillColor = ~pal(log(variavel + 1)),
                           popup = ~legenda, label = ~NOME)
    
  } 

}

executa_grafico_serie_temporal <- function(df, periodicidade){
  
  switch(periodicidade, 
         'Diaria' = df %>% 
           dplyr::mutate(dia = lubridate::date(data_hora)) %>% 
           dplyr::group_by(dia) %>% 
           dplyr::summarise(n = dplyr::n()) %>%
           dplyr::right_join(expand.grid(dia = seq.Date(from = as.Date('2016-01-01'), to = as.Date('2019-12-31'), by = 1)), by = "dia") %>%
           dplyr::arrange(dia) %>% 
           dplyr::mutate(n = ifelse(is.na(n), 0, n)) %>% 
           ggplot2::ggplot(ggplot2::aes(x = dia, y = n)) + 
           ggplot2::geom_line() + 
           ggplot2::geom_smooth(method = 'loess', formula = y~x, se = FALSE, col = 'red') + 
           ggplot2::theme_bw() +
           ggplot2::xlab('Data') +
           ggplot2::ylab('Número de acidentes'),
         
         'Semanal' = df %>% 
           dplyr::mutate(semana = lubridate::floor_date(data_hora,unit = 'week')) %>% 
           dplyr::group_by(semana) %>% 
           dplyr::summarise(n = dplyr::n()) %>%
           dplyr::mutate(semana = lubridate::date(semana)) %>%
           dplyr::right_join(expand.grid(semana =seq.Date(from = as.Date('2015-12-27'), to = as.Date('2019-12-29'), by = 7)), by = "semana") %>%
           dplyr::mutate(n = ifelse(is.na(n), 0, n)) %>% 
           ggplot2::ggplot(ggplot2::aes(x = semana, y = n)) + 
           ggplot2::geom_line() + 
           ggplot2::geom_smooth(method = 'loess', formula = y~x, se = FALSE, col = 'red') + 
           ggplot2::theme_bw() +
           ggplot2::xlab('Data') +
           ggplot2::ylab('Número de acidentes'),
         
         'Mensal' = df %>% 
           dplyr::mutate(mes = lubridate::floor_date(data_hora,unit = 'month')) %>% 
           dplyr::group_by(mes) %>% 
           dplyr::summarise(n = dplyr::n()) %>%
           dplyr::mutate(mes = lubridate::date(mes)) %>%
           dplyr::right_join(expand.grid(mes = seq.Date(from = as.Date('2016-01-01'), to = as.Date('2019-12-31'), by = 1)) %>% 
                               dplyr::filter(stringr::str_detect(mes, '\\-01$')), by = "mes") %>%
           dplyr::mutate(n = ifelse(is.na(n), 0, n)) %>% 
           dplyr::mutate(aux = stringr::str_c(lubridate::month(mes, label = TRUE, abbr = FALSE), '/', lubridate::year(mes))) %>% 
           dplyr::mutate(ref = stringr::str_c(aux, '\nnumero acidentes:', n)) %>% 
           ggplot2::ggplot(ggplot2::aes(x = mes, y = n, label = ref)) + 
           ggplot2::geom_line() + 
           ggplot2::geom_smooth(method = 'loess', formula = y~x, se = FALSE, col = 'red') + 
           ggplot2::theme_bw() +
           ggplot2::xlab('Data') +
           ggplot2::ylab('Número de acidentes'),
         
         'Anual' = df %>% 
           dplyr::mutate(ano = lubridate::year(data_hora)) %>% 
           dplyr::group_by(ano) %>% 
           dplyr::summarise(n = dplyr::n()) %>%
           dplyr::right_join(expand.grid(ano = 2016:2019), by = "ano") %>%
           dplyr::mutate(n = ifelse(is.na(n), 0, n)) %>% 
           ggplot2::ggplot(ggplot2::aes(x = ano, y = n)) + 
           ggplot2::geom_line() + 
           ggplot2::theme_bw() +
           ggplot2::xlab('Data') +
           ggplot2::ylab('Número de acidentes')
         
  )
  
}

executa_estdesc_serie_temporal <- function(df, periodicidade){
  
  switch(periodicidade, 
         'Diaria' = df %>% 
           dplyr::mutate(dia = lubridate::date(data_hora)) %>% 
           dplyr::group_by(dia) %>% 
           dplyr::summarise(n = dplyr::n()) %>%
           dplyr::right_join(expand.grid(dia = seq.Date(from = as.Date('2016-01-01'), to = as.Date('2019-12-31'), by = 1)), by = "dia") %>%
           dplyr::arrange(dia) %>% 
           dplyr::mutate(n = ifelse(is.na(n), 0, n)) %>% 
           summarise('Mínimo' = min(n),
                     '1Quartil' = quantile(n, 0.25),
                     'Média' = mean(n),
                     'Mediana' = quantile(n, 0.5),
                     '3Quartil' = quantile(n, 0.75),
                     'Máximo' = max(n)),
         
         'Semanal' = df %>% 
           dplyr::mutate(semana = lubridate::floor_date(data_hora,unit = 'week')) %>% 
           dplyr::group_by(semana) %>% 
           dplyr::summarise(n = dplyr::n()) %>%
           dplyr::mutate(semana = lubridate::date(semana)) %>%
           dplyr::right_join(expand.grid(semana =seq.Date(from = as.Date('2015-12-27'), to = as.Date('2019-12-29'), by = 7)), by = "semana") %>%
           dplyr::mutate(n = ifelse(is.na(n), 0, n)) %>% 
           summarise('Mínimo' = min(n),
                     '1Quartil' = quantile(n, 0.25),
                     'Média' = mean(n),
                     'Mediana' = quantile(n, 0.5),
                     '3Quartil' = quantile(n, 0.75),
                     'Máximo' = max(n)),
         
         'Mensal' = df %>% 
           dplyr::mutate(mes = lubridate::floor_date(data_hora,unit = 'month')) %>% 
           dplyr::group_by(mes) %>% 
           dplyr::summarise(n = dplyr::n()) %>%
           dplyr::mutate(mes = lubridate::date(mes)) %>%
           dplyr::right_join(expand.grid(mes = seq.Date(from = as.Date('2016-01-01'), to = as.Date('2019-12-31'), by = 1)) %>% 
                               dplyr::filter(stringr::str_detect(mes, '\\-01$')), by = "mes") %>%
           dplyr::mutate(n = ifelse(is.na(n), 0, n)) %>% 
           summarise('Mínimo' = min(n),
                     '1Quartil' = quantile(n, 0.25),
                     'Média' = mean(n),
                     'Mediana' = quantile(n, 0.5),
                     '3Quartil' = quantile(n, 0.75),
                     'Máximo' = max(n)),
         
         'Anual' = df %>% 
           dplyr::mutate(ano = lubridate::year(data_hora)) %>% 
           dplyr::group_by(ano) %>% 
           dplyr::summarise(n = dplyr::n()) %>%
           dplyr::right_join(expand.grid(ano = 2016:2019), by = "ano") %>%
           dplyr::mutate(n = ifelse(is.na(n), 0, n)) %>% 
           summarise('Mínimo' = min(n),
                     '1Quartil' = quantile(n, 0.25),
                     'Média' = mean(n),
                     'Mediana' = quantile(n, 0.5),
                     '3Quartil' = quantile(n, 0.75),
                     'Máximo' = max(n))
         
         
  )
  
}

seleciona_obj_lista <- function(x){
  
  switch(x, 
         "Indicio de embriaguez" = "indicio_embriaguez_acidente", 
         "Pedestre envolvido" = "indicio_pedestre_envolvido", 
         "Indicio de evasao" = "indicio_evasao", 
         "Indicio de fatalidade" = "indicio_fatalidade",
         "Numero de pessoas envolvidas" = "numero_pessoas_envolvidas",
         "Numero de veiculos envolvidos" = "numero_veiculos_envolvidos",
         "Sexo do condutor" = "sexo_condutor",
         "Faixa etaria do condutor" = "faixa_idade_condutor",
         "Grau de severidade dos ferimentos do condutor" = "ferimentos_condutor",
         "Indicio de uso do cinto de seguranca pelos condutores" = "indicio_uso_cinto_seguranca",
         "Periodo do dia" = "periodo_dia", 
         "Tipo acidente" = "tipo_acidente", 
         "Descricao veiculos" = "descricao_veiculos")
  
}

mostra_info_tabela <- function(x1, x2, .lst = lst_tables){
  
  lst_return <- list()
  
    
    if(x1 == x2|x2 == ""){
      
      lst_return$tbl_geral <- .lst[[seleciona_obj_lista(x1)]]$tbl_geral
      lst_return$comentario <- .lst[[seleciona_obj_lista(x1)]]$comentario
      
    } else{
      
      lst_return$tbl_geral <- .lst[[seleciona_obj_lista(x1)]]$tbl_geral
      lst_return$comentario <- .lst[[seleciona_obj_lista(x1)]]$comentario
      lst_return$tbl_info <- .lst[[seleciona_obj_lista(x1)]][[seleciona_obj_lista(x2)]]
    } 
    

  return(lst_return)
  
}

pagina_inicial <- function(){

  shiny::tags$head(
    shiny::tags$meta(name = "author", content = "Gustavo Godinho"),
    shiny::tags$meta(name = "creation_date", content = "16/02/2021"),
    shiny::tags$meta(name = "charset", content = "UTF-8"),
    shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
    )
  
   shiny::tabPanel('Início',
     shiny::fluidPage(

         shiny::tags$style("
         .textofmt {text-align: justify}
         .textofmt {font-family: 'Arial Narrow'; font-weight: 500; font-size: 20px; line-height: 1.25;color: 404040; text-align: justify}
         .textofmt2 {font-family: 'Arial Narrow'; font-weight: 500; font-size: 16px; color:red; text-align: justify}
         .topimg {margin-left:-30px; margin-right:-30px; margin-top:-15px;}
         .titulofmt {font-family: 'Arial Narrow'; font-weight: 500; font-size: 30px;line-height: 1;color: 404040; text-align: center}
         table {font-family: 'Arial Narrow'; background-color: white; font-size:15px}
         table tr:nth-child(even){background-color: #f2f2f2}
         table th{background-color: #f2f2f2};
         table td{width: 5000px};
         h4 {font-family: 'Arial Narrow'; font-size: 20px;}
         a:link {color: #5c00e6; background-color: transparent; text-decoration: none;}
         a:visited {color: pink; background-color: transparent; text-decoration: none;}
         a:hover {color: red;background-color: transparent;text-decoration: underline;}
         a:active {color: yellow;background-color: transparent;text-decoration: underline;}
         mark {background-color: #f2f2f2;}"),
       
       shiny::column(1, ""),
       shiny::column(10,
          
         
         shiny::div(class="topimg", img(src="logo_capa.png", height="100%", width="100%")),

         shiny::div(shiny::strong('Acidentes de trânsito em BH'), class = 'titulofmt'),

         shiny::hr(),

         shiny::p(class = 'textofmt', shiny::HTML('&emsp;&emsp;'), 
         "Belo Horizonte é um dos maiores centros urbanos brasileiros 
         e foi a primeira cidade do país a ser planejada, quando ainda no 
         final do século XIX, Aarão Reis a arquitetou para ser a capital 
         do estado de Minas Gerais. O crescimento populacional desenfreado fez 
         BH ter atualmente cerca de 2 milhões e meio de habitantes,
         uma densidade populacional em torno de 7500 pessoas por km² e 
         consequentemente a cidade se expandiu para fora do perímetro 
         planejado quando começou a ser habitada em 1897."),
         
         shiny::br(),
         
         shiny::p(class = 'textofmt', shiny::HTML('&emsp;&emsp;'), 
         "A capital mineira não conseguiu se reestruturar como outras 
         grandes cidades do país para ter um transporte público metroviário 
         eficiente e abrangente e também por consequência disso, a frota 
         veicular da cidade cresce anualmente, ", 
         shiny::tags$a(href = "https://www.hojeemdia.com.br/horizontes/com-o-ritmo-atual-de-crescimento-da-frota-bh-deve-ter-mais-carro-do-que-gente-em-2022-1.756689", 
                       "ameaçando em 2022 existir mais carros que pessoas em BH.", target = '_blank'),
         " Com mais veículos nas ruas, aumentam as chances de acidentes, 
         já que várias ruas e avenidas da cidade não comportam o atual 
         ecossistema veicular. Além disso, em Belo Horizonte existe 
         o ", shiny::tags$mark('Anel Rodoviário')," que é uma união de 
         rodovias federais que cortam o município e é uma opção de 
         deslocamento rápido sem passar pela zona central da metrópole. 
         O maior problema dessa via é que por lá circulam em conjunto 
         veículos grandes e de passeio, sendo comum notícias sobre acidentes 
         graves nessa via."),
         
         shiny::br(),
         
         shiny::p(class = 'textofmt', shiny::HTML('&emsp;&emsp;'), 
         "Esse projeto tem o objetivo de fazer uma análise de dados 
         dos boletins de ocorrência sobre acidentes de trânsito em Belo Horizonte,  
         identificando pontos críticos e disseminando as informações. 
         Qualquer belo-horizontino sabe que o ", 
         shiny::tags$mark('Anel Rodoviário'), " representa o maior 
         problema para acidentes de trânsito na cidade. O poder da 
         Estatística está em nos mostrar qual a gravidade desse problema, 
         quais são os outros pontos críticos, os tipos mais comuns de 
         acidentes e os perfis das pessoas que se envolvem neles."), 
         
         shiny::br(),
         
         shiny::p(class = 'textofmt', shiny::HTML('&emsp;&emsp;'), 
         "Na aba ", shiny::strong("Mapa"), " está a visualização 
         geográfica interativa de todas os acidentes de trânsito relatados 
         em BH no período entre 2016 e 2019 e alguns gráficos 
         e tabelas sobre características de acidentes registados na 
         cidade. Na aba ", shiny::strong('Locais mais frequentes'), 
         " há os pontos com mais ocorrências em Belo Horizonte por 
         vários aspectos. Na aba ", 
         shiny::strong('Distribuição por bairros'), " você pode 
         conferir a distribuição espacial dos acidentes por bairros 
         ou regionais da cidade. Na aba ", shiny::strong('Tabelas'), 
         " foi feito um estudo analisando as tabelas de contingência 
         das variáveis coletadas nos registros dos acidentes e que 
         complementam a análise e em ", shiny::strong('Considerações'), 
         " há mais informações dos dados e informações sobre o 
         código para reproduzir essa aplicação. Eu fiz um vídeo no youtube reunindo 
         várias notícias dos problemas de mobilidade urbana de Belo Horizonte e introduzindo
         o que será mostrado aqui nessa aplicação. Você pode acessá-lo ", 
         shiny::tags$a(href = "https://www.youtube.com/watch?v=bauvQXktuyU&lc=UgzFhN_74CZYCObnsS54AaABAg", "clicando aqui.", target = '_blank'),
         "Esse ", shiny::tags$em('Shiny app'), " possui vários mapas e tabelas 
         e o servidor do shinyapps não é dos mais potentes, tenha 
         paciência que todo o conteúdo será carregado."), 
         
         shiny::br()
         
         # shiny::p(class = 'textofmt', shiny::HTML('&emsp;&emsp;'), 
         # "Os dados são um resumo das informações contidas em 49718 boletins 
         # de ocorrência dos acidentes de trânsito em Belo Horizonte no período 
         # entre 2016 e 2019 e estão disponíveis na página da BHTrans que é o 
         # órgão público responsável pelo assunto no município. Infelizmente, 
         # há erros e omissão de preenchimento nos boletins, como o local exato 
         # da ocorrência e informações sobre endereço e envolvidos, também existe 
         # o hábito no Brasil de nem sempre fazer o registro sobre a ocorrência 
         # de um acidente, mas ainda assim acredito que conseguimos extrair 
         # conhecimento desses dados.")
         
       )
     )
   )

 }

pagina_mapa <- function(){

   shiny::tabPanel('Mapa',

     shiny::fluidPage(
       shiny::column(1, ''),
       shiny::column(10,

          shiny::div(shiny::HTML('&emsp;&emsp;'), "O mapa abaixo 
          possui todos os acidentes de trânsito registrados e 
          georeferenciados que aconteceram em Belo Horizonte entre 
          01/01/2016 e 31/12/2019. As ocorrências estão agrupadas mas 
          é só aumentar o zoom no mapa para ver mais detalhes sobre as 
          coordenadas geográficas das mesmas.", 
          class = 'textofmt'),
          
          shiny::br(),
          
          shiny::div(class = 'textofmt',
            shiny::HTML('&emsp;&emsp;'), "A análise dos acidentes 
            nessa aba estão se referindo a toda cidade de Belo 
            Horizonte mas é possível pesquisar por acidentes de 
            trânsito em um determinado local ou situação específica 
            usando a barra abaixo. Você pode pesquisar por logradouro(s), 
            regional(is), bairro(s), ano(s), mês(es), dia(s) ou tipo(s) de acidente. 
            Por exemplo, para ver detalhes dos atropelamentos de pedestres relatados na ", 
            shiny::tags$mark('avenida Afonso Pena'), " nos meses de 
            Dezembro dos anos de 2016 e 2017, basta digitar as tags:", 
            shiny::em("Avenida Afonso Pena;"), 
            shiny::em("mês Dezembro;"),
            shiny::em("ano 2016;"), 
            shiny::em("ano 2017;"),
            shiny::em("Atropelamento de pedestre;")),
          
          shiny::br(),
          
          shiny::div(class = 'textofmt',
            shiny::strong('Observação:'), "Os tipos de acidentes permitidos na barra de pesquisa são: 
            Atropelamento de animal, Atropelamento de pedestre, Capotamento de veículo, Colisão de veículos, 
            Colisão de veículo com objeto parado, Outros tipos de acidentes, Queda de pessoa de veículo e 
            Vazamento de carga do veículo, pois foram esses os tipos de acidentes relatados no conjunto de dados."),
          
          shiny::selectizeInput(
            inputId = 'pesquisa_mapa_obs', 
            label = '', 
            choices = c(), 
            selected = NULL, 
            multiple = TRUE, 
            options = list(create = TRUE)),

         leaflet::leafletOutput('mapa_principal', height = 700),

         shiny::div(class = 'textofmt2',
            shiny::textOutput('mensagem_retorno_pesquisa_mapa_obs')),
         
         shiny::br(),
         
         shiny::div(shiny::HTML('&emsp;&emsp;'), "Em ", 
         shiny::tags$a(href = '#serie_temporal', "Série temporal"), 
         " há o acompanhamento da frequência de acidentes registrados na 
         condição pesquisada por várias periodicidades. Em ", 
         shiny::tags$a(href = '#detalhes_acidentes', "Descrição dos acidentes"), 
         " você encontra detalhes sobre os acontecimentos e em ", 
         shiny::tags$a(href = '#detalhes_pessoas', "Descrição das pessoas envolvidas"), 
         " há detalhes sobre os envolvidos nos acidentes na condição  
         pesquisada.", class = 'textofmt'),
         
         shiny::hr(),
         
         shiny::h4(shiny::strong('Série temporal'), id = 'serie_temporal'),
         
         shiny::div(shiny::HTML('&emsp;&emsp;'),"Aqui está a série 
         histórica de registros de acidentes para a pesquisa realizada, 
         você pode escolher a periodicidade da série temporal na caixa 
         de seleção abaixo. A linha em vermelho nos gráficos é a curva ", 
         shiny::em('loess'), " que basicamente é uma suavização da 
         média móvel do número de acidentes registrados ao longo do tempo.", 
         class = 'textofmt'),
         
         shiny::selectInput('valor_periodicidade', label = '', 
           choices = c('Diaria', 'Semanal', 'Mensal', 'Anual'), 
           selected = 'Semanal'),
         
         shiny::div(class = 'textofmt',
           shiny::strong(shiny::textOutput('titulo_fig_serie_temporal'))),

         shiny::div(plotly::plotlyOutput({'fig_serie_temporal'}), align = 'center'),
         
         shiny::div(class = 'textofmt',
           shiny::HTML('&emsp;&emsp;'),"Algumas estatísticas 
           descritivas da série temporal exibida acima:"),
         
         shiny::br(),
         
         shiny::div(shiny::tableOutput({'estdesc_serietemporal'}), align = 'center'),
         
         shiny::br(),
         
         shiny::div(class = 'textofmt',
           shiny::HTML('&emsp;&emsp;'),"Há algumas informações 
           relevantes nessas séries temporais. No período analisado 
           houve em média quase 31  acidentes de trânsito relatados  
           na capital por dia, sendo em 18 de Outubro de 2019, o dia 
           com menos ocorrências registradas, apenas 2. A véspera do 
           feriado de Proclamação da república também em 2019, foi o 
           dia com mais observações registradas, 53 ocorrências de 
           acidentes de trânsito. Das outras 6 vezes que o número de 
           ocorrências ultrapassou a marca de 
           50 registros por dia, uma delas foi na véspera do feriado 
           de Tiradentes e uma outra na sexta-feira antes da semana do 
           dia das crianças. Para a série de periodicidade semanal, 
           há um ponto interessante: a semana (completa) com menos registros teve 
           123 ocorrências e foi a última do mês de Maio e início do mês de Junho 
           de 2018, período pertencente a greve dos caminhoneiros que 
           afetou todo o país, onde foi difícil e/ou caro achar 
           combustível nos postos. Na série temporal com medição mensal, 
           há padrões que podem ser interpretados como indícios bem fracos de 
           sazonalidade. Com exceção de 2018, em todos os outros anos os 
           registros de ocorrências foram baixos em Janeiro e Fevereiro, 
           aumentaram durante o ano e decresceram em Dezembro. Em 2018, 
           apesar do mês de Fevereiro ter sido o mês com o menor número de 
           ocorrências em toda a série, apenas 693, o ano não segue o 
           mesmo padrão que os outros aparentam seguir."),
         
         shiny::hr(),
       
         shiny::h4(shiny::strong('Descrição dos acidentes'), id = 'detalhes_acidentes'),
         
         shiny::div(shiny::HTML('&emsp;&emsp;'),"Aqui há tabelas 
         detalhando as frequências dos acidentes selecionados na 
         pesquisa. Em ", shiny::em('Veículos'), "há uma tabela com as 
         15 combinações de veículos mais frequentes envolvidos nos 
         acidentes pesquisados, em parênteses está a 
         quantidade de veículos daquele tipo envolvido. Por exemplo, 
         em ", shiny::em('Automovel (1), Motocicleta (1)'), "representa 
         que 1 automóvel e 1 motocicleta se envolveram naquele 
         acidente. Em ", shiny::em('Tipos'), " está a tabela de 
         frequência dos tipos de acidentes que ocorreram de acordo com 
         a pesquisa feita. Em ", shiny::em('Recorrência'), " há duas 
         tabelas com estatísticas descritivas da diferença de tempo 
         em minutos e em horas entre um acidente e o anterior, em ", 
         shiny::em('Horários'), " os acidentes estão expostos em um
         gráfico de barras de acordo com a hora em que ocorreram, em ", 
         shiny::em('Número de veículos'), " há uma tabela descrevendo 
         a quantidade de veículos que participaram dos acidentes na condição
         pesquisada  e em ", shiny::em('Número de pessoas'), " há uma tabela 
         com a frequência de pessoas envolvidas nesses acidentes.", 
         class = 'textofmt'),
         
         shiny::br(),
         
         shiny::div(
           shiny::tabsetPanel(
             shiny::tabPanel('Veículos', 
                shiny::div(
                  shiny::br(),
                  shiny::tableOutput({'tbl_veiculos_mapa'}), align = 'center', id = 'mapa_veiculo')),
             
             shiny::tabPanel('Tipos', 
                shiny::div(
                  shiny::br(),
                  shiny::tableOutput({'tbl_tipos_acidentes_mapa'}), align = 'center', id = 'mapa_tipo')),
             
             shiny::tabPanel('Recorrência', 
                shiny::div(
                  shiny::br(),
                  shiny::div(shiny::strong("Recorrência dos acidentes em minutos"), class = '.titulofmt'),
                  shiny::div(shiny::tableOutput({'tbl_recorrencia_acidentes_minutos'})),
                  shiny::div(shiny::strong("Recorrência dos acidentes em horas"), 
                             class = '.titulofmt'),
                  shiny::div(shiny::tableOutput({'tbl_recorrencia_acidentes_horas'})),
                  align = 'center')),
            
             shiny::tabPanel('Horários', 
                shiny::div(
                  shiny::br(),
                  plotly::plotlyOutput({'fig_horario_acidentes'}), align = 'center')),
             
             shiny::tabPanel('Número de pessoas', 
                shiny::div(
                  shiny::br(),
                  shiny::tableOutput({'tbl_num_pessoas_envolvidas'}), align = 'center')),
             
             shiny::tabPanel('Número de veículos', 
                shiny::div(
                  shiny::br(),
                  shiny::tableOutput({'tbl_num_veiculos_envolvidos'}), align = 'center'))
             
           )
         ),
         
         shiny::div(shiny::HTML('&emsp;&emsp;'),"Se nota que em geral, 
         o tipo mais frequente de acidente relatado envolveu 1 automóvel 
         somente com o motorista e 1 motocicleta somente com o piloto, 
         ambos em movimento, se colidindo. Na cidade acontece um acidente de 
         trânsito com boletim de ocorrência em média a cada 47 minutos. 
         Os horários mais frequentes para os incidentes são: o horário de 
         pico da manhã, entre 7h e 9h e principalmente o horário de pico 
         da tarde, entre 17h e 20h da noite. É díficil pesquisar por um local 
         onde os tipos mais comuns de acidentes relatados fujam disso, nem é 
         esse o objetivo dessa aba, mas conhecendo um pouco da cidade, 
         notamos padrões esperados nos dados. Por exemplo, acidentes 
         envolvendo 1 automóvel e 1 bicicleta representam 2% do total 
         das ocorrências, mas na ", 
         shiny::tags$mark("avenida Otacílio Negrão de Lima"), "que 
         margeia a Lagoa da Pampulha, local comum para a prática de 
         atividades físicas ao ar livre em BH, o percentual de acidentes relatados com 
         essa combinação de veículos chegou a mais de 6%, sendo o quarto mais 
         frequente nessa avenida. Outro exemplo são os acidentes 
         envolvendo apenas 1 ônibus, que representam cerca de 4% do total 
         de ocorrências, mas nas ", shiny::tags$mark("avenidas Santos Dumont e Paraná"), 
         " que são passagens exclusivas dos ônibus do tipo MOVE no 
         centro da capital, os acidentes envolvendo esse tipo de 
         veículo representam mais de 70% do total e o atropelamento de 
         pedestre foi o segundo incidente mais comum nessas vias.", 
         class = 'textofmt'),
         
         shiny::hr(),
         
         shiny::h4(shiny::strong('Descrição das pessoas envolvidas'), id = 'detalhes_pessoas'),
         
         shiny::div(shiny::HTML('&emsp;&emsp;'),"Vimos nas tabelas 
         anteriores que aproximadamente 2/3 dos acidentes relatados na cidade 
         tiveram a participação de 2 veículos e um pouco mais de 2/3 deles tiveram
         somente 2 pessoas envolvidas. Aqui vamos entrar em detalhes sobre 
         características das pessoas que se envolveram nos acidentes. Essa 
         seção está dividida em três partes: informações sobre os condutores 
         dos veículos, informações sobre os passageiros e informações sobre 
         os pedestres. A lógica das tabelas é a mesma da seção anterior.", class = 'textofmt'),
         
         shiny::br(),
         
         shiny::h4(shiny::strong('Descrição dos condutores')),

         shiny::div(class = 'textofmt',
           shiny::HTML('&emsp;&emsp;'),"Aqui há detalhes sobre os condutores 
           de veículos que participaram de acidentes relatados em Belo Horizonte 
           entre 2016 e 2019. Geralmente há mais de 1 condutor envolvido por 
           ocorrência, por isso as frequências absolutas exibidas 
           abaixo possuem soma maior que o número de acidentes registrados. 
           Exemplo: em ", shiny::em('Sexo'), "na coluna ", 
           shiny::em('Masculino'), "está a frequência absoluta de condutores 
           de sexo masculino que se envolveram em acidentes e o 
           percentual descreve a porcentagem dos condutores envolvidos em acidentes 
           relatados que eram do sexo masculino."),
         
         shiny::br(),
       
         shiny::div(
           shiny::tabsetPanel(
             shiny::tabPanel('Sexo', 
                shiny::div(
                  shiny::br(),
                  shiny::tableOutput({'tbl_sexo_condutor'}), align = 'center')),
             
             shiny::tabPanel('Faixa de idade',
                shiny::div(
                  shiny::br(),
                  plotly::plotlyOutput({'fig_faixa_idade_condutor'}), align = 'center')),
      
             shiny::tabPanel('Ferimentos',
                shiny::div(
                  shiny::br(),
                  shiny::tableOutput({'tbl_ferimentos_condutor'}), align = 'center'))
           )
         ),
         
         shiny::h4(shiny::strong('Descrição dos passageiros')),
         
         shiny::div(class = 'textofmt',
           shiny::HTML('&emsp;&emsp;'),"Em ", 
           shiny::em('Número de passageiros'), " está descrita a 
           quantidade de passageiros reportada por acidente relatado. Quando 
           o valor é igual a 0, significa que não foi anotado pelos 
           agentes públicos responsáveis, pessoas como passageiras 
           naquelas ocorrências."),
         
         shiny::br(),
         
         shiny::div(
           shiny::tabsetPanel(
             shiny::tabPanel('Número de passageiros', 
                shiny::div(
                  shiny::br(),
                  shiny::tableOutput({'tbl_num_passageiros'}), align = 'center')),
             
             shiny::tabPanel('Sexo', 
                shiny::div(
                  shiny::br(),
                  shiny::tableOutput({'tbl_sexo_passageiro'}), align = 'center')),
             
             shiny::tabPanel('Faixa de idade',
                shiny::div(
                  shiny::br(),
                  plotly::plotlyOutput({'fig_faixa_idade_passageiro'}), align = 'center')),
             
             shiny::tabPanel('Ferimentos',
                  shiny::div(
                  shiny::br(),
                  shiny::tableOutput({'tbl_ferimentos_passageiro'}), align = 'center'))
           )
         ),
         
         shiny::h4(shiny::strong('Descrição dos pedestres')),
         
         shiny::div(shiny::HTML('&emsp;&emsp;'),"Em ", 
         shiny::em('Número de pedestres'), " está descrita a 
         quantidade de pedestres envolvidas por acidente. Quando o 
         valor é igual a 0, significa que não houve a participação de pedestres nas ocorrências.", class = 'textofmt'),
         
         shiny::br(),
         
         shiny::div(
           shiny::tabsetPanel(
             shiny::tabPanel('Número de pedestres', 
               shiny::div(
               shiny::br(),
               shiny::tableOutput({'tbl_num_pedestres'}), align = 'center')),
             
             shiny::tabPanel('Sexo', 
               shiny::div(
               shiny::br(),
               shiny::tableOutput({'tbl_sexo_pedestre'}), align = 'center')),
             
             shiny::tabPanel('Faixa de idade',
               shiny::div(
               shiny::br(),
               plotly::plotlyOutput({'fig_faixa_idade_pedestre'}), align = 'center')),
             
             shiny::tabPanel('Ferimentos',
               shiny::div(
               shiny::br(),
               shiny::tableOutput({'tbl_ferimentos_pedestre'}), align = 'center'))
           )
         ),
         
         shiny::div(shiny::HTML('&emsp;&emsp;'), "Existem informações 
         interessantes nessas tabelas. Considerando toda a cidade, em 
         quase 80% das vezes que um veículo se envolveu em um acidente 
         em Belo Horizonte nos últimos anos que gerou um boletim de ocorrência, o 
         condutor era do sexo masculino. Repare que isso não nos dá 
         indícios sobre homens terem maior tendência a se envolverem 
         em acidentes de trânsito, não temos informações suficientes 
         para afirmar isso. Mais de 60% dos condutores relatados 
         nas ocorrências possuíam idade entre 20 e 39 anos na data do 
         acontecido e quase metade dos condutores se machucaram 
         mas não fatalmente.", class = 'textofmt'),
         
         shiny::br(),
         
         shiny::div(shiny::HTML('&emsp;&emsp;'), "Também conseguimos 
         ver que considerando todos os acidentes, somente em 23% deles 
         foi relatado pelo menos uma pessoa como passageira em um dos 
         veículos e quando relatado, cerca 2/3 desses passageiros eram 
         do sexo feminino. A faixa etária mais frequente para os 
         passageiros envolvidos em acidentes foi de 20 a 29 anos e 
         quase 88% deles tiveram ferimentos não-fatais.", class = 'textofmt'),
         
         shiny::br(),
         
         shiny::div(shiny::HTML('&emsp;&emsp;'), "Nas tabelas 
         referentes aos pedestres, notamos que aproximadamente 14% dos 
         acidentes relatados na cidade tiveram envolvimento de pedestres. 
         É interessante observar que as frequências estão quase que 
         igualmente distribuídas nas categorias de sexo e faixa etária, 
         indicando que não existe um perfil dominante entre as pessoas 
         que são atropelados na cidade. Na gravidade dos ferimentos 
         se nota que em Belo Horizonte 2,28% dos pedestres envolvidos em 
         acidentes com boletim de ocorrência morreram. Pesquisando nas 
         grandes vias da capital, observamos que esse percentual de fatalidade 
         de pedestres chegou a 5,77% das pessoas atropeladas  
         na ", shiny::tags$mark("avenida Cristiano Machado"), 
         ", cerca de 6% na ", shiny::tags$mark("avenida Presidente Antônio Carlos"), 
         "e a mais de 15% dos pedestres atropelados no ", shiny::tags$mark("Anel Rodoviário."), 
         class = 'textofmt'),
         
         shiny::br(),
         
         shiny::div(shiny::HTML('&emsp;&emsp;'), "Na próxima aba há 
         uma descrição dos locais com maiores recorrências de acidentes 
         registrados na cidade.", class = 'textofmt'),
         
         shiny::br(),
         shiny::br()

       )

     )

   )
 }

pagina_lf <- function(){
  
  shiny::tabPanel('Locais mais frequentes',
    
    shiny::fluidPage(
      shiny::column(1, ""),
      shiny::column(10,
                    
        shiny::div(class = 'textofmt',
          shiny::HTML('&emsp;&emsp;'), "Aqui você poderá ver os 10 
          locais com mais ocorrências relatadas de acidentes de 
          trânsito em Belo Horizonte por tipo de acidente, por 
          veículos envolvidos, por parte do dia, por gravidade, entre 
          outros. Os locais dos acidentes foram agrupados em raios de 
          aproximadamente 100 metros. Isso foi feito para facilitar a 
          visualização dos mapas, evitando que acidentes com diferenças 
          mínimas das coordenadas fossem considerados como se fossem em 
          locais diferentes."),
                    
        shiny::br(),
                    
        shiny::div(class = 'textofmt',
        shiny::HTML('&emsp;&emsp;'), "Abaixo você pode buscar por 
        alguma seção específica:"), 
        
        shiny::br(),

        shiny::div(class = 'textofmt',
          shiny::tags$ul(shiny::tags$a(href = '#mais_acidentes_geral', 'Locais com mais acidentes')),
          shiny::tags$ul(shiny::tags$a(href = '#mais_acidentes_por_tipo', 'Locais com mais acidentes por tipo de acidente')),
          shiny::tags$ul(shiny::tags$a(href = '#mais_acidentes_por_tipo_veiculo', 'Locais com mais acidentes por tipo de veículo')),
          shiny::tags$ul(shiny::tags$a(href = '#mais_acidentes_por_periodo_do_dia', 'Locais com mais acidentes por período do dia')),
          shiny::tags$ul(shiny::tags$a(href = '#mais_acidentes_por_regional', 'Locais com mais acidentes por regional')),
          shiny::tags$ul(shiny::tags$a(href = '#mais_acidentes_com_vitimas_fatais', 'Locais com mais acidentes com vítimas fatais')),
          shiny::tags$ul(shiny::tags$a(href = '#mais_acidentes_com_grande_escala', 'Locais com mais acidentes de grande escala')),
          shiny::tags$ul(shiny::tags$a(href = '#mais_acidentes_fins_semana', 'Locais com mais acidentes aos fins de semana e feriados'))),

        shiny::hr(),

        #titulo: locais mais frequentes
        shiny::h4(shiny::strong('Locais com mais acidentes'), id = 'mais_acidentes_geral'),

        shiny::div(class = 'textofmt',
          shiny::HTML('&emsp;&emsp;'), "No mapa e tabela abaixo são 
          exibidos os locais com mais ocorrências de acidentes de 
          trânsito registrados em Belo Horizonte; Das 5 primeiras colocações o ",
          shiny::tags$mark("Anel Rodoviário"), " ocupa 4. A ",
          shiny::tags$mark("avenida Cristiano Machado"), " também
          aparece como destaque negativo, são 4 pontos da avenida
          entre os 10 com mais acidentes na capital. Inclusive o trevo
          de interseção entre essas duas vias aparece entre os locais
          mais perigosos de BH para acidentes de trânsito na sétima 
          colocação. Clicando nos pontos há a possibilidade de visualizá-los 
          no Google Street View. O trecho do ", 
          shiny::tags$mark("Anel Rodoviário"), "conhecido como descida do 
          Betânia apareceu em 2º lugar nesse ranking e a ", 
          shiny::tags$mark("Rodovia Papa João Paulo II"), "no trecho mais 
          conhecido como ", shiny::tags$mark("MG-010"), "ficou em 4º lugar."),

        shiny::br(),

        leaflet::leafletOutput('mapa_locais_frequentes_geral', height = 500),

        shiny::br(),

        shiny::div(
          shiny::tableOutput({'tbl_locais_frequentes_geral'}), align = 'center'),

        shiny::hr(),
        
        #titulo: locais com mais acidentes por tipo
        shiny::h4(shiny::strong('Locais com mais acidentes por tipo'), id = 'mais_acidentes_por_tipo'),

        shiny::div(class = 'textofmt',
          shiny::HTML('&emsp;&emsp;'), "Abaixo estão os 10 locais com 
          mais colisões de veículo na capital. Na tabela há mais 
          detalhes sobre esses locais. Também há uma caixa de seleção 
          em que você pode escolher o tipo de acidente e ver quais são 
          os locais mais perigosos para aquele tipo de ocorrência."),

        shiny::br(),

        leaflet::leafletOutput('mapa_locais_frequentes_tp_acidentes'),

        shiny::br(),

        shiny::div(class = 'textofmt',
          shiny::selectizeInput(
            inputId = 'pesquisamapa_tp_acidente', label = NULL,
            choices = c("Colisão de veículos","Colisão de veículo com objeto parado",
                        "Atropelamento de pedestre", "Capotamento de veículo",
                        "Queda de pessoa de veículo", "Outros tipos de acidentes",
                        "Atropelamento de animal", "Vazamento de carga do veículo"),
            selected = "Colisão entre veículos", multiple = FALSE, 
            options = list(create = FALSE))),

        shiny::br(),

        shiny::div(
          shiny::tableOutput({'tbl_locais_frequentes_tp_acidentes'}), align = 'center'),

        shiny::div(class = 'textofmt',
          shiny::HTML('&emsp;&emsp;'), "Observando os dados não é 
          díficil ver que o ", shiny::tags$mark("Anel Rodoviário"),
          " é o grande problema no assunto acidentes de trânsito em 
          Belo Horizonte. Nas colisões e em capotamento de veículos,
          o ", shiny::tags$mark("Anel")," ocupa as três primeiras
          posições. Ainda nesses tipos de acidentes, a ", 
          shiny::tags$mark("avenida Cristiano Machado") , " e a ",
          shiny::tags$mark("rodovia Papa João Paulo II"), " no trecho 
          mais conhecido como ", shiny::tags$mark("MG-010"), "aparecem
          sempre. Outra grande avenida que está presente em colisão 
          com objeto parado e capotamento de veículos é a ", 
          shiny::tags$mark("Presidente Antônio Carlos"), " mas em 
          pontos diferentes. A avenida entra no ranking dos locais
          mais perigosos para colisões no trecho pouco depois do
          campus da UFMG no sentido bairro e está no ranking dos
          capotamentos por conta do trecho próximo ao hospital Belo
          Horizonte. A avenida ", shiny::tags$mark("Presidente Carlos Luz"),
          " também tem lugar no ranking de locais com mais capotamentos,
          no fragmento em frente ao Shopping Del Rey, com 9 
          capotamentos de veículos relatados em 4 anos."),

        shiny::br(),

        shiny::div(class = 'textofmt',
          shiny::HTML('&emsp;&emsp;'), "Para atropelamento de 
          pedestres, o ", shiny::tags$mark("Anel Rodoviário"), " tem 
          o trecho com maior incidência desse tipo de acidente. No 
          entanto, há um padrão curioso aqui, 5 dos 10 locais onde 
          mais se atropelam pessoas em BH estão em 5 esquinas 
          consecutivas no centro da cidade. Quem já andou pela área 
          central de Belo Horizonte já deve ter percebido que os 
          cruzamentos são complicados para os pedestres, principalmente 
          nas duas maiores avenidas,", shiny::tags$mark("Afonso Pena"), 
          " e ", shiny::tags$mark("Amazonas"), ". Os pedestres menos 
          atentos podem ficar em apuros, pois o tempo de sinal livre 
          é curto e os veículos surgem de direções não esperadas. O 
          ponto mais perigoso no centro está próximo do cruzamento da ", 
          shiny::tags$mark("avenida Afonso Pena"), " com as ruas ", 
          shiny::tags$mark("São Paulo"), " e ", 
          shiny::tags$mark("dos Tupinambás"), "com 41 atropelamentos 
          em 48 meses."),

        shiny::br(),

        shiny::div(class = 'textofmt',
          shiny::HTML('&emsp;&emsp;'), "Outras curiosidades dos dados
          estão nos acidentes registrados como queda de pessoa do
          veículo. O local com mais acidentes desse tipo está na ",
          shiny::tags$mark("avenida Professor Alfredo Balena"), 
          " na frente do hospital João XXIII e dentro da área hospitalar 
          da cidade. O segundo local com maior frequência desse tipo de acidente 
          está na ", shiny::tags$mark("rua Aurélio Lopes"), " no Barreiro, 
          curiosamente também em frente à um local de atendimento de infermos, 
          a UPA Barreiro. A ", shiny::tags$mark("avenida Amazonas"), 
          " também chama atenção nesse tipo de acidente, são 3 trechos 
          entre os 10 em que as quedas de veículo mais ocorreram. 
          Para o que foi assinalado como outros tipos de acidentes, 
          5 dos 10 locais mais frequentes para esse tipo de ocorrência 
          estão bem próximos do hospital Risoleta Neves na regional 
          Venda Nova."),

        shiny::hr(),

        #titulo: locais com mais acidentes por tipo de veículo
        shiny::h4(shiny::strong('Locais com mais acidentes por tipo de veículo'),
          id = 'mais_acidentes_por_tipo_veiculo'),


        shiny::div(class = 'textofmt',
          shiny::HTML('&emsp;&emsp;'), "No mapa abaixo estão os 10 
          locais com mais acidentes relatados envolvendo automóveis em Belo
          Horizonte. Na tabela há mais detalhes sobre esses locais.
          Aqui também há uma caixa de seleção em que você pode verificar
          os locais com mais ocorrências para outros tipos de veículos."),

        shiny::br(),

        leaflet::leafletOutput('mapa_locais_frequentes_tp_veiculos'),

        shiny::div(class = 'textofmt2',
          shiny::textOutput('texto_tpveiculos')),

        shiny::br(),

        shiny::div(
          shiny::selectizeInput(
            inputId = 'pesquisamapa_tp_veiculo', label = NULL,
            choices = c("Automoveis", "Motocicletas", "Onibus", 
                        "Caminhonetes", "Bicicletas e Patinetes", 
                        "Caminhoes", "Veiculos de tracao animal", 
                        "Outros"), selected = "Automoveis", 
            multiple = FALSE, options = list(create = FALSE))),

        shiny::div(shiny::tableOutput({'tbl_locais_frequentes_tp_veiculos'}), align = 'center'),

        shiny::div(class = 'textofmt',
          shiny::HTML('&emsp;&emsp;'), "Há alguns pontos novos
          disponíveis nas diversas possibilidades geradas pelo
          mapa e tabela acima. Trechos no ",
          shiny::tags$mark("Anel Rodoviário"), ", na ",
          shiny::tags$mark('avenida Cristiano Machado'), " e na ",
          shiny::tags$mark('rodovia Papa João Paulo II'), "(trecho chamado de ", 
          shiny::tags$mark('MG-010'), ") aparecem entre os locais 
          com mais ocorrências de acidentes com automóveis e 
          motocicletas, que são os tipos de veículos que mais se 
          envolvem em acidentes em BH. Vamos focar
          em outros trechos que não aparecem sempre, como a ", 
          shiny::tags$mark("avenida Presidente Antônio Carlos"),
          " aparecendo em décimo lugar no ranking para acidentes
          envolvendo motocicletas. Para os acidentes envolvendo
          ônibus, 5 dos 10 locais no mapa estão na área central
          de Belo Horizonte. Sendo o local com mais ocorrências
          o trecho da ",
          shiny::tags$mark("avenida Professor Alfredo Balena"), 
          " próximo ao cruzamento com a ", shiny::tags$mark("rua Paraíba"), " e ",
          shiny::tags$mark("rua Padre Rolim"), ", próximo do 
          ponto final de várias linhas do MOVE. A ", 
          shiny::tags$mark("rua Aurélio Lopes"), " no Barreiro 
          aparece como terceiro ponto com mais acidentes envolvendo
          ônibus e também é próxima da Estação Diamante que é um local 
          de intenso tráfego desses veículos. A ", 
          shiny::tags$mark("avenida Amazonas"), " teve 3 pontos
          nesse ranking, dois deles coincidindo com os trechos com
          mais queda de pessoas de veículos."),

        shiny::br(),

        shiny::div(class = 'textofmt',
          shiny::HTML('&emsp;&emsp;'), "Outros pontos de destaque
          nos dados são as ", shiny::tags$mark("avenidas Dom Pedro I e II"),
          " aparecendo entre os locais com mais ocorrências
          para acidentes envolvendo caminhonetes e 2 dos 10
          locais com mais acidentes envolvendo bicicletas ou
          patinetes, sendo bem próximos da orla da Lagoa da 
          Pampulha."),

        shiny::hr(),

        shiny::h4(shiny::strong('Locais com mais acidentes por período do dia'),
          id = 'mais_acidentes_por_periodo_do_dia'),


        shiny::div(class = 'textofmt',
          shiny::HTML('&emsp;&emsp;'), "No mapa seguinte estão 
          marcados os 10 locais com mais acidentes relatados pelas
          manhãs em Belo Horizonte. Na tabela há mais detalhes sobre 
          esses locais. Na caixa de seleção entre o mapa e a tabela 
          você pode mudar o período do dia e ver quais são os locais 
          com mais acidentes registrados ocorridos nas tardes, noites ou 
          madrugadas belo-horizontinas."),

         shiny::br(),

         leaflet::leafletOutput('mapa_locais_frequentes_periodo_dia'),

         shiny::div(class = 'textofmt2',
          shiny::textOutput('info_texto_periodo_dia')),

         shiny::br(),

         shiny::div(
           shiny::selectizeInput(
             inputId = 'pesquisamapa_periodo_dia', label = NULL,
             choices = c("Manha", "Tarde", "Noite", "Madrugada"),
             selected = "Manha", multiple = FALSE, options = list(create = FALSE))),

        shiny::div(
          shiny::tableOutput({'tbl_locais_frequentes_periodo_dia'}), align = 'center'),

        shiny::div(class = 'textofmt',
          shiny::HTML('&emsp;&emsp;'), "Os líderes do ranking 
          permanecem praticamente os mesmos locais dos outros mapas
          que já vimos, mas ainda há informações interessantes a 
          serem retiradas daqui. Por exemplo, a ", 
          shiny::tags$mark("avenida Professor Alfredo Balena"),
          " aparece entre os 10 locais com mais ocorrências nas 
          manhãs e tardes, mas não aparece nos períodos noturnos.
          O cruzamento mais famoso de BH, entre as ",
          shiny::tags$mark("avenidas Afonso Pena e Amazonas"),
          " aparece no ranking de locais com mais acidentes a
          noite e o acesso da ", 
          shiny::tags$mark("avenida Cristiano Machado"),
          " pela ", shiny::tags$mark("avenida Sebastião de Brito"),
          ", local próximo ao conhecido por ser a zona boêmia
          do bairro Dona Clara entra no ranking dos locais com
          mais acidentes nas madrugadas da capital mineira."),

        shiny::hr(),

        #titulo: locais com mais acidentes por regional
        shiny::h4(shiny::strong('Locais com mais acidentes por regional'),
          id = 'mais_acidentes_por_regional'),

        shiny::div(class = 'textofmt',
          shiny::HTML('&emsp;&emsp;'), "Abaixo você pode consultar os 
          locais campeões de acidentes relatados por regional de BH. 
          Inicialmente o mapa e a tabela destacam os acidentes na 
          regional Centro Sul, mas há uma caixa de seleção em que você 
          pode ver qualquer outra regional da cidade"),

        shiny::br(),

        leaflet::leafletOutput('mapa_locais_frequentes_regionais'),

        shiny::div(class = 'textofmt2', shiny::textOutput('texto_regional')),

        shiny::br(),

        shiny::div(class = 'textofmt',
          shiny::selectizeInput(
            inputId = 'pesquisamapa_regionais', label = NULL,
            choices = c("Barreiro", "Nordeste", "Venda Nova", "Pampulha", 
                        "Centro Sul", "Leste", "Oeste", "Norte", "Noroeste"),
            selected = "Centro Sul", multiple = FALSE,
            options = list(create = FALSE))),

        shiny::div(
          shiny::tableOutput({'tbl_locais_frequentes_regionais'}), align = 'center'),

        shiny::div(class = 'textofmt',
          shiny::HTML('&emsp;&emsp;'), "Pode se notar que as grandes 
          avenidas dominam o mapa em cada uma das regionais. Isso é
          natural, já que elas muitas vezes são a ligação entre a 
          área central e os diversos cantos da Região Metropolitana de 
          Belo Horizonte. Por exemplo, em Venda Nova, a ",
          shiny::tags$mark("avenida Vilarinho"), " que é uma das mais 
          importantes da região e é caminho para a cidade de Ribeirão 
          das Neves domina o ranking. Na regional Leste, as ", 
          shiny::tags$mark("avenidas dos Andradas e Silviano Brandão"),
          " que são fontes de ligação entre moradores dessa região e
          da cidade de Sabará ao centro, aparecem com vários trechos 
          entre os 10 mais perigosos. Na região Oeste, a ", 
          shiny::tags$mark("avenida Amazonas"), " já fora da área 
          central, novamente se destaca negativamente com vários 
          trechos no mapa. Trechos da ", 
          shiny::tags$mark("avenida Barão Homem de Melo"),
          " aparecem por duas vezes."),

        shiny::div(class = 'textofmt',
          shiny::HTML('&emsp;&emsp;'), "No Barreiro, há vários
          trechos da ", shiny::tags$mark("avenida Waldyr Soeiro Emrich"),
          " a partir da Via do Minério entre os pontos com mais 
          acidentes na regional. Já na regional Centro Sul, os 
          acidentes se concentram nos fluxos causados pelas", 
          shiny::tags$mark("avenidas Afonso Pena e Amazonas"), "."),
        
        shiny::hr(),

        shiny::div(class = 'textofmt',
          shiny::HTML('&emsp;&emsp;'), "A partir daqui, os mapas 
          vão deixar de ser interativos e serão expostos por 
          determinadas segregações para tentar identificar gargalos 
          da cidade."),

        #titulo: locais com mais acidentes por período do dia

        shiny::h4(shiny::strong('Locais com mais acidentes com vítimas fatais'),
          id = 'mais_acidentes_com_vitimas_fatais'),

        shiny::div(class = 'textofmt',
          shiny::HTML('&emsp;&emsp;'), "No mapa abaixo estão os
          10 locais com mais acidentes relatados em Belo Horizonte que
          levaram morte de pelo menos um dos envolvidos no período contido nos dados."),

        shiny::br(),

        leaflet::leafletOutput('mapa_locais_frequentes_fatalidade'),

        shiny::div(class = 'textofmt2', shiny::textOutput('info_texto_fatalidade')),

        shiny::br(),

        shiny::div(shiny::tableOutput({'tbl_locais_frequentes_fatalidade'}), align = 'center'),

        shiny::div(class = 'textofmt',
          shiny::HTML('&emsp;&emsp;'), "Felizmente as ocorrências de 
          acidentes de trânsito com vítimas fatais representam menos 
          de 1% das ocorrências, mas ainda assim há pontos de atenção 
          nesses dados. Como já dito anteriormente, o ",
          shiny::tags$mark("Anel Rodoviário"), " tem vários trechos
          problemáticos e eles podem ser vistos também aqui nos 
          acidentes com vítimas fatais. Repare também que a presença
          de trechos da ", shiny::tags$mark("avenida Cristiano Machado"),
          " diminuiu em relação aos mapas anteriores, isso nos dá 
          indício que os acidentes nessa avenida são mais brandos em 
          relação aos acidentes ocorridos no ", shiny::tags$mark("Anel"), 
          ". Note que aqui apareceu um trecho na ", 
          shiny::tags$mark("Avenida Camilo Teixeira da Costa"), ", trecho 
          mais conhecido como ", shiny::tags$mark("MG-020"), "."),
        
        shiny::hr(),

        #titulo: locais com mais acidentes em grande escala
        shiny::h4(shiny::strong('Locais com mais acidentes de grande escala'),
          id = 'mais_acidentes_com_grande_escala'),

        shiny::div(class = 'textofmt',
          shiny::HTML('&emsp;&emsp;'), "No mapa abaixo estão os 10 
          locais com mais acidentes que envolveram 4 veículos ou mais. 
          Felizmente mais uma vez, acidentes assim representam cerca 
          de 1% do total, mas repare novamente a presença dominante do ",
          shiny::tags$mark("Anel Rodoviário"), " nas primeiras posições. "),

        shiny::br(),

        leaflet::leafletOutput('mapa_locais_frequentes_grandes_acidentes'),
        
        shiny::div(shiny::textOutput('info_texto_grandes_acidentes'), class = 'textofmt2'),

        shiny::br(),

        shiny::div(shiny::tableOutput({'tbl_locais_frequentes_grandes_acidentes'}), align = 'center'),

        shiny::hr(),

        #titulo: locais com mais acidentes em grande escala
        shiny::h4(shiny::strong('Locais com mais acidentes nos fins de semanas e feriados'),
          id = 'mais_acidentes_fins_semana'),

        shiny::div(class = 'textofmt',
          shiny::HTML('&emsp;&emsp;'), "Abaixo estão os 10 locais com 
          mais ocorrêcias de acidentes em finais de semana e feriados 
          na cidade no período contido nos dados."),

        shiny::br(),

        leaflet::leafletOutput('mapa_locais_frequentes_fins_semana'),

        shiny::div(shiny::textOutput('info_texto_fins_semana'), class = 'textofmt2'),

        shiny::br(),

        shiny::div(shiny::tableOutput({'tbl_locais_frequentes_fins_semana'}), align = 'center'),

        shiny::div(class = 'textofmt',
          shiny::HTML('&emsp;&emsp;'), "É importante deixar 
          compreensível que o conceito de fim de semana usado foi o 
          período entre as 20h das sextas até o fim dos domingos. Os 
          feriados prolongados também foram considerados. De imediato, 
          podemos notar que 12008 ocorrências aconteceram em finais de 
          semana ou feriados, isso representa quase 27% do total de 
          incidentes relatados. Nos locais mais frequentes não se nota 
          grandes alterações em relação aos locais com mais ocorrências 
          (primeiro mapa dessa página), somente a saída da ",
          shiny::tags$mark("avenida Presidente Antônio Carlos"),
          "para a entrada da ", 
          shiny::tags$mark("rodovia Camilo Teixeira da Costa"), ", trecho 
          mais conhecido como ", shiny::tags$mark("MG-020"), "."),

        shiny::hr(),

        shiny::div(class = 'textofmt',
          shiny::HTML('&emsp;&emsp;'), "Na próxima aba, várias das 
          informações mostradas aqui serão exibidas de um modo 
          diferente para tentarmos extrair mais conhecimentos dos dados."),

        shiny::br(),
        shiny::br()
          #           
                    
      ) # close column
      
    ) # close fluidPage
    
  )# close tabPanel
  
  
}

pagina_distribuicao_bairros <- function(){
  
  shiny::tabPanel('Distribuição por bairros',
                  
    shiny::fluidPage(
      shiny::column(1, ''),
      shiny::column(10,
        
        shiny::div(shiny::HTML('&emsp;&emsp;'), "Nessa aba você pode 
        verificar a distribuição espacial dos acidentes por bairro e 
        regional. Na tabela abaixo serão exibidos os 5 bairros com maior 
        frequência de acidentes pelo aspecto selecionado e também há um ", 
        shiny::em("choropleth map")," que é um tipo de mapa que aqui 
        será usado para representar a quantidade relatada de acidentes 
        do tipo selecionado por bairro e regional. Quanto mais escuro 
        o polígono do bairro/regional, mais acidentes daquele tipo 
        ocorreram ali. Há a opção de considerar as frequências absolutas 
        de acidentes relatados nos bairros ou então os percentuais, 
        que também são chamados de frequências relativas. As 
        delimitações e nomes dos bairros e regionais são os usados 
        pela prefeitura de Belo Horizonte.", class = 'textofmt'),
        
        shiny::br(),
        
        shiny::div("Selecione aqui a variável pelo qual você deseja 
        discriminar os registros de acidentes:", class = 'textofmt'),
        
        shiny::div(
          shiny::selectizeInput(
            inputId = 'pesquisa_variavel', label = NULL,
            choices = c("Tipo de acidente", "Tipo de veiculo", 
                        "Periodo do dia", "Sexo do condutor", 
                        "Numero de veiculos envolvidos", 
                        "Numero de pessoas envolvidas", 
                        "Indicativo de pedestre envolvido", 
                        "Indicativo de embriaguez", 
                        "Indicativo de acidentes com vitima fatal"),
            selected = "Indicativo de pedestre envolvido", 
            multiple = FALSE, options = list(create = FALSE))
        ),
        
        shiny::div("Quando necessário, coloque aqui o valor da 
                   variável que você queira ver no mapa:", 
                   class = 'textofmt'),
        
        shiny::div(
          shiny::selectizeInput(
            inputId = 'pesquisa_valor', label = NULL,
            choices = "",
            selected = "", multiple = FALSE, options = list(create = FALSE))
        ),

        # shiny::div(shiny::checkboxInput('percentual_info', label = "Selecione aqui para ordenar os bairros por percentual."), 
        # class = 'textofmt', align = 'justify'),
        
        shiny::div(
          shiny::checkboxInput('percentual_info', 
                               label = "Ordenar tabelas e mapas por percentual.", 
                               value = FALSE), 
          style = "font-family: 'Arial Narrow';"),
        
        shiny::tabsetPanel(
          shiny::tabPanel('Bairros', 
            
            shiny::br(),
            
            shiny::div(shiny::tableOutput({'tbl_dist_bairros'}), align = 'center'),
            
            shiny::div(class = 'textofmt2',
              shiny::textOutput({'info_texto_bairro_perc'}), align = 'left'),
                             
            leaflet::leafletOutput('choropeth_mapa_bairros', height = 500),
            
            shiny::textInput(inputId = 'corte_num_acidentes', value = 45, label = 'Número de acidentes mínimos em um bairro para que ele seja considerado:', width = 600)               
                             
          ),
          shiny::tabPanel('Regionais', 
            
            shiny::br(),
            
            shiny::div(shiny::tableOutput({'tbl_dist_regionais'}), align = 'center'),
            
            shiny::div(class = 'textofmt2',
                       shiny::textOutput({'info_texto_regional_perc'}), align = 'left'),
            
            leaflet::leafletOutput('choropeth_mapa_regionais', height=500)
                             
          )
        ),
        
        shiny::br(),
        
        shiny::div(shiny::HTML('&emsp;&emsp;'), "Antes de qualquer 
        análise é preciso ponderar que para as ordenações que são 
        feitas por percentual, não se leva em consideração todos os 
        bairros da cidade. Isso porque há vários bairros muito 
        pequenos e que houveram poucos acidentes, podendo fazer com 
        que o percentual de determinada variável fique distorcido. 
        Por isso, quando a caixa de seleção", 
        shiny::em("Ordenar tabelas e mapas por percentual"), 
        " estiver marcada, só estão sendo considerados os bairros 
        em que foram registrados 45 ocorrências ou mais, isso foi 
        feito para tomarmos conclusões baseadas em conjuntos de 
        dados de tamanhos consistentes, mas se você achar esse valor 
        pequeno ou grande demais há a possibilidade de mudá-lo no 
        campo abaixo do mapa e a análise se atualizará automaticamente. 
        Para frequência absoluta no painel de bairros e para as 
        regionais, não há nenhum tipo de consideração. Destacado isso, 
        vamos analisar os dados.", class = 'textofmt'),
        
        shiny::br(),
        
        shiny::div(shiny::HTML('&emsp;&emsp;'), "Há várias informações 
        muito legais a serem extraídas desses mapas e tabelas. Podemos 
        notar nos mapas que o ", shiny::tags$mark("Centro"), " é o bairro 
        com maior frequência absoluta e relativa de registros de acidentes 
        envolvendo pedestres, quase 40% dos acidentes relatados por lá teve a participação de pelo 
        menos um pedestre e pode se notar algum 
        indício de correlação espacial, já que boa parte dos bairros ao 
        redor do", shiny::tags$mark("Centro"), " possuem percentual de registro de acidentes 
        envolvendo pedestres maior ou igual a 14%, que é a média da cidade. 
        Isso é natural, já que ali fica o centro comercial e cultural de Belo 
        Horizonte e há uma grande circulação de pedestres. Os bairros ", 
        shiny::tags$mark("Renascença"), " e ", shiny::tags$mark("Caiçara Adeilaide"),
        " se destacaram em acidentes envolvendo automóveis e colisões entre veículos, 
        mais de 85% dos acidentes nesses bairros envolveram automóveis e mais 
        de 80% foram colisões entre veículos em movimento. Os acidentes envolvendo 
        ônibus representam em torno de 10% das ocorrências de acidentes da capital, 
        mas nos bairros ", shiny::tags$mark("Diamante"), ",", shiny::tags$mark("Flávio Marques Lisboa"), 
        " e ", shiny::tags$mark("Bonsucesso"), " no Barreiro esses 
        valores percentuais foram bem maiores, no mínimo 20% das ocorrências nesses 
        bairros tiveram ônibus envolvidos. No ", shiny::tags$mark("Centro"), " também há 
        um percentual considerável de acidentes envolvendo esse tipo de 
        veículo, quase 26% do total de acidentes que ocorreram por lá.", 
        class = 'textofmt'),
        
        shiny::br(),
        
        shiny::div(shiny::HTML('&emsp;&emsp;'), "Alguns dos bairros com 
        proporcionalmente mais acidentes de bicicletas ou patinetes 
        estão associados com locais de diversão ao ar livre em Belo 
        Horizonte. No ", shiny::tags$mark("Mangabeiras"), " onde está a Praça do Papa e 
        o Parque das Mangabeiras, os relatos de acidentes envolvendo esses tipos 
        de veículos chegaram a quase 12% do total de incidentes registrados. 
        Há 2 bairros na orla da Lagoa da Pampulha nessa lista, o bairro 
        de mesmo nome e o ", shiny::tags$mark("Jardim Atlântico"), ", ambos com percentual maior 
        que 10% para registro de acidentes envolvendo bicicletas ou 
        patinetes. Para acidentes envolvendo caminhões, alguns dos 
        bairros que margeiam o", shiny::tags$mark("Anel Rodoviário"), " dominaram a lista:",
        shiny::tags$mark("Jardim São José"), ',', shiny::tags$mark("Olhos D'água"), ",",
        shiny::tags$mark("Califórnia"), " e ", shiny::tags$mark("Padre Eustáquio"), ".", 
        class = 'textofmt'),
        
        shiny::br(),
        
        shiny::div(shiny::HTML('&emsp;&emsp;'), "Para acidentes por 
        período do dia há alguns pontos curiosos. Por que alguns bairros 
        possuem quase metade dos seus registros de acidentes em determinado 
        período? Por exemplo, no ", shiny::tags$mark("Ribeiro de Abreu"), 
        " 34 das 68 ocorrências aconteceram no período da tarde, relação tão intensa como a existente entre o ",
        shiny::tags$mark("bairro da Graça"), "e as manhãs, onde 54 das 117 ocorrências desse bairro foram 
        registradas no período matutino. Nas madrugadas são relatados muito menos acidentes, o 
        percentual médio de ocorrências de acidentes nesse período na cidade gira em torno de 6%, mas nos bairros:",
        shiny::tags$mark("João Pinheiro"), " e seu vizinho ", shiny::tags$mark("Gameleira"), 
        " os acidentes nesse período representaram 14% e 12% das ocorrências por lá, respectivamente. 
        O ", shiny::tags$mark("Nova Cachoeirinha"), " e ", shiny::tags$mark("Mangabeiras"), " também 
        se destacam nos acidentes ocorridos entre meia-noite e 6h da manhã.", 
        class = 'textofmt'),
        
        shiny::br(),
        
        shiny::div(shiny::HTML('&emsp;&emsp;'), "Em sexo do condutor parece 
        ocorrer um efeito interessante. Cerca de 27% dos acidentes relatados 
        na cidade no período analisado tiveram pelo menos uma condutora do 
        sexo feminino presente. No entanto, nos bairros do ", shiny::tags$mark("Nova Floresta"), 
        ", ", shiny::tags$mark("Anchieta"), ", ", shiny::tags$mark("Cruzeiro"), ", ", 
        shiny::tags$mark("Carmo"), " e ", shiny::tags$mark("Luxemburgo"), " esse valor 
        percentual passou dos 40%. Outros bairros que não estão na lista mas 
        que também possuem um percentual alto de acidentes ocorridos com a participação 
        de pelo menos uma condutora do sexo feminino foram: ", shiny::tags$mark("Gutierrez"), ", ", 
        shiny::tags$mark("Cidade Jardim"), ", ", shiny::tags$mark("Buritis"), ", ", shiny::tags$mark("Castelo"), 
        ", ", shiny::tags$mark("Caiçara Adelaide"), ", ", shiny::tags$mark("Sion"), ", ", 
        shiny::tags$mark("Santo Antônio"), ", ", shiny::tags$mark("Graça"), ", ", shiny::tags$mark("Jaraguá"), 
        " e ", shiny::tags$mark("Sagrada Família."), "Todos esses, 
        são bairros abastados da cidade, ou seja, em bairros conhecidos 
        pelo alto poder aquisitivo, onde a locomoção das pessoas quase não é feita 
        por transporte público, podemos notar que a distribuição do 
        sexo do condutor envolvido em acidentes de trânsito relatados na capital 
        fica menos desigual, talvez porque nesses locais há mais mulheres 
        condutoras que em outros bairros da capital. Reforço mais uma vez 
        que esses dados não nos dão a possibilidade de inferir que um 
        sexo específico do condutor leva mais a acidentes que outro.", 
        class = 'textofmt'),
        
        shiny::br(),
        
        shiny::div(shiny::HTML('&emsp;&emsp;'), "Não está explicito nos dados, 
        mas é razoável supor que quando o sexo, a data de nascimento e nem informações 
        sobre os ferimentos de uma pessoa estão preenchidas, pode ser um indício de que 
        aquele indivíduo não estava ali no momento de preenchimento do boletim de ocorrência, 
        ou seja, o envolvido evadiu do local antes da chegada do agente público que cuidou da 
        ocorrência. Três bairros próximos estão entre os cinco com maior 
        percentual de não preenchimento do sexo do condutor, são eles:", 
        shiny::tags$mark("Alto Caiçaras"), ", ", shiny::tags$mark("Santo André"), " e ",
        shiny::tags$mark("Nova Cachoeirinha."), "O bairro ", 
        shiny::tags$mark("Alto Vera Cruz"), "no extremo leste da cidade também se destaca nesse quesito.", 
        class = 'textofmt'),
        
        shiny::br(),
        
        shiny::div(shiny::HTML('&emsp;&emsp;'), "Quando foi relatado 
        que algum dos envolvidos no acidente possuia sinais de embriaguez, 
        parece haver um indício de dependência espacial, 4 dos 5 bairros 
        do ranking por percentual, estão bem próximos na região Oeste da 
        cidade, são eles: ", shiny::tags$mark("Cabana do Pai Tomás"), ", ", 
        shiny::tags$mark("Vista Alegre"), ", ", shiny::tags$mark("Jardinópolis"), " e ",
        shiny::tags$mark("João Pinheiro"), ", inclusive os dois últimos possuem valores percentuais 
        consideráveis para acidentes ocorridos nas madrugadas. Já para 
        acidentes com vítimas fatais, os bairros:", shiny::tags$mark("Mangabeiras"), 
        ",", shiny::tags$mark("Betânia"), " e ", shiny::tags$mark("Camargos"), "são alguns 
        dos que se destacam com percentuais de ocorrência maiores que 3%, valores altos se 
        comparados com a média da cidade, onde em menos de 1% das ocorrências de acidentes houve 
        vítimas fatais.", class = 'textofmt'),
        
        shiny::br(),
        
        shiny::div(shiny::HTML('&emsp;&emsp;'), "Na próxima aba há muitas tabelas de contingência para 
                   tentarmos identificar mais indícios de padrões nos dados.", class = 'textofmt'),
        
        shiny::br(),
        shiny::br()
        
      )
                    
    )
                  
 )
  
}

pagina_tabelas <- function(){
  
  shiny::tabPanel('Tabelas',
                  
    shiny::fluidPage(
      shiny::column(1, ''),
      shiny::column(10,
        
        shiny::div(class = 'textofmt',
          shiny::HTML('&emsp;&emsp;'), "Aqui você vai encontrar várias 
          tabelas mostrando a distribuição das variáveis categóricas e as 
          relações bivariadas entre elas. Com toda certeza essa foi a parte 
          dessa aplicação mais massante de ser feita, mas acredito eu que pode 
          nos dar informações que ainda não vimos nos dados. 
          Nessa página há ainda uma Análise de Correspondência Múltipla das 
          informações contidas sobre os acidentes nos boletins de ocorrência. 
          Essa técnica permite verificar associação entre valores de variáveis 
          categóricas de maneira muito instintiva e de fácil visualização. 
          Nas tabelas construídas nessa aba, a menos que se diga o contrário, 
          todos os percentuais contidos nas linhas somam 100%, ficando assim para 
          o leitor comparar as distribuições pelas colunas das tabelas. 
          Aqui, ao contrário das abas com mapas, estão sendo considerados 
          todos os boletins de ocorrência contidos no conjunto de dados, 
          até aqueles que não tinham as coordenadas dos acidentes."),
        
        shiny::br(),
        
        shiny::div(class = 'textofmt',
          shiny::HTML('&emsp;&emsp;'), "Você pode notar algumas 
          inconsistências nas frequências que estão nas tabelas, mas 
          acredito eu que isso não invalide os dados, já que devemos levar em conta  
          que alguns acidentes são complexos, portanto deve ser difícil encaixá-los 
          em um formulário com campos já pré-determinados. Também sempre 
          há a possibilidade de erro humano ao preencher. Antes das tabelas, 
          há um pequeno resumo das variáveis que foram usadas."),
        
        shiny::h4(shiny::strong('Descrição das variáveis'), id = 'mais_acidentes_geral'),
        
        shiny::div(
          shiny::tags$ul(
            shiny::strong("Indício de embriaguez:"), "Essa variável foi medida ao nível de acidente, 
            ou seja, cada acidente possui um valor que pode ser: com embriaguez e sem embriaguez. 
            Quando pelo menos um dos envolvidos (condutores, passageiros ou pedestres) tinha 
            sinais de estar embriagado, então o acidente foi marcado com embriaguez."),
          
          shiny::tags$ul(  
            shiny::strong("Pedestre envolvido:"), "Essa variável também foi medida ao nível de acidente.
             Ela foi assinalada como verdadeiro quando pelo menos uma das pessoas envolvidas nos acidentes 
             era pedestre."),
          
          shiny::tags$ul(
            shiny::strong("Indício de evasão:"), 'Existe a prática de alguns condutores que 
            se envolvem em acidentes de "fugir" antes do registro do boletim de ocorrência. Isso 
            não está explícito no conjunto de dados, mas quando não foi preenchido o sexo, a 
            idade e nem o grau dos ferimentos de uma das pessoas envolvidas, foi assinalado 
            verdadeiro para o indício de evasão dessa pessoa. Essa variável é medida ao nível de 
            acidente e se pelo menos um dos envolvidos tem indício de evasão, então o acidente 
            também tem.'),
            
          shiny::tags$ul(
            shiny::strong("Indício de fatalidade:"), "Essa variável também é medida para os acidentes.
            Se no boletim de ocorrência foi assinalado que pelo menos uma das pessoas envolvidas morreu 
            em decorrência do acidente, então essa variável foi marcada como verdadeira. Note que 
            possivelmente essa variável não reflete necessariamente o total de mortes causadas nos 
            acidentes de trânsito, já que muitas pessoas podem se machucar, ir para o hospital e vir 
            a falecer alguns dias depois."),
          
          shiny::tags$ul(
            shiny::strong("Número de pessoas envolvidas:"), "Variável com a contagem de pessoas 
            envolvidas nas ocorrências. Analisando os dados, acho que provavelmente houve um viés 
            no preenchimento de uma parte dos boletins, no sentido de só relatar como envolvido os 
            condutores, os pedestres e as pessoas que se machucaram nas ocorrências."),
            
          shiny::tags$ul(
            shiny::strong("Número de veículos envolvidos:"), "Variável com a contagem de veículos 
            envolvidos nas ocorrências."),
            
          shiny::tags$ul(
            shiny::strong("Sexo do condutor:"), "Essa variável é medida ao nível de pessoa, há três 
            valores possíveis: Feminino, Masculino e Sem informação. Quando exibida nas tabelas mostra 
            a presença ou não de pelo menos um condutor de determinado sexo e quando mostrada em tabelas 
            com variáveis medidas ao nível de acidente, as linhas não serão 
            mutuamente exclusivas, ou seja, os acidentes podem ser considerados em mais de uma categoria, 
            já que em um mesmo acidente há a possibilidade do envolvimento de condutores de sexos diferentes."),
            
          shiny::tags$ul(
            shiny::strong("Faixa etária do condutor:"), "Variável medida ao nível de pessoa, os 
            valores possíveis: Até 20 anos, de 20 a 29 anos, 30 a 39 anos, 40 a 49 anos, 50 a 59 anos, 
            60 a 69 anos, mais de 70 anos e Sem informação. Quando exibida nas tabelas mostra a presença 
            ou não de pelo menos um condutor de determinada faixa etária e quando mostrada em tabelas 
            com variáveis medidas ao nível de acidente, as linhas não serão 
            mutuamente exclusivas, ou seja, os acidentes podem ser considerados em mais de uma categoria, 
            já que em um mesmo acidente há a possibilidade do envolvimento de condutores de faixas etárias diferentes."),
            
          shiny::tags$ul(
            shiny::strong("Grau de severidade dos ferimentos do condutor:"), "Variável medida ao 
            nível de pessoa, os valores possíveis: Sem ferimentos, Ferimento não fatal, Ferimento fatal e 
            Sem informação. Quando exibida nas tabelas mostra a presença ou não de pelo menos um 
            condutor de determinada faixa etária e quando mostrada em tabelas com variáveis medidas 
            ao nível de acidente, as linhas não serão mutuamente exclusivas, 
            ou seja, os acidentes podem ser considerados em mais de uma categoria, 
            já que em um mesmo acidente há a possibilidade de graus de ferimentos diferentes entre os condutores."),
            
          shiny::tags$ul(
            shiny::strong("Indício de uso do cinto de segurança pelos condutores:"), "Variável medida ao 
            nível de acidente. Se pelo menos um dos condutores envolvidos no acidente não usava cinto, 
            então foi assinalado que no acidente nem todos os condutores usavam cinto de segurança. Não 
            está explícito nos dados, mas imagino eu que o que foi considerado como cinto de segurança para 
            motocicletas, bicicletas e patinetes foi o uso ou não de capacete."),

          shiny::tags$ul(            
            shiny::strong("Período do dia:"), "Variável medida ao 
            nível de acidente. Relata o turno que os acidentes aconteceram. Apesar da ordem 
            natural dos turnos do dia serem: Madrugada, Manhã, Tarde e Noite, aqui foi considerada a 
            seguinte ordem: Manhã, Tarde, Noite e Madrugada, já que grande parte das pessoas começa 
            seu dia no período da Manhã. Além disso, Manhã foi considerado o período compreendido entre 
            6h e meio-dia, Tarde entre meio-dia e 18h, Noite entre 18h e meia-noite e Madrugada o período 
            entre meia-noite e 6h."),
            
          shiny::tags$ul(
            shiny::strong("Tipo acidente:"), "Variável medida ao nível de acidente e relata o 
            tipo de ocorrência acontecida."),
            
          shiny::tags$ul(
            shiny::strong("Descrição veículos:"), "Variável medida ao nível de acidente. Ela mostra 
            as combinações de veículos envolvidos nas ocorrências e a quantidade deles. Por exemplo, 
            em 'automóvel(1), bicicleta(1)' significa que 1 automóvel e 1 bicicleta se envolveram 
            naqueles acidentes."), 
          class = 'textofmt'),
        
        shiny::br(),

        shiny::h4(shiny::strong('Tabelas de contingência'), id = 'tabelas_contingencia'),
        
        shiny::div("Selecione aqui a variável que você quer ver a distribuição dos acidentes, geralmente 
                   essa variável estará nas colunas da tabela:", class = 'textofmt'),
        
        shiny::div(
          shiny::selectizeInput(
            inputId = 'variavel_coluna', label = NULL,
            choices = c("Indicio de embriaguez", "Pedestre envolvido", 
                        "Indicio de evasao", "Indicio de fatalidade",
                        "Numero de pessoas envolvidas", 
                        "Numero de veiculos envolvidos", "Sexo do condutor",
                        "Faixa etaria do condutor", "Grau de severidade dos ferimentos do condutor",
                        "Indicio de uso do cinto de seguranca pelos condutores", 
                        "Periodo do dia", "Tipo acidente", "Descricao veiculos"),
            selected = "Indicio de embriaguez", 
            multiple = FALSE, options = list(create = FALSE))
        ),
        
        shiny::div("Selecione aqui a variável que você deseja discriminar os acidentes, 
                   geralmente essa variável estará nas linhas da tabela:", 
                   class = 'textofmt'),
        
        shiny::div(
          shiny::selectizeInput(
            inputId = 'variavel_linha', label = NULL,
            choices = c("", "Indicio de embriaguez", "Pedestre envolvido", 
                        "Indicio de evasao", "Indicio de fatalidade",
                        "Numero de pessoas envolvidas", 
                        "Numero de veiculos envolvidos", "Sexo do condutor",
                        "Faixa etaria do condutor", "Grau de severidade dos ferimentos do condutor",
                        "Indicio de uso do cinto de seguranca pelos condutores", 
                        "Periodo do dia", "Tipo acidente", "Descricao veiculos"),
            selected = "", multiple = FALSE, options = list(create = FALSE))
        ),
    
       
       shiny::div(class = 'textofmt',
                  shiny::textOutput({'comentario_tbl_geral_contingencia'}), align = 'justify'),
       
       shiny::br(),
       
       shiny::div(shiny::tableOutput({'tbl_geral_contingencia'}), align = 'center'),
       
       shiny::div(class = 'textofmt',
                  shiny::textOutput({'comentario_tbl_bivariada_contingencia'}), align = 'justify'),
       
       shiny::br(),
       
       shiny::div(shiny::tableOutput({'tbl_bivariada_contingencia'}), align = 'center'),
       
       shiny::h4(shiny::strong('Análise de Correspondência Múltipla'), id = 'acm'),
       
       shiny::br(),
       
       shiny::div(class = 'textofmt',
          shiny::HTML('&emsp;&emsp;'), "Abaixo está a Análise de Correspondência Múltipla 
          para esse conjunto de dados. Note que a visualização não está nítida pois são muitas 
          categorias e algumas delas são tão associadas que os textos se sobreporam. Para 
          tentar melhorar isso, eu deixei os gráficos dinâmicos, ou seja, você pode desmarcar 
          alguns grupos de pontos (para isso é só clicar no grupo de variáveis que você não deseja ver na legenda do gráfico) ou 
          você pode marcar somente o grupo de variáveis que você deseja visualizar (para isso basta um duplo clique sobre o 
          grupo na legenda do gráfico). Além disso, passando o mouse por cima dos pontos, aparece um popup 
          com a legenda mostrando qual categoria e variável aquele ponto representa e também há a 
          possibilidade de dar zoom no gráfico."),
       
       shiny::br(),
       
       shiny::div(class = 'textofmt',
          shiny::HTML('&emsp;&emsp;'), "A Análise de Correspondência Múltipla é uma técnica exploratória 
          muito legal e útil para visualizar relações entre dados categóricos. Fazendo uma analogia, 
          essa técnica está para os dados categóricos assim como a Análise de Componentes Principais está para 
          os dados quantitativos. Digo isso por que na ACM existe a decomposição da variabilidade das variáveis 
          em dimensões, as dimensões são independentes entre si e cada uma delas explica 
          uma quantidade da variabilidade dos dados (também chamada de inércia). A interpretação é bastante 
          simples, quanto mais próximo dois pontos estão, mais associados eles são. Note que aqui, 
          a primeira dimensão explica 28,5% da variabilidade dos dados e a segunda explica 14,1%. Juntas, 
          essas duas dimensões explicam 42,6% da variabilidade contida nesses dados. Esse é um percentual 
          baixo, mas devemos levar em conta que há 79 dimensões nesse problema (1 dimensão para cada variável)."),
       
       shiny::br(),
       
       shiny::div(
         shiny::tabsetPanel(
           shiny::tabPanel('ACM',
              shiny::div(
                shiny::br(),
                plotly::plotlyOutput({'acm_grafico'}, width = '1000px', height = '800px'),
                align = 'center'))
           
         )
       ),
       
       shiny::br(),
       
       shiny::div(class = 'textofmt',
          shiny::HTML('&emsp;&emsp;'), "Veja que há pelo menos 3 grupos de pontos. O primeiro deles são os pontos 
          com valores na dimensão 2 maiores que 1. Note que lá há os pontos que representam a não informação, ou seja,
          acidentes onde não foi preenchido o sexo, os ferimentos, a idade e sem informação de embriaguez ou não dos 
          condutores estão associados. Isso significa que quando um aconteceu os outros aconteceram e consequentemente a 
          variável criada indicando evasão no acidente também está associada a essas categorias. Observe que o segundo 
          grupo está nos pontos com valores na dimensão 1 menor que -1. Esses pontos estão relacionados com 
          acidentes com pedestres. O único tipo de acidente relacionado aos pedestres é logicamente o atropelamento de 
          pedestres, todas as faixas etárias, ferimentos e sexos de pedestres estão nesse conjunto de pontos, mas note por 
          exemplo que os ferimentos fatais de pedestres estão mais associados com a faixa etária de 40 a 49 anos dos mesmos. 
          Veja também que a combinação de veículos mais associada aos acidentes com pedestres foram os acidentes com 1 automóvel."),
       
       shiny::br(),
       
       shiny::div(class = 'textofmt',
          shiny::HTML('&emsp;&emsp;'), "Observe também que os acidentes com fatalidade estão associados principalmente com 
          acidentes com 1 ônibus, acidentes com embriaguez de pelo menos um dos envolvidos e acidentes com 1 motocicleta. Há 
          também um grande grupo com muitos pontos que estão tão agrupados que fica difícil de visualizar, mas note que há 
          associações relevantes ali. Por exemplo, os tipos de acidentes mais associados com acidentes com 1 pessoa envolvida 
          foram: capotamento de veículo e atropelamento de animal. Veja também que as combinações de veículos: 1 automóvel e 1 
          motocicleta, 1 automóvel e 1 caminhoneta, 2 automóveis e 1 motocicleta, 1 automóvel e 1 ônibus, 2 automóveis, 1 
          motocicleta e 1 ônibus, 3 automóveis e 1 automóvel e 1 caminhonete estão muito próximos. Isso significa que acidentes 
          com essas combinações de veículos ocorreram em condições parecidas. Já os acidentes com 
          1 caminhonete e 1 motocicleta, 1 caminhão e 1 motocicleta, 1 automóvel e 1 bicicleta e outras combinações de veículos 
          estão mais próximos, ou seja, aconteceram características em comum em acidentes envolvendo essas combinações de 
          veículos."),
       
       shiny::br(),
       
       shiny::div(class = 'textofmt',
          shiny::HTML('&emsp;&emsp;'), "Explorando essa Análise de Correspondência e todas as tabelas disponíveis nessa 
          aba, acredito eu que conseguimos extrair muitas informações desse conjunto de dados. Na próxima aba há considerações 
          sobre os dados e onde você pode encontrar o código e os arquivos para reproduzir essa aplicação."),
       
       shiny::br()
        
      )  
    
    )              
 )
  
}

pagina_consideracoes <- function(){
  
  shiny::tabPanel('Considerações',
                  
    shiny::fluidPage(
      shiny::column(1, ''),
      shiny::column(10,
        
        shiny::div(class = 'textofmt',
          shiny::HTML('&emsp;&emsp;'), "Essa aplicação se propôs a analisar os 49718 boletins de ocorrência de trânsito 
          acontecidos entre 01/01/2016 e 31/12/2019 em Belo Horizonte, onde cerca de 90% deles tinha o local em que o 
          acidente aconteceu. Eu não conheço toda a cidade, então posso ter deixado relações óbvias nos dados passarem 
          desapercebidas, se você notá-las por favor me contacte. Os dados estão disponíveis no site de dados abertos da 
          PBH, são de responsabilidade da BHTrans e você pode acessá-los ", 
          shiny::tags$a(href = "https://dados.pbh.gov.br/dataset", "clicando aqui.", target = '_blank'), 
          " Nesse site há também os dados de acidentes de trânsito no período entre 2011 e 2015, mas pelo que eu entendi 
          a forma de coleta era diferente e por isso eu preferi não usá-los. Note que durante toda a análise de dados eu 
          procurei usar o termo 'acidentes relatados', isso por que há no nosso país a prática de não se registrar um 
          boletim de ocorrência quando um acidente de trânsito ocorre. 
          Então esses boletins nos dizem que aconteceram no mínimo 49718 acidentes nos 4 anos analisados em BH."),
        
        shiny::br(),
        
        shiny::div(class = 'textofmt',
          shiny::HTML('&emsp;&emsp;'), "Além disso, os boletins são registrados em sua maioria pela Polícia Militar (há também boletins 
          registrados pela Polícia Civil e pelo Corpo de Bombeiros), depois de um tempo eles são lançados no sistema e possivelmente 
          depois de mais algum tempo são disponibilizados para a BHTrans e sempre há a possibilidade de erro humano afetar esses dados 
          durante esse processo. Fazendo a análise desses dados, eu notei inconsistências nas coordenadas e nas informações passadas, 
          mas acredito que isso não inviabilize o que foi feito aqui. O repositório no github dessa aplicação com todos os conjuntos 
          de dados criados e código para reproduzir o que foi feito está ", 
          shiny::tags$a(href = "https://github.com/gustavohpgodinho/acidentes_transito_bh", "aqui.", target = '_blank'), "Eu usei o 
          site ", shiny::tags$a(href = "http://www.sinaldetransito.com.br", "sinaldetransito", target = '_blank'), " como ajuda para 
          descobrir algumas características de acidentes de trânsito e deixar os tipos de acidentes mais compreensíveis."),
        
        shiny::br(),
        
        shiny::h4(shiny::strong('Sobre mim')),
        
        shiny::br(),
        
        shiny::div(class = 'textofmt',
          shiny::HTML('&emsp;&emsp;'), "Meu nome é Gustavo Godinho e eu sou formado em ", 
          shiny::tags$a(href = "http://est.ufmg.br", "Estatística pela UFMG.", target = '_blank'), " Eu nasci e fui criado em Belo 
          Horizonte e essa cidade está entre um dos meus assuntos preferidos, mesmo não morando mais lá. Meu primeiro contato com esses dados 
          foi em Setembro de 2020 e desde lá, eu usei parte do meu tempo livre fazendo essa aplicação para tentar melhorar minha 
          capacidade de contar histórias com dados. Qualquer dúvida, crítica, elogio ou sugestão por favor me contacte pelo ", 
          shiny::tags$a(href = "https://www.linkedin.com/in/gustavo-godinho-0b90aa141", "meu linkedin", target = '_blank'), 
          " ou pelo meu email: ", shiny::tags$em("gustavohpgodinho@gmail.com"), ", eu também fiz um vídeo para introduzir essa aplicação 
          que você pode ver ", 
          shiny::tags$a(href = "https://www.youtube.com/watch?v=bauvQXktuyU&lc=UgzFhN_74CZYCObnsS54AaABAg", "clicando aqui", target = '_blank'),  
          " e por mais difícil que seja um estatístico editando vídeo, acho que ele retrata bem os problemas de mobilidade urbana de BH e 
          introduz o problema que esse site se propôs a analisar."),  
        
        shiny::br(),
        
        shiny::div(class = 'textofmt',
          shiny::HTML('&emsp;&emsp;'), "Abaixo deixo duas páginas de projetos com a minha participação. O primeiro deles é o FutScience, onde 
          buscamos dar predições para jogos de futebol usando Machine Learning. ",  
          shiny::tags$a(href = "https://futscience.com/", "Clicando aqui", target = '_blank'), " você será redirecionado para o nosso site e 
          tem mais detalhes sobre o que fazemos. Eu 
          também já fiz um ", shiny::tags$em('Shiny App'), " parecido com esse aqui para a Premier League, que é o campeonato inglês de futebol, 
          você o acessa ", shiny::tags$a(href = "https://gustavogodinho.shinyapps.io/shiny_pl/", "clicando aqui.", target = '_blank'))
    
      )              
    )
 )
  
}




