
lst_tables <- list() 

lst_tables$indicio_embriaguez_acidente$tbl_geral <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::group_by(ind_embriaguez) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_embriaguez, text)
  
}
lst_tables$indicio_pedestre_envolvido$tbl_geral <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::group_by(ind_pedestre_envolvido) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_pedestre_envolvido, text)
  
}
lst_tables$indicio_evasao$tbl_geral <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>%
    dplyr::group_by(ind_evasao) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_evasao, text)
  
}
lst_tables$indicio_fatalidade$tbl_geral <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>%
    dplyr::group_by(ind_fatalidade) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_fatalidade, text)
  
}
lst_tables$numero_pessoas_envolvidas$tbl_geral <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::group_by(num_pessoas_envolvidas) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(num_pessoas_envolvidas, text)
  
}
lst_tables$numero_veiculos_envolvidos$tbl_geral <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::group_by(num_veiculos_envolvidos) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(complete.cases(.)) %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(num_veiculos_envolvidos, text)
  
}
lst_tables$sexo_condutor$tbl_geral <- {
  
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>% 
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexomasculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexofeminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'seminformacao', sexo)) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(sexo, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::summarise(
      'sexo masculino' = sum(sexomasculino),
      'sexo feminino' = sum(sexofeminino),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(soma)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text)
  
}
lst_tables$faixa_idade_condutor$tbl_geral <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate20", "de20a29", "de30a39", "de40a49", 
                                             "de50a59", "de60a69", "de70oumais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'seminformacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>% 
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(cut_idade, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::summarise(
      'ate 20' = sum(ate20),
      'de 20 a 29' = sum(de20a29),
      'de 30 a 39' = sum(de30a39),
      'de 40 a 49' = sum(de40a49),
      'de 50 a 59' = sum(de50a59),
      'de 60 a 69' = sum(de60a69),
      'de 70 ou mais' = sum(de70oumais),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(soma)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text)
  
}
lst_tables$ferimentos_condutor$tbl_geral <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'seminformacao', desc_severidade_acidente)) %>% 
    dplyr::mutate(desc_severidade_acidente = stringr::str_remove_all(desc_severidade_acidente, '\\s+')) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(desc_severidade_acidente, aux) %>%
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>% 
    dplyr::summarise(
      fatal = sum(fatal),
      'nao fatal' = sum(naofatal),
      'sem ferimentos'= sum(semferimentos),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(soma)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text)
  
 
}
lst_tables$indicio_uso_cinto_seguranca$tbl_geral <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::group_by(ind_cinto_seguranca) %>% 
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_cinto_seguranca, text)
  
}
lst_tables$periodo_dia$tbl_geral <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::group_by(periodo) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(periodo, text)
}
lst_tables$tipo_acidente$tbl_geral <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::select(desc_acidente) %>% 
    dplyr::group_by(desc_acidente) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::arrange(desc(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    dplyr::rename('frequência (percentual)' = text) %>% 
    dplyr::rename(`tipo acidente` = desc_acidente)
  
}
lst_tables$descricao_veiculos$tbl_geral <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::select(desc_veiculos) %>% 
    dplyr::group_by(desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::arrange(desc(n)) %>% 
    dplyr::slice(1:15) %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    dplyr::rename('frequência (percentual)' = text) %>% 
    dplyr::rename(`combinacao veiculos` = desc_veiculos)
  
}

lst_tables$indicio_embriaguez_acidente$periodo_dia$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::group_by(periodo, ind_embriaguez) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_embriaguez, text)
  
}
lst_tables$indicio_pedestre_envolvido$periodo_dia$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::group_by(periodo, ind_pedestre_envolvido) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_pedestre_envolvido, text)
  
}
lst_tables$indicio_evasao$periodo_dia$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>%
    dplyr::group_by(periodo, ind_evasao) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_evasao, text)
  
}
lst_tables$indicio_fatalidade$periodo_dia$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>%
    dplyr::group_by(periodo, ind_fatalidade) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_fatalidade, text)
}
lst_tables$numero_pessoas_envolvidas$periodo_dia$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::group_by(periodo, num_pessoas_envolvidas) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(num_pessoas_envolvidas, text)
  
}
lst_tables$numero_veiculos_envolvidos$periodo_dia$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::group_by(periodo, num_veiculos_envolvidos) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>% 
    dplyr::filter(complete.cases(.)) %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(num_veiculos_envolvidos, text)
  
}
lst_tables$sexo_condutor$periodo_dia$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>% 
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexomasculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexofeminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'seminformacao', sexo)) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(sexo, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::group_by(periodo) %>%
    dplyr::summarise(
      'sexo masculino' = sum(sexomasculino),
      'sexo feminino' = sum(sexofeminino),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(soma, periodo)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text)

}
lst_tables$faixa_idade_condutor$periodo_dia$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate20", "de20a29", "de30a39", "de40a49", 
                                             "de50a59", "de60a69", "de70oumais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'seminformacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>% 
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(cut_idade, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::group_by(periodo) %>% 
    dplyr::summarise(
      'ate 20' = sum(ate20),
      'de 20 a 29' = sum(de20a29),
      'de 30 a 39' = sum(de30a39),
      'de 40 a 49' = sum(de40a49),
      'de 50 a 59' = sum(de50a59),
      'de 60 a 69' = sum(de60a69),
      'de 70 ou mais' = sum(de70oumais),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(periodo, soma)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text)

  
}
lst_tables$ferimentos_condutor$periodo_dia$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'seminformacao', desc_severidade_acidente)) %>% 
    dplyr::mutate(desc_severidade_acidente = stringr::str_remove_all(desc_severidade_acidente, '\\s+')) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(desc_severidade_acidente, aux) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>% 
    dplyr::group_by(periodo) %>%
    dplyr::summarise(
      fatal = sum(fatal),
      'nao fatal' = sum(naofatal),
      'sem ferimentos'= sum(semferimentos),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(periodo, soma)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text)
}
lst_tables$indicio_uso_cinto_seguranca$periodo_dia$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::group_by(periodo, ind_cinto_seguranca) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_cinto_seguranca, text)
  
}
lst_tables$tipo_acidente$periodo_dia$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::group_by(periodo, desc_acidente) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(periodo, text)  %>% 
    dplyr::rename(`tipo acidente` = desc_acidente)
  
}
lst_tables$descricao_veiculos$periodo_dia$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::inner_join(
      db_ocorrencias_transito_bh$ocorrencias %>% 
        dplyr::group_by(desc_veiculos) %>% 
        dplyr::summarise(n = dplyr::n()) %>% 
        dplyr::arrange(desc(n)) %>% 
        dplyr::slice(1:15),
      by = 'desc_veiculos') %>%  
    dplyr::group_by(periodo, desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(periodo, text)  %>% 
    dplyr::rename(`combinacao veiculos` = desc_veiculos)
  
}

lst_tables$indicio_embriaguez_acidente$indicio_pedestre_envolvido$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>% 
    dplyr::group_by(ind_pedestre_envolvido, ind_embriaguez) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_embriaguez, text) %>% 
    dplyr::rename(`pedestre envolvido` = ind_pedestre_envolvido)
  
}
lst_tables$indicio_evasao$indicio_pedestre_envolvido$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::group_by(ind_pedestre_envolvido, ind_evasao) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_evasao, text) %>% 
    dplyr::rename(`pedestre envolvido` = ind_pedestre_envolvido)
  
}
lst_tables$indicio_fatalidade$indicio_pedestre_envolvido$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::group_by(ind_pedestre_envolvido, ind_fatalidade) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_fatalidade, text) %>% 
    dplyr::rename(`pedestre envolvido` = ind_pedestre_envolvido)
}
lst_tables$numero_pessoas_envolvidas$indicio_pedestre_envolvido$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::group_by(ind_pedestre_envolvido, num_pessoas_envolvidas) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(num_pessoas_envolvidas, text) %>% 
    dplyr::rename(`pedestre envolvido` = ind_pedestre_envolvido)
  
}
lst_tables$numero_veiculos_envolvidos$indicio_pedestre_envolvido$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::group_by(ind_pedestre_envolvido, num_veiculos_envolvidos) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(complete.cases(.)) %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(num_veiculos_envolvidos, text) %>% 
    dplyr::rename(`pedestre envolvido` = ind_pedestre_envolvido)
  
}
lst_tables$sexo_condutor$indicio_pedestre_envolvido$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>% 
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexomasculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexofeminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'seminformacao', sexo)) %>%
    dplyr::mutate(aux = 1) %>%
    tidyr::spread(sexo, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::group_by(ind_pedestre_envolvido) %>%
    dplyr::summarise(
      'sexo masculino' = sum(sexomasculino),
      'sexo feminino' = sum(sexofeminino),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(soma, ind_pedestre_envolvido)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text)

}
lst_tables$faixa_idade_condutor$indicio_pedestre_envolvido$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate20", "de20a29", "de30a39", "de40a49", 
                                             "de50a59", "de60a69", "de70oumais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'seminformacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>% 
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(cut_idade, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::group_by(ind_pedestre_envolvido) %>% 
    dplyr::summarise(
      'ate 20' = sum(ate20),
      'de 20 a 29' = sum(de20a29),
      'de 30 a 39' = sum(de30a39),
      'de 40 a 49' = sum(de40a49),
      'de 50 a 59' = sum(de50a59),
      'de 60 a 69' = sum(de60a69),
      'de 70 ou mais' = sum(de70oumais),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(ind_pedestre_envolvido, soma)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text) %>% 
    dplyr::rename(`pedestre envolvido` = ind_pedestre_envolvido)
  
  
}
lst_tables$ferimentos_condutor$indicio_pedestre_envolvido$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'seminformacao', desc_severidade_acidente)) %>% 
    dplyr::mutate(desc_severidade_acidente = stringr::str_remove_all(desc_severidade_acidente, '\\s+')) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(desc_severidade_acidente, aux) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>% 
    dplyr::group_by(ind_pedestre_envolvido) %>%
    dplyr::summarise(
      fatal = sum(fatal),
      'nao fatal' = sum(naofatal),
      'sem ferimentos'= sum(semferimentos),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(ind_pedestre_envolvido, soma)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text) %>% 
    dplyr::rename(`pedestre envolvido` = ind_pedestre_envolvido)
  
}
lst_tables$indicio_uso_cinto_seguranca$indicio_pedestre_envolvido$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::group_by(ind_pedestre_envolvido, ind_cinto_seguranca) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_cinto_seguranca, text) %>% 
    dplyr::rename(`pedestre envolvido` = ind_pedestre_envolvido)
  
}
lst_tables$periodo_dia$indicio_pedestre_envolvido$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::select(periodo, ind_pedestre_envolvido) %>%
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::group_by(ind_pedestre_envolvido,periodo) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(periodo, text) %>% 
    dplyr::rename(`pedestre envolvido` = ind_pedestre_envolvido)
  
}
lst_tables$tipo_acidente$indicio_pedestre_envolvido$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::group_by(ind_pedestre_envolvido, desc_acidente) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_pedestre_envolvido, text) %>% 
    dplyr::mutate_if(.predicate = is.character, 
                     .funs = function(x){ifelse(is.na(x), '0 (0%)', x)}) %>% 
    dplyr::rename(`tipo acidente` = desc_acidente)
  
  
}
lst_tables$descricao_veiculos$indicio_pedestre_envolvido$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::group_by(desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    dplyr::slice(1:15) %>% 
    dplyr::select(desc_veiculos) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = c('desc_veiculos')) %>% 
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::group_by(ind_pedestre_envolvido, desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_pedestre_envolvido, text) %>% 
    dplyr::rename(`combinacao veiculos` = desc_veiculos)
  
}

lst_tables$indicio_embriaguez_acidente$indicio_evasao$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::group_by(ind_evasao, ind_embriaguez) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_embriaguez, text) %>% 
    dplyr::rename(`indicio evasao` = ind_evasao)
  
}
lst_tables$indicio_pedestre_envolvido$indicio_evasao$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>%
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>% 
    dplyr::group_by(ind_evasao, ind_pedestre_envolvido) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_pedestre_envolvido, text) %>% 
    dplyr::rename(`indicio evasao` = ind_evasao)
  
}
lst_tables$indicio_fatalidade$indicio_evasao$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::group_by(ind_evasao, ind_fatalidade) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_fatalidade, text) %>% 
    dplyr::rename(`indicio evasao` = ind_evasao)
  
}
lst_tables$numero_pessoas_envolvidas$indicio_evasao$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::group_by(ind_evasao, num_pessoas_envolvidas) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(num_pessoas_envolvidas, text) %>%
    dplyr::mutate_all(.funs = function(x){ifelse(is.na(x), '0 (0%)', x)}) %>% 
    dplyr::rename(`indicio evasao` = ind_evasao)
  
}
lst_tables$numero_veiculos_envolvidos$indicio_evasao$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::group_by(ind_evasao, num_veiculos_envolvidos) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(complete.cases(.)) %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(num_veiculos_envolvidos, text) %>% 
    dplyr::rename(`indicio evasao` = ind_evasao)
  
}
lst_tables$sexo_condutor$indicio_evasao$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>% 
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexomasculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexofeminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'seminformacao', sexo)) %>%
    dplyr::mutate(aux = 1) %>%
    tidyr::spread(sexo, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::group_by(ind_evasao) %>%
    dplyr::summarise(
      'sexo masculino' = sum(sexomasculino),
      'sexo feminino' = sum(sexofeminino),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(soma, ind_evasao)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text) %>% 
    dplyr::rename(`indicio evasao` = ind_evasao)

}
lst_tables$faixa_idade_condutor$indicio_evasao$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate20", "de20a29", "de30a39", "de40a49", 
                                             "de50a59", "de60a69", "de70oumais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'seminformacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>% 
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(cut_idade, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::group_by(ind_evasao) %>% 
    dplyr::summarise(
      'ate 20' = sum(ate20),
      'de 20 a 29' = sum(de20a29),
      'de 30 a 39' = sum(de30a39),
      'de 40 a 49' = sum(de40a49),
      'de 50 a 59' = sum(de50a59),
      'de 60 a 69' = sum(de60a69),
      'de 70 ou mais' = sum(de70oumais),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(ind_evasao, soma)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text) %>% 
    dplyr::rename(`indicio evasao` = ind_evasao)
  
}
lst_tables$ferimentos_condutor$indicio_evasao$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'seminformacao', desc_severidade_acidente)) %>% 
    dplyr::mutate(desc_severidade_acidente = stringr::str_remove_all(desc_severidade_acidente, '\\s+')) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(desc_severidade_acidente, aux) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>% 
    dplyr::group_by(ind_evasao) %>%
    dplyr::summarise(
      fatal = sum(fatal),
      'nao fatal' = sum(naofatal),
      'sem ferimentos'= sum(semferimentos),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(ind_evasao, soma)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text) %>% 
    dplyr::rename(`indicio evasao` = ind_evasao)
  
}
lst_tables$indicio_uso_cinto_seguranca$indicio_evasao$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::group_by(ind_evasao, ind_cinto_seguranca) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_cinto_seguranca, text) %>% 
    dplyr::rename(`indicio evasao` = ind_evasao)
  
}
lst_tables$periodo_dia$indicio_evasao$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::select(periodo, ind_evasao) %>%
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>%
    dplyr::group_by(ind_evasao, periodo) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(periodo, text) %>% 
    dplyr::rename(`indicio evasao` = ind_evasao)
  
}
lst_tables$tipo_acidente$indicio_evasao$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::group_by(ind_evasao, desc_acidente) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_evasao, text)  %>% 
    dplyr::rename(`tipo acidente` = desc_acidente)
  
}
lst_tables$descricao_veiculos$indicio_evasao$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::group_by(desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    dplyr::slice(1:15) %>% 
    dplyr::select(desc_veiculos) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = c('desc_veiculos')) %>% 
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::group_by(ind_evasao, desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_evasao, text) %>% 
    dplyr::rename(`combinacao veiculos` = desc_veiculos)
  
}

lst_tables$indicio_embriaguez_acidente$indicio_fatalidade$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::group_by(ind_fatalidade, ind_embriaguez) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_embriaguez, text) %>% 
    dplyr::rename(`fatalidade` = ind_fatalidade)
}
lst_tables$indicio_pedestre_envolvido$indicio_fatalidade$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>% 
    dplyr::group_by(ind_fatalidade, ind_pedestre_envolvido) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_pedestre_envolvido, text) %>% 
    dplyr::rename(fatalidade = ind_fatalidade)
  
}
lst_tables$indicio_evasao$indicio_fatalidade$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::group_by(ind_fatalidade, ind_evasao) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_evasao, text) %>% 
    dplyr::rename(fatalidade = ind_fatalidade)
  
}
lst_tables$numero_pessoas_envolvidas$indicio_fatalidade$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::group_by(ind_fatalidade, num_pessoas_envolvidas) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(num_pessoas_envolvidas, text) %>% 
    dplyr::rename(fatalidade = ind_fatalidade)
  
}
lst_tables$numero_veiculos_envolvidos$indicio_fatalidade$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::group_by(ind_fatalidade, num_veiculos_envolvidos) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(complete.cases(.)) %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(num_veiculos_envolvidos, text) %>% 
    dplyr::rename(fatalidade = ind_fatalidade)
  
}
lst_tables$sexo_condutor$indicio_fatalidade$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>% 
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexomasculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexofeminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'seminformacao', sexo)) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(sexo, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::group_by(ind_fatalidade) %>%
    dplyr::summarise(
      'sexo masculino' = sum(sexomasculino),
      'sexo feminino' = sum(sexofeminino),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(soma, ind_fatalidade)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text) %>% 
    dplyr::rename(fatalidade = ind_fatalidade)
  
}
lst_tables$faixa_idade_condutor$indicio_fatalidade$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate20", "de20a29", "de30a39", "de40a49", 
                                             "de50a59", "de60a69", "de70oumais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'seminformacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>% 
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(cut_idade, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::group_by(ind_fatalidade) %>% 
    dplyr::summarise(
      'ate 20' = sum(ate20),
      'de 20 a 29' = sum(de20a29),
      'de 30 a 39' = sum(de30a39),
      'de 40 a 49' = sum(de40a49),
      'de 50 a 59' = sum(de50a59),
      'de 60 a 69' = sum(de60a69),
      'de 70 ou mais' = sum(de70oumais),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(ind_fatalidade, soma)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text) %>% 
    dplyr::rename(fatalidade = ind_fatalidade)
  
}
lst_tables$ferimentos_condutor$indicio_fatalidade$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'seminformacao', desc_severidade_acidente)) %>% 
    dplyr::mutate(desc_severidade_acidente = stringr::str_remove_all(desc_severidade_acidente, '\\s+')) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(desc_severidade_acidente, aux) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>% 
    dplyr::group_by(ind_fatalidade) %>%
    dplyr::summarise(
      fatal = sum(fatal),
      'nao fatal' = sum(naofatal),
      'sem ferimentos'= sum(semferimentos),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(ind_fatalidade, soma)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text) %>% 
    dplyr::rename(fatalidade = ind_fatalidade)
  
}
lst_tables$indicio_uso_cinto_seguranca$indicio_fatalidade$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::group_by(ind_fatalidade, ind_cinto_seguranca) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_cinto_seguranca, text) %>% 
    dplyr::rename(fatalidade = ind_fatalidade)
  
}
lst_tables$periodo_dia$indicio_fatalidade$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::select(periodo, ind_fatalidade) %>%
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>%
    dplyr::group_by(ind_fatalidade, periodo) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(periodo, text) %>% 
    dplyr::rename(fatalidade = ind_fatalidade)
}
lst_tables$tipo_acidente$indicio_fatalidade$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::group_by(ind_fatalidade, desc_acidente) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_fatalidade, text) %>% 
    dplyr::mutate_if(.predicate = is.character, 
                     .funs = function(x){ifelse(is.na(x), '0 (0%)', x)}) %>% 
    dplyr::rename(`tipo acidente` = desc_acidente)
  
}
lst_tables$descricao_veiculos$indicio_fatalidade$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::group_by(desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    dplyr::slice(1:15) %>% 
    dplyr::select(desc_veiculos) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = c('desc_veiculos')) %>% 
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::group_by(ind_fatalidade, desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_fatalidade, text) %>% 
    dplyr::rename(`combinacao veiculos` = desc_veiculos)
  
}

lst_tables$indicio_embriaguez_acidente$numero_pessoas_envolvidas$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::group_by(num_pessoas_envolvidas, ind_embriaguez) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_embriaguez, text) %>% 
    dplyr::rename(`pessoas envolvidas` = num_pessoas_envolvidas)
  
}
lst_tables$indicio_pedestre_envolvido$numero_pessoas_envolvidas$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::group_by(num_pessoas_envolvidas, ind_pedestre_envolvido) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_pedestre_envolvido, text) %>% 
    dplyr::rename(`pessoas envolvidas` = num_pessoas_envolvidas)
  
}
lst_tables$indicio_evasao$numero_pessoas_envolvidas$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::group_by(num_pessoas_envolvidas, ind_evasao) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_evasao, text) %>%
    dplyr::mutate_all(.funs = function(x){ifelse(is.na(x), '0 (0%)', x)}) %>% 
    dplyr::rename(`pessoas envolvidas` = num_pessoas_envolvidas)
  
}
lst_tables$indicio_fatalidade$numero_pessoas_envolvidas$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::group_by(num_pessoas_envolvidas,ind_fatalidade) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_fatalidade, text) %>% 
    dplyr::rename(`pessoas envolvidas` = num_pessoas_envolvidas)
  
}
lst_tables$numero_veiculos_envolvidos$numero_pessoas_envolvidas$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::group_by(num_pessoas_envolvidas, num_veiculos_envolvidos) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(complete.cases(.)) %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(num_veiculos_envolvidos, text) %>% 
    dplyr::rename(`pessoas envolvidas` = num_pessoas_envolvidas)
  
}
lst_tables$sexo_condutor$numero_pessoas_envolvidas$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>% 
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexomasculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexofeminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'seminformacao', sexo)) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(sexo, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::group_by(num_pessoas_envolvidas) %>%
    dplyr::summarise(
      'sexo masculino' = sum(sexomasculino),
      'sexo feminino' = sum(sexofeminino),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(soma, num_pessoas_envolvidas)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text) %>% 
    dplyr::rename(`pessoas envolvidas` = num_pessoas_envolvidas)

}
lst_tables$faixa_idade_condutor$numero_pessoas_envolvidas$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate20", "de20a29", "de30a39", "de40a49", 
                                             "de50a59", "de60a69", "de70oumais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'seminformacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>% 
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(cut_idade, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::group_by(num_pessoas_envolvidas) %>% 
    dplyr::summarise(
      'ate 20' = sum(ate20),
      'de 20 a 29' = sum(de20a29),
      'de 30 a 39' = sum(de30a39),
      'de 40 a 49' = sum(de40a49),
      'de 50 a 59' = sum(de50a59),
      'de 60 a 69' = sum(de60a69),
      'de 70 ou mais' = sum(de70oumais),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(num_pessoas_envolvidas, soma)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text) %>% 
    dplyr::rename(`pessoas envolvidas` = num_pessoas_envolvidas)
  
}
lst_tables$ferimentos_condutor$numero_pessoas_envolvidas$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'seminformacao', desc_severidade_acidente)) %>% 
    dplyr::mutate(desc_severidade_acidente = stringr::str_remove_all(desc_severidade_acidente, '\\s+')) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(desc_severidade_acidente, aux) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%    
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>% 
    dplyr::group_by(num_pessoas_envolvidas) %>%
    dplyr::summarise(
      fatal = sum(fatal),
      'nao fatal' = sum(naofatal),
      'sem ferimentos'= sum(semferimentos),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(num_pessoas_envolvidas, soma)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text) %>% 
    dplyr::rename(`pessoas envolvidas` = num_pessoas_envolvidas)
  
}
lst_tables$indicio_uso_cinto_seguranca$numero_pessoas_envolvidas$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%    
    dplyr::group_by(num_pessoas_envolvidas, ind_cinto_seguranca) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_cinto_seguranca, text) %>% 
    dplyr::rename(`pessoas envolvidas` = num_pessoas_envolvidas)
  
}
lst_tables$periodo_dia$numero_pessoas_envolvidas$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::group_by(num_pessoas_envolvidas, periodo) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(periodo, text) %>% 
    dplyr::rename(`pessoas envolvidas` = num_pessoas_envolvidas)
}
lst_tables$tipo_acidente$numero_pessoas_envolvidas$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::group_by(num_pessoas_envolvidas, desc_acidente) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(num_pessoas_envolvidas, text) %>% 
    dplyr::mutate_if(.predicate = is.character, 
                     .funs = function(x){ifelse(is.na(x), '0 (0%)', x)}) %>% 
    dplyr::rename(`tipo acidente` = desc_acidente)
  
}
lst_tables$descricao_veiculos$numero_pessoas_envolvidas$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::group_by(desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    dplyr::slice(1:15) %>% 
    dplyr::select(desc_veiculos) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = c('desc_veiculos')) %>% 
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>% 
    dplyr::group_by(num_pessoas_envolvidas, desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(num_pessoas_envolvidas, text) %>% 
    dplyr::mutate_if(.predicate = is.character, 
                     .funs = function(x){ifelse(is.na(x), '0 (0%)', x)}) %>% 
    dplyr::rename(`combinacao veiculos` = desc_veiculos)
  
}

lst_tables$indicio_embriaguez_acidente$numero_veiculos_envolvidos$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::group_by(num_veiculos_envolvidos, ind_embriaguez) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(complete.cases(.)) %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_embriaguez, text) %>% 
    dplyr::rename(`veículos envolvidos` = num_veiculos_envolvidos)
  
}
lst_tables$indicio_pedestre_envolvido$numero_veiculos_envolvidos$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::group_by(num_veiculos_envolvidos, ind_pedestre_envolvido) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(complete.cases(.)) %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_pedestre_envolvido, text) %>% 
    dplyr::rename(`veículos envolvidos` = num_veiculos_envolvidos)
  
}
lst_tables$indicio_evasao$numero_veiculos_envolvidos$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::group_by(num_veiculos_envolvidos, ind_evasao) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(complete.cases(.)) %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_evasao, text) %>%
    dplyr::mutate_all(.funs = function(x){ifelse(is.na(x), '0 (0%)', x)}) %>% 
    dplyr::rename(`veículos envolvidos` = num_veiculos_envolvidos)
  
}
lst_tables$indicio_fatalidade$numero_veiculos_envolvidos$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::group_by(num_veiculos_envolvidos, ind_fatalidade) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(complete.cases(.)) %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_fatalidade, text) %>% 
    dplyr::rename(`veículos envolvidos` = num_veiculos_envolvidos)
  
}
lst_tables$numero_pessoas_envolvidas$numero_veiculos_envolvidos$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::group_by(num_veiculos_envolvidos, num_pessoas_envolvidas) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(complete.cases(.)) %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(num_pessoas_envolvidas, text) %>% 
    dplyr::rename(`veículos envolvidos` = num_veiculos_envolvidos)
  
}
lst_tables$sexo_condutor$numero_veiculos_envolvidos$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>% 
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexomasculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexofeminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'seminformacao', sexo)) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(sexo, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::group_by(num_veiculos_envolvidos) %>%
    dplyr::summarise(
      'sexo masculino' = sum(sexomasculino),
      'sexo feminino' = sum(sexofeminino),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    dplyr::filter(complete.cases(.)) %>% 
    tidyr::gather(key, n, -c(soma, num_veiculos_envolvidos)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text) %>% 
    dplyr::rename(`veículos envolvidos` = num_veiculos_envolvidos)

}
lst_tables$faixa_idade_condutor$numero_veiculos_envolvidos$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate20", "de20a29", "de30a39", "de40a49", 
                                             "de50a59", "de60a69", "de70oumais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'seminformacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>% 
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(cut_idade, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::group_by(num_veiculos_envolvidos) %>% 
    dplyr::summarise(
      'ate 20' = sum(ate20),
      'de 20 a 29' = sum(de20a29),
      'de 30 a 39' = sum(de30a39),
      'de 40 a 49' = sum(de40a49),
      'de 50 a 59' = sum(de50a59),
      'de 60 a 69' = sum(de60a69),
      'de 70 ou mais' = sum(de70oumais),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    dplyr::filter(complete.cases(.)) %>% 
    tidyr::gather(key, n, -c(num_veiculos_envolvidos, soma)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text) %>% 
    dplyr::rename(`veículos envolvidos` = num_veiculos_envolvidos)
  
}
lst_tables$ferimentos_condutor$numero_veiculos_envolvidos$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'seminformacao', desc_severidade_acidente)) %>% 
    dplyr::mutate(desc_severidade_acidente = stringr::str_remove_all(desc_severidade_acidente, '\\s+')) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(desc_severidade_acidente, aux) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%    
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>% 
    dplyr::group_by(num_veiculos_envolvidos) %>%
    dplyr::summarise(
      fatal = sum(fatal),
      'nao fatal' = sum(naofatal),
      'sem ferimentos'= sum(semferimentos),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    dplyr::filter(complete.cases(.)) %>% 
    tidyr::gather(key, n, -c(num_veiculos_envolvidos, soma)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text) %>% 
    dplyr::rename(`veículos envolvidos` = num_veiculos_envolvidos)
  
}
lst_tables$indicio_uso_cinto_seguranca$numero_veiculos_envolvidos$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%   
    dplyr::group_by(num_veiculos_envolvidos, ind_cinto_seguranca) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(complete.cases(.)) %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_cinto_seguranca, text) %>% 
    dplyr::rename(`veículos envolvidos` = num_veiculos_envolvidos)
  
}
lst_tables$periodo_dia$numero_veiculos_envolvidos$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::group_by(num_veiculos_envolvidos, periodo) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(complete.cases(.)) %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(periodo, text) %>% 
    dplyr::rename(`veículos envolvidos` = num_veiculos_envolvidos)
}
lst_tables$tipo_acidente$numero_veiculos_envolvidos$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::group_by(num_veiculos_envolvidos, desc_acidente) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::filter(complete.cases(.)) %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(num_veiculos_envolvidos, text) %>% 
    dplyr::mutate_if(.predicate = is.character, 
                     .funs = function(x){ifelse(is.na(x), '0 (0%)', x)}) %>% 
    dplyr::rename(`tipo acidente` = desc_acidente)
  
}
lst_tables$descricao_veiculos$numero_veiculos_envolvidos$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::group_by(desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    dplyr::slice(1:15) %>% 
    dplyr::select(desc_veiculos) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = c('desc_veiculos')) %>% 
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>% 
    dplyr::group_by(num_veiculos_envolvidos, desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(num_veiculos_envolvidos, text) %>% 
    dplyr::mutate_if(.predicate = is.character, 
                     .funs = function(x){ifelse(is.na(x), '0 (0%)', x)}) %>% 
    dplyr::rename(`combinacao veiculos` = desc_veiculos)
  
}

lst_tables$indicio_embriaguez_acidente$sexo_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexo masculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexo feminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'sem informacao', sexo)) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::group_by(sexo, ind_embriaguez) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_embriaguez, text) %>% 
    dplyr::rename(`sexo condutor` = sexo)
  
}
lst_tables$indicio_pedestre_envolvido$sexo_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexo masculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexo feminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'sem informacao', sexo)) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::group_by(sexo, ind_pedestre_envolvido) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_pedestre_envolvido, text) %>% 
    dplyr::rename(`sexo condutor` = sexo)
  
}
lst_tables$indicio_evasao$sexo_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexo masculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexo feminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'sem informacao', sexo)) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::group_by(sexo, ind_evasao) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_evasao, text) %>% 
    dplyr::rename(`sexo condutor` = sexo)
  
}
lst_tables$indicio_fatalidade$sexo_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexo masculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexo feminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'sem informacao', sexo)) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::group_by(sexo, ind_fatalidade) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_fatalidade, text) %>% 
    dplyr::rename(`sexo condutor` = sexo)
  
}
lst_tables$numero_pessoas_envolvidas$sexo_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexo masculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexo feminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'sem informacao', sexo)) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::group_by(sexo, num_pessoas_envolvidas) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(num_pessoas_envolvidas, text) %>% 
    dplyr::rename(`sexo condutor` = sexo)
  
}
lst_tables$numero_veiculos_envolvidos$sexo_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexo masculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexo feminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'sem informacao', sexo)) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::group_by(sexo, num_veiculos_envolvidos) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(complete.cases(.)) %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(num_veiculos_envolvidos, text) %>% 
    dplyr::rename(`sexo condutor` = sexo)

}
lst_tables$faixa_idade_condutor$sexo_condutor$tbl_formato <- {

  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, idade) %>% 
    dplyr::mutate(aux = 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate20", "de20a29", "de30a39", "de40a49", 
                                             "de50a59", "de60a69", "de70oumais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'sem informacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade, aux) %>% 
    tidyr::spread(cut_idade, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(
      db_ocorrencias_transito_bh$pessoas %>% 
        dplyr::filter(ind_condutor == 1) %>% 
        dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexo masculino', sexo)) %>%
        dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexo feminino', sexo)) %>%
        dplyr::mutate(sexo = ifelse(is.na(sexo), 'sem informacao', sexo)) %>%
        dplyr::distinct(num_boletim, sexo),
      by = 'num_boletim') %>% 
    dplyr::group_by(sexo) %>%
    dplyr::summarise(
      'ate 20' = sum(ate20), 
      'de 20 a 29' = sum(de20a29), 
      'de 30 a 39' = sum(de30a39), 
      'de 40 a 49' = sum(de40a49),
      'de 50 a 59' = sum(de50a59), 
      'de 60 a 69' = sum(de60a69), 
      'de 70 ou mais' = sum(de70oumais),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(soma, sexo)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text) %>% 
    dplyr::rename(`sexo condutor` = sexo)
  
}
lst_tables$ferimentos_condutor$sexo_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(aux = 1) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>% 
    dplyr::mutate(desc_severidade_acidente = stringr::str_remove_all(desc_severidade_acidente, '\\s+')) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente, aux) %>% 
    tidyr::spread(desc_severidade_acidente, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(
      db_ocorrencias_transito_bh$pessoas %>% 
        dplyr::filter(ind_condutor == 1) %>% 
        dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexo masculino', sexo)) %>%
        dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexo feminino', sexo)) %>%
        dplyr::mutate(sexo = ifelse(is.na(sexo), 'sem informacao', sexo)) %>%
        dplyr::distinct(num_boletim, sexo),
      by = 'num_boletim') %>% 
    dplyr::group_by(sexo) %>%
    dplyr::summarise(
      'fatal' = sum(fatal), 
      'nao fatal' = sum(naofatal), 
      'sem ferimentos' = sum(semferimentos), 
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(soma, sexo)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text) %>% 
    dplyr::rename(`sexo condutor` = sexo)
  
}
lst_tables$indicio_uso_cinto_seguranca$sexo_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexo masculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexo feminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'sem informacao', sexo)) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::inner_join(
      db_ocorrencias_transito_bh$pessoas %>% 
        dplyr::filter(ind_condutor == 1) %>% 
        dplyr::group_by(num_boletim) %>% 
        dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
        dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
        dplyr::ungroup(), 
      by = 'num_boletim') %>% 
    dplyr::group_by(sexo, ind_cinto_seguranca) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_cinto_seguranca, text) %>% 
    dplyr::rename(`sexo condutor` = sexo)
  
}
lst_tables$periodo_dia$sexo_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::select(num_boletim, sexo) %>% 
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexo masculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexo feminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'sem informacao', sexo)) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::select(periodo, sexo) %>% 
    dplyr::mutate(periodo = factor(periodo, levels = c('manha', 'tarde', 'noite', 'madrugada'))) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::group_by(sexo, periodo) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(periodo, text) %>% 
    dplyr::rename(`sexo condutor` = sexo)
  
}
lst_tables$tipo_acidente$sexo_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexo masculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexo feminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'sem informacao', sexo)) %>%
    dplyr::group_by(sexo, desc_acidente) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(sexo, text) %>% 
    dplyr::rename(`tipo acidente` = desc_acidente)
  
}
lst_tables$descricao_veiculos$sexo_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::inner_join(
      db_ocorrencias_transito_bh$ocorrencias %>% 
        dplyr::group_by(desc_veiculos) %>% 
        dplyr::summarise(n = dplyr::n()) %>% 
        dplyr::arrange(desc(n)) %>% 
        dplyr::slice(1:15),
      by = 'desc_veiculos') %>% 
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexo masculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexo feminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'sem informacao', sexo)) %>%
    dplyr::group_by(sexo, desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(sexo, text) %>% 
    dplyr::rename(`combinacao veiculos` = desc_veiculos)
  
}

lst_tables$indicio_embriaguez_acidente$faixa_idade_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate 20", "de 20 a 29", "de 30 a 39", "de 40 a 49", 
                                             "de 50 a 59", "de 60 a 69", "de 70 ou mais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'sem informacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::group_by(cut_idade, ind_embriaguez) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_embriaguez, text) %>% 
    dplyr::rename(`faixa etaria` = cut_idade)
  
}
lst_tables$indicio_pedestre_envolvido$faixa_idade_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate 20", "de 20 a 29", "de 30 a 39", "de 40 a 49", 
                                             "de 50 a 59", "de 60 a 69", "de 70 ou mais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'sem informacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::group_by(cut_idade, ind_pedestre_envolvido) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_pedestre_envolvido, text) %>% 
    dplyr::rename(`faixa etaria` = cut_idade)
  
}
lst_tables$indicio_evasao$faixa_idade_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate 20", "de 20 a 29", "de 30 a 39", "de 40 a 49", 
                                             "de 50 a 59", "de 60 a 69", "de 70 ou mais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'sem informacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>%
    dplyr::group_by(cut_idade, ind_evasao) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_evasao, text) %>% 
    dplyr::rename(`faixa etaria` = cut_idade)
  
}
lst_tables$indicio_fatalidade$faixa_idade_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate 20", "de 20 a 29", "de 30 a 39", "de 40 a 49", 
                                             "de 50 a 59", "de 60 a 69", "de 70 ou mais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'sem informacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::group_by(cut_idade, ind_fatalidade) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_fatalidade, text) %>% 
    dplyr::rename(`faixa etaria` = cut_idade)
  
}
lst_tables$numero_pessoas_envolvidas$faixa_idade_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate 20", "de 20 a 29", "de 30 a 39", "de 40 a 49", 
                                             "de 50 a 59", "de 60 a 69", "de 70 ou mais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'sem informacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::group_by(cut_idade, num_pessoas_envolvidas) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(num_pessoas_envolvidas, text) %>% 
    dplyr::rename(`faixa etaria` = cut_idade)
  
}
lst_tables$numero_veiculos_envolvidos$faixa_idade_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate 20", "de 20 a 29", "de 30 a 39", "de 40 a 49", 
                                             "de 50 a 59", "de 60 a 69", "de 70 ou mais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'sem informacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::group_by(cut_idade, num_veiculos_envolvidos) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::filter(complete.cases(.)) %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(num_veiculos_envolvidos, text) %>% 
    dplyr::rename(`faixa etaria` = cut_idade)
  
}
lst_tables$sexo_condutor$faixa_idade_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>% 
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexomasculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexofeminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'seminformacao', sexo)) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(sexo, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(
      db_ocorrencias_transito_bh$pessoas %>% 
        dplyr::filter(ind_condutor == 1) %>% 
        dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                      labels = c("ate 20", "de 20 a 29", "de 30 a 39", "de 40 a 49", 
                                                 "de 50 a 59", "de 60 a 69", "de 70 ou mais"))) %>% 
        dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'sem informacao', as.character(cut_idade))) %>% 
        dplyr::distinct(num_boletim, cut_idade),
      by = 'num_boletim') %>% 
    dplyr::group_by(cut_idade) %>%
    dplyr::summarise(
      'sexo masculino' = sum(sexomasculino),
      'sexo feminino' = sum(sexofeminino),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(soma, cut_idade)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text) %>% 
    dplyr::rename(`faixa etaria` = cut_idade)
  
}
lst_tables$ferimentos_condutor$faixa_idade_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>% 
    dplyr::mutate(desc_severidade_acidente = stringr::str_remove_all(desc_severidade_acidente, '\\s+')) %>% 
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(desc_severidade_acidente, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(
      db_ocorrencias_transito_bh$pessoas %>% 
        dplyr::filter(ind_condutor == 1) %>% 
        dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                      labels = c("ate 20", "de 20 a 29", "de 30 a 39", "de 40 a 49", 
                                                 "de 50 a 59", "de 60 a 69", "de 70 ou mais"))) %>% 
        dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'sem informacao', as.character(cut_idade))) %>% 
        dplyr::distinct(num_boletim, cut_idade),
      by = 'num_boletim') %>% 
    dplyr::group_by(cut_idade) %>%
    dplyr::summarise(
      'fatal' = sum(fatal),
      'nao fatal' = sum(naofatal),
      'sem ferimentos' = sum(semferimentos),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(soma, cut_idade)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text) %>% 
    dplyr::rename(`faixa etaria` = cut_idade)
  
}
lst_tables$indicio_uso_cinto_seguranca$faixa_idade_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate 20", "de 20 a 29", "de 30 a 39", "de 40 a 49", 
                                             "de 50 a 59", "de 60 a 69", "de 70 ou mais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'sem informacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::inner_join(
      db_ocorrencias_transito_bh$pessoas %>% 
        dplyr::filter(ind_condutor == 1) %>% 
        dplyr::group_by(num_boletim) %>% 
        dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
        dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
        dplyr::ungroup(), 
      by = 'num_boletim') %>% 
    dplyr::group_by(cut_idade, ind_cinto_seguranca) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_cinto_seguranca, text) %>% 
    dplyr::rename(`faixa etaria` = cut_idade)
  
}
lst_tables$periodo_dia$faixa_idade_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate 20", "de 20 a 29", "de 30 a 39", "de 40 a 49", 
                                             "de 50 a 59", "de 60 a 69", "de 70 ou mais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'sem informacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::group_by(cut_idade, periodo) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(periodo, text) %>% 
    dplyr::rename(`faixa etaria` = cut_idade)
  
}
lst_tables$tipo_acidente$faixa_idade_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate 20", "de 20 a 29", "de 30 a 39", "de 40 a 49", 
                                             "de 50 a 59", "de 60 a 69", "de 70 ou mais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'sem informacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::group_by(cut_idade, desc_acidente) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(cut_idade, text) %>% 
    dplyr::mutate_if(.predicate = is.character, 
                     .funs = function(x){ifelse(is.na(x), '0 (0%)', x)}) %>% 
    dplyr::rename(`tipo acidente` = desc_acidente)

}
lst_tables$descricao_veiculos$faixa_idade_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate 20", "de 20 a 29", "de 30 a 39", "de 40 a 49", 
                                             "de 50 a 59", "de 60 a 69", "de 70 ou mais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'sem informacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::inner_join(
      db_ocorrencias_transito_bh$ocorrencias %>% 
        dplyr::group_by(desc_veiculos) %>% 
        dplyr::summarise(n = dplyr::n()) %>% 
        dplyr::arrange(desc(n)) %>% 
        dplyr::slice(1:15),
      by = 'desc_veiculos') %>%  
    dplyr::group_by(cut_idade, desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(cut_idade, text) %>% 
    dplyr::mutate_if(.predicate = is.character, 
                     .funs = function(x){ifelse(is.na(x), '0 (0%)', x)}) %>% 
    dplyr::rename(`combinacao veiculos` = desc_veiculos)
  
}

lst_tables$indicio_embriaguez_acidente$ferimentos_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>%
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>% 
    dplyr::group_by(desc_severidade_acidente, ind_embriaguez) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_embriaguez, text) %>%
    dplyr::mutate_if(.predicate = is.character, 
                     .funs = function(x){ifelse(is.na(x), '0 (0%)', x)}) %>% 
    dplyr::rename(`ferimentos condutor` = desc_severidade_acidente)
  
}
lst_tables$indicio_pedestre_envolvido$ferimentos_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::select(num_boletim, desc_severidade_acidente) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>% 
    dplyr::distinct() %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::group_by(desc_severidade_acidente, ind_pedestre_envolvido) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_pedestre_envolvido, text) %>% 
    dplyr::rename(`ferimentos condutor` = desc_severidade_acidente)
  
}
lst_tables$indicio_evasao$ferimentos_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::select(num_boletim, desc_severidade_acidente) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>%
    dplyr::group_by(desc_severidade_acidente, ind_evasao) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_evasao, text) %>% 
    dplyr::rename(`ferimentos condutor` = desc_severidade_acidente)
  
}
lst_tables$indicio_fatalidade$ferimentos_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::group_by(desc_severidade_acidente, ind_fatalidade) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_fatalidade, text) %>%
    dplyr::mutate_all(.funs = function(x){ifelse(is.na(x), '0 (0%)', x)}) %>% 
    dplyr::rename(`ferimentos condutor` = desc_severidade_acidente)
  
}
lst_tables$numero_pessoas_envolvidas$ferimentos_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::group_by(desc_severidade_acidente, num_pessoas_envolvidas) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(num_pessoas_envolvidas, text) %>%
    dplyr::mutate_all(.funs = function(x){ifelse(is.na(x), '0 (0%)', x)}) %>% 
    dplyr::rename(`ferimentos condutor` = desc_severidade_acidente)
  
}
lst_tables$numero_veiculos_envolvidos$ferimentos_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::group_by(desc_severidade_acidente, num_veiculos_envolvidos) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(complete.cases(.)) %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(num_veiculos_envolvidos, text) %>%
    dplyr::mutate_all(.funs = function(x){ifelse(is.na(x), '0 (0%)', x)}) %>% 
    dplyr::rename(`ferimentos condutor` = desc_severidade_acidente)
  
}
lst_tables$sexo_condutor$ferimentos_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>% 
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexomasculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexofeminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'seminformacao', sexo)) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(sexo, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(
      db_ocorrencias_transito_bh$pessoas %>% 
        dplyr::filter(ind_condutor == 1) %>% 
        dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>% 
        dplyr::distinct(num_boletim, desc_severidade_acidente),
      by = 'num_boletim') %>% 
    dplyr::group_by(desc_severidade_acidente) %>%
    dplyr::summarise(
      'sexo masculino' = sum(sexomasculino),
      'sexo feminino' = sum(sexofeminino),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(soma, desc_severidade_acidente)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text) %>% 
    dplyr::rename(`ferimentos condutor` = desc_severidade_acidente)
  
}
lst_tables$faixa_idade_condutor$ferimentos_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::select(num_boletim, idade) %>% 
    dplyr::mutate(cut_idade = cut(idade, 
                                  breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate20", "de20a29", "de30a39", 
                                             "de40a49", "de50a59", "de60a69", "de70oumais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'seminformacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>% 
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(cut_idade, aux) %>%
    dplyr::inner_join(
      db_ocorrencias_transito_bh$pessoas %>%
        dplyr::filter(ind_condutor == 1) %>%
        dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>% 
        dplyr::distinct(num_boletim, desc_severidade_acidente), 
      by = 'num_boletim') %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::group_by(desc_severidade_acidente) %>%
    dplyr::summarise(
      'ate 20' = sum(ate20),
      'de 20 a 29' = sum(de20a29),
      'de 30 a 39' = sum(de30a39),
      'de 40 a 49' = sum(de40a49),
      'de 50 a 59' = sum(de50a59),
      'de 60 a 69' = sum(de60a69),
      'de 70 ou mais' = sum(de70oumais),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(desc_severidade_acidente, soma)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text) %>% 
    dplyr::rename(`ferimentos condutor` = desc_severidade_acidente)
  
}
lst_tables$indicio_uso_cinto_seguranca$ferimentos_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>% 
    dplyr::inner_join(
      db_ocorrencias_transito_bh$pessoas %>% 
        dplyr::filter(ind_condutor == 1) %>% 
        dplyr::group_by(num_boletim) %>% 
        dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
        dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
        dplyr::ungroup(), 
      by = 'num_boletim') %>% 
    dplyr::group_by(desc_severidade_acidente, ind_cinto_seguranca) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_cinto_seguranca, text) %>% 
    dplyr::rename(`ferimentos condutor` = desc_severidade_acidente)
  
}
lst_tables$periodo_dia$ferimentos_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::select(num_boletim, desc_severidade_acidente) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>%
    dplyr::distinct() %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%    
    dplyr::group_by(desc_severidade_acidente, periodo) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(periodo, text) %>% 
    dplyr::rename(`ferimentos condutor` = desc_severidade_acidente)
}
lst_tables$tipo_acidente$ferimentos_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>% 
    dplyr::group_by(desc_severidade_acidente, desc_acidente) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(desc_severidade_acidente, text) %>%
    dplyr::mutate_all(.funs = function(x){ifelse(is.na(x), '0 (0%)', x)}) %>% 
    dplyr::rename(`tipo acidente` = desc_acidente)
  
}
lst_tables$descricao_veiculos$ferimentos_condutor$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::inner_join(
      db_ocorrencias_transito_bh$ocorrencias %>% 
        dplyr::group_by(desc_veiculos) %>% 
        dplyr::summarise(n = dplyr::n()) %>% 
        dplyr::arrange(desc(n)) %>% 
        dplyr::slice(1:15),
      by = 'desc_veiculos') %>%  
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>% 
    dplyr::group_by(desc_severidade_acidente, desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(desc_severidade_acidente, text) %>%
    dplyr::mutate_all(.funs = function(x){ifelse(is.na(x), '0 (0%)', x)}) %>% 
    dplyr::rename(`combinacao veiculos` = desc_veiculos)
}

lst_tables$indicio_embriaguez_acidente$indicio_uso_cinto_seguranca$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::group_by(ind_cinto_seguranca, ind_embriaguez) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_embriaguez, text) %>% 
    dplyr::rename(`indicio uso cinto` = ind_cinto_seguranca)
  
}
lst_tables$indicio_pedestre_envolvido$indicio_uso_cinto_seguranca$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::group_by(ind_cinto_seguranca, ind_pedestre_envolvido) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_pedestre_envolvido, text) %>% 
    dplyr::rename(`indicio uso cinto` = ind_cinto_seguranca)
  
}
lst_tables$indicio_evasao$indicio_uso_cinto_seguranca$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::group_by(ind_cinto_seguranca, ind_evasao) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_evasao, text) %>% 
    dplyr::rename(`indicio uso cinto` = ind_cinto_seguranca)
  
}
lst_tables$indicio_fatalidade$indicio_uso_cinto_seguranca$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::group_by(ind_cinto_seguranca, ind_fatalidade) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_fatalidade, text) %>% 
    dplyr::rename(`indicio uso cinto` = ind_cinto_seguranca)
  
}
lst_tables$numero_pessoas_envolvidas$indicio_uso_cinto_seguranca$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%    
    dplyr::group_by(ind_cinto_seguranca, num_pessoas_envolvidas) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(num_pessoas_envolvidas, text) %>% 
    dplyr::rename(`indicio uso cinto` = ind_cinto_seguranca)
  
}
lst_tables$numero_veiculos_envolvidos$indicio_uso_cinto_seguranca$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::group_by(ind_cinto_seguranca, num_veiculos_envolvidos) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(complete.cases(.)) %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(num_veiculos_envolvidos, text) %>% 
    dplyr::rename(`indicio uso cinto` = ind_cinto_seguranca)
  
}
lst_tables$sexo_condutor$indicio_uso_cinto_seguranca$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(
      db_ocorrencias_transito_bh$pessoas %>% 
        dplyr::filter(ind_condutor == 1) %>% 
        dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexo masculino', sexo)) %>%
        dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexo feminino', sexo)) %>%
        dplyr::mutate(sexo = ifelse(is.na(sexo), 'sem informacao', sexo)) %>%
        dplyr::distinct(num_boletim, sexo),
      by = 'num_boletim') %>% 
    dplyr::group_by(sexo, ind_cinto_seguranca) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_cinto_seguranca, text) %>% 
    dplyr::rename(`sexo condutor` = sexo)
  
}
lst_tables$faixa_idade_condutor$indicio_uso_cinto_seguranca$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(
      db_ocorrencias_transito_bh$pessoas %>% 
        dplyr::filter(ind_condutor == 1) %>% 
        dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                      labels = c("ate20", "de20a29", "de30a39", "de40a49", 
                                                 "de50a59", "de60a69", "de70oumais"))) %>% 
        dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'seminformacao', as.character(cut_idade))) %>% 
        dplyr::distinct(num_boletim, cut_idade) %>% 
        dplyr::mutate(aux = 1) %>% 
        tidyr::spread(cut_idade, aux) %>% 
        dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}),
      by = 'num_boletim') %>% 
    dplyr::group_by(ind_cinto_seguranca) %>%
    dplyr::summarise(
      'ate 20' = sum(ate20),
      'de 20 a 29' = sum(de20a29),
      'de 30 a 39' = sum(de30a39),
      'de 40 a 49' = sum(de40a49),
      'de 50 a 59' = sum(de50a59),
      'de 60 a 69' = sum(de60a69),
      'de 70 ou mais' = sum(de70oumais),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(ind_cinto_seguranca, soma)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text) %>% 
    dplyr::rename(`indicio uso cinto` = ind_cinto_seguranca)
  
}
lst_tables$ferimentos_condutor$indicio_uso_cinto_seguranca$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(
      db_ocorrencias_transito_bh$pessoas %>% 
        dplyr::filter(ind_condutor == 1) %>% 
        dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>% 
        dplyr::distinct(num_boletim, desc_severidade_acidente) %>% 
        dplyr::mutate(desc_severidade_acidente = stringr::str_remove_all(desc_severidade_acidente, '\\s+')) %>% 
        dplyr::mutate(aux = 1) %>% 
        tidyr::spread(desc_severidade_acidente, aux) %>% 
        dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}),
      by = 'num_boletim') %>% 
    dplyr::group_by(ind_cinto_seguranca) %>%
    dplyr::summarise(
      fatal = sum(fatal),
      'nao fatal' = sum(naofatal),
      'sem ferimentos'= sum(semferimentos),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(ind_cinto_seguranca, soma)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text) %>% 
    dplyr::rename(`indicio uso cinto` = ind_cinto_seguranca)
  
}
lst_tables$periodo_dia$indicio_uso_cinto_seguranca$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::group_by(ind_cinto_seguranca, periodo) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(periodo, text) %>% 
    dplyr::rename(`indicio uso cinto` = ind_cinto_seguranca)
  
}
lst_tables$tipo_acidente$indicio_uso_cinto_seguranca$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>%
    dplyr::group_by(ind_cinto_seguranca, desc_acidente) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_cinto_seguranca, text) %>% 
    dplyr::rename(`tipo acidente` = desc_acidente)
  
}
lst_tables$descricao_veiculos$indicio_uso_cinto_seguranca$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>%
    dplyr::inner_join(
      db_ocorrencias_transito_bh$ocorrencias %>% 
        dplyr::group_by(desc_veiculos) %>% 
        dplyr::summarise(n = dplyr::n()) %>% 
        dplyr::arrange(desc(n)) %>% 
        dplyr::slice(1:15),
      by = 'desc_veiculos') %>%  
    dplyr::group_by(ind_cinto_seguranca, desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_cinto_seguranca, text) %>% 
    dplyr::rename(`combinacao veiculos` = desc_veiculos)
  
}

lst_tables$indicio_embriaguez_acidente$tipo_acidente$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::group_by(desc_acidente, ind_embriaguez) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_embriaguez, text) %>% 
    dplyr::mutate_if(.predicate = is.character, 
                     .funs = function(x){ifelse(is.na(x), '0 (0%)', x)}) %>% 
    dplyr::rename(`tipo acidente` = desc_acidente)
  
}
lst_tables$indicio_pedestre_envolvido$tipo_acidente$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::group_by(desc_acidente, ind_pedestre_envolvido) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_pedestre_envolvido, text) %>% 
    dplyr::rename(`tipo acidente` = desc_acidente)
  
}
lst_tables$indicio_evasao$tipo_acidente$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::group_by(desc_acidente, ind_evasao) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_evasao, text) %>% 
    dplyr::mutate_if(.predicate = is.character, 
                     .funs = function(x){ifelse(is.na(x), '0 (0%)', x)}) %>% 
    dplyr::rename(`tipo acidente` = desc_acidente)
  
}
lst_tables$indicio_fatalidade$tipo_acidente$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::group_by(desc_acidente, ind_fatalidade) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_fatalidade, text) %>% 
    dplyr::mutate_if(.predicate = is.character, 
                     .funs = function(x){ifelse(is.na(x), '0 (0%)', x)}) %>% 
    dplyr::rename(`tipo acidente` = desc_acidente)
  
}
lst_tables$numero_pessoas_envolvidas$tipo_acidente$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::group_by(desc_acidente, num_pessoas_envolvidas) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(num_pessoas_envolvidas, text) %>% 
    dplyr::mutate_if(.predicate = is.character, 
                     .funs = function(x){ifelse(is.na(x), '0 (0%)', x)}) %>% 
    dplyr::rename(`tipo acidente` = desc_acidente)
  
}
lst_tables$numero_veiculos_envolvidos$tipo_acidente$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::group_by(desc_acidente, num_veiculos_envolvidos) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::filter(complete.cases(.)) %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(num_veiculos_envolvidos, text) %>% 
    dplyr::mutate_if(.predicate = is.character, 
                     .funs = function(x){ifelse(is.na(x), '0 (0%)', x)}) %>% 
    dplyr::rename(`tipo acidente` = desc_acidente)
  
}
lst_tables$sexo_condutor$tipo_acidente$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>% 
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexomasculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexofeminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'seminformacao', sexo)) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(sexo, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::group_by(desc_acidente) %>%
    dplyr::summarise(
      'sexo masculino' = sum(sexomasculino),
      'sexo feminino' = sum(sexofeminino),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(soma, desc_acidente)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text) %>% 
    dplyr::rename(`tipo acidente` = desc_acidente)
  
}
lst_tables$faixa_idade_condutor$tipo_acidente$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate20", "de20a29", "de30a39", "de40a49", 
                                             "de50a59", "de60a69", "de70oumais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'seminformacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>% 
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(cut_idade, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::group_by(desc_acidente) %>% 
    dplyr::summarise(
      'ate 20' = sum(ate20),
      'de 20 a 29' = sum(de20a29),
      'de 30 a 39' = sum(de30a39),
      'de 40 a 49' = sum(de40a49),
      'de 50 a 59' = sum(de50a59),
      'de 60 a 69' = sum(de60a69),
      'de 70 ou mais' = sum(de70oumais),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(desc_acidente, soma)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text) %>% 
    dplyr::rename(`tipo acidente` = desc_acidente)
  
  
}
lst_tables$ferimentos_condutor$tipo_acidente$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>% 
    dplyr::mutate(desc_severidade_acidente = stringr::str_remove_all(desc_severidade_acidente, '\\s+')) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::select(num_boletim, desc_acidente, desc_severidade_acidente) %>% 
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(desc_severidade_acidente, aux) %>%     
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>% 
    dplyr::group_by(desc_acidente) %>%
    dplyr::summarise(
      fatal = sum(fatal),
      'nao fatal' = sum(naofatal),
      'sem ferimentos'= sum(semferimentos),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(desc_acidente, soma)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text) %>% 
    dplyr::rename(`tipo acidente` = desc_acidente)
  
}
lst_tables$indicio_uso_cinto_seguranca$tipo_acidente$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-soma) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::group_by(desc_acidente, ind_cinto_seguranca) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_cinto_seguranca, text) %>%
    dplyr::mutate_all(.funs = function(x){ifelse(is.na(x), '0 (0%)', x)}) %>% 
    dplyr::rename(`tipo acidente` = desc_acidente)
  
}
lst_tables$periodo_dia$tipo_acidente$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>%
    dplyr::group_by(desc_acidente, periodo) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(periodo, text) %>% 
    dplyr::rename(`tipo acidente` = desc_acidente)
  
}

lst_tables$indicio_embriaguez_acidente$descricao_veiculos$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::group_by(desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    dplyr::slice(1:15) %>% 
    dplyr::select(desc_veiculos) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = c('desc_veiculos')) %>% 
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::group_by(desc_veiculos, ind_embriaguez) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_embriaguez, text) %>% 
    dplyr::mutate_if(.predicate = is.character, 
                     .funs = function(x){ifelse(is.na(x), '0 (0%)', x)}) %>% 
    dplyr::rename(`combinacao veiculos` = desc_veiculos)
  
}
lst_tables$indicio_pedestre_envolvido$descricao_veiculos$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::group_by(desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    dplyr::slice(1:15) %>% 
    dplyr::select(desc_veiculos) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = c('desc_veiculos')) %>% 
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::group_by(desc_veiculos, ind_pedestre_envolvido) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_pedestre_envolvido, text) %>% 
    dplyr::rename(`combinacao veiculos` = desc_veiculos)
  
}
lst_tables$indicio_evasao$descricao_veiculos$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::group_by(desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    dplyr::slice(1:15) %>% 
    dplyr::select(desc_veiculos) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = c('desc_veiculos')) %>% 
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::group_by(desc_veiculos, ind_evasao) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_evasao, text) %>% 
    dplyr::mutate_if(.predicate = is.character, 
                     .funs = function(x){ifelse(is.na(x), '0 (0%)', x)}) %>% 
    dplyr::rename(`combinacao veiculos` = desc_veiculos)
  
}
lst_tables$indicio_fatalidade$descricao_veiculos$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::group_by(desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    dplyr::slice(1:15) %>% 
    dplyr::select(desc_veiculos) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = c('desc_veiculos')) %>% 
    dplyr::select(desc_veiculos, ind_fatalidade) %>% 
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::group_by(desc_veiculos, ind_fatalidade) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_fatalidade, text) %>% 
    dplyr::mutate_if(.predicate = is.character, 
                     .funs = function(x){ifelse(is.na(x), '0 (0%)', x)}) %>% 
    dplyr::rename(`combinacao veiculos` = desc_veiculos)
  
}
lst_tables$numero_pessoas_envolvidas$descricao_veiculos$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::group_by(desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    dplyr::slice(1:15) %>% 
    dplyr::select(desc_veiculos) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = c('desc_veiculos')) %>% 
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::group_by(desc_veiculos, num_pessoas_envolvidas) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(num_pessoas_envolvidas, text) %>% 
    dplyr::mutate_if(.predicate = is.character, 
                     .funs = function(x){ifelse(is.na(x), '0 (0%)', x)}) %>% 
    dplyr::rename(`combinacao veiculos` = desc_veiculos)
  
}
lst_tables$numero_veiculos_envolvidos$descricao_veiculos$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::group_by(desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    dplyr::slice(1:15) %>% 
    dplyr::select(desc_veiculos) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = c('desc_veiculos')) %>% 
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::group_by(desc_veiculos, num_veiculos_envolvidos) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(num_veiculos_envolvidos, text) %>% 
    dplyr::mutate_if(.predicate = is.character, 
                     .funs = function(x){ifelse(is.na(x), '0 (0%)', x)}) %>% 
    dplyr::rename(`combinacao veiculos` = desc_veiculos)
  
}
lst_tables$sexo_condutor$descricao_veiculos$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>% 
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexomasculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexofeminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'seminformacao', sexo)) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(sexo, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::inner_join(
      db_ocorrencias_transito_bh$ocorrencias %>% 
        dplyr::select(desc_veiculos) %>% 
        dplyr::group_by(desc_veiculos) %>% 
        dplyr::summarise(n = dplyr::n()) %>% 
        dplyr::arrange(desc(n)) %>% 
        dplyr::slice(1:15) %>% 
        dplyr::select(desc_veiculos),
      by = 'desc_veiculos') %>% 
    dplyr::group_by(desc_veiculos) %>%
    dplyr::summarise(
      'sexo masculino' = sum(sexomasculino),
      'sexo feminino' = sum(sexofeminino),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(soma, desc_veiculos)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text) %>% 
    dplyr::rename(`combinacao veiculos` = desc_veiculos)
  
}
lst_tables$faixa_idade_condutor$descricao_veiculos$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate20", "de20a29", "de30a39", "de40a49", 
                                             "de50a59", "de60a69", "de70oumais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'seminformacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>% 
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(cut_idade, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::inner_join(
      db_ocorrencias_transito_bh$ocorrencias %>% 
        dplyr::group_by(desc_veiculos) %>% 
        dplyr::summarise(n = dplyr::n()) %>% 
        dplyr::arrange(desc(n)) %>% 
        dplyr::slice(1:15), 
      by = 'desc_veiculos') %>% 
    dplyr::group_by(desc_veiculos) %>% 
    dplyr::summarise(
      'ate 20' = sum(ate20),
      'de 20 a 29' = sum(de20a29),
      'de 30 a 39' = sum(de30a39),
      'de 40 a 49' = sum(de40a49),
      'de 50 a 59' = sum(de50a59),
      'de 60 a 69' = sum(de60a69),
      'de 70 ou mais' = sum(de70oumais),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(desc_veiculos, soma)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text) %>% 
    dplyr::rename(`combinacao veiculos` = desc_veiculos)
  
}
lst_tables$ferimentos_condutor$descricao_veiculos$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>% 
    dplyr::mutate(desc_severidade_acidente = stringr::str_remove_all(desc_severidade_acidente, '\\s+')) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>%
    dplyr::inner_join(
      db_ocorrencias_transito_bh$ocorrencias %>% 
        dplyr::group_by(desc_veiculos) %>% 
        dplyr::summarise(n = dplyr::n()) %>% 
        dplyr::arrange(desc(n)) %>% 
        dplyr::slice(1:15),
      by = 'desc_veiculos') %>% 
    dplyr::select(num_boletim, desc_veiculos, desc_severidade_acidente) %>% 
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(desc_severidade_acidente, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>% 
    dplyr::group_by(desc_veiculos) %>%
    dplyr::summarise(
      fatal = sum(fatal),
      'nao fatal' = sum(naofatal),
      'sem ferimentos'= sum(semferimentos),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(desc_veiculos, soma)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text) %>% 
    dplyr::rename(`combinacao veiculos` = desc_veiculos)
  
}
lst_tables$indicio_uso_cinto_seguranca$descricao_veiculos$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-soma) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::inner_join(
      db_ocorrencias_transito_bh$ocorrencias %>% 
        dplyr::group_by(desc_veiculos) %>% 
        dplyr::summarise(n = dplyr::n()) %>% 
        dplyr::arrange(desc(n)) %>% 
        dplyr::slice(1:15),
      by = 'desc_veiculos') %>% 
    dplyr::group_by(desc_veiculos, ind_cinto_seguranca) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_cinto_seguranca, text) %>%
    dplyr::mutate_all(.funs = function(x){ifelse(is.na(x), '0 (0%)', x)}) %>% 
    dplyr::rename(`combinacao veiculos` = desc_veiculos)
  
  
}
lst_tables$periodo_dia$descricao_veiculos$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>%
    dplyr::inner_join(
      db_ocorrencias_transito_bh$ocorrencias %>% 
        dplyr::group_by(desc_veiculos) %>% 
        dplyr::summarise(n = dplyr::n()) %>% 
        dplyr::arrange(desc(n)) %>% 
        dplyr::slice(1:15),
      by = 'desc_veiculos') %>% 
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::group_by(desc_veiculos, periodo) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(periodo, text) %>% 
    dplyr::rename(`combinacao veiculos` = desc_veiculos)
  
}

lst_tables$indicio_pedestre_envolvido$indicio_embriaguez_acidente$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>% 
    dplyr::group_by(ind_embriaguez, ind_pedestre_envolvido) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_pedestre_envolvido, text) %>% 
    dplyr::rename(`indicio embriaguez` = ind_embriaguez)
  
}
lst_tables$indicio_evasao$indicio_embriaguez_acidente$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::group_by(ind_embriaguez, ind_evasao) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_evasao, text) %>% 
    dplyr::rename(`indicio embriaguez` = ind_embriaguez)
  
}
lst_tables$indicio_fatalidade$indicio_embriaguez_acidente$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::group_by(ind_embriaguez, ind_fatalidade) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_fatalidade, text) %>% 
    dplyr::rename(`indicio embriaguez` = ind_embriaguez)
  
}
lst_tables$numero_pessoas_envolvidas$indicio_embriaguez_acidente$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::group_by(ind_embriaguez, num_pessoas_envolvidas) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(num_pessoas_envolvidas, text) %>% 
    dplyr::rename(`indicio embriaguez` = ind_embriaguez)
}
lst_tables$numero_veiculos_envolvidos$indicio_embriaguez_acidente$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::group_by(ind_embriaguez, num_veiculos_envolvidos) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(complete.cases(.)) %>% 
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(num_veiculos_envolvidos, text) %>% 
    dplyr::rename(`indicio embriaguez` = ind_embriaguez)
  
}
lst_tables$sexo_condutor$indicio_embriaguez_acidente$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>% 
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexomasculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexofeminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'seminformacao', sexo)) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(sexo, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::group_by(ind_embriaguez) %>%
    dplyr::summarise(
      'sexo masculino' = sum(sexomasculino),
      'sexo feminino' = sum(sexofeminino),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(soma, ind_embriaguez)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text) %>% 
    dplyr::rename(`indicio embriaguez` = ind_embriaguez)
  
}
lst_tables$faixa_idade_condutor$indicio_embriaguez_acidente$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate20", "de20a29", "de30a39", "de40a49", 
                                             "de50a59", "de60a69", "de70oumais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'seminformacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>% 
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(cut_idade, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::group_by(ind_embriaguez) %>% 
    dplyr::summarise(
      'ate 20' = sum(ate20),
      'de 20 a 29' = sum(de20a29),
      'de 30 a 39' = sum(de30a39),
      'de 40 a 49' = sum(de40a49),
      'de 50 a 59' = sum(de50a59),
      'de 60 a 69' = sum(de60a69),
      'de 70 ou mais' = sum(de70oumais),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(ind_embriaguez, soma)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text) %>% 
    dplyr::rename(`indicio embriaguez` = ind_embriaguez)
  
}
lst_tables$ferimentos_condutor$indicio_embriaguez_acidente$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'seminformacao', desc_severidade_acidente)) %>% 
    dplyr::mutate(desc_severidade_acidente = stringr::str_remove_all(desc_severidade_acidente, '\\s+')) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(desc_severidade_acidente, aux) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>% 
    dplyr::group_by(ind_embriaguez) %>%
    dplyr::summarise(
      fatal = sum(fatal),
      'nao fatal' = sum(naofatal),
      'sem ferimentos'= sum(semferimentos),
      'sem informacao' = sum(seminformacao),
      soma = dplyr::n_distinct(num_boletim)
    ) %>%
    tidyr::gather(key, n, -c(ind_embriaguez, soma)) %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(key, text) %>% 
    dplyr::rename(`indicio embriaguez` = ind_embriaguez)
  
}
lst_tables$indicio_uso_cinto_seguranca$indicio_embriaguez_acidente$tbl_formato <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::group_by(ind_embriaguez, ind_cinto_seguranca) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_cinto_seguranca, text) %>% 
    dplyr::rename(`indicio embriaguez` = ind_embriaguez)
  
}
lst_tables$periodo_dia$indicio_embriaguez_acidente$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::select(periodo, ind_embriaguez) %>%
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::group_by(ind_embriaguez, periodo) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(soma = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(periodo, text)
}
lst_tables$tipo_acidente$indicio_embriaguez_acidente$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::group_by(ind_embriaguez, desc_acidente) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_embriaguez, text) %>% 
    dplyr::mutate_if(.predicate = is.character, 
                     .funs = function(x){ifelse(is.na(x), '0 (0%)', x)}) %>% 
    dplyr::rename(`tipo acidente` = desc_acidente)
  
}
lst_tables$descricao_veiculos$indicio_embriaguez_acidente$tbl_formato <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::group_by(desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    dplyr::slice(1:15) %>% 
    dplyr::select(desc_veiculos) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = c('desc_veiculos')) %>% 
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::group_by(ind_embriaguez, desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>% 
    dplyr::mutate(soma = sum(n)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/soma) %>%
    dplyr::mutate(text = stringr::str_c(n, ' (', round(perc, 2), '%)')) %>%
    dplyr::select(-c(n, soma, perc)) %>%
    tidyr::spread(ind_embriaguez, text) %>% 
    dplyr::rename(`combinacao veiculos` = desc_veiculos)
}



lst_tables$indicio_embriaguez_acidente$periodo_dia$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::select(periodo, ind_embriaguez) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_pedestre_envolvido$periodo_dia$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::select(periodo, ind_pedestre_envolvido) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_evasao$periodo_dia$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>%
    dplyr::select(periodo, ind_evasao) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_fatalidade$periodo_dia$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>%
    dplyr::select(periodo, ind_fatalidade) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$numero_pessoas_envolvidas$periodo_dia$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::select(periodo, num_pessoas_envolvidas) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$numero_veiculos_envolvidos$periodo_dia$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::select(periodo, num_veiculos_envolvidos) %>%
    dplyr::filter(complete.cases(.)) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$sexo_condutor$periodo_dia$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>% 
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexomasculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexofeminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'seminformacao', sexo)) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(sexo, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::group_by(periodo) %>%
    dplyr::summarise(
      'sexo masculino' = sum(sexomasculino),
      'sexo feminino' = sum(sexofeminino),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()
  
}
lst_tables$faixa_idade_condutor$periodo_dia$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate20", "de20a29", "de30a39", "de40a49", 
                                             "de50a59", "de60a69", "de70oumais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'seminformacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>% 
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(cut_idade, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::group_by(periodo) %>% 
    dplyr::summarise(
      'ate 20' = sum(ate20),
      'de 20 a 29' = sum(de20a29),
      'de 30 a 39' = sum(de30a39),
      'de 40 a 49' = sum(de40a49),
      'de 50 a 59' = sum(de50a59),
      'de 60 a 69' = sum(de60a69),
      'de 70 ou mais' = sum(de70oumais),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()
  
}
lst_tables$ferimentos_condutor$periodo_dia$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'seminformacao', desc_severidade_acidente)) %>% 
    dplyr::mutate(desc_severidade_acidente = stringr::str_remove_all(desc_severidade_acidente, '\\s+')) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(desc_severidade_acidente, aux) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>% 
    dplyr::group_by(periodo) %>%
    dplyr::summarise(
      fatal = sum(fatal),
      'nao fatal' = sum(naofatal),
      'sem ferimentos'= sum(semferimentos),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()
}
lst_tables$indicio_uso_cinto_seguranca$periodo_dia$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::select(periodo, ind_cinto_seguranca) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$tipo_acidente$periodo_dia$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::select(periodo, desc_acidente) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$descricao_veiculos$periodo_dia$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::inner_join(
      db_ocorrencias_transito_bh$ocorrencias %>% 
        dplyr::group_by(desc_veiculos) %>% 
        dplyr::summarise(n = dplyr::n()) %>% 
        dplyr::arrange(desc(n)) %>% 
        dplyr::slice(1:15),
      by = 'desc_veiculos') %>%  
    dplyr::select(periodo, desc_acidente) %>% 
    table() %>% 
    chisq.test()
}

lst_tables$indicio_embriaguez_acidente$indicio_pedestre_envolvido$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>% 
    dplyr::select(ind_pedestre_envolvido, ind_embriaguez) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_evasao$indicio_pedestre_envolvido$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::select(ind_pedestre_envolvido, ind_evasao) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_fatalidade$indicio_pedestre_envolvido$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::select(ind_pedestre_envolvido, ind_fatalidade) %>%
    table() %>% 
    chisq.test()
}
lst_tables$numero_pessoas_envolvidas$indicio_pedestre_envolvido$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::select(ind_pedestre_envolvido, num_pessoas_envolvidas) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$numero_veiculos_envolvidos$indicio_pedestre_envolvido$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::select(ind_pedestre_envolvido, num_veiculos_envolvidos) %>%
    dplyr::filter(complete.cases(.)) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$sexo_condutor$indicio_pedestre_envolvido$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>% 
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexomasculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexofeminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'seminformacao', sexo)) %>%
    dplyr::mutate(aux = 1) %>%
    tidyr::spread(sexo, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::group_by(ind_pedestre_envolvido) %>%
    dplyr::summarise(
      'sexo masculino' = sum(sexomasculino),
      'sexo feminino' = sum(sexofeminino),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()
  
}
lst_tables$faixa_idade_condutor$indicio_pedestre_envolvido$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate20", "de20a29", "de30a39", "de40a49", 
                                             "de50a59", "de60a69", "de70oumais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'seminformacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>% 
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(cut_idade, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::group_by(ind_pedestre_envolvido) %>% 
    dplyr::summarise(
      'ate 20' = sum(ate20),
      'de 20 a 29' = sum(de20a29),
      'de 30 a 39' = sum(de30a39),
      'de 40 a 49' = sum(de40a49),
      'de 50 a 59' = sum(de50a59),
      'de 60 a 69' = sum(de60a69),
      'de 70 ou mais' = sum(de70oumais),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()
  
}
lst_tables$ferimentos_condutor$indicio_pedestre_envolvido$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'seminformacao', desc_severidade_acidente)) %>% 
    dplyr::mutate(desc_severidade_acidente = stringr::str_remove_all(desc_severidade_acidente, '\\s+')) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(desc_severidade_acidente, aux) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>% 
    dplyr::group_by(ind_pedestre_envolvido) %>%
    dplyr::summarise(
      fatal = sum(fatal),
      'nao fatal' = sum(naofatal),
      'sem ferimentos'= sum(semferimentos),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()
  
}
lst_tables$indicio_uso_cinto_seguranca$indicio_pedestre_envolvido$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::select(ind_pedestre_envolvido, ind_cinto_seguranca) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$periodo_dia$indicio_pedestre_envolvido$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::select(periodo, ind_pedestre_envolvido) %>%
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::select(ind_pedestre_envolvido,periodo) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$tipo_acidente$indicio_pedestre_envolvido$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::select(ind_pedestre_envolvido, desc_acidente) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$descricao_veiculos$indicio_pedestre_envolvido$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::group_by(desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    dplyr::slice(1:15) %>% 
    dplyr::select(desc_veiculos) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = c('desc_veiculos')) %>% 
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::select(ind_pedestre_envolvido, desc_veiculos) %>% 
    table() %>% 
    chisq.test()
  
}

lst_tables$indicio_embriaguez_acidente$indicio_evasao$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::select(ind_evasao, ind_embriaguez) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_pedestre_envolvido$indicio_evasao$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>%
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>% 
    dplyr::select(ind_evasao, ind_pedestre_envolvido) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_fatalidade$indicio_evasao$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::select(ind_evasao, ind_fatalidade) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$numero_pessoas_envolvidas$indicio_evasao$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::select(ind_evasao, num_pessoas_envolvidas) %>%
    table() %>% 
    chisq.test()
}
lst_tables$numero_veiculos_envolvidos$indicio_evasao$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::select(ind_evasao, num_veiculos_envolvidos) %>%
    dplyr::filter(complete.cases(.)) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$sexo_condutor$indicio_evasao$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>% 
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexomasculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexofeminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'seminformacao', sexo)) %>%
    dplyr::mutate(aux = 1) %>%
    tidyr::spread(sexo, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::group_by(ind_evasao) %>%
    dplyr::summarise(
      'sexo masculino' = sum(sexomasculino),
      'sexo feminino' = sum(sexofeminino),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()
  
}
lst_tables$faixa_idade_condutor$indicio_evasao$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate20", "de20a29", "de30a39", "de40a49", 
                                             "de50a59", "de60a69", "de70oumais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'seminformacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>% 
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(cut_idade, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::group_by(ind_evasao) %>% 
    dplyr::summarise(
      'ate 20' = sum(ate20),
      'de 20 a 29' = sum(de20a29),
      'de 30 a 39' = sum(de30a39),
      'de 40 a 49' = sum(de40a49),
      'de 50 a 59' = sum(de50a59),
      'de 60 a 69' = sum(de60a69),
      'de 70 ou mais' = sum(de70oumais),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()

}
lst_tables$ferimentos_condutor$indicio_evasao$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'seminformacao', desc_severidade_acidente)) %>% 
    dplyr::mutate(desc_severidade_acidente = stringr::str_remove_all(desc_severidade_acidente, '\\s+')) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(desc_severidade_acidente, aux) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>% 
    dplyr::group_by(ind_evasao) %>%
    dplyr::summarise(
      fatal = sum(fatal),
      'nao fatal' = sum(naofatal),
      'sem ferimentos'= sum(semferimentos),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()
  
}
lst_tables$indicio_uso_cinto_seguranca$indicio_evasao$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::select(ind_evasao, ind_cinto_seguranca) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$periodo_dia$indicio_evasao$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::select(periodo, ind_evasao) %>%
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>%
    dplyr::select(ind_evasao, periodo) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$tipo_acidente$indicio_evasao$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::select(ind_evasao, desc_acidente) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$descricao_veiculos$indicio_evasao$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::group_by(desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    dplyr::slice(1:15) %>% 
    dplyr::select(desc_veiculos) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = c('desc_veiculos')) %>% 
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::select(ind_evasao, desc_veiculos) %>% 
    table() %>% 
    chisq.test()
  
}

lst_tables$indicio_embriaguez_acidente$indicio_fatalidade$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::select(ind_fatalidade, ind_embriaguez) %>%
    table() %>% 
    chisq.test()
}
lst_tables$indicio_pedestre_envolvido$indicio_fatalidade$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>% 
    dplyr::select(ind_fatalidade, ind_pedestre_envolvido) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_evasao$indicio_fatalidade$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::select(ind_fatalidade, ind_evasao) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$numero_pessoas_envolvidas$indicio_fatalidade$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::select(ind_fatalidade, num_pessoas_envolvidas) %>%
    table() %>% 
    chisq.test()
}
lst_tables$numero_veiculos_envolvidos$indicio_fatalidade$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::select(ind_fatalidade, num_veiculos_envolvidos) %>%
    dplyr::filter(complete.cases(.)) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$sexo_condutor$indicio_fatalidade$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>% 
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexomasculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexofeminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'seminformacao', sexo)) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(sexo, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::group_by(ind_fatalidade) %>%
    dplyr::summarise(
      'sexo masculino' = sum(sexomasculino),
      'sexo feminino' = sum(sexofeminino),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()

}
lst_tables$faixa_idade_condutor$indicio_fatalidade$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate20", "de20a29", "de30a39", "de40a49", 
                                             "de50a59", "de60a69", "de70oumais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'seminformacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>% 
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(cut_idade, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::group_by(ind_fatalidade) %>% 
    dplyr::summarise(
      'ate 20' = sum(ate20),
      'de 20 a 29' = sum(de20a29),
      'de 30 a 39' = sum(de30a39),
      'de 40 a 49' = sum(de40a49),
      'de 50 a 59' = sum(de50a59),
      'de 60 a 69' = sum(de60a69),
      'de 70 ou mais' = sum(de70oumais),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()
  
}
lst_tables$ferimentos_condutor$indicio_fatalidade$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'seminformacao', desc_severidade_acidente)) %>% 
    dplyr::mutate(desc_severidade_acidente = stringr::str_remove_all(desc_severidade_acidente, '\\s+')) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(desc_severidade_acidente, aux) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>% 
    dplyr::group_by(ind_fatalidade) %>%
    dplyr::summarise(
      fatal = sum(fatal),
      'nao fatal' = sum(naofatal),
      'sem ferimentos'= sum(semferimentos),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()
  
}
lst_tables$indicio_uso_cinto_seguranca$indicio_fatalidade$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::select(ind_fatalidade, ind_cinto_seguranca) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$periodo_dia$indicio_fatalidade$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::select(periodo, ind_fatalidade) %>%
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>%
    dplyr::select(ind_fatalidade, periodo) %>%
    table() %>% 
    chisq.test()
}
lst_tables$tipo_acidente$indicio_fatalidade$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::select(ind_fatalidade, desc_acidente) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$descricao_veiculos$indicio_fatalidade$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::group_by(desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    dplyr::slice(1:15) %>% 
    dplyr::select(desc_veiculos) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = c('desc_veiculos')) %>% 
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::select(ind_fatalidade, desc_veiculos) %>% 
    table() %>% 
    chisq.test()
}

lst_tables$indicio_embriaguez_acidente$numero_pessoas_envolvidas$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::select(num_pessoas_envolvidas, ind_embriaguez) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_pedestre_envolvido$numero_pessoas_envolvidas$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::select(num_pessoas_envolvidas, ind_pedestre_envolvido) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_evasao$numero_pessoas_envolvidas$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::select(num_pessoas_envolvidas, ind_evasao) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_fatalidade$numero_pessoas_envolvidas$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::select(num_pessoas_envolvidas,ind_fatalidade) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$numero_veiculos_envolvidos$numero_pessoas_envolvidas$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::select(num_pessoas_envolvidas, num_veiculos_envolvidos) %>%
    dplyr::filter(complete.cases(.)) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$sexo_condutor$numero_pessoas_envolvidas$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>% 
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexomasculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexofeminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'seminformacao', sexo)) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(sexo, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::group_by(num_pessoas_envolvidas) %>%
    dplyr::summarise(
      'sexo masculino' = sum(sexomasculino),
      'sexo feminino' = sum(sexofeminino),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()
}
lst_tables$faixa_idade_condutor$numero_pessoas_envolvidas$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate20", "de20a29", "de30a39", "de40a49", 
                                             "de50a59", "de60a69", "de70oumais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'seminformacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>% 
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(cut_idade, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::group_by(num_pessoas_envolvidas) %>% 
    dplyr::summarise(
      'ate 20' = sum(ate20),
      'de 20 a 29' = sum(de20a29),
      'de 30 a 39' = sum(de30a39),
      'de 40 a 49' = sum(de40a49),
      'de 50 a 59' = sum(de50a59),
      'de 60 a 69' = sum(de60a69),
      'de 70 ou mais' = sum(de70oumais),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()

}
lst_tables$ferimentos_condutor$numero_pessoas_envolvidas$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'seminformacao', desc_severidade_acidente)) %>% 
    dplyr::mutate(desc_severidade_acidente = stringr::str_remove_all(desc_severidade_acidente, '\\s+')) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(desc_severidade_acidente, aux) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%    
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>% 
    dplyr::group_by(num_pessoas_envolvidas) %>%
    dplyr::summarise(
      fatal = sum(fatal),
      'nao fatal' = sum(naofatal),
      'sem ferimentos'= sum(semferimentos),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()

}
lst_tables$indicio_uso_cinto_seguranca$numero_pessoas_envolvidas$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%    
    dplyr::select(num_pessoas_envolvidas, ind_cinto_seguranca) %>%
    table() %>% 
    chisq.test()
}
lst_tables$periodo_dia$numero_pessoas_envolvidas$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::select(num_pessoas_envolvidas, periodo) %>%
    table() %>% 
    chisq.test()
}
lst_tables$tipo_acidente$numero_pessoas_envolvidas$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::select(num_pessoas_envolvidas, desc_acidente) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$descricao_veiculos$numero_pessoas_envolvidas$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::group_by(desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    dplyr::slice(1:15) %>% 
    dplyr::select(desc_veiculos) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = c('desc_veiculos')) %>% 
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>% 
    dplyr::select(num_pessoas_envolvidas, desc_veiculos) %>% 
    table() %>% 
    chisq.test()
  
}

lst_tables$indicio_embriaguez_acidente$numero_veiculos_envolvidos$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::select(num_veiculos_envolvidos, ind_embriaguez) %>%
    dplyr::filter(complete.cases(.)) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_pedestre_envolvido$numero_veiculos_envolvidos$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::select(num_veiculos_envolvidos, ind_pedestre_envolvido) %>%
    dplyr::filter(complete.cases(.)) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_evasao$numero_veiculos_envolvidos$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::select(num_veiculos_envolvidos, ind_evasao) %>%
    dplyr::filter(complete.cases(.)) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_fatalidade$numero_veiculos_envolvidos$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::select(num_veiculos_envolvidos, ind_fatalidade) %>%
    dplyr::filter(complete.cases(.)) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$numero_pessoas_envolvidas$numero_veiculos_envolvidos$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::select(num_veiculos_envolvidos, num_pessoas_envolvidas) %>%
    dplyr::filter(complete.cases(.)) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$sexo_condutor$numero_veiculos_envolvidos$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>% 
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexomasculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexofeminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'seminformacao', sexo)) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(sexo, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::group_by(num_veiculos_envolvidos) %>%
    dplyr::summarise(
      'sexo masculino' = sum(sexomasculino),
      'sexo feminino' = sum(sexofeminino),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    dplyr::filter(complete.cases(.)) %>% 
    chisq.test()
  
}
lst_tables$faixa_idade_condutor$numero_veiculos_envolvidos$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate20", "de20a29", "de30a39", "de40a49", 
                                             "de50a59", "de60a69", "de70oumais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'seminformacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>% 
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(cut_idade, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::group_by(num_veiculos_envolvidos) %>% 
    dplyr::summarise(
      'ate 20' = sum(ate20),
      'de 20 a 29' = sum(de20a29),
      'de 30 a 39' = sum(de30a39),
      'de 40 a 49' = sum(de40a49),
      'de 50 a 59' = sum(de50a59),
      'de 60 a 69' = sum(de60a69),
      'de 70 ou mais' = sum(de70oumais),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    dplyr::filter(complete.cases(.)) %>% 
    chisq.test()
}
lst_tables$ferimentos_condutor$numero_veiculos_envolvidos$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'seminformacao', desc_severidade_acidente)) %>% 
    dplyr::mutate(desc_severidade_acidente = stringr::str_remove_all(desc_severidade_acidente, '\\s+')) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(desc_severidade_acidente, aux) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%    
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>% 
    dplyr::group_by(num_veiculos_envolvidos) %>%
    dplyr::summarise(
      fatal = sum(fatal),
      'nao fatal' = sum(naofatal),
      'sem ferimentos'= sum(semferimentos),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::filter(complete.cases(.)) %>% 
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()
  
}
lst_tables$indicio_uso_cinto_seguranca$numero_veiculos_envolvidos$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%   
    dplyr::select(num_veiculos_envolvidos, ind_cinto_seguranca) %>%
    dplyr::filter(complete.cases(.)) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$periodo_dia$numero_veiculos_envolvidos$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::select(num_veiculos_envolvidos, periodo) %>%
    dplyr::filter(complete.cases(.)) %>% 
    dplyr::select(num_veiculos_envolvidos, periodo) %>%
    dplyr::filter(complete.cases(.)) %>% 
    table() %>% 
    chisq.test()
}
lst_tables$tipo_acidente$numero_veiculos_envolvidos$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::select(num_veiculos_envolvidos, desc_acidente) %>% 
    dplyr::filter(complete.cases(.)) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$descricao_veiculos$numero_veiculos_envolvidos$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::group_by(desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    dplyr::slice(1:15) %>% 
    dplyr::select(desc_veiculos) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = c('desc_veiculos')) %>% 
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>% 
    dplyr::select(num_veiculos_envolvidos, desc_veiculos) %>% 
    dplyr::filter(complete.cases(.)) %>% 
    table() %>% 
    chisq.test()
  
}

lst_tables$indicio_embriaguez_acidente$sexo_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexo masculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexo feminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'sem informacao', sexo)) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::select(sexo, ind_embriaguez) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_pedestre_envolvido$sexo_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexo masculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexo feminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'sem informacao', sexo)) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::select(sexo, ind_pedestre_envolvido) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_evasao$sexo_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexo masculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexo feminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'sem informacao', sexo)) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::select(sexo, ind_evasao) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_fatalidade$sexo_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexo masculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexo feminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'sem informacao', sexo)) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::select(sexo, ind_fatalidade) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$numero_pessoas_envolvidas$sexo_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexo masculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexo feminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'sem informacao', sexo)) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::select(sexo, num_pessoas_envolvidas) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$numero_veiculos_envolvidos$sexo_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexo masculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexo feminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'sem informacao', sexo)) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::select(sexo, num_veiculos_envolvidos) %>% 
    dplyr::filter(complete.cases(.)) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$faixa_idade_condutor$sexo_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, idade) %>% 
    dplyr::mutate(aux = 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate20", "de20a29", "de30a39", "de40a49", 
                                             "de50a59", "de60a69", "de70oumais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'sem informacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade, aux) %>% 
    tidyr::spread(cut_idade, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(
      db_ocorrencias_transito_bh$pessoas %>% 
        dplyr::filter(ind_condutor == 1) %>% 
        dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexo masculino', sexo)) %>%
        dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexo feminino', sexo)) %>%
        dplyr::mutate(sexo = ifelse(is.na(sexo), 'sem informacao', sexo)) %>%
        dplyr::distinct(num_boletim, sexo),
      by = 'num_boletim') %>% 
    dplyr::group_by(sexo) %>%
    dplyr::summarise(
      'ate 20' = sum(ate20), 
      'de 20 a 29' = sum(de20a29), 
      'de 30 a 39' = sum(de30a39), 
      'de 40 a 49' = sum(de40a49),
      'de 50 a 59' = sum(de50a59), 
      'de 60 a 69' = sum(de60a69), 
      'de 70 ou mais' = sum(de70oumais)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()
  
}
lst_tables$ferimentos_condutor$sexo_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(aux = 1) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>% 
    dplyr::mutate(desc_severidade_acidente = stringr::str_remove_all(desc_severidade_acidente, '\\s+')) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente, aux) %>% 
    tidyr::spread(desc_severidade_acidente, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(
      db_ocorrencias_transito_bh$pessoas %>% 
        dplyr::filter(ind_condutor == 1) %>% 
        dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexo masculino', sexo)) %>%
        dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexo feminino', sexo)) %>%
        dplyr::mutate(sexo = ifelse(is.na(sexo), 'sem informacao', sexo)) %>%
        dplyr::distinct(num_boletim, sexo),
      by = 'num_boletim') %>% 
    dplyr::group_by(sexo) %>%
    dplyr::summarise(
      'fatal' = sum(fatal), 
      'nao fatal' = sum(naofatal), 
      'sem ferimentos' = sum(semferimentos), 
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()
  
}
lst_tables$indicio_uso_cinto_seguranca$sexo_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexo masculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexo feminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'sem informacao', sexo)) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::inner_join(
      db_ocorrencias_transito_bh$pessoas %>% 
        dplyr::filter(ind_condutor == 1) %>% 
        dplyr::group_by(num_boletim) %>% 
        dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
        dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
        dplyr::ungroup(), 
      by = 'num_boletim') %>% 
    dplyr::select(sexo, ind_cinto_seguranca) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$periodo_dia$sexo_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::select(num_boletim, sexo) %>% 
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexo masculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexo feminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'sem informacao', sexo)) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::select(periodo, sexo) %>% 
    dplyr::mutate(periodo = factor(periodo, levels = c('manha', 'tarde', 'noite', 'madrugada'))) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::select(sexo, periodo) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$tipo_acidente$sexo_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexo masculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexo feminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'sem informacao', sexo)) %>%
    dplyr::select(sexo, desc_acidente) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$descricao_veiculos$sexo_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::inner_join(
      db_ocorrencias_transito_bh$ocorrencias %>% 
        dplyr::group_by(desc_veiculos) %>% 
        dplyr::summarise(n = dplyr::n()) %>% 
        dplyr::arrange(desc(n)) %>% 
        dplyr::slice(1:15),
      by = 'desc_veiculos') %>% 
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexo masculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexo feminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'sem informacao', sexo)) %>%
    dplyr::select(sexo, desc_veiculos) %>% 
    table() %>% 
    chisq.test()
  
}

lst_tables$indicio_embriaguez_acidente$faixa_idade_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate 20", "de 20 a 29", "de 30 a 39", "de 40 a 49", 
                                             "de 50 a 59", "de 60 a 69", "de 70 ou mais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'sem informacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::select(cut_idade, ind_embriaguez) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_pedestre_envolvido$faixa_idade_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate 20", "de 20 a 29", "de 30 a 39", "de 40 a 49", 
                                             "de 50 a 59", "de 60 a 69", "de 70 ou mais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'sem informacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::select(cut_idade, ind_pedestre_envolvido) %>% 
    table() %>% 
    chisq.test()
}
lst_tables$indicio_evasao$faixa_idade_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate 20", "de 20 a 29", "de 30 a 39", "de 40 a 49", 
                                             "de 50 a 59", "de 60 a 69", "de 70 ou mais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'sem informacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>%
    dplyr::select(cut_idade, ind_evasao) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_fatalidade$faixa_idade_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate 20", "de 20 a 29", "de 30 a 39", "de 40 a 49", 
                                             "de 50 a 59", "de 60 a 69", "de 70 ou mais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'sem informacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::select(cut_idade, ind_fatalidade) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$numero_pessoas_envolvidas$faixa_idade_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate 20", "de 20 a 29", "de 30 a 39", "de 40 a 49", 
                                             "de 50 a 59", "de 60 a 69", "de 70 ou mais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'sem informacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::select(cut_idade, num_pessoas_envolvidas) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$numero_veiculos_envolvidos$faixa_idade_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate 20", "de 20 a 29", "de 30 a 39", "de 40 a 49", 
                                             "de 50 a 59", "de 60 a 69", "de 70 ou mais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'sem informacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::select(cut_idade, num_veiculos_envolvidos) %>% 
    dplyr::filter(complete.cases(.)) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$sexo_condutor$faixa_idade_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>% 
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexomasculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexofeminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'seminformacao', sexo)) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(sexo, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(
      db_ocorrencias_transito_bh$pessoas %>% 
        dplyr::filter(ind_condutor == 1) %>% 
        dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                      labels = c("ate 20", "de 20 a 29", "de 30 a 39", "de 40 a 49", 
                                                 "de 50 a 59", "de 60 a 69", "de 70 ou mais"))) %>% 
        dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'sem informacao', as.character(cut_idade))) %>% 
        dplyr::distinct(num_boletim, cut_idade),
      by = 'num_boletim') %>% 
    dplyr::group_by(cut_idade) %>%
    dplyr::summarise(
      'sexo masculino' = sum(sexomasculino),
      'sexo feminino' = sum(sexofeminino),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()
  
}
lst_tables$ferimentos_condutor$faixa_idade_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>% 
    dplyr::mutate(desc_severidade_acidente = stringr::str_remove_all(desc_severidade_acidente, '\\s+')) %>% 
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(desc_severidade_acidente, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(
      db_ocorrencias_transito_bh$pessoas %>% 
        dplyr::filter(ind_condutor == 1) %>% 
        dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                      labels = c("ate 20", "de 20 a 29", "de 30 a 39", "de 40 a 49", 
                                                 "de 50 a 59", "de 60 a 69", "de 70 ou mais"))) %>% 
        dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'sem informacao', as.character(cut_idade))) %>% 
        dplyr::distinct(num_boletim, cut_idade),
      by = 'num_boletim') %>% 
    dplyr::group_by(cut_idade) %>%
    dplyr::summarise(
      'fatal' = sum(fatal),
      'nao fatal' = sum(naofatal),
      'sem ferimentos' = sum(semferimentos),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()
  
}
lst_tables$indicio_uso_cinto_seguranca$faixa_idade_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate 20", "de 20 a 29", "de 30 a 39", "de 40 a 49", 
                                             "de 50 a 59", "de 60 a 69", "de 70 ou mais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'sem informacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::inner_join(
      db_ocorrencias_transito_bh$pessoas %>% 
        dplyr::filter(ind_condutor == 1) %>% 
        dplyr::group_by(num_boletim) %>% 
        dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
        dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
        dplyr::ungroup(), 
      by = 'num_boletim') %>% 
    dplyr::select(cut_idade, ind_cinto_seguranca) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$periodo_dia$faixa_idade_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate 20", "de 20 a 29", "de 30 a 39", "de 40 a 49", 
                                             "de 50 a 59", "de 60 a 69", "de 70 ou mais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'sem informacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::select(cut_idade, periodo) %>% 
    table() %>% 
    chisq.test()  
}
lst_tables$tipo_acidente$faixa_idade_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate 20", "de 20 a 29", "de 30 a 39", "de 40 a 49", 
                                             "de 50 a 59", "de 60 a 69", "de 70 ou mais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'sem informacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::select(cut_idade, desc_acidente) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$descricao_veiculos$faixa_idade_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate 20", "de 20 a 29", "de 30 a 39", "de 40 a 49", 
                                             "de 50 a 59", "de 60 a 69", "de 70 ou mais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'sem informacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::inner_join(
      db_ocorrencias_transito_bh$ocorrencias %>% 
        dplyr::group_by(desc_veiculos) %>% 
        dplyr::summarise(n = dplyr::n()) %>% 
        dplyr::arrange(desc(n)) %>% 
        dplyr::slice(1:15),
      by = 'desc_veiculos') %>%  
    dplyr::select(cut_idade, desc_veiculos) %>% 
    table() %>% 
    chisq.test()
  
}

lst_tables$indicio_embriaguez_acidente$ferimentos_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>%
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>% 
    dplyr::select(desc_severidade_acidente, ind_embriaguez) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_pedestre_envolvido$ferimentos_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::select(num_boletim, desc_severidade_acidente) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>% 
    dplyr::distinct() %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::select(desc_severidade_acidente, ind_pedestre_envolvido) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_evasao$ferimentos_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::select(num_boletim, desc_severidade_acidente) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>%
    dplyr::select(desc_severidade_acidente, ind_evasao) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_fatalidade$ferimentos_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::select(desc_severidade_acidente, ind_fatalidade) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$numero_pessoas_envolvidas$ferimentos_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::select(desc_severidade_acidente, num_pessoas_envolvidas) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$numero_veiculos_envolvidos$ferimentos_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::select(desc_severidade_acidente, num_veiculos_envolvidos) %>%
    dplyr::filter(complete.cases(.)) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$sexo_condutor$ferimentos_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>% 
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexomasculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexofeminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'seminformacao', sexo)) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(sexo, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(
      db_ocorrencias_transito_bh$pessoas %>% 
        dplyr::filter(ind_condutor == 1) %>% 
        dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>% 
        dplyr::distinct(num_boletim, desc_severidade_acidente),
      by = 'num_boletim') %>% 
    dplyr::group_by(desc_severidade_acidente) %>%
    dplyr::summarise(
      'sexo masculino' = sum(sexomasculino),
      'sexo feminino' = sum(sexofeminino),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()
  
}
lst_tables$faixa_idade_condutor$ferimentos_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::select(num_boletim, idade) %>% 
    dplyr::mutate(cut_idade = cut(idade, 
                                  breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate20", "de20a29", "de30a39", 
                                             "de40a49", "de50a59", "de60a69", "de70oumais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'seminformacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>% 
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(cut_idade, aux) %>%
    dplyr::inner_join(
      db_ocorrencias_transito_bh$pessoas %>%
        dplyr::filter(ind_condutor == 1) %>%
        dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>% 
        dplyr::distinct(num_boletim, desc_severidade_acidente), 
      by = 'num_boletim') %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::group_by(desc_severidade_acidente) %>%
    dplyr::summarise(
      'ate 20' = sum(ate20),
      'de 20 a 29' = sum(de20a29),
      'de 30 a 39' = sum(de30a39),
      'de 40 a 49' = sum(de40a49),
      'de 50 a 59' = sum(de50a59),
      'de 60 a 69' = sum(de60a69),
      'de 70 ou mais' = sum(de70oumais),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()
  
}
lst_tables$indicio_uso_cinto_seguranca$ferimentos_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>% 
    dplyr::inner_join(
      db_ocorrencias_transito_bh$pessoas %>% 
        dplyr::filter(ind_condutor == 1) %>% 
        dplyr::group_by(num_boletim) %>% 
        dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
        dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
        dplyr::ungroup(), 
      by = 'num_boletim') %>% 
    dplyr::select(desc_severidade_acidente, ind_cinto_seguranca) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$periodo_dia$ferimentos_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::select(num_boletim, desc_severidade_acidente) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>%
    dplyr::distinct() %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%    
    dplyr::select(desc_severidade_acidente, periodo) %>%
    table() %>% 
    chisq.test()
}
lst_tables$tipo_acidente$ferimentos_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>% 
    dplyr::select(desc_severidade_acidente, desc_acidente) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$descricao_veiculos$ferimentos_condutor$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::inner_join(
      db_ocorrencias_transito_bh$ocorrencias %>% 
        dplyr::group_by(desc_veiculos) %>% 
        dplyr::summarise(n = dplyr::n()) %>% 
        dplyr::arrange(desc(n)) %>% 
        dplyr::slice(1:15),
      by = 'desc_veiculos') %>%  
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>% 
    dplyr::select(desc_severidade_acidente, desc_veiculos) %>% 
    table() %>% 
    chisq.test()
}

lst_tables$indicio_embriaguez_acidente$indicio_uso_cinto_seguranca$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::select(ind_cinto_seguranca, ind_embriaguez) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_pedestre_envolvido$indicio_uso_cinto_seguranca$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::select(ind_cinto_seguranca, ind_pedestre_envolvido) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_evasao$indicio_uso_cinto_seguranca$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::select(ind_cinto_seguranca, ind_evasao) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_fatalidade$indicio_uso_cinto_seguranca$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::select(ind_cinto_seguranca, ind_fatalidade) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$numero_pessoas_envolvidas$indicio_uso_cinto_seguranca$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%    
    dplyr::select(ind_cinto_seguranca, num_pessoas_envolvidas) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$numero_veiculos_envolvidos$indicio_uso_cinto_seguranca$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::select(ind_cinto_seguranca, num_veiculos_envolvidos) %>%
    dplyr::filter(complete.cases(.)) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$sexo_condutor$indicio_uso_cinto_seguranca$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(
      db_ocorrencias_transito_bh$pessoas %>% 
        dplyr::filter(ind_condutor == 1) %>% 
        dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexo masculino', sexo)) %>%
        dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexo feminino', sexo)) %>%
        dplyr::mutate(sexo = ifelse(is.na(sexo), 'sem informacao', sexo)) %>%
        dplyr::distinct(num_boletim, sexo),
      by = 'num_boletim') %>% 
    dplyr::select(sexo, ind_cinto_seguranca) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$faixa_idade_condutor$indicio_uso_cinto_seguranca$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(
      db_ocorrencias_transito_bh$pessoas %>% 
        dplyr::filter(ind_condutor == 1) %>% 
        dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                      labels = c("ate20", "de20a29", "de30a39", "de40a49", 
                                                 "de50a59", "de60a69", "de70oumais"))) %>% 
        dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'seminformacao', as.character(cut_idade))) %>% 
        dplyr::distinct(num_boletim, cut_idade) %>% 
        dplyr::mutate(aux = 1) %>% 
        tidyr::spread(cut_idade, aux) %>% 
        dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}),
      by = 'num_boletim') %>% 
    dplyr::group_by(ind_cinto_seguranca) %>%
    dplyr::summarise(
      'ate 20' = sum(ate20),
      'de 20 a 29' = sum(de20a29),
      'de 30 a 39' = sum(de30a39),
      'de 40 a 49' = sum(de40a49),
      'de 50 a 59' = sum(de50a59),
      'de 60 a 69' = sum(de60a69),
      'de 70 ou mais' = sum(de70oumais),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()
  
}
lst_tables$ferimentos_condutor$indicio_uso_cinto_seguranca$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(
      db_ocorrencias_transito_bh$pessoas %>% 
        dplyr::filter(ind_condutor == 1) %>% 
        dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>% 
        dplyr::distinct(num_boletim, desc_severidade_acidente) %>% 
        dplyr::mutate(desc_severidade_acidente = stringr::str_remove_all(desc_severidade_acidente, '\\s+')) %>% 
        dplyr::mutate(aux = 1) %>% 
        tidyr::spread(desc_severidade_acidente, aux) %>% 
        dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}),
      by = 'num_boletim') %>% 
    dplyr::group_by(ind_cinto_seguranca) %>%
    dplyr::summarise(
      fatal = sum(fatal),
      'nao fatal' = sum(naofatal),
      'sem ferimentos'= sum(semferimentos),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()
  
}
lst_tables$periodo_dia$indicio_uso_cinto_seguranca$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::select(ind_cinto_seguranca, periodo) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$tipo_acidente$indicio_uso_cinto_seguranca$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>%
    dplyr::select(ind_cinto_seguranca, desc_acidente) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$descricao_veiculos$indicio_uso_cinto_seguranca$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>%
    dplyr::inner_join(
      db_ocorrencias_transito_bh$ocorrencias %>% 
        dplyr::group_by(desc_veiculos) %>% 
        dplyr::summarise(n = dplyr::n()) %>% 
        dplyr::arrange(desc(n)) %>% 
        dplyr::slice(1:15),
      by = 'desc_veiculos') %>%  
    dplyr::select(ind_cinto_seguranca, desc_veiculos) %>% 
    table() %>% 
    chisq.test()
  
}

lst_tables$indicio_embriaguez_acidente$tipo_acidente$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::select(desc_acidente, ind_embriaguez) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_pedestre_envolvido$tipo_acidente$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::select(desc_acidente, ind_pedestre_envolvido) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_evasao$tipo_acidente$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::select(desc_acidente, ind_evasao) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_fatalidade$tipo_acidente$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::select(desc_acidente, ind_fatalidade) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$numero_pessoas_envolvidas$tipo_acidente$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::select(desc_acidente, num_pessoas_envolvidas) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$numero_veiculos_envolvidos$tipo_acidente$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::select(desc_acidente, num_veiculos_envolvidos) %>% 
    dplyr::filter(complete.cases(.)) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$sexo_condutor$tipo_acidente$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>% 
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexomasculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexofeminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'seminformacao', sexo)) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(sexo, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::group_by(desc_acidente) %>%
    dplyr::summarise(
      'sexo masculino' = sum(sexomasculino),
      'sexo feminino' = sum(sexofeminino),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()
  
}
lst_tables$faixa_idade_condutor$tipo_acidente$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate20", "de20a29", "de30a39", "de40a49", 
                                             "de50a59", "de60a69", "de70oumais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'seminformacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>% 
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(cut_idade, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::group_by(desc_acidente) %>% 
    dplyr::summarise(
      'ate 20' = sum(ate20),
      'de 20 a 29' = sum(de20a29),
      'de 30 a 39' = sum(de30a39),
      'de 40 a 49' = sum(de40a49),
      'de 50 a 59' = sum(de50a59),
      'de 60 a 69' = sum(de60a69),
      'de 70 ou mais' = sum(de70oumais),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()
  
  
}
lst_tables$ferimentos_condutor$tipo_acidente$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>% 
    dplyr::mutate(desc_severidade_acidente = stringr::str_remove_all(desc_severidade_acidente, '\\s+')) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::select(num_boletim, desc_acidente, desc_severidade_acidente) %>% 
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(desc_severidade_acidente, aux) %>%     
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>% 
    dplyr::group_by(desc_acidente) %>%
    dplyr::summarise(
      fatal = sum(fatal),
      'nao fatal' = sum(naofatal),
      'sem ferimentos'= sum(semferimentos),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()
  
}
lst_tables$indicio_uso_cinto_seguranca$tipo_acidente$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-soma) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::select(desc_acidente, ind_cinto_seguranca) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$periodo_dia$tipo_acidente$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>%
    dplyr::select(desc_acidente, periodo) %>% 
    table() %>% 
    chisq.test()
  
}

lst_tables$indicio_embriaguez_acidente$descricao_veiculos$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::group_by(desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    dplyr::slice(1:15) %>% 
    dplyr::select(desc_veiculos) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = c('desc_veiculos')) %>% 
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::select(desc_veiculos, ind_embriaguez) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_pedestre_envolvido$descricao_veiculos$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::group_by(desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    dplyr::slice(1:15) %>% 
    dplyr::select(desc_veiculos) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = c('desc_veiculos')) %>% 
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>%
    dplyr::select(desc_veiculos, ind_pedestre_envolvido) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_evasao$descricao_veiculos$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::group_by(desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    dplyr::slice(1:15) %>% 
    dplyr::select(desc_veiculos) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = c('desc_veiculos')) %>% 
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::select(desc_veiculos, ind_evasao) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_fatalidade$descricao_veiculos$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::group_by(desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    dplyr::slice(1:15) %>% 
    dplyr::select(desc_veiculos) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = c('desc_veiculos')) %>% 
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::select(desc_veiculos, ind_fatalidade) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$numero_pessoas_envolvidas$descricao_veiculos$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::group_by(desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    dplyr::slice(1:15) %>% 
    dplyr::select(desc_veiculos) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = c('desc_veiculos')) %>% 
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::select(desc_veiculos, num_pessoas_envolvidas) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$numero_veiculos_envolvidos$descricao_veiculos$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::group_by(desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    dplyr::slice(1:15) %>% 
    dplyr::select(desc_veiculos) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = c('desc_veiculos')) %>% 
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::select(desc_veiculos, num_veiculos_envolvidos) %>% 
    dplyr::filter(complete.cases(.)) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$sexo_condutor$descricao_veiculos$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>% 
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexomasculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexofeminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'seminformacao', sexo)) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(sexo, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::inner_join(
      db_ocorrencias_transito_bh$ocorrencias %>% 
        dplyr::select(desc_veiculos) %>% 
        dplyr::group_by(desc_veiculos) %>% 
        dplyr::summarise(n = dplyr::n()) %>% 
        dplyr::arrange(desc(n)) %>% 
        dplyr::slice(1:15) %>% 
        dplyr::select(desc_veiculos),
      by = 'desc_veiculos') %>% 
    dplyr::group_by(desc_veiculos) %>%
    dplyr::summarise(
      'sexo masculino' = sum(sexomasculino),
      'sexo feminino' = sum(sexofeminino),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()
}
lst_tables$faixa_idade_condutor$descricao_veiculos$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate20", "de20a29", "de30a39", "de40a49", 
                                             "de50a59", "de60a69", "de70oumais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'seminformacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>% 
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(cut_idade, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::inner_join(
      db_ocorrencias_transito_bh$ocorrencias %>% 
        dplyr::group_by(desc_veiculos) %>% 
        dplyr::summarise(n = dplyr::n()) %>% 
        dplyr::arrange(desc(n)) %>% 
        dplyr::slice(1:15), 
      by = 'desc_veiculos') %>% 
    dplyr::group_by(desc_veiculos) %>% 
    dplyr::summarise(
      'ate 20' = sum(ate20),
      'de 20 a 29' = sum(de20a29),
      'de 30 a 39' = sum(de30a39),
      'de 40 a 49' = sum(de40a49),
      'de 50 a 59' = sum(de50a59),
      'de 60 a 69' = sum(de60a69),
      'de 70 ou mais' = sum(de70oumais),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()
  
}
lst_tables$ferimentos_condutor$descricao_veiculos$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem informacao', desc_severidade_acidente)) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>% 
    dplyr::mutate(desc_severidade_acidente = stringr::str_remove_all(desc_severidade_acidente, '\\s+')) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>%
    dplyr::inner_join(
      db_ocorrencias_transito_bh$ocorrencias %>% 
        dplyr::group_by(desc_veiculos) %>% 
        dplyr::summarise(n = dplyr::n()) %>% 
        dplyr::arrange(desc(n)) %>% 
        dplyr::slice(1:15),
      by = 'desc_veiculos') %>% 
    dplyr::select(num_boletim, desc_veiculos, desc_severidade_acidente) %>% 
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(desc_severidade_acidente, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>% 
    dplyr::group_by(desc_veiculos) %>%
    dplyr::summarise(
      fatal = sum(fatal),
      'nao fatal' = sum(naofatal),
      'sem ferimentos'= sum(semferimentos),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()
  
}
lst_tables$indicio_uso_cinto_seguranca$descricao_veiculos$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-soma) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::inner_join(
      db_ocorrencias_transito_bh$ocorrencias %>% 
        dplyr::group_by(desc_veiculos) %>% 
        dplyr::summarise(n = dplyr::n()) %>% 
        dplyr::arrange(desc(n)) %>% 
        dplyr::slice(1:15),
      by = 'desc_veiculos') %>% 
    dplyr::select(desc_veiculos, ind_cinto_seguranca) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$periodo_dia$descricao_veiculos$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>%
    dplyr::inner_join(
      db_ocorrencias_transito_bh$ocorrencias %>% 
        dplyr::group_by(desc_veiculos) %>% 
        dplyr::summarise(n = dplyr::n()) %>% 
        dplyr::arrange(desc(n)) %>% 
        dplyr::slice(1:15),
      by = 'desc_veiculos') %>% 
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::select(desc_veiculos, periodo) %>% 
    table() %>% 
    chisq.test()
  
}

lst_tables$indicio_pedestre_envolvido$indicio_embriaguez_acidente$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com pedestre', 'sem pedestre')) %>% 
    dplyr::select(ind_embriaguez, ind_pedestre_envolvido) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_evasao$indicio_embriaguez_acidente$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com evasao', 'sem evasao')) %>% 
    dplyr::select(ind_embriaguez, ind_evasao) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$indicio_fatalidade$indicio_embriaguez_acidente$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com fatalidade', 'sem fatalidade')) %>% 
    dplyr::select(ind_embriaguez, ind_fatalidade) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$numero_pessoas_envolvidas$indicio_embriaguez_acidente$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>%
    dplyr::select(ind_embriaguez, num_pessoas_envolvidas) %>%
    table() %>% 
    chisq.test()
}
lst_tables$numero_veiculos_envolvidos$indicio_embriaguez_acidente$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>%
    dplyr::select(ind_embriaguez, num_veiculos_envolvidos) %>%
    dplyr::filter(complete.cases(.)) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$sexo_condutor$indicio_embriaguez_acidente$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::distinct(num_boletim, sexo) %>% 
    dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexomasculino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexofeminino', sexo)) %>%
    dplyr::mutate(sexo = ifelse(is.na(sexo), 'seminformacao', sexo)) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(sexo, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::group_by(ind_embriaguez) %>%
    dplyr::summarise(
      'sexo masculino' = sum(sexomasculino),
      'sexo feminino' = sum(sexofeminino),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()

}
lst_tables$faixa_idade_condutor$indicio_embriaguez_acidente$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                  labels = c("ate20", "de20a29", "de30a39", "de40a49", 
                                             "de50a59", "de60a69", "de70oumais"))) %>% 
    dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'seminformacao', as.character(cut_idade))) %>% 
    dplyr::distinct(num_boletim, cut_idade) %>% 
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(cut_idade, aux) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>%
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::group_by(ind_embriaguez) %>% 
    dplyr::summarise(
      'ate 20' = sum(ate20),
      'de 20 a 29' = sum(de20a29),
      'de 30 a 39' = sum(de30a39),
      'de 40 a 49' = sum(de40a49),
      'de 50 a 59' = sum(de50a59),
      'de 60 a 69' = sum(de60a69),
      'de 70 ou mais' = sum(de70oumais),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()

}
lst_tables$ferimentos_condutor$indicio_embriaguez_acidente$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'seminformacao', desc_severidade_acidente)) %>% 
    dplyr::mutate(desc_severidade_acidente = stringr::str_remove_all(desc_severidade_acidente, '\\s+')) %>% 
    dplyr::distinct(num_boletim, desc_severidade_acidente) %>%
    dplyr::mutate(aux = 1) %>% 
    tidyr::spread(desc_severidade_acidente, aux) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::mutate_if(.predicate = is.numeric, .funs = function(x){ifelse(is.na(x), 0, x)}) %>% 
    dplyr::group_by(ind_embriaguez) %>%
    dplyr::summarise(
      fatal = sum(fatal),
      'nao fatal' = sum(naofatal),
      'sem ferimentos'= sum(semferimentos),
      'sem informacao' = sum(seminformacao)
    ) %>%
    dplyr::select_if(.predicate = is.numeric) %>% 
    chisq.test()
  
}
lst_tables$indicio_uso_cinto_seguranca$indicio_embriaguez_acidente$teste_chisq <- {
  
  db_ocorrencias_transito_bh$pessoas %>% 
    dplyr::filter(ind_condutor == 1) %>% 
    dplyr::group_by(num_boletim) %>% 
    dplyr::summarise(soma = sum(ind_cinto_seguranca, na.rm = TRUE)/dplyr::n()) %>% 
    dplyr::mutate(ind_cinto_seguranca = ifelse(soma == 1, 'com cinto seguranca', 'sem cinto seguranca')) %>% 
    dplyr::select(-soma) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::select(ind_embriaguez, ind_cinto_seguranca) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$periodo_dia$indicio_embriaguez_acidente$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>%
    dplyr::select(periodo, ind_embriaguez) %>%
    dplyr::mutate(periodo = factor(periodo, c('manha', 'tarde', 'noite', 'madrugada'))) %>%
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::select(ind_embriaguez, periodo) %>%
    table() %>% 
    chisq.test()
  
}
lst_tables$tipo_acidente$indicio_embriaguez_acidente$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::select(ind_embriaguez, desc_acidente) %>% 
    table() %>% 
    chisq.test()
  
}
lst_tables$descricao_veiculos$indicio_embriaguez_acidente$teste_chisq <- {
  
  db_ocorrencias_transito_bh$ocorrencias %>% 
    dplyr::group_by(desc_veiculos) %>% 
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    dplyr::slice(1:15) %>% 
    dplyr::select(desc_veiculos) %>% 
    dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = c('desc_veiculos')) %>% 
    dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com embriaguez', 'sem embriaguez')) %>%
    dplyr::select(ind_embriaguez, desc_veiculos) %>% 
    table() %>% 
    chisq.test()
}




lst_tables$indicio_embriaguez_acidente$comentario <- {
  
  "A tabela mostra a distribuição do indício de embriaguez nos acidentes 
  relatados. Se nota que 3,6% das ocorrências teve pelo menos um dos 
  envolvidos com sinais de embriaguez."
  
}

lst_tables$indicio_embriaguez_acidente$periodo_dia$comentario <- {
  
 "Nessa tabela há a distribuição de acidentes relatados envolvendo 
  embriaguez por período do dia em que as ocorrências aconteceram.  
  Note que em 1,45% dos acidentes no período da manhã teve pelo menos 
  um envolvido com indício de embriaguez e esse percentual cresceu ao 
  longo do dia, chegando a mais de 14% dos acidentes relatados nas
  madrugadas tendo pelo menos uma das pessoas envolvidas com sinais 
  de embriaguez."
  
}
lst_tables$indicio_embriaguez_acidente$indicio_pedestre_envolvido$comentario <- {
  
  "Na tabela abaixo há a distribuição de acidentes relatados envolvendo 
  embriaguez por envolvimento de pedestre nas ocorrências. Podemos verificar 
  que nos acidentes relatados com pedestre(s), em 8,3% deles havia 
  indício de embriaguez de pelo menos um dos envolvidos. Quando não houve 
  pedestre envolvido nas ocorrências, o respectivo percentual é bem menor, 2,85%.
  Parece haver uma relação considerável entre acidente com algum dos 
  envolvidos (condutor, passageiro ou pedestre) com sinais de embriaguez 
  e o envolvimento de pedestres."

}
lst_tables$indicio_embriaguez_acidente$indicio_evasao$comentario <- {
  
  "A tabela exibida aqui mostra a distribuição de acidentes relatados envolvendo 
  embriaguez por indício ou não de evasão de pelo menos um dos envolvidos. Veja  
  que quando houve indício de evasão de pelo menos uma pessoa, o percentual 
  observado de acidentes com embriaguez foi de 1,96%. Já nos acidentes 
  relatados com todos os envolvidos presentes no momento do registro do boletim, 
  o percentual de embriaguez nas ocorrências foi de 3,71%. A interpretação dessa tabela fica 
  prejudicada, já que quando houve evasão de pelo menos uma dos envolvidos, o agente público 
  responsável pela ocorrência não conseguiu checar se todos as pessoas tinham ou não sinais 
  de embriaguez."
  
}
lst_tables$indicio_embriaguez_acidente$indicio_fatalidade$comentario <- {
  
  "Aqui há a distribuição de acidentes relatados envolvendo 
  embriaguez por fatalidade nas ocorrências. Podemos verificar 
  que nas ocorrências de acidentes com fatalidade de uma das pessoas envolvidas, 
  em 5,4% delas havia pelo menos uma pessoa com indício de embriaguez. 
  Quando os acidentes não tiveram vítimas fatais, o percentual de embriaguez foi de 3,6%."

}
lst_tables$indicio_embriaguez_acidente$numero_pessoas_envolvidas$comentario <- {
  
  "Nessa tabela há a distribuição do indício de embriaguez nas ocorrências e o 
  número de pessoas envolvidas. Notemos que quando foi relatado 5 pessoas ou mais 
  nos acidentes, em 11% das vezes pelo menos uma delas apresentava sinais de 
  embriaguez."
  
}
lst_tables$indicio_embriaguez_acidente$numero_veiculos_envolvidos$comentario <- {
  
  "A tabela abaixo relaciona a distribuição do indício de embriaguez nas ocorrências e o 
  número de pessoas envolvidas. Veja que curioso, dos acidentes ocorridos com 2 veículos, 
  que representam cerca de 2/3 das ocorrências, em 2,36% deles tinha pelo menos um dos 
  envolvidos com indício de embriaguez. Em acidentes relatados com outras quantidades de 
  veículos, os percentuais de sinais de embriaguez em pelo menos um dos envolvidos nas 
  ocorrências foram maiores."
  
}
lst_tables$indicio_embriaguez_acidente$sexo_condutor$comentario <- {
  
  "A tabela abaixo mostra a distribuição do indício de embriaguez nas ocorrências por 
  sexo dos condutores que participaram daqueles acidentes. Ocorrências que estão em uma 
  das linhas também podem estar presentes nas outras, pois em um acidente podemos ter 
  um condutor do sexo masculino e também uma condutora do sexo feminino, por exemplo. 
  Dos acidentes com pelo menos um condutor do sexo masculino, em 3,6% deles teve indício 
  de embriaguez de pelo menos uma das pessoas envolvidas. Para acidentes envolvendo 
  condutoras do sexo feminino, o respectivo percentual foi de 2,16% e em 1,82% das 
  ocorrências em que não foi informado o sexo de pelo menos um dos condutores envolvidos 
  houve indício de embriaguez."
}
lst_tables$indicio_embriaguez_acidente$faixa_idade_condutor$comentario <- {
  
  "Essa tabela mostra a distribuição do indício de embriaguez nos acidentes que envolveram 
  pelo menos um condutor de determinada faixa etária. Os acidentes contidos em uma linha podem 
  estar sendo contados novamente em outras linhas, pois condutores de faixas etárias diferentes 
  podem ter se envolvido nos mesmos acidentes. Note que houve indício de embriaguez em 3,5% 
  dos acidentes relatados que envolviam pelo menos um condutor com  idade entre 30 a 39 anos. 
  Esse percentual permaneceu entre 2,5% e 3,5% para todas as outras faixas de idade, exceto 
  para os acidentes envolvendo condutores de 70 anos ou mais."
  
}
lst_tables$indicio_embriaguez_acidente$ferimentos_condutor$comentario <- {
  
  "Essa tabela mostra a distribuição do indício de embriaguez nas ocorrências por grau de 
  ferimentos de pelo menos um dos condutores. Os registros nas linhas da tabela não são 
  mutuamente exclusivos, ou seja, em linhas diferentes podem estar sendo contados os mesmos 
  acidentes, pois em uma ocorrência um condutor pode ter tido um grau de ferimento e o(s) 
  outro(s) condutor(es) um grau diferente. Se nota que em acidentes relatados 
  com pelo menos um condutor como vítima fatal, o percentual de indício de embriaguez nessas  
  ocorrências foi de 2,88%. Os percentuais de indício de embriaguez nas ocorrências com outros 
  graude de ferimenot de pelo menos um dos condutores foram próximos a esse valor."
  
}
lst_tables$indicio_embriaguez_acidente$indicio_uso_cinto_seguranca$comentario <- {
  
  "Aqui temos a distribuição de indício de embriaguez nas ocorrências e a falta do uso do cinto 
  de segurança de pelo menos um dos condutores envolvidos. Observe que nas ocorrências em que 
  todos os condutores estavam usando o cinto de segurança, em 3,69% delas teve indício de embriaguez 
  de pelo menos uma das pessoas envolvidas. Para acidentes onde pelo menos um dos 
  condutores não estava usando o cinto, o respectivo percentual foi de 3,41%."
  
}
lst_tables$indicio_embriaguez_acidente$tipo_acidente$comentario <- {
  
  "Essa tabela relaciona o indício de embriaguez nas ocorrências e o tipo de acidente ocorrido. 
  Há informações muito interessantes aqui. Veja que nos atropelamentos de pedestres e em 
  colisões com objeto parado, o percentual de embriaguez nessas ocorrências foram bem maiores 
  que em outros tipos de acidentes, 8,35% e 6,1%, respectivamente."
  
}
lst_tables$indicio_embriaguez_acidente$descricao_veiculos$comentario <- {
  
  "A tabela abaixo relaciona o indício de embriaguez nos acidentes relatados e as 15 
  combinações mais frequentes de veículos envolvidos nas ocorrências. Acidentes relatados 
  em que há somente 1 automóvel envolvido apresentaram o maior percentual de embriaguez, 
  10,55% dessas ocorrências. Outras combinações de veículos onde os acidentes apresentaram 
  percentuais elevados de indício de embriaguez foram: acidentes com 1 automóvel e 1 
  caminhonete, acidentes relatados envolvendo 3 automóveis e acidentes relatados com 
  2 automóveis."
  
}


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

lst_tables$indicio_pedestre_envolvido$comentario <- {
  
  "A tabela abaixo mostra a distribuição do envolvimento de pedestres nas ocorrências. 
  Note que em 13,74% dos acidentes relatados teve pelo menos um pedestre envolvido."
  
}


lst_tables$indicio_pedestre_envolvido$periodo_dia$comentario <- {
  
  "A tabela exibe a distribuição de acidentes envolvendo pedestres 
  por período do dia em que a ocorrência aconteceu. Repare que em 16% dos acidentes 
  relatados a noite envolveram pedestres. Esse percentual foi de quase 12% nos acidentes 
  nas manhãs, 14% nas ocorrências das tardes e em torno de 10% nos acidentes que 
  aconteceram de madrugada."
  
}
lst_tables$indicio_pedestre_envolvido$indicio_embriaguez_acidente$comentario <- {
  
  "A tabela exibida mostra a distribuição de acidentes relatados com pedestres e o 
  indício de embriaguez ou não nas ocorrências. Existe uma informação muito alarmante 
  aqui. Em quase 32% dos acidentes com indício de embriaguez de pelo menos um dos 
  envolvidos teve também pedestre(s) envolvido(s). Nos acidentes relatados sem 
  embriaguez, o respectivo percentual foi de 13%."
  
}
lst_tables$indicio_pedestre_envolvido$indicio_evasao$comentario <- {
  
  "A tabela mostra a distribuição do envolvimento de pedestres nas 
  ocorrências por indício de evasão ou não nos acidentes. Aproximadamente em 21% 
  dos acidentes relatados com evasão também envolveram pedestres. Dos acidentes 
  sem indício de evasão, somente 13% deles tinha envolvimento de pedestres."
  
}
lst_tables$indicio_pedestre_envolvido$indicio_fatalidade$comentario <- {
  
  "Nessa tabela há a distribuição de envolvimento de pedestres nas ocorrências por 
  fatalidade ou não de pelo menos um dos envolvidos. Há uma informação preocupante  
  porém esperada. Cerca de 38% dos acidentes relatados com fatalidade de pelo 
  menos uma das pessoas envolvidas teve também a participação de pedestres. Nos acidentes 
  sem fatalidade, somente cerca de 13% deles envolveram pedestres. Essa grande 
  diferença era de se esperar, já que o pedestre é a parte mais frágil em um 
  acidente de trânsito."
  
}
lst_tables$indicio_pedestre_envolvido$numero_pessoas_envolvidas$comentario <- {
  
  "Aqui há a distribuição do envolvimento de pedestres nas ocorrências e o número de 
  pessoas envolvidas. Dos acidentes relatados com 2 pessoas envolvidas, em quase 18% 
  deles teve também a participação de pedestre, ou seja, 1 pedestre e 1 condutor envolvido.
  Esse percentual é consideravelmente maior que para acidentes com outras quantidades de 
  pessoas envolvidas. Nessa tabela também há uma inconsistência, 29 ocorrências foram 
  relatadas com uma pessoa envolvida e também com pedestre. Esse valor não faz sentido, 
  já que para um acidente de trânsito é necessário ter pelo menos um veículo envolvido."
  
}
lst_tables$indicio_pedestre_envolvido$numero_veiculos_envolvidos$comentario <- {
  
  "Aqui há a distribuição de pedestres envolvidos ou não por número de veículos 
  nas ocorrências. Veja que em 45% dos acidentes relatados com somente 1 veículo
  também teve pedestre(s) envolvido(s). Para ocorrências com mais
  veículos envolvidos, o percentual de envolvimento de pedestres foi bem menor."
  
}
lst_tables$indicio_pedestre_envolvido$sexo_condutor$comentario <- {
  
  "A tabela abaixo mostra a distribuição da presença de pedestres nas ocorrências 
  por participação por sexo de pelo menos um dos condutores. As ocorrências que estão 
  em uma das linhas também podem estar presentes nas outras, já que em um acidente 
  podemos ter um condutor do sexo masculino e também uma condutora do sexo feminino, 
  por exemplo. Notemos que dos acidentes relatado com pelo menos uma condutora do sexo 
  feminino envolvido, em 8% deles também teve envolvido pelo menos um pedestre.
  Esse respectivo percentual em acidentes envolvendo pelo menos um condutor do sexo 
  masculino é de 11% e em acidentes em que não houve informação do sexo de pelo menos 
  um dos condutores envolvidos esse percentual chegou a mais de 20%."
  
  
}
lst_tables$indicio_pedestre_envolvido$faixa_idade_condutor$comentario <- {
  
  "A tabela abaixo mostra a distribuição da presença de pedestres nas ocorrências 
  por participação por faixa etária de pelo menos um dos condutores. As linhas não 
  são mutuamente exclusivas aqui, ou seja, acidentes relatados envolvendo condutores de 
  uma faixa etária também podem estar relatados nos acidentes envolvendo condutores de 
  outra faixa etária, para isso basta que condutores de faixas etárias diferentes tenham 
  se envolvido na mesma ocorrência. Veja que nos acidentes relatados  
  envolvendo pelo menos um condutor de até 20 anos, o percentual de participação de 
  pedestres nessas ocorrências foi pouco maior que 6%, já para os acidentes que 
  envolveram pelo menos um condutor de 70 anos ou mais, esse percentual foi de 9,4%. 
  Em 17,45% dos acidentes onde não foi informada a idade de pelo menos um dos condutores
  teve pedestre(s) envolvido(s)."
  
}
lst_tables$indicio_pedestre_envolvido$ferimentos_condutor$comentario <- {
  
  "A tabela abaixo mostra a distribuição do envolvimento de pedestre nas ocorrências por 
  grau dos ferimentos de pelo menos um dos condutores também envolvidos. Os registros nas 
  linhas da tabela não são mutuamente exclusivos, ou seja, em linhas diferentes podem estar 
  sendo contados os mesmos acidentes, pois em uma ocorrência um condutor pode ter tido um 
  grau de ferimento e o(s) outro(s) condutor(es) um grau diferente. Observe que dos acidentes 
  relatados com pelo menos um condutor como vítima fatal, em quase 3% deles também teve o 
  envolvimento de pedestre(s). Para acidentes onde não foi relatado o grau dos ferimentos de 
  pelo menos um dos condutores, o percentual de envolvimento de pedestres foi bem maior, cerca de
  19%."

}
lst_tables$indicio_pedestre_envolvido$indicio_uso_cinto_seguranca$comentario <- {
  
  "Aqui há a distribuição do envolvimento ou não de pedestres nas ocorrências por falta do 
  uso do cinto de segurança de pelo menos um dos condutores envolvidos. Observe que nas 
  ocorrências em que todos os condutores estavam usando o cinto de segurança, o percentual 
  de envolvimento de pedestres foi de 13,31%. Para acidentes onde pelo menos um dos condutores 
  não estavam usando o cinto, esse percentual foi ligeiramente maior, 14,64% das ocorrências 
  envolveram pedestres."
  
}
lst_tables$indicio_pedestre_envolvido$tipo_acidente$comentario <- {
  
  "Nessa tabela há a distribuição do envolvimento de pedestre(s) nas ocorrências e o 
  tipo de acidente. Essa tabela pode parecer redundante mas nos dá informações importantes. 
  Como era de se esperar, quase 100% dos atropelamentos de pedestres possuem pedestres envolvidos. 
  Os 7 boletins de ocorrências assinalados como atropelamento de pedestre mas sem pedestre 
  envolvido, são inconsistências do conjunto de dados, ou seja, ou o tipo de acidente está errado 
  ou não foi coletada as informações do pedestre atropelado nessas ocorrências. Note que o 
  percentual de envolvimento de pedestre não é zero para os outros tipos de acidentes, pois 
  imagino eu que como acidentes de trânsto podem ser complexos, fica difícil explicá-los em 
  um boletim de ocorrência com campos pré-determinados."
  
}
lst_tables$indicio_pedestre_envolvido$descricao_veiculos$comentario <- {
  
  "A tabela abaixo mostra a distribuição do envolvimento de pedestres nos 
  acidentes relatados pelas 15 combinações de veículos com maior frequência de 
  envolvimento nas ocorrências. Acidentes relatados em que há somente 1 automóvel 
  envolvido apresentaram o maior percentual de envolvimento de pedestres, 64,26%. 
  Outras combinações de veículos que apresentaram percentuais elevados de envolvimento 
  de pedestres nas ocorrências foram: acidentes com somente 1 motocicleta e 
  acidentes relatados com somente 1 ônibus."
}


lst_tables$indicio_evasao$comentario <- {
  
  "A tabela abaixo mostra a distribuição do indício de evasão 
  nas ocorrências. Observe que em 6,76% dos acidentes contidos no conjunto de dados 
  tiveram pelo menos um dos envolvidos com indício de não ter esperado a lavratura 
  do boletim de ocorrência."
  
}

lst_tables$indicio_evasao$periodo_dia$comentario <- {
  
  "Na tabela abaixo temos a distribuição do indício de evasão nas ocorrências 
  por período do dia que os acidentes ocorreram. Veja que em 5,44% dos acidentes 
  relatados nas manhãs tiveram indício de evasão de pelo menos uma das pessoas envolvidas, 
  nos acidentes ocorridos a tarde o respectivo percentual foi de 5,93%. Nos acidentes 
  ocorridos nos períodos da noite e da madrugada, o percentual de indício de evasão 
  neles foram relativamente maiores, 8,22% e 10,87%, respectivamente."

}
lst_tables$indicio_evasao$indicio_embriaguez_acidente$comentario <- {
  
  "Abaixo está exibida a distribuição do indício de evasão de pelo menos um dos envolvidos
  por indício de embriaguez nas ocorrências. Nos acidentes relatados com todos os envolvidos 
  sem sinais de ingestão de álcool, o percentual de indício de evasão de pelo menos um dos 
  envolvidos nessas ocorrências foi de 6,88%. Nas ocorrências relatadas com algum 
  dos envolvidos apresentando sinais de embriaguez, o percentual de acidentes que houve 
  indício de evasão foi de 3,69%."
  
}
lst_tables$indicio_evasao$indicio_pedestre_envolvido$comentario <- {
  
  "A tabela abaixo mostra a distribuição do indício de evasão de pelo menos 
  um dos envolvidos nas ocorrências e o envolvimento de pedestres. Quando não 
  houve pedestre(s) envolvido(s) nas ocorrências, o percentual desses acidentes 
  com indício de evasão de pelo menos um dos envolvidos foi de 6,16%. Já dos 
  acidentes relatados envolvendo pedestres, em 10,54% deles teve indício de 
  evasão de pelo menos um dos envolvidos."

}
lst_tables$indicio_evasao$indicio_fatalidade$comentario <- {
  
  "Aqui há a distribuição do indício de evasão nas ocorrências relatadas 
  por fatalidade de pelo menos um dos envolvidos. Veja que dos acidentes 
  com fatalidade, em 9,44% deles houve indício de evasão. Já nos acidentes 
  sem fatalidade, a incidência do indício de evasão foi de 6,74%."
  
}
lst_tables$indicio_evasao$numero_pessoas_envolvidas$comentario <- {
  
  "Nessa tabela para cada quantidade de pessoas envolvidas nas ocorrências 
  há a distribuição do indício ou não de evasão de pelo menos um dos envolvidos. Veja 
  que nas ocorrências com 1 pessoa, logicamente não existe indício de evasão, já que 
  é necessário pelo menos um envolvido para o registro do boletim de ocorrência. 
  Para quantidades maiores de envolvidos, a frequência relativa de evasão nessas 
  ocorrências ficou entre 7% e 8%."
}
lst_tables$indicio_evasao$numero_veiculos_envolvidos$comentario <- {
  
  "Nessa tabela há a distribuição do indício de evasão de algum dos envolvidos 
  pelo número de veículos nas ocorrências. Note que nos acidentes com 1 veículo envolvido, 
  o percentual deles com indício de evasão foi de 5,4%. Esse percentual cresceu nos 
  acidentes relatados com mais veículos envolvidos e em mais de 10% dos acidentes com 5 
  veículos ou mais tiveram pelo menos um dos envolvidos com indício de fuga dessas ocorrências."
  
}
lst_tables$indicio_evasao$sexo_condutor$comentario <- {
  
  "Aqui está exibida a distribuição do indício de evasão de pelo menos um 
  dos envolvidos nas ocorrências por participação por sexo de pelo menos um 
  dos condutores. Como nas outras tabelas em que a variável sexo do condutor está 
  nas linhas, as ocorrências que estão representadas em uma das linhas da tabela 
  também podem estar presentes nas outras, já que em um acidente podemos ter um condutor 
  do sexo masculino e também uma condutora do sexo feminino, por exemplo. Observe que 
  nos acidentes envolvendo pelo menos uma condutora do sexo feminino, em quase 3% 
  deles houve indício de evasão de pelo menos um dos envolvidos. Esse percentual em 
  acidentes envolvendo pelo menos um condutor do sexo masculino foi em torno de 5% e 
  em acidentes em que não foi informado o sexo de pelo menos um dos envolvidos, o percentual 
  do indício de evasão foi em torno de 80% dessas ocorrências, isso já era esperado, 
  pois essa variável é uma das que se não informada levanta a suspeita de evasão."
  
}
lst_tables$indicio_evasao$faixa_idade_condutor$comentario <- {
  
  "Essa tabela mostra a distribuição do indício de evasão de pelo menos um 
  dos envolvidos por participação por faixa etária de pelo menos um dos condutores. 
  As linhas não são mutuamente exclusivas aqui, ou seja, acidentes relatados 
  envolvendo condutores de uma faixa etária também podem estar relatados nos 
  acidentes envolvendo condutores de outra faixa etária, para isso basta que 
  condutores de faixas etárias diferentes tenham se envolvido na mesma ocorrência.
  Note que em 4,54% dos acidentes relatados com pelo menos um condutor de até 20 
  anos teve indício de evasão de pelo menos um dos envolvidos. Para os acidentes 
  que envolveram pelo menos um condutor com idade mais avançada, a partir dos 50 anos, 
  o percentual de indício de evasão nessas ocorrências foi sempre menor que 3%. Em um  
  pouco mais da metade dos acidentes onde não foi infomada a idade de pelo menos um 
  dos condutores houve indício de evasão de pelo menos um dos envolvidos."
}
lst_tables$indicio_evasao$ferimentos_condutor$comentario <- {
  
  "A tabela exibida aqui mostra a distribuição do indício de evasão dos 
  acidentes com o grau de severidade dos ferimentos de pelo menos um dos 
  condutores envolvidos. Os registros nas linhas da tabela não são mutuamente 
  exclusivos, ou seja, em linhas diferentes podem estar sendo contados os 
  mesmos acidentes, pois em uma ocorrência um condutor pode ter tido um grau 
  de ferimento e o(s) outro(s) condutor(es) um grau diferente. Veja que em 
  pouco mais de 5% das ocorrências em que pelo menos um dos condutores teve 
  ferimento fatal, pelo menos um dos outros condutores envolvidos pode ter 
  fugido da ocorrência. Para quase 70% dos acidentes onde não foi relatado 
  o grau de severidade dos ferimentos de pelo menos um dos condutores também 
  houve indício de evasão."
}
lst_tables$indicio_evasao$indicio_uso_cinto_seguranca$comentario <- {
  
  "Aqui há a distribuição do indício de evasão de pelo menos um dos envolvidos 
  nos acidentes e o uso ou não de cinto de segurança pelos condutores. 
  Nos acidentes onde foi assinalado que todos os condutores usavam cinto de 
  segurança, somente em 0,11% das vezes houve indício de evasão. Já em acidentes 
  em que pelo menos um dos condutores estava sem cinto de segurança, o 
  percentual de evasão foi bem maior, 20,53% desses acidentes."
}
lst_tables$indicio_evasao$tipo_acidente$comentario <- {
  
  "Aqui temos a distribuição do indício de evasão dos acidentes por 
  tipo de ocorrência. Note como os percentuais de evasão são diferentes 
  por tipo de acidente. Por exemplo, do total de acidentes relatados como 
  outros tipos, 11,7% deles teve indício de evasão de pelo menos um dos 
  envolvidos. Para atropelamentos de pedestres o percentual de evasão 
  também foi alto, em média 1 a cada 10 atropelamentos de pedestres na 
  capital teve indício de fuga de pelo menos um dos envolvidos."
  
}
lst_tables$indicio_evasao$descricao_veiculos$comentario <- {
  
  "Aqui temos a distribuição do indício de evasão dos acidentes pelas 15 combinações 
  de veículos com maior frequência de envolvimento nas ocorrências. Note como os percentuais 
  de evasão são diferentes para acidentes envolvendo cada uma das combinações de veículos. 
  Veja também que as três combinações de veículos com maiores taxas de evasão nas ocorrências 
  envolveram motocicletas."
  
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

lst_tables$indicio_fatalidade$comentario <- {
  
  "A tabela mostra a distribuição de fatalidade de pelo menos um 
  dos envolvidos nas ocorrências. Menos de 1% dos acidentes relatados 
  teve alguma morte até o momento de lavração do boletim de ocorrência."
  
}

lst_tables$indicio_fatalidade$periodo_dia$comentario <- {
  
  "Aqui há a distribuição de fatalidade ou não nos acidentes por período do dia em 
  que o incidente aconteceu. Dos acidentes relatados ocorridos nas madrugadas, em 2,64% 
  deles teve morte de pelo menos uma das pessoas envolvidas. Para os outros períodos 
  do dia as taxas de acidentes com vítimas fatais foram bem menores se comparado com os 
  acidentes ocorridos nas madrugadas."
}
lst_tables$indicio_fatalidade$indicio_embriaguez_acidente$comentario <- {
  
  "Aqui há a distribuição de fatalidade nos acidentes por indício de embriaguez 
  de pelo menos um dos envolvidos. Note que dos acidentes relatados com indício de 
  embriaguez de pelo menos um dos envolvidos, em 1,34% deles teve fatalidade de pelo 
  menos um dos envolvidos. Já para acidentes sem indício de embriaguez, a frequência 
  relativa de fatalidade nessas ocorrências foi de 0,88%."
}
lst_tables$indicio_fatalidade$indicio_pedestre_envolvido$comentario <- {
  
  "Nessa tabela há a distribuição de fatalidade ou não nos acidentes relatados por 
  envolvimento ou não de pedestres. Há uma informação relevante, preocupante mas esperada 
  nessa tabela. Dos acidentes que envolveram pedestres, em 2,46% deles tiveram pelo menos um 
  dos envolvidos morrendo. Nos acidentes relatados que não envolveram pedestres, essa 
  frequência relativa foi de 0,65% dos acidentes. Esse alto percentual de acidentes com vítima 
  fatal entre os acidentes com pedestres é de se esperar, já que o pedestre é a parte 
  mais frágil em um eventual acidente de trânsito."
  
}
lst_tables$indicio_fatalidade$indicio_evasao$comentario <- {
  
  "Aqui está exibida a distribuição de fatalidade ou não nas ocorrências por 
  indício de evasão ou não de pelo menos uma dos envolvidos. Dos acidentes com 
  indício de evasão, em 1,25% deles pelo menos um dos envolvidos teve 
  ferimentos fatais e veio a óbito em decorrência do impacto do acidente. 
  Já nas ocorrências sem indício de evasão, o percentual das ocorrências com fatalidade
  foi de 0,87%. Isso pode ser um indício de que um ferimento fatal em um dos 
  envolvidos pode aumentar a chance de pelo menos um dos outros envolvidos 
  evadir do local antes da chegada dos agentes públicos que vão registrar a ocorrência."
  
}
lst_tables$indicio_fatalidade$numero_pessoas_envolvidas$comentario <- {
  
  "Aqui temos a distribuição de acidentes com fatalidade em relação ao número 
  de pessoas envolvidas nas ocorrências. Se nota que quando foi relatado mais que 
  4 pessoas envolvidas, a frequência relativa de fatalidade nessas ocorrências foi de 2,93%."
  
}
lst_tables$indicio_fatalidade$numero_veiculos_envolvidos$comentario <- {
  
  "Aqui há a distribuição de fatalidade nos acidentes por número de veículos 
  envolvidos. Note que para os acidentes mais comuns, aqueles que envolvem 2 
  veículos, o percentual de fatalidade foi menor que 0,5%. Para outras quantidades 
  de veículos nas ocorrências, os percentuais de fatalidade nesses acidentes foram 
  maiores, chegando a acidentes relatados com mais de 4 veículos, ou seja, 
  os grandes acidentes, possuindo um percentual de fatalidade em 2,61% do 
  total dessas ocorrências."
  
}
lst_tables$indicio_fatalidade$sexo_condutor$comentario <- {
  
  "Aqui há a relação entre fatalidade de pelo menos uma das pessoas envolvidas e 
  participação por sexo de pelo menos um dos condutores. Como nas outras tabelas em 
  que a variável sexo do condutor está nas linhas, as ocorrências que estão 
  representadas em uma das linhas da tabela também podem estar presentes nas outras, 
  já que em um acidente podemos ter um condutor do sexo masculino e também uma 
  condutora do sexo feminino, por exemplo. Em acidentes relatados envolvendo pelo 
  menos uma condutora do sexo feminino, em 46 deles tiveram pelo menos uma vítima fatal,  
  representando 0,35% dos acidentes que envolveram condutoras desse sexo. Para ocorrências 
  relatadas envolvendo condutores do sexo masculino, o percentual desses acidentes com 
  vítimas fatais foi de 0,89% e para acidentes onde o sexo de pelo menos um dos 
  condutores não foi informado, 1,12% deles provocaram a morte de pelo menos 
  um dos envolvidos nessas ocorrências."
  
}
lst_tables$indicio_fatalidade$faixa_idade_condutor$comentario <- {
  
  "Essa tabela relaciona fatalidade de pelo menos um dos envolvidos 
  nos acidentes por participação por faixa etária de pelo menos um dos condutores. 
  As linhas não são mutuamente exclusivas aqui, ou seja, acidentes relatados 
  envolvendo condutores de uma faixa etária também podem estar relatados nos 
  acidentes envolvendo condutores de outra faixa etária, para isso basta que 
  condutores de faixas etárias diferentes tenham se envolvido na mesma ocorrência. 
  Note que dos acidentes envolvendo condutores jovens de até 20 anos, em 1,32% deles 
  teve pelo menos uma vítima fatal. Esse percentual de acidentes fatais parece se 
  manter estável com o passar das faixas etárias e cair para acidentes envolvendo pelo 
  menos um condutor de 70 anos ou mais."
  
}
lst_tables$indicio_fatalidade$ferimentos_condutor$comentario <- {
  
  "Aqui há a distribuição de fatalidade nos acidentes, ou seja, pelo menos um dos 
  envolvidos morreu e o grau de severidade dos ferimentos de pelo menos um dos 
  condutores envolvidos. Os registros nas linhas da tabela não são mutuamente 
  exclusivos, ou seja, em linhas diferentes podem estar sendo contados os 
  mesmos acidentes, pois em uma ocorrência um condutor pode ter tido um grau 
  de ferimento e o(s) outro(s) condutor(es) um grau diferente. Nos acidentes 
  relatados com pelo menos um dos condutores com ferimento fatal, 100% deles teve 
  pelo menos um dos envolvidos como vítima fatal, como era de se esperar. Para os 
  acidentes relatados sem informação sobre o estado dos ferimentos de pelo menos 
  um dos condutores, o percentual desses acidentes com pelo menos um dos envolvidos 
  como vítima fatal foi menor que 1%."
  
}
lst_tables$indicio_fatalidade$indicio_uso_cinto_seguranca$comentario <- {
  
  "Aqui temos a distribuição de fatalidade nas ocorrências e o 
  uso ou não de cinto de segurança de todos os condutores envolvidos. Para acidentes 
  relatados com pelo menos um dos condutores sem cinto de segurança, o percentual desses 
  com fatalidade de pelo menos um dos envolvidos foi de 0,97%. Quando houve indício 
  de todos os condutores usando o cinto, o percentual de fatalidade nessas ocorrências 
  não foi muito menor, 0,86%."
  
}
lst_tables$indicio_fatalidade$tipo_acidente$comentario <- {
  
  "Nessa tabela há a distribuição de fatalidade por tipo de acidente ocorrido. 
  Veja que os tipos de acidentes com maiores percentuais de fatalidade nas ocorrências 
  foram: atropelamento de pedestres e outros tipos de acidentes."
  
}
lst_tables$indicio_fatalidade$descricao_veiculos$comentario <- {
  
  "Aqui há a distribuição de fatalidade nas ocorrências pelas 15 combinações 
  de veículos com maior frequência de envolvimento nas ocorrências. Veja que 
  acidentes relatados envolvendo 1 caminhão e 1 motocicleta tiveram um 
  percentual assustador e muito acima dos outros de fatalidade, 4,05%. Outras 
  combinações de veículos com percentuais altos de fatalidade nas suas ocorrências 
  foram: acidentes envolvendo 1 motocicleta e 1 ônibus, somente 1 automóvel e 
  somente 1 ônibus."
}


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

lst_tables$numero_pessoas_envolvidas$comentario <- {
  
  "Essa tabela exibe a distribuição do número de pessoas envolvidas nas 
  ocorrências. Observe que em mais de 2/3 dos acidentes registrados houve 
  somente 2 pessoas envolvidas."
  
}

lst_tables$numero_pessoas_envolvidas$periodo_dia$comentario <- {
  
  "Nessa tabela temos a distribuição do número de pessoas envolvidas nas ocorrências 
  por período em que elas aconteceram. Note como a distribuição da quantidade de 
  pessoas envolvidas nos acidentes no período da madrugada se comporta de maneira 
  diferente em relação ao número de pessoas envolvidas em acidentes em outros períodos."
  
}
lst_tables$numero_pessoas_envolvidas$indicio_embriaguez_acidente$comentario <- {
  
  "Aqui há a distribuição do número de pessoas envolvidas nas ocorrências 
  por indício de embriaguez ou não. Veja que em mais de 10% das ocorrências 
  com embriaguez teve pelo menos 4 pessoas envolvidas. Já nos acidentes relatados 
  sem embriaguez, o percentual de acidentes com pelo menos 4 pessoas não foi 
  maior que 5% dessas ocorrências."
  
}
lst_tables$numero_pessoas_envolvidas$indicio_pedestre_envolvido$comentario <- {
  
  "Aqui temos a distribuição do número de pessoas envolvidas nas ocorrências por 
  envolvimento ou não de pedestres. Note que para acidentes que envolveram 
  pedestres, em quase 88% de deles teve apenas 2 pessoas envolvidas, ou seja, 
  o pedestre e o condutor. Parece ter uma inconsistência nos dados relatados 
  com 1 pessoa envolvida e pedestre envolvido. Essa configuração ocorreu 29 vezes, 
  porém não parece fazer sentido."
  
}
lst_tables$numero_pessoas_envolvidas$indicio_evasao$comentario <- {
  
  "Essa tabela exibe a distribuição do número de pessoas envolvidas por indício 
  ou não de evasão nas ocorrências. Se pode notar que dos acidentes com indício 
  de evasão, em quase 3/4 deles tiveram apenas 2 pessoas envolvidas, ou seja, 
  uma pessoa com indício de fuga do local e a outra que registrou o boletim de ocorrência."
}
lst_tables$numero_pessoas_envolvidas$indicio_fatalidade$comentario <- {
  
  "Nessa tabela há a distribuição do número de pessoas envolvidas por fatalidade de 
  pelo menos uma das pessoas envolvidas nas ocorrências. Veja que nos acidentes 
  envolvendo vítimas fatais, em 16,63% deles tiveram 1 pessoa envolvida que por consequeências 
  dos acidentes foram à óbitos e em 4,72% desses acidentes teve mais que 4 pessoas 
  envolvidas. Já nos acidentes sem fatalidade, 11,04% desses tiveram 1 pessoa envolvida e 
  em 1,41% desses acidentes teve mais que 4 pessoas envolvidas."
}
lst_tables$numero_pessoas_envolvidas$numero_veiculos_envolvidos$comentario <- {
  
  "Aqui há a distribuição do número de pessoas envolvidas pelo número de veículos  
  na ocorrências. Como na tabela transposta a essa, mais uma vez levanto dúvidas 
  sobre a consistência desses dados, pois parece improvável que em um acidente com 
  somente 1 pessoa envolvida há mais que 1 veículo nessas ocorrências. Veja que interessante, 
  em quase 60% dos acidentes com 1 veículo envolvido teve a participação de 2 pessoas. 
  Em acidentes com 2 veículos envolvidos, em 76,68% deles teve também 2 pessoas envolvidas, 
  ou seja, os 2 condutores."
}
lst_tables$numero_pessoas_envolvidas$sexo_condutor$comentario <- {
  
  "Nessa tabela temos a distribuição do número de pessoas envolvidas em acidentes 
  por participação por sexo de pelo menos um dos condutores. Como nas outras tabelas em 
  que a variável sexo do condutor está nas linhas, as ocorrências que estão 
  representadas em uma das linhas da tabela também podem estar presentes nas outras, 
  já que em um acidente podemos ter um condutor do sexo masculino e também uma 
  condutora do sexo feminino, por exemplo. Note que somente 4 dos acidentes onde não 
  foi informado o sexo de pelo menos um dos condutores teve somente 1 pessoa envolvida, 
  ou seja, somente 1 condutor e não foi informado o sexo dele. Veja também que somente 
  em 6,18% dos acidentes que envolveram pelo menos uma condutora do sexo feminino teve 
  somente 1 pessoa envolvida, ou seja, somente essa condutora. Para acidentes envolvendo 
  condutores do sexo masculino, esse respectivo número de pessoas envolvidas representa 
  mais de 10% das ocorrências envolvendo condutores desse sexo."
  
}
lst_tables$numero_pessoas_envolvidas$faixa_idade_condutor$comentario <- {
  
  "Essa tabela relaciona o número de pessoas envolvidas nos acidentes por 
  participação por faixa etária de pelo menos um dos condutores. As linhas não 
  são mutuamente exclusivas aqui, ou seja, acidentes relatados envolvendo 
  condutores de uma faixa etária também podem estar relatados nos acidentes 
  envolvendo condutores de outra faixa etária, para isso basta que condutores 
  de faixas etárias diferentes tenham se envolvido na mesma ocorrência. Veja 
  que quase 10% das ocorrências que os condutores com idade entre 20 e 29 anos 
  se envolvem têm somente o condutor envolvido. Note como o percentual de acidentes 
  com apenas 1 pessoa envolvida cai para acidentes envolvendo pelo menos um 
  condutor de faixa etária maior."
}
lst_tables$numero_pessoas_envolvidas$ferimentos_condutor$comentario <- {
  
  "Essa tabela apresenta a distribuição do número de pessoas envolvidas nas 
  ocorrências de acidentes por grau de severidade dos ferimentos de pelo menos 
  um dos condutores envolvidos. Os registros nas linhas da tabela não são mutuamente 
  exclusivos, ou seja, em linhas diferentes podem estar sendo contados os 
  mesmos acidentes, pois em uma ocorrência um condutor pode ter tido um grau 
  de ferimento e o(s) outro(s) condutor(es) um grau diferente. Note que nos acidentes 
  em que pelo menos um dos condutores teve ferimento fatal, em 30% deles houve 
  somente esse condutor envolvido e em 5,35% desses acidentes tiveram mais que 4 
  pessoas envolvidas, essa frequência relativa é bem maior se comparada com os acidentes 
  com pelo menos um condutor com outros graus de severidade para mais que 4 pessoas 
  envolvidas."
  
}
lst_tables$numero_pessoas_envolvidas$indicio_uso_cinto_seguranca$comentario <- {
  
  "Aqui temos a distribuição do número de pessoas envolvidas em ocorrências com todos 
  os condutores usando cinto e em ocorrências em que nem todos os condutores usavam o cinto. 
  As frequências relativas são muito parecidas para acidentes relatados com 3, 4 ou mais que 4 
  condutores. A diferença percentual está principalmente em acidentes relatados com 1 pessoa, 
  eles representam quase 13% dos acidentes onde os condutores usavam cinto de segurança e 
  7,36% nos acidentes onde pelo menos um dos condutores não usava, nesse caso com 1 condutor 
  envolvido e esse não estava de cinto."
  
}
lst_tables$numero_pessoas_envolvidas$tipo_acidente$comentario <- {
  
  "Essa tabela mostra a distribuição do número de pessoas por tipo de acidente ocorrido.
  Note que 90% dos atropelamentos de pedestres tiveram 2 pessoas envolvidas, um condutor 
  e um pedestre. Nos acidentes relatados como queda de pessoa do veículo, em mais de 77% 
  deles teve 2 pessoas envolvidas e em mais de 5% dos acidentes relatados como colisão 
  de veículo com objeto parado teve 4 pessoas envolvidas."
}
lst_tables$numero_pessoas_envolvidas$descricao_veiculos$comentario <- {
  
  "Aqui há a distribuição do número de pessoas envolvidas pelas 15 combinações de 
  veículos mais frequentes nas ocorrências. Note por exemplo, que 93% dos acidentes 
  relatados com 1 ônibus tiveram 2 pessoas envolvidas."
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

lst_tables$numero_veiculos_envolvidos$comentario <- {
  
  "Essa tabela exiba a distribuição de número de veículos envolvidos 
  nas ocorrências. Note que a maioria dos acidentes teve 2 veículos 
  envolvidos."
  
}

lst_tables$numero_veiculos_envolvidos$periodo_dia$comentario <- {
  
"Nessa tabela há a distribuição do número de veículos envolvidos nas ocorrências 
 pelo período do dia das mesmas. Observe que aqui há o mesmo comportamento da 
 tabela de número de pessoas envolvidas e período do dia. Acidentes com 2 veículos 
 envolvidos representam aproximadamente 2/3 dos acidentes que aconteceram nas 
 manhãs, tardes e noites, mas nas madrugadas essa frequência relativa cai, 
 enquanto o percentual de acidentes com 1 veículo envolvido sobe e é bem maior 
 que nos outros períodos do dia."

}
lst_tables$numero_veiculos_envolvidos$indicio_embriaguez_acidente$comentario <- {
  
  "Na tabela abaixo há a distribuição do número de veículos envolvidos por acidentes 
  com indício de embriaguez. Perceba que quase metade dos acidentes relatados com indício 
  de embriaguez de pelo menos um dos envolvidos teve 1 veículo envolvido. Já nos acidentes 
  sem embriaguez, o percentual deles com 1 veículo envolvido foi de 27,47%. Note também 
  que os acidentes envolvendo, 3, 4 ou mais que 4 veículos tiveram maiores frequências 
  relativas nos acidentes com indício de embriaguez do que nos acidentes sem indício de 
  embriaguez."
  
}
lst_tables$numero_veiculos_envolvidos$indicio_pedestre_envolvido$comentario <- {
  
  "Abaixo temos a distribuição do número de veículos envolvidos em acidentes relatados 
  com e sem pedestres. Perceba nessa tabela como o envolvimento de pedestre muda a 
  configuração de número de veículos nos acidentes. Quando há pedestre(s) envolvido(s) em 
  mais de 90% dessas ocorrências houve só 1 veículo. Quando não há pedestres envolvidos 
  nos acidentes, quase 3/4 deles teve 2 veículos envolvidos."
}
lst_tables$numero_veiculos_envolvidos$indicio_evasao$comentario <- {
  
  "Aqui há a distribuição do número de veículos envolvidos em acidentes por  
  indício de evasão de pelo menos um dos envolvidos. Note que as distribuições 
  do número de veículos envolvidos são parecidas entre os acidentes com indício 
  de evasão e os acidentes sem indício de evasão."
  
}
lst_tables$numero_veiculos_envolvidos$indicio_fatalidade$comentario <- {
  
  "Nessa tabela está exibida a distribuição do número de veículos envolvidos em 
  acidentes por ocorrência de fatalidade ou não nesses acidentes. Observe que dos 
  acidentes com vítimas fatais, cerca de 57% deles teve apenas 1 veículo envolvido. 
  Para acidentes relatados sem fatalidade, somente cerca de 28% deles teve apenas 
  1 veículo envolvido. Os acidentes com 3, 4 ou mais que 4 veículos também tiveram
  maiores frequências relativas entre os acidentes com fatalidade em relação aos 
  acidentes com as mesmas quantidades de veículos que não tiveram vítimas fatais."
}
lst_tables$numero_veiculos_envolvidos$numero_pessoas_envolvidas$comentario <- {
  
  "Aqui há a distribuição do número de veículos envolvidos pelo número de pessoas 
  com participação nas ocorrências de trânsito. Note que interessante, em quase 90% 
  dos acidentes com 1 pessoa envolvida teve 1 veículo envolvido. Já nos acidentes com 
  a participação de mais pessoas, o número de veículos mais frequentes nessas ocorrências 
  foi 2, mas com frequências relativas diferentes, dos acidentes com 2 pessoas envolvidas 
  em 73,77% deles teve 2 veículos envolvidos (1 pessoa em cada veículo), já nos acidentes
  com mais que 4 pessoas envolvidas em 40,59% deles teve a participação de 2 veículos."
}
lst_tables$numero_veiculos_envolvidos$sexo_condutor$comentario <- {
  
  "A tabela abaixo mostra a distribuição do número de veículos envolvidos nas 
  ocorrências em que havia pelo menos um condutor de determinado sexo. Veja que 
  interessante, somente em cerca de 14% dos acidentes em que uma condutora do 
  sexo feminino se envolveu havia somente o veículo dessa condutora na ocorrência, 
  para os condutores do sexo masculino essa respectiva frequência relativa foi de 24,81% 
  dos acidentes envolvendo condutores desse sexo."
} 
lst_tables$numero_veiculos_envolvidos$faixa_idade_condutor$comentario <- {
  
"Aqui está exibida a distribuição do número de veículos envolvidos nas ocorrências 
por participação de pelo menos um condutor de determinada faixa etária. 
Note que em mais de 70% dos acidentes envolvendo pelo menos um condutor de qualquer 
faixa etária teve 2 veículos envolvidos. Veja também que em quase 10% das ocorrências 
envolvendo pelo menos um condutor de 50 anos ou mais teve 3 veículos envolvidos."
  
}
lst_tables$numero_veiculos_envolvidos$ferimentos_condutor$comentario <- {
  
  "Aqui há a distribuição do número de veículos nas ocorrências de acidentes por 
  severidade de pelo menos um dos condutores envolvidos. Note que em quase 40% dos 
  acidentes que um condutor teve ferimento fatal, só havia o veículo desse condutor 
  envolvido na ocorrência. Veja também que a frequência relativa da participação de 
  4 ou mais que 4 veículos foi maior nos acidentes com vítimas fatais se comparado 
  com as frequências relativas de envolvimento de 4 ou mais que 4 veículos em acidentes 
  com pelo menos um condutor com outros graus de ferimentos."
  
}
lst_tables$numero_veiculos_envolvidos$indicio_uso_cinto_seguranca$comentario <- {
  
  "Está exibida aqui a distribuição do número de veículos envolvidos nas ocorrências em 
  que todos os condutores usavam cinto de segurança e nas ocorrências em que pelo menos um 
  dos condutores não usava. Veja que em quase 30% das ocorrências relatadas com os 
  condutores usando cinto de segurança houve somente 1 veículo envolvido, ou seja, somente 1 
  condutor e ele estava usando cinto de segurança. O respectivo percentual para acidentes 
  onde pelo menos um dos condutores não usava cinto de segurança foi de cerca de 25%, ou seja,
  somente aquele condutor e ele não estava usando cinto."
  
}
lst_tables$numero_veiculos_envolvidos$tipo_acidente$comentario <- {
  
  "Aqui há a distribuição do número de veículos envolvidos por tipo 
  de acidente ocorrido. Note como o número de veículos se distribuem diferentes 
  por tipos de acidentes, por exemplo: em mais de 95% dos atropelamentos de 
  pedestres teve 1 veículo envolvido, as quedas de pessoas de veículos tiveram 
  1 veículo envolvido em quase 95% desses acidentes. Já nas colisões de veículos, 
  em quase 92% delas teve 2 veículos envolvidos."
  
}
lst_tables$numero_veiculos_envolvidos$descricao_veiculos$comentario <- {
  
  "Essa tabela mostra a distribuição do número de veículos pelas 15 combinações  
  de veículos mais envolvidos nas ocorrências. Não é surpresa os dados exibidos 
  aqui, já que uma variável é derivada da outra."
}


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

lst_tables$sexo_condutor$comentario <- {
  
  "Essa tabela exibe a relação de participação do sexo dos condutores 
  nas ocorrências. Note que em mais de 91% das ocorrências que ocorreram 
  na cidade entre 2016 e 2019 teve pelo menos um condutor do sexo 
  masculino envolvido. As condutoras do sexo feminino participaram de pouco 
  mais de 26% das ocorrências presentes no conjunto de dados."
  
}

lst_tables$sexo_condutor$periodo_dia$comentario <- {
  
  "Aqui temos a relação entre participação por sexo dos condutores e 
  período do dia. Perceba que os percentuais das linhas não somam 100% 
  pois em um mesmo acidente pode haver condutores de sexos diferentes. 
  Há informações muito interessantes nessa tabela. Cerca de 30% dos 
  acidentes relatados que aconteceram nas manhãs tiveram pelo menos uma 
  condutora do sexo feminino envolvido. Para os acidentes nos períodos 
  da tarde e noite, o percentual deles com participação de pelo menos 
  uma condutora do sexo feminino caiu em relação a manhã e nas madrugadas 
  o percentual de participação feminina nos acidentes caiu drasticamente 
  em relação aos outros períodos do dia. Para os condutores do sexo 
  masculino, o percentual de participação nos acidentes se manteve quase 
  que estável ao longo dos períodos, ou seja, no mínimo 90% dos acidentes 
  ocorridos em qualquer fase do dia envolveram pelo menos um condutor do 
  sexo masculino."

  
}
lst_tables$sexo_condutor$indicio_embriaguez_acidente$comentario <- {
  
  "Aqui há a relação entre a participação nas ocorrências por sexo dos 
  condutores e indício de embriaguez. Perceba que os percentuais das linhas 
  não somam 100% pois em um mesmo acidente pode haver condutores de sexos 
  diferentes. Há informações relevantes nessa tabela. Em 26,8% dos acidentes 
  relatados sem indício de embriaguez das pessoas envolvidas havia pelo menos 
  uma condutora do sexo feminino envolvida. Esse respectivo percentual para 
  acidentes com indício de embriaguez foi em torno de 16%. Para a participação 
  de condutores do sexo masculino, o percentual ficou em torno de 91% em 
  acidentes com e sem embriaguez."
  
}
lst_tables$sexo_condutor$indicio_pedestre_envolvido$comentario <- {
  
  "Na tabela abaixo está relacionado a participação por sexo dos condutores e 
  o envolvimento de pedestre(s) nas ocorrências. Observe que os percentuais das 
  linhas dessa tabela não somam 100%, já que eu um mesmo acidente podemos ter 
  condutores de sexo diferentes envolvidos. Note que em acidentes relatados 
  sem pedestres, em 94% e 28% deles possui pelo menos um condutor do sexo masculino 
  e pelo menos um condutor do sexo feminino, respectivamente e nas ocorrências com 
  pedestres as frequências relativas da participação de condutores desses sexos 
  caíram consideravelmente. Veja também que em 12,3% das ocorrências com pedestres 
  não foi informado o sexo do condutor. Em comparação com os acidentes sem 
  pedestres esse percentual foi de 7,66% desses acidentes."
  
}
lst_tables$sexo_condutor$indicio_evasao$comentario <- {
  
  "Aqui há a relação entre a participação nos acidentes por sexo de pelo 
  menos um dos condutores e o indício de evasão das ocorrências. Observe 
  que em mais de 98% dos acidentes relatados com indício de evasão de pelo 
  menos uma das pessoas envolvidas tinha pelo menos um condutor com sexo 
  não informado. Esse percentual é alto e esperado, já que não ter a informação 
  do sexo da pessoa envolvida é um dos componentes para acender o alerta de 
  indício de evasão da ocorrência."
  
}
lst_tables$sexo_condutor$indicio_fatalidade$comentario <- {
  
  "Na tabela abaixo está exibida a relação entre participação por sexo 
  de pelo menos um dos condutores nos acidentes com fatalidade ou não. 
  Lembre-se que os percentuais contidos nas linhas não somam 100%, pois 
  em um mesmo acidente podemos ter condutores de sexo diferentes envolvidos. 
  Note que somente em 10% das ocorrências com vítimas fatais tinha pelo 
  menos uma condutora do sexo feminino envolvida. Note que a participação 
  de pelo menos um condutor de sexo masculino em acidentes com fatalidade e 
  acidentes sem fatalidade se manteve próximo a 90%."
}
lst_tables$sexo_condutor$numero_pessoas_envolvidas$comentario <- {
  
  "Essa tabela relaciona a presença por sexo de pelo menos um dos condutores 
  por número de pessoas envolvidas nas ocorrências. Note que com exceção da 
  primeira linha, os percentuais não somam 100%, isso porque condutores de 
  sexo diferentes podem se envolver na mesma ocorrência. Na primeira linha 
  a soma dos percentuais deveria somar 100%, pois em qualquer acidente de 
  trânsito necessita ter pelo menos um condutor envolvido. No entanto, a 
  soma dos percentuais nessa linha é um pouco menor que 100%, isso é uma 
  inconsistência nos dados. Apesar disso, veja que interessante, nos acidentes 
  relatados com somente 1 pessoa envolvida, ou seja, somente o condutor, em 
  quase de 85% deles quem estava sob controle do veículo era do sexo masculino, 
  em menos de 15% deles quem manejava o veículo era do sexo feminino. Repare 
  que com o aumento do número de pessoas envolvidas nas ocorrências, o 
  percentual de participação de pelo menos um condutor do sexo masculino e 
  do sexo feminino aumentaram. Em acidentes com 5 pessoas ou mais envolvidas, 
  em 97,49% deles teve pelo menos um condutor do sexo masculino envolvido e 
  38,63% deles tiveram a presença de pelo menos uma condutora do sexo feminino."

}
lst_tables$sexo_condutor$numero_veiculos_envolvidos$comentario <- {
  
  "Essa tabela mostra a relação da participação por sexo dos condutores 
  envolvidos em ocorrências de acidentes de trânsito por número de veículos 
  envolvidos nas mesmas. Note que exceto a primeira linha, as outras não 
  devem ter os percentuais somando 100%, já que condutores de sexos diferentes 
  podem se envolver na mesma ocorrência. A primeira linha deveria somar 100%, 
  mas houve alguma inconsistência no preenchimento de algum dos boletins e isso 
  não aconteceu. Apesar disso, note que há informações interessantes contidas 
  nessa tabela. Em cerca de 80% dos acidentes relatados com 1 veículo envolvido, 
  esse veículo era manejado por um condutor do sexo masculino e somente em 13% 
  dessas ocorrências quem manejava o veículo envolvido era uma condutora do 
  sexo feminino. Note também que com o aumento do número de veículos nas 
  ocorrências, mais provável foi ter pelo menos um condutor do sexo masculino 
  e do sexo feminino envolvidos."
  
}
lst_tables$sexo_condutor$faixa_idade_condutor$comentario <- {
  
  "Essa tabela relaciona a faixa de idade de pelo menos um dos condutores 
  envolvidos e a participação por sexo de pelo menos um dos condutores envolvidos. 
  Essa tabela pode parecer um pouco confusa de se analisar, mas a interpretação 
  não é tão complicada. Por exemplo, em 97,85% dos acidentes envolvendo pelo menos 
  um condutor de até 20 anos tinha também pelo menos um condutor do sexo masculino. 
  Em 31,61% dos acidentes com pelo menos um condutor com idade entre 30 a 39 anos 
  teve pelo menos uma condutora do sexo feminino envolvida."
}
lst_tables$sexo_condutor$ferimentos_condutor$comentario <- {
  
  "Essa tabela relaciona a participação por sexo de pelo menos um dos condutores 
  nas ocorrências pelo grau da severidade dos ferimentos de pelo menos um deles. 
  Pode parecer confusa essa tabela, mas a interpretação não é tão complicada. 
  Por exemplo, em 97,94% dos acidentes que tiveram pelo menos um condutor como 
  vítima fatal também teve pelo menos um condutor do sexo masculino envolvido. 
  Para acidentes com o mesmo grau de severidade dos ferimentos, somente em 12,35% 
  deles teve a participação de pelo menos uma condutora do sexo feminino. Note 
  também que em quase 70% dos acidentes que o grau dos ferimentos de pelo menos 
  um dos condutores não foi informado também teve pelo menos um condutor sem a 
  informação do seu sexo."
  
}
lst_tables$sexo_condutor$indicio_uso_cinto_seguranca$comentario <- {
  
  "Essa tabela relaciona a participação de pelo menos um condutor de 
  determinado sexo por uso de cinto de segurança ou não de todos os 
  condutores envolvidos. Note que nos acidentes em que houve a participação 
  de pelo menos um condutor do sexo masculino, em 68,86% deles todos os 
  condutores usavam cinto de segurança. Para acidentes envolvendo pelo 
  menos uma condutora do sexo feminino, essa respectiva segmentação representou 
  71,85% dos acidentes em que elas estiveram envolvidas."
}
lst_tables$sexo_condutor$tipo_acidente$comentario <- {
  
  "Essa tabela relaciona a participação por sexo de pelo menos um dos 
  condutores por tipo de acidente ocorrido. Veja que interessante, em 
  96% das colisões de veículos teve pelo menos um condutor do sexo masculino 
  envolvido e em quase 32% desses acidentes teve a participação de pelo 
  menos uma condutora do sexo feminino. Observe também que os tipos de 
  acidentes que possuíram maiores percentuais de participação de não informação 
  do sexo de pelo menos um dos condutores foram: vazamento de carga do veículo e 
  os outros tipos de acidentes."
}
lst_tables$sexo_condutor$descricao_veiculos$comentario <- {
  
  "Há aqui a relação entre a presença de pelo menos um dos condutores 
  de determinado sexo pelas 15 combinações de veículos mais frequentes 
  nos acidentes relatados na capital. Note que interessante a variação 
  dos percentuais de participação de pelo menos um condutor de determinado 
  sexo ao longo das combinações de veículos. Por exemplo, em 44,58% dos 
  acidentes relatados envolvendo 2 automóveis tinha pelo menos uma condutora 
  do sexo feminino envolvida, para 3 automóveis pelo menos um dos condutoras 
  era do sexo feminino em 55,12% dessas ocorrências. Também se nota que os 
  acidentes envolvendo 1 motocicleta e acidentes envolvendo somente 1 ônibus 
  tiveram a menor participação percentual de condutoras do sexo feminino, 
  somente 8,8% e 1,93%, respectivamente. O percentual de presença de condutores 
  do sexo masculino é alta em todas as combinações de veículos, por exemplo, 
  em quase todos os acidentes envolvendo 1 caminhonete e 1 motocicleta teve 
  pelo menos um dos 2 condutores envolvidos sendo do sexo masculino. Os acidentes 
  por combinações de veículos com maior percentual de sexo de pelo menos um 
  dos condutores com sexo não informado foram quando 1 caminhão e 1 
  motocicleta se envolveram na ocorrência e quando 2 motocicletas estiveram 
  na mesma ocorrência."
  
}


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

lst_tables$faixa_idade_condutor$comentario <- {
  
  "Nessa tabela há a distribuição por participação por faixa 
  etária de pelo menos um dos condutores nas ocorrências. 
  Note como os condutores com faixa etária entre 20 e 29 anos 
  participaram de 45,27% dos acidentes relatados na capital. 
  Condutores com faixa etária entre 30 e 39 anos participaram 
  de quase 42% dos acidentes."
  
}

lst_tables$faixa_idade_condutor$periodo_dia$comentario <- {
  
  "Aqui temos a relação de participação de pelo menos um 
  condutor de determinada faixa etária e o período do dia 
  que o acidente ocorreu. Observe que interessante, cerca 
  de 43% dos acidentes nas manhãs e nas tardes possuem 
  pelo menos um condutor com idade entre 20 e 29 anos, 
  mas nos acidentes durante as noites e madrugadas quase 
  50% deles teve a participação de pelo menos um condutor 
  com essa faixa etária. Veja também como a frquência 
  relativa da participação de pelo menos um condutor 
  com mais de 40 anos cai nos acidentes ao longo do dia, 
  por exemplo: nas manhãs 30% dos acidentes teve a participação 
  de pelo menos um condutor com idade entre 40 e 49 anos, 
  mas nas madrugadas os acidentes que tiveram pelo menos 
  um condutor com essa faixa etária representaram 18,47% 
  das ocorrências nesse período do dia."

  
}
lst_tables$faixa_idade_condutor$indicio_embriaguez_acidente$comentario <- {
  
  "Aqui temos a relação entre participação por faixa etária de pelo menos um dos condutores 
  nas ocorrências por indício de embriaguez ou não. Note como nos acidentes sem indício de 
  embriaguez, a faixa etária mais frequente com presença nessas ocorrências foi de 20 a 29 anos. 
  No entanto, nos acidentes relatados com indício de embriaguez, a faixa etária de participação 
  dos condutores mais frequente nessas ocorrências foi de idade entre 30 e 39 anos."

}
lst_tables$faixa_idade_condutor$indicio_pedestre_envolvido$comentario <- {
  
  "A tabela abaixo mostra a relação entre a participação por faixa etária de pelo menos 
  um dos condutores nos acidentes por pedestre envolvido ou não. Note que interessante, 
  em pouco mais de 1/4 dos acidentes com pedestres houve pelo menos um condutor com idade 
  entre 30 e 39 anos envolvido. Em quase 1/4 deles possui pelo menos um condutor com idade 
  entre 20 e 29 anos e em quase 17% dos acidentes relatados com pedestres não teve idade 
  informada de pelo menos um dos condutores envolvidos."

}
lst_tables$faixa_idade_condutor$indicio_evasao$comentario <- {
  
  "Nessa tabela há a relação da presença das faixas etárias de pelo menos um dos 
  condutores envolvidos em acidentes que houveram ou não indício de evasão de pelo 
  menos um dos envolvidos. Note que 98,57% dos acidentes relatados com indício de 
  evasão teve pelo menos um dos condutores envolvidos nessas ocorrências com idade não 
  informada. Em quase 1/3 dos acidentes relatados com indício de evasão teve pelo 
  menos um condutor com idade entre 20 e 29 anos envolvido."
  
}
lst_tables$faixa_idade_condutor$indicio_fatalidade$comentario <- {
  
  "Essa tabela relaciona a participação de condutores de determinada faixa 
  etária por fatalidade ou não dos acidentes. Note que em mais de 1/3 dos 
  acidentes relatados com fatalidade teve pelo menos um condutor com idade 
  entre 30 e 39 anos e em quase 40% deles teve um condutor com idade entre 
  20 e 29 anos envolvido."

}
lst_tables$faixa_idade_condutor$numero_pessoas_envolvidas$comentario <- {
  
  "Aqui há a relação entre a presença de pelo menos um condutor de determinada 
  faixa etária e o número de pessoas envolvidas nas ocorrências. Note que em 
  38,54% dos acidentes relatados com somente 1 pessoa envolvida, ou seja, o condutor, 
  ele(a) tinha entre 20 e 29 anos. Note também que com o aumento do número de 
  pessoas nas ocorrências, maior a frequência relativa de participação de pelo menos 
  um condutor de quase todas as faixas etárias."
  
}
lst_tables$faixa_idade_condutor$numero_veiculos_envolvidos$comentario <- {
  
  "Nessa tabela está a relação da participação de pelo menos um condutor de determinada 
  faixa etária pelo número de veículos envolvidos nas ocorrências. Nos acidentes que teve 1 
  veículo envolvido, em 28,87% deles o condutor desse veículo tinha entre 20 e 29 anos. Nos 
  acidentes com 2 veículos envolvidos, a participação de condutores dessa faixa etária 
  cresce consideravelmente. Comportamento parecido se teve para condutores com idade entre 
  30 e 39 anos."
  
}
lst_tables$faixa_idade_condutor$sexo_condutor$comentario <- {
  
  "Essa tabela relaciona a participação nas ocorrências por faixa etária de pelo menos um 
  dos condutores pelo sexo de pelo menos um dos condutores no acidente. Notepor exemplo que 
  em 50,12% das ocorrências que havia pelo menos uma condutora do sexo feminino envolvido 
  também teve pelo menos um condutor com idade entre 30 e 39 anos."
}
lst_tables$faixa_idade_condutor$ferimentos_condutor$comentario <- {
  
  "Essa tabela relaciona a participação por faixa etária de pelo menos um dos condutores 
  nas ocorrências pelo grau de severidade dos ferimentos de pelo menos um desses condutores. 
  Note que em 46,09% dos acidentes relatados com pelo menos um condutor como vítima fatal 
  também teve pelo menos um condutor com idade entre 20 e 29 anos. Repare também que em 
  94,77% dos acidentes onde não foi relatado o ferimento de pelo menos um dos condutores 
  também não foi relatado a idade de pelo menos um dos condutores envolvidos nessas ocorrências."
}
lst_tables$faixa_idade_condutor$indicio_uso_cinto_seguranca$comentario <- {
  
  "Essa tabela relaciona a participação por faixa etária de pelo menos um dos 
  condutores por uso ou não de cinto nas ocorrências. Note que 48% das ocorrências 
  onde todos os condutores estavam usando cinto de segurança tinha pelo menos um 
  condutor com idade entre 20 e 29 anos. A participação de pelo menos um condutor 
  dessa mesma faixa etária em acidentes em que nem todos eles usavam cinto de segurança 
  foi de 39,61%."
}
lst_tables$faixa_idade_condutor$tipo_acidente$comentario <- {
  
  "Essa tabela relaciona a participação por faixa etária dos condutores e 
  o tipo de acidente ocorrido. Note como são interessantes as informações 
  contidas aqui. Por exemplo, Em 52,83% das colisões de veiculos teve pelo 
  menos um condutor com idade entre 20 e 29 anos, mas nas quedas de pessoas 
  de veículos, somente em 14,2% deles teve a participação de pelo menos um 
  condutor dessa faixa etária."
}
lst_tables$faixa_idade_condutor$descricao_veiculos$comentario <- {
  
  "Essa tabela relaciona a participação por faixa etária de pelo menos um dos 
  condutores nas ocorrências pelas 15 combinações de veiculos mais frequentes. 
  Note como as frequências relativas de participação de pelo menos um condutor 
  de determinada faixa etária são diferentes por combinação de veículos envolvidos.
  Por exemplo, em quase 32% dos acidentes envolvendo 1 automóvel e 1 bicicleta 
  tinha pelo menos um condutor de até 20 anos envolvido. Nas outras combinações 
  de veículos, o percentual de acidentes com a participação de pelo menos um condutor 
  com até 20 anos foram bem menores. Repare também nos acidentes envolvendo 1 motocicleta, 
  em 42,13% deles o condutor tinha idade entre 20 e 29 anos e em 27,9% desses 
  acidentes tinha pelo menos um condutor com idade entre 30 e 39 anos e 
  para as faixas etárias superiores a participação de condutores mais velhos possuem 
  frequência relativa bem menores. Note como essas frequências relativas são diferentes 
  por faixa etária em acidentes envolvendo 1 ônibus. Veja também que quase em 30% 
  dos acidentes com 2 motocicletas não foi relatada a idade de pelo menos um dos 
  dois condutores envolvidos."
  
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

lst_tables$ferimentos_condutor$comentario <- {
  
  "Essa tabela mostra a distribuição do grau de severidade dos ferimentos dos 
  condutores por acidente. Veja que em quase 0,5% dos acidentes relatados teve 
  pelo menos um condutor como vítima fatal. Em 78,37% dos acidentes teve 
  pelo menos um condutor com ferimentos não fatais e em 74,09% deles houve 
  pelo menos um condutor sem ferimentos."
  
}

lst_tables$ferimentos_condutor$periodo_dia$comentario <- {
  
  "A tabela exibida aqui relaciona o grau de severidade dos ferimentos de pelo menos 
  um dos condutores por período do dia que as ocorrências aconteceram. Veja que em 
  1,83% dos acidentes ocorridos na madrugada teve pelo menos um condutor morrendo, 
  foi a maior frequência relativa se comparado com os outros períodos do dia. Note 
  também que 14,52% dos acidentes ocorridos nas madrugadas não tiveram informação dos 
  ferimentos de pelo menos um dos condutores."
  
}
lst_tables$ferimentos_condutor$indicio_embriaguez_acidente$comentario <- {
  
  "Aqui há a relação entre o grau de severidade dos ferimentos de pelo menos um 
  dos condutores e o indício de embriaguez ou não nas ocorrências. Note que em 0,39% 
  dos acidentes com indício de embriaguez teve pelo menos um condutor como vítima fatal, 
  já em acidentes sem embriaguez, 0,49% deles teve pelo menos um condutor morrendo."

}
lst_tables$ferimentos_condutor$indicio_pedestre_envolvido$comentario <- {
  
  "Essa tabela relaciona o grau de severidade dos ferimentos de pelo menos um dos 
  condutores por envolvimento ou não de pedestre(s) nas ocorrências. Veja que somente 
  em 7 dos acidentes que envolveram pedestre(s) teve morte de pelo menos um dos 
  condutores envolvidos. Veja também que em 13,36% dos acidentes envolvendo 
  pedestre(s) não houve informação sobre os ferimentos de pelo menos um dos 
  condutores envolvidos."
}
lst_tables$ferimentos_condutor$indicio_evasao$comentario <- {
  
  "A tabela exibida aqui mostra a relação entre o grau de severidade dos ferimentos
  de pelo menos um dos condutores e o indício de evasão ou não dos acidentes. 
  Veja que em mais de 98% das ocorrências com indício de evasão teve pelo menos um 
  dos condutores com o sexo não informado. Isso é esperado, já que a 
  variável indício de evasão tem um de seus pilares quando o grau de severidade dos 
  ferimentos de um dos envolvidos não foi informado."

}
lst_tables$ferimentos_condutor$indicio_fatalidade$comentario <- {
  
  "Essa tabela relaciona o grau de severidade dos ferimentos de pelo menos um
  dos condutores e a fatalidade ou não nas ocorrências. Essa tabela pode parecer 
  redundante mas trás informações interessantes. Por exemplo, em 54,61% dos acidentes 
  relatados com pelo menos uma vítima fatal teve a morte de pelo menos um dos condutores
  envolvidos, ou seja, nos outros 45,39% acidentes relatados com vítimas fatais não 
  teve condutores mortos."
  
}
lst_tables$ferimentos_condutor$numero_pessoas_envolvidas$comentario <- {
  
  "Aqui temos a relação entre número de pessoas envolvidas e grau de severidade dos 
  ferimentos de pelo menos um dos condutores presente nas ocorrências. Veja que em 
  1,35% dos acidentes com 1 pessoa envolvida, essa pessoa era um condutor e acabou 
  morrendo. Veja também que em 1,82% dos acidentes com mais de 4 pessoas envolvidas 
  teve pelo menos uma vítima fatal entre os condutores."
}
lst_tables$ferimentos_condutor$numero_veiculos_envolvidos$comentario <- {
  
  "A tabela exibida aqui relaciona o grau de severidade dos ferimentos de 
  pelo menos um dos condutores envolvidos com o número de veículos nas ocorrências. 
  Note que dos acidentes com 1 veículo envolvido, em 7,13% deles não houve informação 
  sobre o grau dos ferimentos do condutor envolvido nessas ocorrências. Para 
  acidentes com mais veículos envolvidos, a taxa de não-informação dos ferimentos de 
  pelo menos um dos condutores representaram um percentual maior dessas ocorrências. Por 
  exemplo, em 15,03% dos acidentes com mais de 4 veículos envolvidos teve pelo menos para 
  um dos condutores a não-informação do grau de severidade dos seus acidentes. Veja 
  também que quase 2% dos acidentes com mais de 4 veículos levou a óbito pelo menos 
  um dos condutores que participaram dessas ocorrências."
}
lst_tables$ferimentos_condutor$sexo_condutor$comentario <- {
  
  "A tabela abaixo relaciona o grau de severidade dos ferimentos dos condutores e a 
  participação por sexo de pelo menos um dos condutores envolvidos nas ocorrências. 
  As linhas aqui não são mutuamente exclusivas, ou seja, acidentes contados em uma 
  das linhas também podem estar sendo considerados nas outras,
  Veja que interessante, em 0,23% dos acidentes envolvendo condutoras do sexo feminino 
  teve pelo menos um dos condutores como vítima fatal, para os acidentes envolvendo 
  condutores do sexo masculino em 0,52% deles teve pelo menos um condutor morrendo. 
  Veja também que em mais de 80% dos acidentes em que não foi informado o sexo de 
  pelo menos um dos condutores também não foi informado o grau de severidade dos 
  ferimentos de pelo menos um deles."
  
} 
lst_tables$ferimentos_condutor$faixa_idade_condutor$comentario <- {
  
  "Essa tabela relaciona o grau de severidade dos ferimentos de pelo menos um dos 
  condutores por participação por faixa etária de pelo menos um dos condutores nas 
  ocorrências. As linhas aqui não são mutuamente exclusivas, ou seja, acidentes 
  contados em uma das linhas também podem estar sendo considerados nas outras, 
  já que em um acidente há a possibilidade de condutores com faixas etárias diferentes. 
  Veja que dos acidentes relatados envolvendo pelo menos um condutor de até 20 anos 
  de idade, em pouco mais de 1% deles houve morte de pelo menos um dos condutores 
  envolvidos, esse percentual é bastante alto, principalmente que somente 0,5% dos 
  acidentes na cidade têm condutores como vítimas fatais."
  
}
lst_tables$ferimentos_condutor$indicio_uso_cinto_seguranca$comentario <- {
  
  "Essa tabela relaciona o grau de severidade dos ferimentos de pelo menos um dos 
  condutores por falta do uso de cinto de segurança de algum dos condutores nas ocorrências. 
  Veja que em quase 80% dos acidentes em que todos os condutores usavam cinto de 
  segurança teve pelo menos um dos condutores sem ferimentos. Em acidentes onde pelo 
  menos um dos condutores não usava cinto, em 63% deles houve pelo menos um dos 
  condutores sem ferimentos."
  
}
lst_tables$ferimentos_condutor$tipo_acidente$comentario <- {
  
  "Essa tabela relaciona o grau de severidade dos ferimentos de pelo menos um 
  dos condutores por tipo de acidente ocorrido. Veja que interessante, em quase 1,65% 
  dos acidentes relatados como outros tipos teve morte de pelo menos um dos condutores 
  envolvidos. As colisões de veículos com objetos parados também tiveram frequência 
  relativa alta de fatalidade de pelo menos um dos condutores, 1,2% dessas ocorrências."
}
lst_tables$ferimentos_condutor$descricao_veiculos$comentario <- {
  
  "Essa tabela relaciona o grau de severidade dos ferimentos de pelo menos 
  um dos condutores envolvidos e as 15 combinações de veículos mais frequentes 
  envolvidos em acidentes na capital. Veja que interessante, em 3,72% dos 
  acidentes envolvendo 1 caminhão e 1 motocicleta teve pelo menos um desses 
  2 condutores envolvidos morrendo em decorrência desses acidentes, não há 
  confirmação sobre isso, mas é de se imaginar que provavelmente são os pilotos 
  das motocicletas. Essa é a combinação de veículos envolvidos em acidentes 
  com maior incidência de fatalidade de condutores. Veja também que em quase 
  1/4 dos acidentes com 2 motocicletas não teve informações sobre os ferimentos 
  de pelo menos um dos 2 pilotos envolvidos."
}


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

lst_tables$indicio_uso_cinto_seguranca$comentario <- {
  
  "Essa tabela mostra as frequências absoluta e relativa do uso de cinto de segurança pelos 
  condutores nos acidentes na capital entre 2016 e 2019. Em 67,49% das ocorrências todos os condutores 
  envolvidos usavam cinto de segurança."
}

lst_tables$indicio_uso_cinto_seguranca$periodo_dia$comentario <- {
  
  "A tabela exibida abaixo mostra a distribuição do uso de cinto de segurança por 
  todos os condutores envolvidos em acidentes ocorridos em cada período do dia. Note 
  que nos acidentes acontecidos pelas manhãs, em quase 70% deles todos os condutores 
  envolvidos usavam cinto de segurança, dos acidentes ocorridos nas madrugadas, o 
  percentual deles com todos os condutores usando cinto foi menor, 63% desses acidentes."
  
}
lst_tables$indicio_uso_cinto_seguranca$indicio_embriaguez_acidente$comentario <- {
  
  "Essa tabela mostra a distribuição do uso de cinto pelos condutores por indício de 
  embriaguez ou não nas ocorrências. Veja que dos acidentes relatados com embriaguez, 
  em 69,22% deles todos os condutores usavam cinto de segurança, já em acidentes 
  relatados sem indício de embriaguez dos envolvidos, o percentual de acidentes em que 
  todos os condutores usavam de cinto de segurança  foi de 67,43%."

}
lst_tables$indicio_uso_cinto_seguranca$indicio_pedestre_envolvido$comentario <- {
  
  "A tabela abaixo mostra a distribuição do uso de cinto de segurança dos condutores 
  por acidentes envolvendo ou não pedestre(s). Veja que em 65,21% das ocorrências de 
  acidentes com pedestre(s) teve todos os condutores envolvidos com cinto de segurança. 
  Dos acidentes sem pedestre(s), o percentual deles com todos os condutores usando 
  cinto de segurança foi de 67,86%."
  
}
lst_tables$indicio_uso_cinto_seguranca$indicio_evasao$comentario <- {
  
  "Aqui há a distribuição do uso de cinto dos condutores por indício 
  ou não de evasão nas ocorrências. Veja que interessante, em quase 99% 
  das ocorrências com indício de evasão foi assinalado que pelo menos um dos condutores 
  estava sem cinto de segurança, já nos acidentes sem indício de evasão 
  o percentual deles com pelo menos um dos condutores não usando cinto de segurança foi 
  de 27,69%. Parece que houve um viés no preenchimento dos dados, no sentido de 
  que quando não foi preenchido informações cruciais para identificação dos condutores 
  envolvidos (indício de evasão), foi assinalado que essas pessoas não estavam usando cinto."
}
lst_tables$indicio_uso_cinto_seguranca$indicio_fatalidade$comentario <- {
  
  "Aqui há a distribuição do uso de cinto de segurança pelos condutores e 
  fatalidade ou não nas ocorrências. Em quase 65% dos acidentes relatados 
  com pelo menos uma vítima fatal tinha todos os condutores envolvidos usando 
  cinto de segurança. Já nos acidentes sem vítimas fatais em 67,52% deles 
  todos os condutores estavam usando cinto de segurança."
  
}
lst_tables$indicio_uso_cinto_seguranca$numero_pessoas_envolvidas$comentario <- {
  
  "Aqui têm-se as distribuições do uso de cinto de segurança pelos condutores 
  nas ocorrências pelo número de pessoas envolvidas. Veja que em 78,33% dos acidentes 
  com 1 pessoa envolvida, somente o condutor, esse estava usando cinto de segurança no 
  momento do acidente. Veja que as frequências relativas de uso de cinto por todos os 
  condutores é consideravelmente menor nos acidentes envolvendo mais pessoas."

}
lst_tables$indicio_uso_cinto_seguranca$numero_veiculos_envolvidos$comentario <- {
  
  "Aqui está exibida a distribuição do uso de cinto de segurança dos condutores 
  por número de veículos envolvidos nas ocorrências. Note que em quase 40% dos 
  acidentes com mais de 4 veículos envolvidos tinha pelo menos um dos condutores
  sem cinto de segurança."
}
lst_tables$indicio_uso_cinto_seguranca$sexo_condutor$comentario <- {
  
  "Essa tabela mostra a distribuição do uso de cinto de segurança por parte dos 
  condutores em acidentes que envolveram pelo menos um condutor de determinado sexo. Lembre-se 
  que acidentes considerados em uma linha podem também ser considerados nas outras,
  já que os condutores envolvidos em um mesmo acidente podem ter sexos distintos. Veja 
  que em quase 72% dos acidentes relatados com pelo menos uma condutora do sexo feminino 
  tiveram todos os condutores envolvidos usando cinto de segurança. Nos acidentes envolvendo 
  condutores do sexo masculino esse percentual foi de quase 69% e em acidentes onde não 
  foi informado o sexo de pelo menos um dos condutores em quase 100% deles pelo menos um 
  dos condutores envolvidos não estava usando o cinto de segurança."
}
lst_tables$indicio_uso_cinto_seguranca$faixa_idade_condutor$comentario <- {
  
  "A tabela exibida aqui mostra a distribuição do uso de cinto segurança por parte 
  dos condutores e participação da faixa etária de pelo menos um dos condutores 
  envolvidos. Lembre-se que acidentes considerados em uma linha também podem estar 
  sendo considerado nas outras, já que em um acidente pode ter condutores de faixas 
  etárias diferentes. Veja que quase metade dos acidentes envolvendo condutores com 
  até 20 anos teve pelo menos um dos condutores sem cinto de segurança. No entanto, 
  lembre-se que acidentes envolvendo condutores dessa faixa etária também envolveram 
  muitas bicicletas e esse veículo não possui cinto de segurança."
  
}
lst_tables$indicio_uso_cinto_seguranca$ferimentos_condutor$comentario <- {
  
  "Aqui há a distribuição do indício de uso de cinto de segurança por parte dos 
  condutores pelo grau de severidade dos ferimentos de pelo menos um desses condutores. 
  Veja que em quase 66% dos acidentes em que teve pelo menos um condutor morrendo, todos 
  os condutores envolvidos usavam cinto de segurança."
}
lst_tables$indicio_uso_cinto_seguranca$tipo_acidente$comentario <- {
  
  "Aqui há a distribuição do uso de cinto de segurança pelos condutores por tipo de 
  acidente ocorrido. Note que em quase 78% dos capotamentos de veículos foi assinalado que 
  todos os condutores usavam cinto de segurança no momento do incidente. Já nos acidentes 
  relatados como outros tipos, somente em 58,41% deles tiveram todos os condutores usando 
  cinto de segurança."
  
}
lst_tables$indicio_uso_cinto_seguranca$descricao_veiculos$comentario <- {
  
  "Aqui há a distribuição do uso de cinto de segurança pelos condutores e as 15 combinações 
  de veículos mais frequentes nos acidentes relatados na capital. Note que dos acidentes 
  contendo 1 motocicleta, em quase 75% deles havia indício do uso de cinto de segurança do 
  condutor envolvido (aqui o cinto de segurança pode ser encarado como o uso de capacete), 
  já nos acidentes relatados com 2 motocicletas, somente em 59,62% deles tiveram ambos 
  os pilotos usando cinto de segurança."
} 

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
lst_tables$periodo_dia$comentario <- {
  
  "A tabela exibida mostra a distribuição da frequência de acidentes ocorridos 
  por período do dia. Note que cerca de 35% dos acidentes aconteceram no período 
  da tarde, entre meio-dia e 18h."
}

lst_tables$periodo_dia$indicio_embriaguez_acidente$comentario <- {
  
  "A tabela exibida aqui mostra a distribuição do período do dia dos acidentes 
  com e sem indício de embriaguez. Veja que interessante, quase 43% dos acidentes 
  com indício de embriaguez de pelo menos um dos envolvidos aconteceu no período 
  da noite e 25,56% deles no período da madrugada."
  
}
lst_tables$periodo_dia$indicio_pedestre_envolvido$comentario <- {
  
  "A tabela abaixo relaciona o período em que as ocorrências aconteceram e o 
  envolvimento ou não de pedestres. Note que mais de 1/3 dos acidentes envolvendo 
  pedestres ocorreu no período da noite, outro pouco mais de 1/3 desses acidentes 
  ocorreu no período da tarde."
  
}
lst_tables$periodo_dia$indicio_evasao$comentario <- {
  
  "Aqui há a distribuição do periodo em que as ocorrências com evasão e sem 
  evasão dos envolvidos aconteceram. gera um questionamento interessante: vimos 
  que os acidentes registrados se distribuem quase que de maneira igual entre 
  manhã, tarde e da noite. No entanto, dos acidentes que houve indício de 
  evasão de pelo menos uma das partes envolvidas, somente 24% foram de manhã e quase 
  35% deles a noite. Qual seria uma explicação para isso?"
  
}
lst_tables$periodo_dia$indicio_fatalidade$comentario <- {
  
  "A tabela exibida aqui mostra a distribuição do período do dia em que 
  os acidentes com e sem fatalidade ocorreram.. Note que os acidentes 
  com pelo menos uma vítima fatal se distribuem diferentemente dos 
  registros de acidentes sem vítimas fatais ao longo dos períodos do dia. 
  Nas manhãs e tardes, os acidentes sem vítima fatal ocupam maiores 
  frequências relativas em comparação com o mesmo período em acidentes 
  com fatalidade. Já nos períodos da noite e da madrugada, o contrário 
  acontece. Cerca de 37% dos acidentes relatados com fatalidade acontecem 
  no período da noite e cerca de 19% acontece nas madrugadas, tendo esse 
  período frequência relativa 3 vezes maior se comparado com o percentual 
  de acidentes sem fatalidade que acontecem no mesmo período do dia."
}
lst_tables$periodo_dia$numero_pessoas_envolvidas$comentario <- {
  
  "Essa tabela mostra a distribuição do período do dia que as ocorrências com 
  determinado número de pessoas envolvidas aconteceram. Note por exemplo, que 
  quase 16% dos acidentes ocorridos com 1 pessoa envolvida foram no período 
  da madrugada."
  
}
lst_tables$periodo_dia$numero_veiculos_envolvidos$comentario <- {
  
  "Nessa tabela está temos a distribuição do número de veículos envolvidos nas 
  ocorrências relatadas por período do dia. Note como há grandes diferenças de 
  percentuais entre as linhas. Por exemplo, dos acidentes relatados com 1 veículo, 
  cerca de 30% deles aconteceram a noite. Nos acidentes relatados com mais de 
  4 veículos, somente cerca de 18% deles aconteram nesse período do dia."
  
}
lst_tables$periodo_dia$sexo_condutor$comentario <- {
  
  "Essa tabela mostra a distribuição do período do dia que ocorreram os 
  acidentes com pelo menos um condutor de determinado sexo. As linhas nessa 
  tabela não apresentam ocorrências mutuamente exclusivas, ou seja, acidentes 
  considerados em uma das linhas também podem estar sendo considerado nas 
  outras, já que podemos ter condutores de sexo distinto se envolvendo no 
  mesmo acidente. Note que interessante, dos acidentes em que não foi relatado 
  o sexo de pelo menos um dos condutores, somente cerca de 1/4 deles aconteceram 
  pelas manhãs, já o percentual de acidentes envolvendo pelo menos uma condutora 
  do sexo feminino e pelo menos um condutor do sexo masculino, 33,8% e 30,25% deles 
  aconteceram nesse período, respectivamente."
  
}
lst_tables$periodo_dia$faixa_idade_condutor$comentario <- {
  
  "Essa tabela mostra a distribuição do período do dia que ocorreram os 
  acidentes com pelo menos um condutor de determinada faixa etária. As linhas 
  nessa tabela não são mutuamente exclusivas, ou seja, ocorrências consideradas 
  em uma das linhas também podem ser consideradas em outras. Observe como 
  os percentuais de acidentes por período do dia variam ao longo das faixas etárias 
  de pelo menos um dos condutores, por exemplo: somente cerca de 21% dos acidentes 
  que envolvem condutores de até 20 anos foram pelas manhãs, enquanto que 35% dos 
  acidentes envolvendo condutores de 70 anos ou mais foram nesse período."
  
}
lst_tables$periodo_dia$ferimentos_condutor$comentario <- {
  
  "A tabela abaixo exibe a distribuição dos acidentes por período do dia 
  discriminados por grau de severidade de pelo menos um dos condutores envolvidos. 
  Mais uma vez há o lembrete de que acidentes considerados em uma das linhas também 
  podem ter sido considerado nas outras linhas. Note por exemplo que quase 20% dos 
  acidentes que teve pelo menos um condutor como vítima fatal ocorreram pela manhã e 
  que mais de 31% dos acidentes com pelo menos um condutor sem ferimentos também ocorreram 
  nesse período. Repare também que quase 1/4 dos acidentes que vitimaram condutores 
  aconteceram nas madrugadas."
}
lst_tables$periodo_dia$indicio_uso_cinto_seguranca$comentario <- {
  
  "A tabela abaixo mostra a distribuição do período do dia que aconteceram as 
  ocorrências discriminado pelo uso do cinto de segurança ou não de todos os envolvidos. 
  Note que não há grandes diferenças nas frequências relativas do período do dia 
  que os acidentes com todos os condutores usando cinto em relação aos acidentes em 
  que pelo menos um condutor tinha indício de não usar a proteção."
}
lst_tables$periodo_dia$tipo_acidente$comentario <- {
  
  "A tabela abaixo mostra o uso de cinto de segurança pelos condutores por tipo 
  de acidente. Veja por exemplo, que em quase 78% dos capotamentos de veículos, todos 
  os condutores usavam cinto de segurança. Observe que há acidentes em que o percentual 
  de falta de uso de segurança de pelo menos um dos condutores envolvidos foram maiores 
  que as outras linhas."
  
}
lst_tables$periodo_dia$descricao_veiculos$comentario <- {
  
  "Essa tabela relaciona o uso de cinto de segurança por parte dos condutores e 
  as 15 combinações de veículos que mais se envolveram em ocorrências na capital. 
  Note que somente em pouco mais de 1% dos acidentes com 1 automóvel e 1 biclicleta 
  tinham todos os condutores envolvidos usando cinto de segurança. Nesse caso, 
  o uso de cinto de segurança deve ser encarado como o uso de capacete por parte dos 
  ciclistas (do mesmo modo que para acidentes envolvendo motocicletas), já que 
  bicicletas não possuem cinto de segurança."
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

lst_tables$tipo_acidente$comentario <- {
  
  "Veja que quase 60% dos acidentes relatados ocorridos na capital no período entre 
  2016 e final de 2019 foi colisão entre veículos. Os tipos de acidentes menos 
  frequentes foram atropelamento de animais e vazamento de carga do veículo, ambos 
  representando menos que 1% das ocorrências."
}

lst_tables$tipo_acidente$indicio_embriaguez_acidente$comentario <- {
  
  "Aqui está exibida a distribuição do tipo de acidente por envolvimento ou 
  não de pedestre(s). Diferentemente das outras tabelas, aqui os percentuais das 
  COLUNAS somam 100%. Veja que somente 93,56% dos acidentes com pedestres foram 
  relatados como atropelamento dos mesmos, ou seja, 6,44% dos outros acidentes 
  envolveram pedestres de outra maneira ou foram assinalados de maneira equivocada."
  
}
lst_tables$tipo_acidente$indicio_pedestre_envolvido$comentario <- {
  
  "Aqui está exibida a distribuição do tipo de acidente por indício de embriaguez 
  nas ocorrências. Diferentemente da maioria das outras tabelas, aqui os percentuais 
  das COLUNAS somam 100%. Note como há informações interessantes contidas nessa 
  tabela. Em quase 30% dos acidentes relatados com algum dos envolvidos embriagado foram 
  atropelamentos de pedestres. Já dos acidentes sem indício de embriaguez, somente 
  12,24% deles foram atropelamentos de pedestres."
  
}
lst_tables$tipo_acidente$indicio_evasao$comentario <- {
  
  "A tabela abaixo mostra a distribuição do tipo de acidente por indício de evasão 
  das ocorrências. Diferentemente da maioria das outras tabelas, aqui os percentuais 
  das COLUNAS somam 100%. Note que mais de 2/3 dos acidentes com indício de evasão 
  foram em colisões de veículos. 20,29% dos acidentes com evasão ocorreram em atropelamento 
  de pedestres."
  
}
lst_tables$tipo_acidente$indicio_fatalidade$comentario <- {
  
  "A tabela abaixo mostra a distribuição do tipo de acidente por fatalidade de 
  algum dos envolvidos nas ocorrências. Diferentemente da maioria das outras tabelas, 
  aqui os percentuais das COLUNAS somam 100%. Veja que mais de 35% dos acidentes com 
  pelo menos um dos envolvidos morrendo foram em atropelamentos de pedestres. Note também 
  que quase 30% dos acidentes com fatalidade foram em colisões de veículos."
}
lst_tables$tipo_acidente$numero_pessoas_envolvidas$comentario <- {
  
  "Essa tabela mostra a distribuição do tipo de acidente por número de pessoas envolvidas nas
  ocorrências. Diferentemente da maioria das outras tabelas, aqui os percentuais das COLUNAS 
  somam 100%. Veja que interessante, mais da metade dos acidentes com 1 pessoa envolvida foram 
  capotamentos de veículos. Note também como os percentuais dessa coluna são diferentes em 
  comparação com as colunas que representam acidentes com mais pessoas envolvidas."
}
lst_tables$tipo_acidente$numero_veiculos_envolvidos$comentario <- {
  
  "Aqui há a distribuição do tipo de acidente pelo número de veículos envolvidos. 
  Diferentemente da maioria das outras tabelas, aqui os percentuais 
  das COLUNAS somam 100%. Veja que interessante, mais de 43% dos acidentes envolvendo 
  1 veículo foram atropelamento de pedestres. Mais de 84% dos acidentes envolvendo 
  2 veículos foram colisões entre eles."
}
lst_tables$tipo_acidente$sexo_condutor$comentario <- {
  
  "A tabela abaixo mostra a distribuição por tipo de acidente por participação 
  por sexo de pelo menos um dos condutores envolvidos. Lembre-se que diferentemente 
  da maioria das outras tabelas, aqui os percentuais das COLUNAS somam 100% e que 
  as colunas não são mutuamente exclusivas, ou seja, acidentes considerados em uma 
  das colunas também podem estar sendo considerado nas outras. Veja que as colisões 
  foram os acidentes mais frequentes para todos os sexos dos condutores. Note também 
  que somente pouco mais de 7% dos acidentes envolvendo condutoras do sexo feminino 
  foram atropelamentos de pedestres. Para os condutores do sexo masculino, os 
  atropelamentos de pedestres representaram 10,33% dos tipos de ocorrências que condutores 
  desse sexo tiveram participação."
  
}
lst_tables$tipo_acidente$faixa_idade_condutor$comentario <- {
  
  
  "Aqui há a distribuição por tipo de acidente por participação 
  por faixa etária de pelo menos um dos condutores envolvidos. 
  Lembre-se que diferentemente da maioria das outras tabelas, aqui 
  os percentuais das COLUNAS somam 100% e que as colunas não são 
  mutuamente exclusivas, ou seja, acidentes considerados em uma das 
  colunas também podem estar sendo considerado nas outras. Veja que 
  as colisões foram os acidentes mais frequentes para todas as faixas etárias."
  
}
lst_tables$tipo_acidente$ferimentos_condutor$comentario <- {
  
  "Aqui há a distribuição por tipo de acidente por grau de severidade dos 
  acidentes de pelo menos um dos condutores envolvidos. Diferentemente da 
  maioria das outras tabelas, aqui os percentuais das COLUNAS somam 100% e 
  as colunas não são mutuamente exclusivas, ou seja, acidentes considerados 
  em uma das colunas também podem estar sendo considerado nas outras. Observe 
  que quase 36% dos acidentes com pelo menos um condutor morto foram colisões de 
  veículos com objetos parados. Note também que em mais de 18% dos acidentes 
  onde não houve informação sobre o grau de severidade de pelo menos um dos 
  condutores foram atropelamentos de pedestres."
  
}
lst_tables$tipo_acidente$indicio_uso_cinto_seguranca$comentario <- {
  
  "Aqui está exibida a distribuição do tipo de acidente por uso de cinto de segurança 
  por parte dos condutores. Diferentemente das outras tabelas, aqui os percentuais das 
  COLUNAS somam 100%. Note que dos acidentes relatados com todos os condutores envolvidos 
  usando cinto de segurança, em 57,46% deles eram colisões de veículos. Já em acidentes 
  onde pelo menos um dos condutores não usavam cinto de segurança, 64% deles foram 
  colisões entre veículos."
  
}
lst_tables$tipo_acidente$periodo_dia$comentario <- {
  
  "Abaixo está exibida a distribuição do tipo de acidente por período do dia que 
  as ocorrências aconteram. Diferentemente das outras tabelas, aqui os percentuais das 
  COLUNAS somam 100%. Observe como a madrugada tem comportamento diferente em relação 
  aos outros períodos do dia. Por exemplo, colisões de veículos representaram cerca de 
  60% dos acidentes ocorridos nas manhãs, tardes e noites, mas nas madrugadas somente 
  cerca de 43% dos acidentes que aconteceram nesse período foram colisões de veículos. 
  Note também que quase 30% dos acidentes ocorridos nesse período foram colisões de 
  veículos com objetos parados."
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

lst_tables$descricao_veiculos$comentario <- {
  
  "A tabela mostra as 15 combinações de veículos mais frequentes nos acidentes na 
  capital. Veja que quase 40% dos acidentes envolveram 1 automóvel e 1 motocicleta."
}

lst_tables$descricao_veiculos$indicio_embriaguez_acidente$comentario <- {
  
  "Essa tabela exibe a relação entre as 15 combinações de veículos que mais se 
  envolveram em acidentes na capital entre 2016 e 2019 e o indício de embriaguez 
  nas ocorrências. Diferentemente maioria das outras tabelas, aqui os percentuais das 
  COLUNAS somam 100%. Veja que em quase 1/3 dos acidentes com indício de embriaguez, tinha 
  apenas um condutor e esse estava em 1 automóvel (Lembre-se que a pessoa com sinais 
  de embriaguez pode não ter sido o condutor, nessas ocorrências podem ter também 
  o envolvimento de passageiros e pedestres)."
  
}
lst_tables$descricao_veiculos$indicio_pedestre_envolvido$comentario <- {
  
  "Essa tabela exibe a relação entre as 15 combinações de veículos que mais se 
  envolveram em acidentes na capital entre 2016 e 2019 e o envolvimento de pedestres 
  nas ocorrências. Diferentemente maioria das outras tabelas, aqui os percentuais das 
  COLUNAS somam 100%. Observe que mais da metade dos acidentes que envolveram pedestres 
  teve somente 1 automóvel envolvido. Note também que desses acidentes envolvendo 
  pedestres, em quase 31% deles teve somente 1 motocicleta envolvida."
  
}
lst_tables$descricao_veiculos$indicio_evasao$comentario <- {
  
  "Essa tabela exibe a relação entre as 15 combinações de veículos que mais se 
  envolveram em acidentes relatados e o indício de evasão de pelo menos um dos 
  envolvidos nas ocorrências. Diferentemente maioria das outras tabelas, aqui 
  os percentuais das COLUNAS somam 100%. Veja que um pouco mais de 44% dos acidentes 
  com indício de evasão tiveram 1 automóvel e 1 motocicleta envolvida. Dos acidentes 
  sem indício de evasão, cerca de 44% deles também envolveram 1 automóvel e 
  1 motocicleta."
  
}
lst_tables$descricao_veiculos$indicio_fatalidade$comentario <- {
  
  "A tabela abaixo relaciona as 15 combinações de veículos que mais se envolveram 
  em acidentes relatados na capital e as ocorrências com fatalidade de pelo menos um 
  dos envolvidos.Diferentemente maioria das outras tabelas, aqui os percentuais das 
  COLUNAS somam 100%. Observe que 27,7% dos acidentes com fatalidade tinham somente 
  1 automóvel envolvido, 25,48% dessas ocorrências somente 1 motocicleta e 6,65% delas 
  1 caminhão e 1 motocicleta."
}
lst_tables$descricao_veiculos$numero_pessoas_envolvidas$comentario <- {
  
  "Essa tabela exibe a relação entre as 15 combinações de veículos que mais se 
  envolveram em acidentes relatados e o número de pessoas envolvidas nas ocorrências. 
  Diferentemente maioria das outras tabelas, aqui os percentuais das COLUNAS somam 
  100%. Note por exemplo que 68,5% dos acidentes envolvendo somente 1 pessoa também 
  envolveram motocicletas, ou seja, esses acidentes envolveram somente o piloto da 
  motocicleta envolvida."
  
}
lst_tables$descricao_veiculos$numero_veiculos_envolvidos$comentario <- {
  

  "Nessa tabela há a relação entre as 15 combinações de veículos que mais se 
  envolveram em acidentes relatados e o número de veículos nas ocorrências. 
  Diferentemente maioria das outras tabelas, aqui os percentuais das COLUNAS somam 
  100%. Apesa de parecer redundante, essa tabela possui informações relevantes, por 
  exemplo, veja que 46,21% dos acidentes com 1 veículo foram com motocicletas e 37,7% 
  deles com automóveis."
  
}
lst_tables$descricao_veiculos$sexo_condutor$comentario <- {
  
  "Essa tabela exibe a relação entre as 15 combinações de veículos que mais se 
  envolveram em acidentes relatados por participação por sexo de pelo menos um dos 
  condutores envolvidos. Lembre-se que diferentemente da maioria das outras tabelas, 
  aqui os percentuais das COLUNAS somam 100% e que as colunas não são mutuamente 
  exclusivas, ou seja, acidentes considerados em uma das colunas também podem estar 
  sendo considerado nas outras. Observe por exemplo, que mais da metade dos acidentes 
  envolvendo pelo menos uma condutora do sexo feminino tiveram 2 condutores envolvidos, 
  um deles dirigindo 1 automóvel e o outro pilotando uma motocicleta."
  
}
lst_tables$descricao_veiculos$faixa_idade_condutor$comentario <- {
  
  "Essa tabela exibe a relação entre as 15 combinações de veículos que mais se 
  envolveram em acidentes relatados por participação por faixa etária de pelo 
  menos um dos condutores envolvidos.  Lembre-se que diferentemente da maioria 
  das outras tabelas, aqui os percentuais das COLUNAS somam 100% e que as colunas 
  não são mutuamente exclusivas, ou seja, acidentes considerados em uma das 
  colunas também podem estar sendo considerado nas outras. Veja que interessante, 
  em quase 13% dos acidentes envolvendo condutores de até 20 anos teve 1 automóvel 
  e 1 bicicleta envolvidos. Note também como as frequências relativas de acidentes 
  envolvendo somente 1 motocicleta vão caindo com o passar das faixas etárias."
  
}
lst_tables$descricao_veiculos$ferimentos_condutor$comentario <- {
  
  "Essa tabela exibe a relação entre as 15 combinações de veículos que mais se 
  envolveram em acidentes relatados por grau de severidade dos 
  acidentes de pelo menos um dos condutores envolvidos. Diferentemente da 
  maioria das outras tabelas, aqui os percentuais das COLUNAS somam 100% e 
  as colunas não são mutuamente exclusivas, ou seja, acidentes considerados 
  em uma das colunas também podem estar sendo considerado nas outras. Note por exemplo,
  mais de 31% dos acidentes com morte de pelo menos um dos condutores ocorreram 
  em acidentes com somente 1 motocicleta. Outros acidentes com motocicletas aparecem 
  entre os mais fatais para condutores, como acidentes envolvendo 1 automóvel e 1 
  motocicleta e acidentes envolvendo 1 caminhão e 1 motocicleta."
}
lst_tables$descricao_veiculos$indicio_uso_cinto_seguranca$comentario <- {
  
  "Na tabela abaixo está a relação entre as 15 combinações de veículos que mais se 
  envolveram em acidentes relatados por uso de cinto de segurança por parte dos 
  condutores. Diferentemente das outras tabelas, aqui os percentuais das COLUNAS 
  somam 100%. Observe que quase 46% dos acidentes em que todos os condutores usavam 
  cinto de segurança envolveram 1 automóvel e 1 motocicleta."
  
  
}
lst_tables$descricao_veiculos$periodo_dia$comentario <- {
  
  "Aqui há a relação entre as 15 combinações de veículos mais envolvidos em acidentes 
  relatados na capital e o período do dia que as ocorrências aconteceram. Diferentemente 
  das outras tabelas, aqui os percentuais das COLUNAS somam 100%. Veja que a madrugada 
  possui um comportamento de frequências relativas ao longo das linhas bem diferente em 
  relação aos outros períodos do dia. Por exemplo, enquanto os acidentes envolvendo 
  1 automóvel e 1 motocicleta representam cerca de 45% dos acidentes ocorridos nas manhãs, 
  tardes e noites, nas madrugadas essa combinação de veículos só se envolveu em 25,26% 
  dos acidentes no período."
}

save(lst_tables, file = "D:/Projetos/acidentes_bh/shiny_acidentes_bh/lst_tables.RData")

