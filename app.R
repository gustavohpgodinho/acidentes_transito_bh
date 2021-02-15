#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



# Define UI for application that draws a histogram
ui <- shiny::navbarPage(
    
    title = 'Acidentes de trânsito em BH',
    pagina_inicial(),
    pagina_mapa(),
    pagina_lf(),
    pagina_distribuicao_bairros(),
    pagina_tabelas(),
    pagina_consideracoes()
    
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {

    df <- db_ocorrencias_transito_bh$ocorrencias %>% 
        dplyr::mutate(data_hora1 = format(data_hora, '%d/%m/%Y as %H:%M')) %>% 
        dplyr::filter(!is.na(lat))
     
    dfpesquisamapa <- shiny::reactive({
        
        if(length(input$pesquisa_mapa_obs) == 0){
        
            df 
        
        } else{
        
            pesquisa <- input$pesquisa_mapa_obs %>% 
                stringr::str_replace_all(pattern = '[:punct:]', replacement = ' ') %>% 
                stringr::str_to_lower() %>% 
                stringr::str_trim() %>% 
                stringi::stri_trans_general(id ="latin-ascii") %>% 
                data.frame(value = ., stringsAsFactors = FALSE) %>% 
                dplyr::as_tibble() %>%
                dplyr::mutate(key = stringr::str_extract(value, c("^regional|^bairro|^ano|^mes|^dia"))) %>% 
                dplyr::mutate(value = ifelse(is.na(key), value, stringr::str_remove(value, key))) %>% 
                dplyr::mutate_all(.funs = stringr::str_trim) %>%
                dplyr::mutate(key = ifelse(value %in% c('queda de pessoa de veiculo', 'atropelamento de pedestre',
                                                        'colisao de veiculos', 'colisao de veiculo com objeto parado',
                                                        'capotamento de veiculo', 'capotamento de veiculo',
                                                        'outros tipos de acidentes', 'atropelamento de animal',
                                                        'vazamento de carga do veiculo'), 'tipo_acidente', key)) %>% 
                dplyr::group_split(key) %>% 
                plyr::llply(., function(item){
                    
                if(is.na(unique(item$key))){
                    
                    df %>% 
                        dplyr::filter(grepl(stringr::str_c(item$value, collapse = '|'), logradouros)) %>% 
                        dplyr::distinct(num_boletim) %>% 
                        dplyr::mutate(auxcolumn = length(item$value))
                    
                } else{
                    
                    if(unique(item$key) == 'regional'){
                        
                        df %>% 
                            dplyr::filter(regional %in% stringr::str_to_upper(unique(item$value))) %>% 
                            dplyr::distinct(num_boletim) %>% 
                            dplyr::mutate(auxcolumn = length(item$value))
                        
                    } else if(unique(item$key) == 'bairro'){
                        
                        df %>% 
                            dplyr::filter(bairro %in% stringr::str_to_upper(unique(item$value))) %>% 
                            dplyr::distinct(num_boletim) %>% 
                            dplyr::mutate(auxcolumn = length(item$value))
                        
                    } else if(unique(item$key) == 'ano'){
                        
                        df %>% 
                            dplyr::filter(lubridate::year(data_hora) %in% unique(as.integer(item$value))) %>% 
                            dplyr::distinct(num_boletim) %>% 
                            dplyr::mutate(auxcolumn = length(item$value))
                        
                    } else if(unique(item$key) == 'mes'){
                        
                        df %>% 
                            dplyr::mutate(aux = lubridate::month(data_hora)) %>% 
                            dplyr::mutate(aux = c('janeiro', 'fevereiro', 'marco', 
                                                  'abril', 'maio', 'junho', 
                                                  'julho', 'agosto', 'setembro', 
                                                  'outubro', 'novembro', 'dezembro')[aux]) %>% 
                            dplyr::filter(aux %in% unique(item$value)) %>% 
                            dplyr::distinct(num_boletim) %>% 
                            dplyr::mutate(auxcolumn = length(item$value))
                        
                    } else if(unique(item$key) == 'dia'){
                        
                        df %>% 
                            dplyr::filter(lubridate::day(data_hora) %in% unique((as.integer(item$value)))) %>% 
                            dplyr::distinct(num_boletim) %>% 
                            dplyr::mutate(auxcolumn = length(item$value))
                        
                    } else if(unique(item$key) == 'tipo_acidente'){
                        
                        df %>% 
                            dplyr::filter(desc_acidente %in% item$value) %>% 
                            dplyr::distinct(num_boletim) %>% 
                            dplyr::mutate(auxcolumn = length(item$value))
                        
                    } else{
                        
                        df %>% 
                            dplyr::distinct(num_boletim) %>% 
                            dplyr::mutate(auxcolumn = length(item$value))
                    }
                    
                }
                    
                }) %>% 
                dplyr::bind_rows() %>% 
                dplyr::group_by(num_boletim) %>% 
                dplyr::summarise(n = sum(auxcolumn, na.rm = TRUE),.groups = 'keep') %>% 
                dplyr::ungroup() %>% 
                dplyr::filter(n == length(input$pesquisa_mapa_obs)) %>% 
                dplyr::pull(num_boletim)
        
            dfreturn <- df %>%
                dplyr::filter(num_boletim %in% pesquisa)
            
            if(nrow(dfreturn) == 0){
                
                df %>% 
                    dplyr::mutate(aux_column = 1)
            } else{
                
                dfreturn %>% 
                    dplyr::mutate(aux_column = 0)
            }
        
    }})
    
    output$mapa_principal <- leaflet::renderLeaflet({
        
        dfpesquisamapa() %>% 
            dplyr::mutate(logradouros = stringr::str_to_upper(logradouros)) %>% 
            leaflet::leaflet() %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
            leaflet::addMarkers(~lng, ~lat, 
                popup = ~stringr::str_c('<strong>Local: </strong>', logradouros, 
                                        '<br/><strong>Data acidente: </strong>', data_hora1,
                                        '<br/><strong>Tipo acidente: </strong>', desc_acidente,
                                        '<br/><strong>Numero de pessoas envolvidas: </strong>', num_pessoas_envolvidas),
                                clusterOptions = leaflet::markerClusterOptions())
    })
    
    output$mensagem_retorno_pesquisa_mapa_obs <- shiny::renderText({
        
        if(length(input$pesquisa_mapa_obs) == 0){
            
            ""
        } else{
            
            if(unique(dfpesquisamapa()$aux_column) == 1){
                
                "Sua pesquisa não retornou resultados. Exibindo todas as ocorrências."
                
            } else{
                
                stringr::str_c("Sua pesquisa retornou ", nrow(dfpesquisamapa()), " resultados (", 
                               round(100*nrow(dfpesquisamapa())/nrow(df),2), "% do total de acidentes)." ) 
            }  
        }
    })
    
    output$titulo_fig_serie_temporal <- shiny::renderText({
        
        stringr::str_c('Série temporal ', 
            ifelse(input$valor_periodicidade == 'Diaria', 'diária', 
                stringr::str_to_lower(input$valor_periodicidade)), ' dos acidentes de trânsito')
    })
    
    output$fig_serie_temporal <- plotly::renderPlotly({

        p <- executa_grafico_serie_temporal(df = dfpesquisamapa(), periodicidade = input$valor_periodicidade)

        plotly::ggplotly(p)
        
    })
    
    output$estdesc_serietemporal <- shiny::renderTable({
        
        executa_estdesc_serie_temporal(df = dfpesquisamapa(), periodicidade = input$valor_periodicidade)
        
    })
    
    output$tbl_veiculos_mapa <- shiny::renderTable({
        
        dfpesquisamapa() %>% 
            dplyr::select(desc_veiculos) %>% 
            dplyr::mutate_all(.funs = stringr::str_to_title) %>% 
            table() %>% 
            dplyr::as_tibble() %>% 
            dplyr::arrange(desc(n)) %>% 
            dplyr::mutate(percentual = 100*n/sum(n)) %>% 
            dplyr::mutate(soma_percentual = cumsum(percentual)) %>% 
            setNames(nm = c('combinação veículos', 'frequência', 
                            'percentual', 'percentual acumulado')) %>% 
            dplyr::slice(1:15) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = round, digits = 2) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = as.character) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = function(x){paste0(x, '%')})
        
    })
    
    output$tbl_tipos_acidentes_mapa <- shiny::renderTable({
        
        dfpesquisamapa() %>% 
            dplyr::select(desc_acidente) %>% 
            table() %>% 
            dplyr::as_tibble() %>% 
            dplyr::arrange(desc(n)) %>% 
            dplyr::mutate(percentual = 100*n/sum(n)) %>% 
            dplyr::mutate(soma_percentual = cumsum(percentual)) %>% 
            setNames(nm = c('tipo acidente', 'frequência', 'percentual', 'percentual acumulado')) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = round, digits = 2) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = as.character) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = function(x){paste0(x, '%')})
        
    })
       
    output$tbl_recorrencia_acidentes_minutos <- shiny::renderTable({
    
        dfpesquisamapa() %>% 
            dplyr::arrange(data_hora) %>% 
            dplyr::mutate(lag_data_hora = dplyr::lag(data_hora)) %>% 
            dplyr::mutate(dif = lubridate::interval(lag_data_hora, data_hora)) %>% 
            dplyr::select(dif) %>% 
            dplyr::filter(!is.na(dif)) %>%
            dplyr::mutate(dif = lubridate::as.duration(dif)) %>% 
            dplyr::mutate(dif = as.numeric(dif, 'minutes')) %>% 
            dplyr::summarise(
                'Mínimo' = min(dif),
                '1Quartil' = quantile(dif, 0.25), 
                'Média' = mean(dif), 
                'Mediana' = median(dif), 
                '3Quartil' = quantile(dif, 0.75), 
                'Máximo' = max(dif),
                .groups = 'keep')
    })
       
    output$tbl_recorrencia_acidentes_horas <- shiny::renderTable({
    
        dfpesquisamapa() %>% 
            dplyr::arrange(data_hora) %>% 
            dplyr::mutate(lag_data_hora = dplyr::lag(data_hora)) %>% 
            dplyr::mutate(dif = lubridate::interval(lag_data_hora, data_hora)) %>% 
            dplyr::select(dif) %>% 
            dplyr::filter(!is.na(dif)) %>%
            dplyr::mutate(dif = lubridate::as.duration(dif)) %>% 
            dplyr::mutate(dif = as.numeric(dif, 'hours')) %>% 
            dplyr::summarise(
                'Mínimo' = min(dif),
                '1Quartil' = quantile(dif, 0.25), 
                'Média' = mean(dif), 
                'Mediana' = median(dif), 
                '3Quartil' = quantile(dif, 0.75), 
                'Máximo' = max(dif))
    })
    
    output$fig_horario_acidentes <- plotly::renderPlotly({
        
        p <- dfpesquisamapa() %>% 
            dplyr::group_by(hora_acidente) %>% 
            dplyr::summarise(n = dplyr::n(), .groups = 'keep') %>% 
            dplyr::ungroup() %>% 
            dplyr::mutate(percentual = 100*n/sum(n)) %>% 
            dplyr::mutate(soma_percentual = cumsum(percentual)) %>% 
            setNames(nm = c('hora', 'frequencia', 'percentual', 'percentual_acumulado')) %>% 
            dplyr::mutate(hora = factor(hora, levels = 0:23)) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual_acumulado'), 
                             .funs = round, digits = 2) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual_acumulado'), 
                             .funs = as.character) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual_acumulado'), 
                             .funs = function(x){paste0(x, '%')}) %>% 
            dplyr::mutate(detalhes = stringr::str_c('\nHora: ', hora, 
                                                    '\nFrequência: ', frequencia,
                                                    '\nPercentual: ', percentual,
                                                    '\nPercentual acumulado:', percentual_acumulado)) %>% 
            ggplot2::ggplot(ggplot2::aes(x = hora, y = frequencia, label = detalhes)) + 
            ggplot2::geom_bar(stat="identity") +
            ggplot2::theme_bw() +
            ggplot2::xlab('Hora') + 
            ggplot2::ylab('Frequência') 
            
        plotly::ggplotly(p, dynamicTicks = FALSE, tooltip = 'detalhes')
    })
    
    output$tbl_num_pessoas_envolvidas <- shiny::renderTable({
     
        dfpesquisamapa() %>% 
            dplyr::select(num_pessoas_envolvidas) %>% 
            dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais que 4', num_pessoas_envolvidas)) %>% 
            table() %>% 
            dplyr::as_tibble() %>% 
            dplyr::mutate(percentual = 100*n/sum(n)) %>% 
            dplyr::mutate(soma_percentual = cumsum(percentual)) %>% 
            setNames(nm = c('pessoas envolvidas', 'frequência', 'percentual', 'percentual acumulado')) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = round, digits = 2) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = as.character) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = function(x){paste0(x, '%')})
        
    })
    
    output$tbl_num_veiculos_envolvidos <- shiny::renderTable({
     
        dfpesquisamapa() %>% 
            dplyr::select(num_veiculos_envolvidos) %>% 
            dplyr::mutate(num_veiculos_envolvidos = ifelse(num_veiculos_envolvidos > 4, 'mais que 4', num_veiculos_envolvidos)) %>% 
            table() %>% 
            dplyr::as_tibble() %>% 
            dplyr::mutate(percentual = 100*n/sum(n)) %>% 
            dplyr::mutate(soma_percentual = cumsum(percentual)) %>% 
            setNames(nm = c('veiculos envolvidos', 'frequência', 'percentual', 'percentual acumulado')) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = round, digits = 2) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = as.character) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = function(x){paste0(x, '%')})
        
        
        
    })
    
    output$tbl_sexo_condutor <- shiny::renderTable({
        
        db_ocorrencias_transito_bh$pessoas %>% 
            dplyr::filter(ind_condutor == 1) %>% 
            dplyr::filter(num_boletim %in% dfpesquisamapa()$num_boletim) %>% 
            dplyr::select(sexo) %>% 
            dplyr::mutate(sexo = ifelse(sexo == 'm', 'Masculino', sexo)) %>% 
            dplyr::mutate(sexo = ifelse(sexo == 'f', 'Feminino',  sexo)) %>% 
            dplyr::mutate(sexo = ifelse(is.na(sexo), 'Sem informação', sexo)) %>% 
            table() %>% 
            dplyr::as_tibble() %>% 
            dplyr::arrange(desc(n)) %>% 
            dplyr::mutate(percentual = 100*n/sum(n)) %>% 
            dplyr::mutate(soma_percentual = cumsum(percentual)) %>% 
            setNames(nm = c('sexo condutor', 'frequência', 'percentual', 'percentual acumulado')) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = round, digits = 2) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = as.character) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = function(x){paste0(x, '%')})
    })
    
    output$fig_faixa_idade_condutor <- plotly::renderPlotly({
        
        p <- db_ocorrencias_transito_bh$pessoas %>% 
            dplyr::filter(ind_condutor == 1) %>% 
            dplyr::filter(num_boletim %in% dfpesquisamapa()$num_boletim) %>% 
            dplyr::select(idade) %>% 
            dplyr::mutate(cut_idade = cut(idade, 
                breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                labels = c("Até 20", "20 a 29", "30 a 39", 
                           "40 a 49", "50 a 59", "60 a 69","70 ou mais"))) %>% 
            dplyr::select(cut_idade) %>% 
            dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'Sem informação', as.character(cut_idade))) %>% 
            dplyr::mutate(cut_idade = factor(cut_idade, 
                levels = c("Até 20", "20 a 29", "30 a 39", "40 a 49", "50 a 59", "60 a 69", "70 ou mais", "Sem informação"))) %>% 
            table() %>% 
            data.frame() %>% 
            setNames(nm = c('idade', 'frequencia')) %>%
            dplyr::mutate(percentual = 100*frequencia/sum(frequencia)) %>% 
            dplyr::mutate(percentual = round(percentual, 2)) %>% 
            dplyr::mutate(percentual = stringr::str_c(percentual, '%')) %>% 
            dplyr::mutate(detalhes = stringr::str_c('\nFaixa de idade: ', idade, 
                                                 '\nFrequência: ', frequencia,
                                                 '\nPercentual: ', percentual)) %>% 
            ggplot2::ggplot(ggplot2::aes(x = idade, y = frequencia, label = detalhes)) + 
            ggplot2::geom_bar(stat= "identity") +
            ggplot2::theme_bw() +
            ggplot2::xlab('Faixa de idade') + 
            ggplot2::ylab('Frequência') + 
            ggplot2::theme(axis.text.x = element_text(angle = 15))
        
         plotly::ggplotly(p, dynamicTicks = FALSE, tooltip = 'detalhes')
         
    })
    
    output$tbl_ferimentos_condutor <- shiny::renderTable({
        
        db_ocorrencias_transito_bh$pessoas %>% 
            dplyr::filter(ind_condutor == 1) %>% 
            dplyr::filter(num_boletim %in% dfpesquisamapa()$num_boletim) %>% 
            dplyr::select(desc_severidade_acidente) %>% 
            dplyr::mutate_if(.predicate = is.character, .funs = stringr::str_to_upper) %>% 
            dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'Não informado', desc_severidade_acidente)) %>%  
            dplyr::mutate(desc_severidade_acidente = ifelse(desc_severidade_acidente == 'FATAL', 'Fatal', desc_severidade_acidente)) %>% 
            dplyr::mutate(desc_severidade_acidente = ifelse(desc_severidade_acidente == 'SEM FERIMENTOS', 'Sem ferimentos', desc_severidade_acidente)) %>% 
            dplyr::mutate(desc_severidade_acidente = ifelse(desc_severidade_acidente == 'NAO FATAL', 'Não-fatal', desc_severidade_acidente)) %>% 
            table() %>% 
            dplyr::as_tibble() %>% 
            dplyr::arrange(desc(n)) %>% 
            dplyr::mutate(percentual = 100*n/sum(n)) %>% 
            dplyr::mutate(soma_percentual = cumsum(percentual)) %>% 
            setNames(nm = c('grau ferimentos', 'frequência', 'percentual', 'percentual acumulado')) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = round, digits = 2) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = as.character) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = function(x){paste0(x, '%')})
    })
    
    output$tbl_num_passageiros <- shiny::renderTable({
        
        db_ocorrencias_transito_bh$pessoas %>% 
            dplyr::filter(num_boletim %in% dfpesquisamapa()$num_boletim) %>% 
            dplyr::group_by(num_boletim) %>% 
            dplyr::summarise(
                n = sum(ind_passageiro, na.rm = TRUE), .groups = 'drop') %>% 
            dplyr::select(n) %>%
            dplyr::mutate(n = ifelse(n > 4, 'Mais que 4', n)) %>% 
            table() %>% 
            dplyr::as_tibble() %>% 
            dplyr::mutate(percentual = 100*n/sum(n)) %>% 
            dplyr::mutate(soma_percentual = cumsum(percentual)) %>%
            setNames(nm = c("número de passageiros", 'frequência', 'percentual', 'percentual acumulado')) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = round, digits = 2) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = as.character) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = function(x){paste0(x, '%')})
    })
    
    output$tbl_sexo_passageiro <- shiny::renderTable({
        
        db_ocorrencias_transito_bh$pessoas %>% 
            dplyr::filter(ind_passageiro == 1) %>% 
            dplyr::filter(num_boletim %in% dfpesquisamapa()$num_boletim) %>% 
            dplyr::select(sexo) %>% 
            dplyr::mutate(sexo = ifelse(sexo == 'm', 'Masculino', sexo)) %>% 
            dplyr::mutate(sexo = ifelse(sexo == 'f', 'Feminino', sexo)) %>% 
            dplyr::mutate(sexo = ifelse(is.na(sexo), 'Sem informação', sexo)) %>% 
            table() %>% 
            dplyr::as_tibble() %>% 
            dplyr::arrange(desc(n)) %>% 
            dplyr::mutate(percentual = 100*n/sum(n)) %>% 
            dplyr::mutate(soma_percentual = cumsum(percentual)) %>% 
            setNames(nm = c('sexo passageiro', 'frequência', 'percentual', 'percentual acumulado')) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = round, digits = 2) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = as.character) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = function(x){paste0(x, '%')})
    })
    
    output$fig_faixa_idade_passageiro <- plotly::renderPlotly({
        
        p <- db_ocorrencias_transito_bh$pessoas %>% 
            dplyr::filter(ind_passageiro == 1) %>%
            dplyr::filter(num_boletim %in% dfpesquisamapa()$num_boletim) %>% 
            dplyr::select(idade) %>% 
            dplyr::mutate(cut_idade = cut(idade, 
                breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                labels = c("Até 20", "20 a 29", "30 a 39",
                           "40 a 49", "50 a 59", "60 a 69", "70 ou mais"))) %>% 
            dplyr::select(cut_idade) %>% 
            dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'Sem informação', as.character(cut_idade))) %>% 
            dplyr::mutate(cut_idade = factor(cut_idade, levels = c("Até 20", "20 a 29", "30 a 39", "40 a 49", "50 a 59", "60 a 69", "70 ou mais", "Sem informação"))) %>% 
            table() %>% 
            data.frame() %>% 
            setNames(nm = c('idade', 'frequencia')) %>%
            dplyr::mutate(percentual = 100*frequencia/sum(frequencia)) %>% 
            dplyr::mutate(percentual = round(percentual, 2)) %>% 
            dplyr::mutate(percentual = stringr::str_c(percentual, '%')) %>% 
            dplyr::mutate(detalhes = stringr::str_c('\nFaixa de idade: ', idade, 
                                                 '\nFrequência: ', frequencia,
                                                 '\nPercentual: ', percentual)) %>% 
            ggplot2::ggplot(ggplot2::aes(x = idade, y = frequencia, label = detalhes)) + 
            ggplot2::geom_bar(stat="identity") +
            ggplot2::theme_bw() +
            ggplot2::xlab('Faixa de idade') + 
            ggplot2::ylab('Frequência') + 
            ggplot2::theme(axis.text.x = element_text(angle = 15))
        
         plotly::ggplotly(p, dynamicTicks = FALSE, tooltip = 'detalhes')
         
    })
    
    output$tbl_ferimentos_passageiro <- shiny::renderTable({
        
        db_ocorrencias_transito_bh$pessoas %>% 
            dplyr::filter(ind_passageiro == 1) %>%
            dplyr::filter(num_boletim %in% dfpesquisamapa()$num_boletim) %>% 
            dplyr::select(desc_severidade_acidente) %>% 
            dplyr::mutate_if(.predicate = is.character, .funs = stringr::str_to_upper) %>% 
            dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'Não informado', desc_severidade_acidente)) %>%  
            dplyr::mutate(desc_severidade_acidente = ifelse(desc_severidade_acidente == 'FATAL', 'Fatal', desc_severidade_acidente)) %>% 
            dplyr::mutate(desc_severidade_acidente = ifelse(desc_severidade_acidente == 'SEM FERIMENTOS', 'Sem ferimentos', desc_severidade_acidente)) %>% 
            dplyr::mutate(desc_severidade_acidente = ifelse(desc_severidade_acidente == 'NAO FATAL', 'Não-fatal', desc_severidade_acidente)) %>% 
            table() %>% 
            dplyr::as_tibble() %>% 
            dplyr::arrange(desc(n)) %>% 
            dplyr::mutate(percentual = 100*n/sum(n)) %>% 
            dplyr::mutate(soma_percentual = cumsum(percentual)) %>% 
            setNames(nm = c('grau ferimentos', 'frequência', 'percentual', 'percentual acumulado')) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = round, digits = 2) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = as.character) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = function(x){paste0(x, '%')})
    })
    
    output$tbl_num_pedestres <- shiny::renderTable({
        
        db_ocorrencias_transito_bh$pessoas %>% 
            dplyr::filter(num_boletim %in% dfpesquisamapa()$num_boletim) %>% 
            dplyr::group_by(num_boletim) %>% 
            dplyr::summarise(
                n = sum(ind_pedestre, na.rm = TRUE),.groups = 'drop') %>% 
            dplyr::select(n) %>%
            dplyr::mutate(n = ifelse(n > 4, 'Mais que 4', n)) %>% 
            table() %>% 
            dplyr::as_tibble() %>% 
            dplyr::mutate(percentual = 100*n/sum(n)) %>% 
            dplyr::mutate(soma_percentual = cumsum(percentual)) %>%
            setNames(nm = c("número de pedestres", 'frequência', 'percentual', 'percentual acumulado')) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = round, digits = 2) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = as.character) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = function(x){paste0(x, '%')})
    })
    
    output$tbl_sexo_pedestre <- shiny::renderTable({
        
        db_ocorrencias_transito_bh$pessoas %>% 
            dplyr::filter(ind_pedestre == 1) %>% 
            dplyr::filter(num_boletim %in% dfpesquisamapa()$num_boletim) %>% 
            dplyr::select(sexo) %>% 
            dplyr::mutate(sexo = ifelse(sexo == 'm', 'Masculino', sexo)) %>% 
            dplyr::mutate(sexo = ifelse(sexo == 'f', 'Feminino', sexo)) %>% 
            dplyr::mutate(sexo = ifelse(is.na(sexo), 'Sem informação', sexo)) %>% 
            table() %>% 
            dplyr::as_tibble() %>% 
            dplyr::arrange(desc(n)) %>% 
            dplyr::mutate(percentual = 100*n/sum(n)) %>% 
            dplyr::mutate(soma_percentual = cumsum(percentual)) %>% 
            setNames(nm = c('sexo pedestre', 'frequência', 'percentual', 'percentual acumulado')) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = round, digits = 2) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = as.character) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = function(x){paste0(x, '%')})
    })
    
    output$fig_faixa_idade_pedestre <- plotly::renderPlotly({
        
        p <- db_ocorrencias_transito_bh$pessoas %>% 
            dplyr::filter(ind_pedestre == 1) %>%
            dplyr::filter(num_boletim %in% dfpesquisamapa()$num_boletim) %>% 
            dplyr::select(idade) %>% 
            dplyr::mutate(cut_idade = cut(idade, 
                breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                labels = c("Até 20", "20 a 29", "30 a 39", 
                           "40 a 49", "50 a 59", "60 a 69", "70 ou mais"))) %>% 
            dplyr::select(cut_idade) %>% 
            dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'Sem informação', as.character(cut_idade))) %>% 
            dplyr::mutate(cut_idade = factor(cut_idade, 
                levels = c("Até 20", "20 a 29", "30 a 39", "40 a 49", "50 a 59", "60 a 69", "70 ou mais", "Sem informação"))) %>% 
            table() %>% 
            data.frame() %>% 
            setNames(nm = c('idade', 'frequencia')) %>%
            dplyr::mutate(percentual = 100*frequencia/sum(frequencia)) %>% 
            dplyr::mutate(percentual = round(percentual, 2)) %>% 
            dplyr::mutate(percentual = stringr::str_c(percentual, '%')) %>% 
            dplyr::mutate(detalhes = stringr::str_c('\nFaixa de idade: ', idade, 
                                                    '\nFrequência: ', frequencia,
                                                    '\nPercentual: ', percentual)) %>% 
            ggplot2::ggplot(ggplot2::aes(x = idade, y = frequencia, label = detalhes)) + 
            ggplot2::geom_bar(stat = "identity") +
            ggplot2::theme_bw() +
            ggplot2::xlab('Faixa de idade') + 
            ggplot2::ylab('Frequência') + 
            ggplot2::theme(axis.text.x = element_text(angle = 15))
        
        plotly::ggplotly(p, dynamicTicks = FALSE, tooltip = 'detalhes')
        
    })
    
    output$tbl_ferimentos_pedestre <- shiny::renderTable({
        
        db_ocorrencias_transito_bh$pessoas %>% 
            dplyr::filter(ind_pedestre == 1) %>%
            dplyr::filter(num_boletim %in% dfpesquisamapa()$num_boletim) %>% 
            dplyr::select(desc_severidade_acidente) %>% 
            dplyr::mutate_if(.predicate = is.character, .funs = stringr::str_to_upper) %>% 
            dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'Não informado', desc_severidade_acidente)) %>%  
            dplyr::mutate(desc_severidade_acidente = ifelse(desc_severidade_acidente == 'FATAL', 'Fatal', desc_severidade_acidente)) %>% 
            dplyr::mutate(desc_severidade_acidente = ifelse(desc_severidade_acidente == 'SEM FERIMENTOS', 'Sem ferimentos', desc_severidade_acidente)) %>% 
            dplyr::mutate(desc_severidade_acidente = ifelse(desc_severidade_acidente == 'NAO FATAL', 'Não-fatal', desc_severidade_acidente)) %>% 
            table() %>% 
            dplyr::as_tibble() %>% 
            dplyr::arrange(desc(n)) %>% 
            dplyr::mutate(percentual = 100*n/sum(n)) %>% 
            dplyr::mutate(soma_percentual = cumsum(percentual)) %>% 
            setNames(nm = c('grau ferimentos', 'frequência', 'percentual', 'percentual acumulado')) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = round, digits = 2) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = as.character) %>% 
            dplyr::mutate_at(.vars = c('percentual', 'percentual acumulado'), 
                             .funs = function(x){paste0(x, '%')})
    })
    
    df_mapa_locais_frequentes <- {
        
        db_ocorrencias_transito_bh$ocorrencias %>% 
            dplyr::filter(!is.na(lat)) %>%
            dplyr::mutate(lat1 = as.character(lat),
                          lng1 = as.character(lng)) %>% 
            dplyr::mutate(lat2 = stringr::str_sub(lat1, 1, 7)) %>% 
            dplyr::mutate(lng2 = stringr::str_sub(lng1, 1, 7)) %>% 
            dplyr::group_by(lat2, lng2) %>% 
            dplyr::summarise(
                lat1 = calcula_moda(stringr::str_sub(lat1, 1, 8)),
                lng1 = calcula_moda(stringr::str_sub(lng1, 1, 8)),
                logradouros = calcula_moda(logradouros),
                n = dplyr::n(),
                .groups = 'keep') %>% 
            dplyr::ungroup() %>% 
            dplyr::arrange(desc(n)) %>% 
            dplyr::slice(1:10) %>% 
            dplyr::mutate(aux = seq_len(NROW(.))) %>% 
            dplyr::mutate(lat = as.numeric(lat1),
                          lng = as.numeric(lng1)) %>% 
            dplyr::mutate(aux_coordenate = stringr::str_c(lat, ',', lng)) %>% 
            dplyr::select(-c('lat2', 'lng2')) %>% 
            dplyr::mutate(logradouros = stringr::str_to_upper(logradouros)) %>% 
            dplyr::mutate(google_maps = paste0("<a href='http://www.google.com/maps?layer=c&cbll=", aux_coordenate,"' target='_blank'>Ver no Google Street View</a>"))
        
        }
    
    icones_numeros <- leaflet::iconList(
        '1' = leaflet::makeIcon(iconUrl = 'icone1.png', iconWidth = 30, iconHeight = 30),
        '2' = leaflet::makeIcon(iconUrl = 'icone2.png', iconWidth = 30, iconHeight = 30),
        '3' = leaflet::makeIcon(iconUrl = 'icone3.png', iconWidth = 30, iconHeight = 30),
        '4' = leaflet::makeIcon(iconUrl = 'icone4.png', iconWidth = 30, iconHeight = 30),
        '5' = leaflet::makeIcon(iconUrl = 'icone5.png', iconWidth = 30, iconHeight = 30),
        '6' = leaflet::makeIcon(iconUrl = 'icone6.png', iconWidth = 30, iconHeight = 30),
        '7' = leaflet::makeIcon(iconUrl = 'icone7.png', iconWidth = 30, iconHeight = 30),
        '8' = leaflet::makeIcon(iconUrl = 'icone8.png', iconWidth = 30, iconHeight = 30),
        '9' = leaflet::makeIcon(iconUrl = 'icone9.png', iconWidth = 30, iconHeight = 30),
        '10' = leaflet::makeIcon(iconUrl = 'icone10.png', iconWidth = 30, iconHeight = 30))
    
    output$mapa_locais_frequentes_geral <- leaflet::renderLeaflet({

            
        df_mapa_locais_frequentes %>% 
                leaflet::leaflet() %>% 
                leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
                leaflet::addMarkers(~lng, ~lat, icon = ~icones_numeros[aux],
                                    popup = ~stringr::str_c('<strong>', aux, 'º LUGAR</strong>', 
                                                            '<br/><strong>Local: </strong>', logradouros, 
                                                            '<br/><strong>Número de ocorrências: </strong>', n, 
                                                            '<br/><br/>', google_maps))
    })
    
    output$tbl_locais_frequentes_geral <- shiny::renderTable({
        
        df_mapa_locais_frequentes %>% 
            dplyr::select(aux, logradouros, n, lat1, lng1) %>% 
            dplyr::mutate_at(.vars = c('lat1', 'lng1'), .funs = as.numeric) %>% 
            dplyr::mutate_at(.vars = c('lat1', 'lng1'), .funs = round, digits = 4) %>% 
            dplyr::mutate_at(.vars = c('lat1', 'lng1'), .funs = as.character) %>% 
            setNames(nm = c('posição','local', 'ocorrências', 'lat', 'lng'))
        
    })
    
    
    df_mapa_locais_frequentes_tp_acidentes <- shiny::reactive({
        
        db_ocorrencias_transito_bh$ocorrencias %>% 
            dplyr::filter(!is.na(lat)) %>%
            dplyr::filter(desc_acidente == stringr::str_to_lower(stringi::stri_trans_general(input$pesquisamapa_tp_acidente,id = "latin-ascii"))) %>% 
            dplyr::mutate(lat1 = as.character(lat),
                          lng1 = as.character(lng)) %>% 
            dplyr::mutate(lat2 = stringr::str_sub(lat1, 1, 7)) %>% 
            dplyr::mutate(lng2 = stringr::str_sub(lng1, 1, 7)) %>% 
            dplyr::group_by(lat2, lng2) %>% 
            dplyr::summarise(
                lat1 = calcula_moda(stringr::str_sub(lat1, 1, 8)),
                lng1 = calcula_moda(stringr::str_sub(lng1, 1, 8)),
                logradouros = calcula_moda(logradouros),
                n = dplyr::n(),
                .groups = 'keep') %>% 
            dplyr::ungroup() %>% 
            dplyr::arrange(desc(n)) %>% 
            dplyr::slice(1:10) %>% 
            dplyr::mutate(aux = seq_len(NROW(.))) %>% 
            dplyr::mutate(lat = as.numeric(lat1),
                          lng = as.numeric(lng1)) %>% 
            dplyr::mutate(aux_coordenate = stringr::str_c(lat, ',', lng)) %>% 
            dplyr::select(-c('lat2', 'lng2')) %>% 
            dplyr::mutate(logradouros = stringr::str_to_upper(logradouros)) %>% 
            dplyr::mutate(google_maps = paste0("<a href='http://www.google.com/maps?layer=c&cbll=", aux_coordenate,"' target='_blank'>Ver no Google Street View</a>")) 
        
    })
        
    output$mapa_locais_frequentes_tp_acidentes <- leaflet::renderLeaflet({

        df_mapa_locais_frequentes_tp_acidentes() %>% 
            leaflet::leaflet() %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
            leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
            leaflet::addMarkers(~lng, ~lat, icon = ~icones_numeros[aux],
                                popup = ~stringr::str_c('<strong>', aux, 'º LUGAR</strong>', 
                                                        '<br/><strong>Local: </strong>', logradouros, 
                                                        '<br/><strong>Número de ocorrências: </strong>', n, 
                                                        '<br/><br/>', google_maps))
    })
    
    output$tbl_locais_frequentes_tp_acidentes <- shiny::renderTable({
        
        df_mapa_locais_frequentes_tp_acidentes() %>% 
            dplyr::select(aux, logradouros, n, lat1, lng1) %>% 
            dplyr::mutate_at(.vars = c('lat1', 'lng1'), .funs = as.numeric) %>% 
            dplyr::mutate_at(.vars = c('lat1', 'lng1'), .funs = round, digits = 4) %>% 
            dplyr::mutate_at(.vars = c('lat1', 'lng1'), .funs = as.character) %>% 
            setNames(nm = c('posição','local', 'ocorrências', 'lat', 'lng'))
        
    })

    
    df_mapa_locais_frequentes_tp_veiculos <- shiny::reactive({
        
        db_ocorrencias_transito_bh$veiculos %>% 
            dplyr::filter(desc_tipo_veiculo %in% seleciona_veiculos(input$pesquisamapa_tp_veiculo)) %>% 
            dplyr::distinct(num_boletim) %>% 
            dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
            dplyr::filter(!is.na(lat)) %>%
            dplyr::mutate(lat1 = as.character(lat),
                          lng1 = as.character(lng)) %>% 
            dplyr::mutate(lat2 = stringr::str_sub(lat1, 1, 7)) %>% 
            dplyr::mutate(lng2 = stringr::str_sub(lng1, 1, 7)) %>% 
            dplyr::group_by(lat2, lng2) %>% 
            dplyr::summarise(
                lat1 = calcula_moda(stringr::str_sub(lat1, 1, 8)),
                lng1 = calcula_moda(stringr::str_sub(lng1, 1, 8)),
                logradouros = calcula_moda(logradouros),
                n = dplyr::n(),
                .groups = 'keep') %>% 
            dplyr::ungroup() %>% 
            dplyr::arrange(desc(n)) %>% 
            dplyr::slice(1:10) %>% 
            dplyr::mutate(aux = seq_len(NROW(.))) %>% 
            dplyr::mutate(lat = as.numeric(lat1),
                          lng = as.numeric(lng1)) %>% 
            dplyr::mutate(aux_coordenate = stringr::str_c(lat, ',', lng)) %>% 
            dplyr::select(-c('lat2', 'lng2')) %>% 
            dplyr::mutate(logradouros = stringr::str_to_upper(logradouros)) %>% 
            dplyr::mutate(google_maps = paste0("<a href='http://www.google.com/maps?layer=c&cbll=", aux_coordenate,"' target='_blank'>Ver no Google Street View</a>"))
        
        # db_ocorrencias_transito_bh$veiculos %>% 
        #     dplyr::filter(desc_tipo_veiculo %in% seleciona_veiculos(input$pesquisamapa_tp_veiculo)) %>% 
        #     dplyr::distinct(num_boletim) %>% 
        #     dplyr::inner_join(db_ocorrencias_transito_bh$ocorrencias, by = 'num_boletim') %>% 
        #     dplyr::filter(!is.na(lat)) %>%
        #     dplyr::mutate(lat1 = as.character(lat),
        #                   lng1 = as.character(lng)) %>%
        #     dplyr::mutate(nlinhas = nrow(.)) %>% 
        #     dplyr::group_by(lat1, lng1, logradouros, nlinhas) %>% 
        #     dplyr::summarise(
        #         n = dplyr::n()) %>% 
        #     dplyr::ungroup() %>% 
        #     dplyr::mutate(lat2 = stringr::str_sub(lat1, 1, 7)) %>% 
        #     dplyr::mutate(lng2 = stringr::str_sub(lng1, 1, 7)) %>% 
        #     dplyr::mutate(tam = stringr::str_length(logradouros)) %>% 
        #     dplyr::arrange(desc(tam)) %>% 
        #     dplyr::mutate(aux_coordenate = stringr::str_c(round(as.numeric(lat1), 5), ',',round(as.numeric(lng1), 5))) %>% 
        #     dplyr::group_by(lat2, lng2) %>% 
        #     dplyr::summarise(
        #         lat1 = calcula_moda(lat1),
        #         lng1 = calcula_moda(lng1),
        #         logradouros = logradouros[1],
        #         aux_coordenate = calcula_moda(aux_coordenate),
        #         n = sum(n),
        #         nlinhas = unique(nlinhas)
        #     ) %>% 
        #     dplyr::ungroup() %>% 
        #     dplyr::arrange(desc(n)) %>% 
        #     dplyr::slice(1:10) %>% 
        #     dplyr::mutate(aux = seq_len(NROW(.))) %>% 
        #     dplyr::mutate(lat = as.numeric(lat1),
        #                   lng = as.numeric(lng1)) %>% 
        #     dplyr::select(-c('lat2', 'lng2')) %>% 
        #     dplyr::mutate(logradouros = stringr::str_to_upper(logradouros)) %>% 
        #     dplyr::mutate(google_maps = paste0("<a href='http://www.google.com/maps?layer=c&cbll=", aux_coordenate,"' target='_blank'>Ver no Google Street View</a>")) 

        
    })
    
    output$mapa_locais_frequentes_tp_veiculos <- leaflet::renderLeaflet({
        
        df_mapa_locais_frequentes_tp_veiculos() %>% 
            leaflet::leaflet() %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
            leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
            leaflet::addMarkers(~lng, ~lat, icon = ~icones_numeros[aux],
                                popup = ~stringr::str_c('<strong>', aux, 'º LUGAR</strong>', 
                                                        '<br/><strong>Local: </strong>', logradouros, 
                                                        '<br/><strong>Número de ocorrências: </strong>', n, 
                                                        '<br/><br/>', google_maps))
    })
    
    output$tbl_locais_frequentes_tp_veiculos <- shiny::renderTable({
        
        df_mapa_locais_frequentes_tp_veiculos() %>% 
            dplyr::select(aux, logradouros, n, lat1, lng1) %>% 
            dplyr::mutate_at(.vars = c('lat1', 'lng1'), .funs = as.numeric) %>% 
            dplyr::mutate_at(.vars = c('lat1', 'lng1'), .funs = round, digits = 4) %>% 
            dplyr::mutate_at(.vars = c('lat1', 'lng1'), .funs = as.character) %>% 
            setNames(nm = c('posição','local', 'ocorrências', 'lat', 'lng'))
        
    })
    
    output$info_texto_tp_veiculos <- shiny::renderText({
        
        stringr::str_c("Ocorreram ", unique(df_mapa_locais_frequentes_tp_veiculos()$nlinhas), 
            " acidentes que envolveram os seguintes tipos de veículo: ",
            stringr::str_c(stringr::str_to_lower(seleciona_veiculos(input$pesquisamapa_tp_veiculo)), collapse = ', '))
        
        
    })
    
    
    df_mapa_locais_frequentes_periodo_dia <- shiny::reactive({
        
        
        db_ocorrencias_transito_bh$ocorrencias %>% 
            dplyr::filter(!is.na(lat)) %>%
            dplyr::filter(hora_acidente %in% seleciona_periodo(input$pesquisamapa_periodo_dia)) %>% 
            dplyr::mutate(lat1 = as.character(lat),
                          lng1 = as.character(lng)) %>% 
            dplyr::mutate(lat2 = stringr::str_sub(lat1, 1, 7)) %>% 
            dplyr::mutate(lng2 = stringr::str_sub(lng1, 1, 7)) %>% 
            dplyr::mutate(nlinhas = nrow(.)) %>% 
            dplyr::group_by(lat2, lng2, nlinhas) %>% 
            dplyr::summarise(
                lat1 = calcula_moda(stringr::str_sub(lat1, 1, 8)),
                lng1 = calcula_moda(stringr::str_sub(lng1, 1, 8)),
                logradouros = calcula_moda(logradouros),
                n = dplyr::n(),
                .groups = 'keep') %>% 
            dplyr::ungroup() %>% 
            dplyr::arrange(desc(n)) %>% 
            dplyr::slice(1:10) %>% 
            dplyr::mutate(aux = seq_len(NROW(.))) %>% 
            dplyr::mutate(lat = as.numeric(lat1),
                          lng = as.numeric(lng1)) %>% 
            dplyr::mutate(aux_coordenate = stringr::str_c(lat, ',', lng)) %>% 
            dplyr::select(-c('lat2', 'lng2')) %>% 
            dplyr::mutate(logradouros = stringr::str_to_upper(logradouros)) %>% 
            dplyr::mutate(google_maps = paste0("<a href='http://www.google.com/maps?layer=c&cbll=", aux_coordenate,"' target='_blank'>Ver no Google Street View</a>")) 
        
        
    })
    
    output$mapa_locais_frequentes_periodo_dia <- leaflet::renderLeaflet({
        
        df_mapa_locais_frequentes_periodo_dia() %>% 
            leaflet::leaflet() %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
            leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
            leaflet::addMarkers(~lng, ~lat, icon = ~icones_numeros[aux],
                                popup = ~stringr::str_c('<strong>', aux, 'º LUGAR</strong>', 
                                                        '<br/><strong>Local: </strong>', logradouros, 
                                                        '<br/><strong>Número de ocorrências: </strong>', n, 
                                                        '<br/><br/>', google_maps))
    })
    
    output$tbl_locais_frequentes_periodo_dia <- shiny::renderTable({
        
        df_mapa_locais_frequentes_periodo_dia() %>% 
            dplyr::select(aux, logradouros, n, lat1, lng1) %>% 
            dplyr::mutate_at(.vars = c('lat1', 'lng1'), .funs = as.numeric) %>% 
            dplyr::mutate_at(.vars = c('lat1', 'lng1'), .funs = round, digits = 4) %>% 
            dplyr::mutate_at(.vars = c('lat1', 'lng1'), .funs = as.character) %>% 
            setNames(nm = c('posição','local', 'ocorrências', 'lat', 'lng'))
        
    })
    
    output$info_texto_periodo_dia <- shiny::renderText({
        
        stringr::str_c("Ocorreram ", unique(df_mapa_locais_frequentes_periodo_dia()$nlinhas), " acidentes nesse período.")
        
        
    })
    

    df_mapa_locais_frequentes_regionais <- shiny::reactive({
        
        db_ocorrencias_transito_bh$ocorrencias %>% 
            dplyr::filter(!is.na(lat)) %>%
            dplyr::filter(regional == stringr::str_to_upper(input$pesquisamapa_regionais)) %>% 
            dplyr::mutate(lat1 = as.character(lat),
                          lng1 = as.character(lng)) %>% 
            dplyr::mutate(lat2 = stringr::str_sub(lat1, 1, 7)) %>% 
            dplyr::mutate(lng2 = stringr::str_sub(lng1, 1, 7)) %>% 
            dplyr::mutate(nlinhas = nrow(.)) %>% 
            dplyr::group_by(lat2, lng2, nlinhas) %>% 
            dplyr::summarise(
                lat1 = calcula_moda(stringr::str_sub(lat1, 1, 8)),
                lng1 = calcula_moda(stringr::str_sub(lng1, 1, 8)),
                logradouros = calcula_moda(logradouros),
                n = dplyr::n(),
                .groups = 'keep') %>% 
            dplyr::ungroup() %>% 
            dplyr::arrange(desc(n)) %>% 
            dplyr::slice(1:10) %>% 
            dplyr::mutate(aux = seq_len(NROW(.))) %>% 
            dplyr::mutate(lat = as.numeric(lat1),
                          lng = as.numeric(lng1)) %>% 
            dplyr::mutate(aux_coordenate = stringr::str_c(lat, ',', lng)) %>% 
            dplyr::select(-c('lat2', 'lng2')) %>% 
            dplyr::mutate(logradouros = stringr::str_to_upper(logradouros)) %>% 
            dplyr::mutate(google_maps = paste0("<a href='http://www.google.com/maps?layer=c&cbll=", aux_coordenate,"' target='_blank'>Ver no Google Street View</a>")) 
        
        
    })
    
    output$mapa_locais_frequentes_regionais <- leaflet::renderLeaflet({
        
        df_mapa_locais_frequentes_regionais() %>% 
            leaflet::leaflet() %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
            leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
            leaflet::addMarkers(~lng, ~lat, icon = ~icones_numeros[aux],
                                popup = ~stringr::str_c('<strong>', aux, 'º LUGAR</strong>', 
                                                        '<br/><strong>Local: </strong>', logradouros, 
                                                        '<br/><strong>Número de ocorrências: </strong>', n, 
                                                        '<br/><br/>', google_maps))
    })
    
    output$tbl_locais_frequentes_regionais <- shiny::renderTable({
        
        df_mapa_locais_frequentes_regionais() %>% 
            dplyr::select(aux, logradouros, n, lat1, lng1) %>% 
            dplyr::mutate_at(.vars = c('lat1', 'lng1'), .funs = as.numeric) %>% 
            dplyr::mutate_at(.vars = c('lat1', 'lng1'), .funs = round, digits = 4) %>% 
            dplyr::mutate_at(.vars = c('lat1', 'lng1'), .funs = as.character) %>% 
            setNames(nm = c('posição','local', 'ocorrências', 'lat', 'lng'))
        
    })
    
    output$info_texto_regional <- shiny::renderText({
        
        stringr::str_c("Ocorreram ", unique(df_mapa_locais_frequentes_regionais()$nlinhas), " acidentes entre 2016 e 2019 nessa regional.")
        
        
    })
    
    
    df_mapa_locais_frequentes_fatalidade <- {
        
        
        db_ocorrencias_transito_bh$ocorrencias %>% 
            dplyr::filter(!is.na(lat)) %>%
            dplyr::filter(ind_fatalidade == 1) %>% 
            dplyr::mutate(lat1 = as.character(lat),
                          lng1 = as.character(lng)) %>% 
            dplyr::mutate(lat2 = stringr::str_sub(lat1, 1, 7)) %>% 
            dplyr::mutate(lng2 = stringr::str_sub(lng1, 1, 7)) %>% 
            dplyr::mutate(nlinhas = nrow(.)) %>% 
            dplyr::group_by(lat2, lng2, nlinhas) %>% 
            dplyr::summarise(
                lat1 = calcula_moda(stringr::str_sub(lat1, 1, 8)),
                lng1 = calcula_moda(stringr::str_sub(lng1, 1, 8)),
                logradouros = calcula_moda(logradouros),
                n = dplyr::n(),
                .groups = 'keep') %>% 
            dplyr::ungroup() %>% 
            dplyr::arrange(desc(n)) %>% 
            dplyr::slice(1:10) %>% 
            dplyr::mutate(aux = seq_len(NROW(.))) %>% 
            dplyr::mutate(lat = as.numeric(lat1),
                          lng = as.numeric(lng1)) %>% 
            dplyr::mutate(aux_coordenate = stringr::str_c(lat, ',', lng)) %>% 
            dplyr::select(-c('lat2', 'lng2')) %>% 
            dplyr::mutate(logradouros = stringr::str_to_upper(logradouros)) %>% 
            dplyr::mutate(google_maps = paste0("<a href='http://www.google.com/maps?layer=c&cbll=", aux_coordenate,"' target='_blank'>Ver no Google Street View</a>")) 

        }
    
    output$mapa_locais_frequentes_fatalidade <- leaflet::renderLeaflet({
        
        df_mapa_locais_frequentes_fatalidade %>% 
            leaflet::leaflet() %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
            leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
            leaflet::addMarkers(~lng, ~lat, icon = ~icones_numeros[aux],
                                popup = ~stringr::str_c('<strong>', aux, 'º LUGAR</strong>', 
                                                        '<br/><strong>Local: </strong>', logradouros, 
                                                        '<br/><strong>Número de ocorrências: </strong>', n, 
                                                        '<br/><br/>', google_maps))
    })
    
    output$tbl_locais_frequentes_fatalidade <- shiny::renderTable({
        
        df_mapa_locais_frequentes_fatalidade %>% 
            dplyr::select(aux, logradouros, n, lat1, lng1) %>% 
            dplyr::mutate_at(.vars = c('lat1', 'lng1'), .funs = as.numeric) %>% 
            dplyr::mutate_at(.vars = c('lat1', 'lng1'), .funs = round, digits = 4) %>% 
            dplyr::mutate_at(.vars = c('lat1', 'lng1'), .funs = as.character) %>% 
            setNames(nm = c('posição','local', 'ocorrências', 'lat', 'lng'))
        
    })
    
    output$info_texto_fatalidade <- shiny::renderText({
        
        stringr::str_c("Ocorreram ", unique(df_mapa_locais_frequentes_fatalidade$nlinhas), " acidentes com vítimas fatais em Belo Horizonte entre 2016 e 2019.")
        
        
    })
    
    
    df_mapa_locais_frequentes_grandes_acidentes <- {
        
        db_ocorrencias_transito_bh$ocorrencias %>% 
            dplyr::filter(!is.na(lat)) %>%
            dplyr::filter(num_veiculos_envolvidos > 3) %>% 
            dplyr::mutate(lat1 = as.character(lat),
                          lng1 = as.character(lng)) %>% 
            dplyr::mutate(lat2 = stringr::str_sub(lat1, 1, 7)) %>% 
            dplyr::mutate(lng2 = stringr::str_sub(lng1, 1, 7)) %>% 
            dplyr::mutate(nlinhas = nrow(.)) %>% 
            dplyr::group_by(lat2, lng2, nlinhas) %>% 
            dplyr::summarise(
                lat1 = calcula_moda(stringr::str_sub(lat1, 1, 8)),
                lng1 = calcula_moda(stringr::str_sub(lng1, 1, 8)),
                logradouros = calcula_moda(logradouros),
                n = dplyr::n(),
                .groups = 'keep') %>% 
            dplyr::ungroup() %>% 
            dplyr::arrange(desc(n)) %>% 
            dplyr::slice(1:10) %>% 
            dplyr::mutate(aux = seq_len(NROW(.))) %>% 
            dplyr::mutate(lat = as.numeric(lat1),
                          lng = as.numeric(lng1)) %>% 
            dplyr::mutate(aux_coordenate = stringr::str_c(lat, ',', lng)) %>% 
            dplyr::select(-c('lat2', 'lng2')) %>% 
            dplyr::mutate(logradouros = stringr::str_to_upper(logradouros)) %>% 
            dplyr::mutate(google_maps = paste0("<a href='http://www.google.com/maps?layer=c&cbll=", aux_coordenate,"' target='_blank'>Ver no Google Street View</a>")) 
        

        }

    output$mapa_locais_frequentes_grandes_acidentes <- leaflet::renderLeaflet({
        
        df_mapa_locais_frequentes_grandes_acidentes %>% 
            leaflet::leaflet() %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
            leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
            leaflet::addMarkers(~lng, ~lat, icon = ~icones_numeros[aux],
                                popup = ~stringr::str_c('<strong>', aux, 'º LUGAR</strong>', 
                                                        '<br/><strong>Local: </strong>', logradouros, 
                                                        '<br/><strong>Número de ocorrências: </strong>', n, 
                                                        '<br/><br/>', google_maps))
    })
    
    output$tbl_locais_frequentes_grandes_acidentes <- shiny::renderTable({
        
        df_mapa_locais_frequentes_grandes_acidentes %>% 
            dplyr::select(aux, logradouros, n, lat1, lng1) %>% 
            dplyr::mutate_at(.vars = c('lat1', 'lng1'), .funs = as.numeric) %>% 
            dplyr::mutate_at(.vars = c('lat1', 'lng1'), .funs = round, digits = 4) %>% 
            dplyr::mutate_at(.vars = c('lat1', 'lng1'), .funs = as.character) %>% 
            setNames(nm = c('posição','local', 'ocorrências', 'lat', 'lng'))
        
    })
    
    output$info_texto_grandes_acidentes <- shiny::renderText({
        
        stringr::str_c("Ocorreram ", unique(df_mapa_locais_frequentes_grandes_acidentes$nlinhas), " acidentes envolvendo 4 veículos ou mais na cidade no período entre 2016 e 2019.")
        
        
    })
    
    
    df_mapa_locais_frequentes_fins_semana <- {
       
        db_ocorrencias_transito_bh$ocorrencias %>% 
            dplyr::filter(!is.na(lat)) %>%
            dplyr::filter(num_veiculos_envolvidos > 3) %>% 
            dplyr::mutate(lat1 = as.character(lat),
                          lng1 = as.character(lng)) %>% 
            dplyr::mutate(lat2 = stringr::str_sub(lat1, 1, 7)) %>% 
            dplyr::mutate(lng2 = stringr::str_sub(lng1, 1, 7)) %>% 
            dplyr::mutate(nlinhas = nrow(.)) %>% 
            dplyr::group_by(lat2, lng2, nlinhas) %>% 
            dplyr::summarise(
                lat1 = calcula_moda(stringr::str_sub(lat1, 1, 8)),
                lng1 = calcula_moda(stringr::str_sub(lng1, 1, 8)),
                logradouros = calcula_moda(logradouros),
                n = dplyr::n(),
                .groups = 'keep') %>% 
            dplyr::ungroup() %>% 
            dplyr::arrange(desc(n)) %>% 
            dplyr::slice(1:10) %>% 
            dplyr::mutate(aux = seq_len(NROW(.))) %>% 
            dplyr::mutate(lat = as.numeric(lat1),
                          lng = as.numeric(lng1)) %>% 
            dplyr::mutate(aux_coordenate = stringr::str_c(lat, ',', lng)) %>% 
            dplyr::select(-c('lat2', 'lng2')) %>% 
            dplyr::mutate(logradouros = stringr::str_to_upper(logradouros)) %>% 
            dplyr::mutate(google_maps = paste0("<a href='http://www.google.com/maps?layer=c&cbll=", aux_coordenate,"' target='_blank'>Ver no Google Street View</a>")) 
        
        
        obj_datas <- c('2016-01-01', '2017-01-01', '2018-01-01', '2019-01-01',
                       '2016-02-08', '2016-02-09', '2016-02-10', '2016-03-25',
                       '2016-04-21', '2016-04-22', '2016-05-26', '2016-05-27',
                       '2016-08-15', '2016-09-07', '2016-10-12', '2016-11-02',
                       '2016-11-14', '2016-11-15', '2016-12-08', '2017-08-27', 
                       '2017-02-28', '2017-03-01', '2017-04-21', '2017-05-01', 
                       '2017-06-15', '2017-06-16', '2017-08-14', '2017-08-15', 
                       '2017-09-07', '2017-09-08', '2017-10-12', '2017-10-13', 
                       '2017-11-02', '2017-11-03', '2017-11-15', '2017-12-18', 
                       '2017-12-25', '2018-02-12', '2018-02-13', '2018-02-14', 
                       '2018-03-30', '2018-04-30', '2018-05-01', '2018-05-31', 
                       '2018-06-01', '2018-08-15', '2018-09-07', '2018-10-12', 
                       '2018-11-02', '2018-11-15', '2018-11-16', '2018-12-24', 
                       '2018-12-25', '2019-03-02', '2019-03-03', '2019-03-04',
                       '2019-03-05', '2019-03-06', '2019-04-19', '2019-05-01',
                       '2019-06-20', '2019-06-21', '2019-08-15', '2019-08-16',
                       '2019-11-15', '2019-12-24', '2019-12-25') %>% 
            as.Date()
        
        
     db_ocorrencias_transito_bh$ocorrencias %>% 
        dplyr::mutate(diasemana = lubridate::wday(data_hora, label = TRUE)) %>% 
        dplyr::mutate(aux = ifelse(diasemana %in% c('sex', 'sáb', 'dom'), 1, 0)) %>% 
        dplyr::mutate(aux = ifelse(hora_acidente < 20 & diasemana == 'sex', 0, aux)) %>% 
        dplyr::mutate(aux = ifelse(data_hora %in% obj_datas, 1, aux)) %>% 
        dplyr::filter(aux == 1) %>% 
        dplyr::filter(!is.na(lat)) %>%
         dplyr::mutate(lat1 = as.character(lat),
                       lng1 = as.character(lng)) %>% 
         dplyr::mutate(lat2 = stringr::str_sub(lat1, 1, 7)) %>% 
         dplyr::mutate(lng2 = stringr::str_sub(lng1, 1, 7)) %>% 
         dplyr::mutate(nlinhas = nrow(.)) %>% 
         dplyr::group_by(lat2, lng2, nlinhas) %>% 
         dplyr::summarise(
             lat1 = calcula_moda(stringr::str_sub(lat1, 1, 8)),
             lng1 = calcula_moda(stringr::str_sub(lng1, 1, 8)),
             logradouros = calcula_moda(logradouros),
             n = dplyr::n(),
             .groups = 'keep') %>% 
         dplyr::ungroup() %>% 
         dplyr::arrange(desc(n)) %>% 
         dplyr::slice(1:10) %>% 
         dplyr::mutate(aux = seq_len(NROW(.))) %>% 
         dplyr::mutate(lat = as.numeric(lat1),
                       lng = as.numeric(lng1)) %>% 
         dplyr::mutate(aux_coordenate = stringr::str_c(lat, ',', lng)) %>% 
         dplyr::select(-c('lat2', 'lng2')) %>% 
         dplyr::mutate(logradouros = stringr::str_to_upper(logradouros)) %>% 
         dplyr::mutate(google_maps = paste0("<a href='http://www.google.com/maps?layer=c&cbll=", aux_coordenate,"' target='_blank'>Ver no Google Street View</a>")) 
     
        }

    output$mapa_locais_frequentes_fins_semana <- leaflet::renderLeaflet({
        
        df_mapa_locais_frequentes_fins_semana %>% 
            leaflet::leaflet() %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
            leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
            leaflet::addMarkers(~lng, ~lat, icon = ~icones_numeros[aux],
                                popup = ~stringr::str_c('<strong>', aux, 'º LUGAR</strong>', 
                                                        '<br/><strong>Local: </strong>', logradouros, 
                                                        '<br/><strong>Número de ocorrências: </strong>', n, 
                                                        '<br/><br/>', google_maps))
    })
    
    output$tbl_locais_frequentes_fins_semana <- shiny::renderTable({
        
        df_mapa_locais_frequentes_fins_semana %>% 
            dplyr::select(aux, logradouros, n, lat1, lng1) %>% 
            dplyr::mutate_at(.vars = c('lat1', 'lng1'), .funs = as.numeric) %>% 
            dplyr::mutate_at(.vars = c('lat1', 'lng1'), .funs = round, digits = 4) %>% 
            dplyr::mutate_at(.vars = c('lat1', 'lng1'), .funs = as.character) %>% 
            setNames(nm = c('posição','local', 'ocorrências', 'lat', 'lng'))
        
    })
    
    output$info_texto_fins_semana <- shiny::renderText({
        
        stringr::str_c("Ocorreram ", unique(df_mapa_locais_frequentes_fins_semana$nlinhas), " acidentes nos fins de semana ou feriados no período entre 2016 e 2019.")
        
        
    })
    
    output$choropeth_mapa_bairros <- leaflet::renderLeaflet({
        
        cria_mapa_bairros(input$pesquisa_variavel, input$pesquisa_valor, input$percentual_info, input$corte_num_acidentes)
        
    })

    output$choropeth_mapa_regionais <- leaflet::renderLeaflet({
        
        cria_mapa_regionais(input$pesquisa_variavel, input$pesquisa_valor, input$percentual_info)
        
    })

    output$tbl_dist_bairros <- shiny::renderTable({
            
        if(input$percentual_info){
            
            muda_objsp(.variavel = input$pesquisa_variavel, .valor = input$pesquisa_valor) %>% 
                dplyr::select(bairro, n, soma, perc) %>%
                dplyr::filter(n >= as.integer(input$corte_num_acidentes)) %>% 
                dplyr::arrange(desc(perc)) %>% 
                dplyr::slice(1:5) %>% 
                dplyr::mutate(soma = as.integer(soma)) %>%
                dplyr::select(bairro, soma, perc) %>% 
                setNames(nm = c('bairro', 'frequência', 'percentual')) %>% 
                dplyr::mutate(percentual = stringr::str_c(round(percentual, 2), '%'))
            
        } else{
            
            muda_objsp(.variavel = input$pesquisa_variavel, .valor = input$pesquisa_valor) %>% 
                dplyr::select(bairro, n, soma, perc) %>%
                dplyr::arrange(desc(soma)) %>% 
                dplyr::slice(1:5) %>% 
                dplyr::mutate(soma = as.integer(soma)) %>%
                dplyr::select(bairro, soma, perc) %>% 
                setNames(nm = c('bairro', 'frequência', 'percentual')) %>% 
                dplyr::mutate(percentual = stringr::str_c(round(percentual, 2), '%'))
        }
        
    })
    
    output$info_texto_bairro_perc <- shiny::renderText({
        
        obj_perc <- muda_objsp(.variavel = input$pesquisa_variavel,
                               .valor = input$pesquisa_valor) %>% 
            dplyr::summarise(n = sum(n), 
                             soma = sum(soma),
                             .groups = 'keep') %>% 
            dplyr::mutate(perc = 100*soma/n) %>% 
            dplyr::pull(perc)
        
        stringr::str_c("Valor percentual da variável selecionada na cidade: ", round(obj_perc, 2), "%.")
            
    })
    
    output$info_texto_regional_perc <- shiny::renderText({
        
        obj_perc <- muda_objsp(.variavel = input$pesquisa_variavel,
                               .valor = input$pesquisa_valor) %>% 
            dplyr::summarise(n = sum(n), soma = sum(soma)) %>% 
            dplyr::mutate(perc = 100*soma/n) %>% 
            dplyr::pull(perc)
        
        stringr::str_c("Valor do percentual selecionado na cidade: ", round(obj_perc, 2), "%.")
            
    })
        
    output$tbl_dist_regionais <- shiny::renderTable({
            
        if(input$percentual_info){
            
            muda_objsp(.variavel = input$pesquisa_variavel,.valor = input$pesquisa_valor, .divisao = 'regional') %>% 
                dplyr::select(regional, n, soma, perc) %>%
                dplyr::arrange(desc(perc)) %>% 
                dplyr::mutate(soma = as.integer(soma)) %>%
                dplyr::select(regional, soma, perc) %>% 
                setNames(nm = c('regional', 'frequência', 'percentual')) %>% 
                dplyr::mutate(percentual = stringr::str_c(round(percentual, 2), '%'))
            
        } else{
            
            muda_objsp(.variavel = input$pesquisa_variavel,.valor = input$pesquisa_valor, .divisao = 'regional') %>% 
                dplyr::select(regional, n, soma, perc) %>%
                dplyr::arrange(desc(soma)) %>% 
                dplyr::mutate(soma = as.integer(soma)) %>%
                dplyr::select(regional, soma, perc) %>% 
                setNames(nm = c('regional', 'frequência', 'percentual')) %>% 
                dplyr::mutate(percentual = stringr::str_c(round(percentual, 2), '%'))
        }
        
    })

    dfchoices_search <- shiny::reactive({
        
        
        df_aux <- data.frame(
            key = c(rep("Tipo de acidente", 8), rep("Tipo de veiculo", 8), rep("Periodo do dia", 4), 
                    rep("Sexo do condutor", 3), rep("Numero de veiculos envolvidos", 5),
                    rep("Numero de pessoas envolvidas", 5)),
            value = c("Colisão de veículos","Colisão de veículo com objeto parado",
                      "Atropelamento de pedestre", "Capotamento de veículo",
                      "Queda de pessoa de veículo", "Outros tipos de acidentes",
                      "Atropelamento de animal", "Vazamento de carga do veículo", 
                      "Automoveis", "Motocicletas", "Onibus", "Caminhonetes", 
                      "Bicicletas e Patinetes", "Caminhoes", 
                      "Veiculos de tracao animal", "Outros", 
                      "Manha", "Tarde", "Noite", "Madrugada", 
                      "Feminino", "Masculino", "Nao informado",
                      '1','2','3','4','Mais que 4','1','2','3','4','Mais que 4'),
            stringsAsFactors = FALSE) %>% 
            dplyr::as_tibble()
        
        aux <- df_aux %>% dplyr::filter(key == input$pesquisa_variavel)
        
        if(nrow(aux) == 0){ 
            
            NA 
            
            } else{
            df_aux %>% 
                dplyr::filter(key == input$pesquisa_variavel)

        }
    })
    
    shiny::observe({
        
        if(input$pesquisa_variavel == "Tipo de acidente"){
            shiny::updateSelectizeInput(session,"pesquisa_valor", "", choices = unique(dfchoices_search()$value))
        }
        
        else if(input$pesquisa_variavel == "Tipo de veiculo"){
            shiny::updateSelectizeInput(session,"pesquisa_valor", "", choices = unique(dfchoices_search()$value))
        }
        
        else if(input$pesquisa_variavel == "Periodo do dia"){
            shiny::updateSelectizeInput(session,"pesquisa_valor", "", choices = unique(dfchoices_search()$value))
        }
        else if(input$pesquisa_variavel == "Sexo do condutor"){
            shiny::updateSelectizeInput(session,"pesquisa_valor", "", choices = unique(dfchoices_search()$value))
        }
        else if(input$pesquisa_variavel == "Numero de veiculos envolvidos"){
            shiny::updateSelectizeInput(session,"pesquisa_valor", "", choices = unique(dfchoices_search()$value))
        }
        else if(input$pesquisa_variavel == "Numero de pessoas envolvidas"){
            shiny::updateSelectizeInput(session,"pesquisa_valor", "", choices = unique(dfchoices_search()$value))
        } else{
            shiny::updateSelectizeInput(session,"pesquisa_valor", "", choices = "")
        }

    })
    
    output$comentario_tbl_geral_contingencia <- shiny::renderText({
        
        df <- mostra_info_tabela(x1 = input$variavel_coluna,
                                 x2 = input$variavel_linha)

        df$comentario        

    })
    
    output$tbl_geral_contingencia <- shiny::renderTable({
        
        df <- mostra_info_tabela(x1 = input$variavel_coluna,
                                 x2 = input$variavel_linha)
        
        df$tbl_geral

    })
    
    output$comentario_tbl_bivariada_contingencia <- shiny::renderText({
        
            
        df <- mostra_info_tabela(x1 = input$variavel_coluna,
                                 x2 = input$variavel_linha)
        
        df$tbl_info$comentario

        
    })
    
    output$tbl_bivariada_contingencia <- shiny::renderTable({
        
            
        df <- mostra_info_tabela(x1 = input$variavel_coluna,
                                 x2 = input$variavel_linha)
        
        df$tbl_info$tbl_formato

        
    })
    
    df_acm <- function(){
        
        x1 <- db_ocorrencias_transito_bh$ocorrencias %>% 
            dplyr::select(num_boletim, periodo,
                          num_pessoas_envolvidas, ind_fatalidade,
                          ind_embriaguez, ind_pedestre_envolvido,
                          ind_falta_cinto_seguranca_condutor, 
                          ind_evasao, desc_veiculos, desc_acidente) %>% 
            dplyr::left_join(
                db_ocorrencias_transito_bh$ocorrencias %>% 
                    dplyr::group_by(desc_veiculos) %>% 
                    dplyr::summarise(n = dplyr::n()) %>% 
                    dplyr::arrange(desc(n)) %>% 
                    dplyr::slice(1:15),
                by = 'desc_veiculos') %>%
            dplyr::mutate(desc_veiculos = ifelse(is.na(n), 'outros', desc_veiculos)) %>% 
            dplyr::select(-n) %>% 
            dplyr::mutate(periodo = stringr::str_c('periodo_', periodo)) %>% 
            dplyr::mutate(num_pessoas_envolvidas = ifelse(num_pessoas_envolvidas > 4, 'mais_que_4', num_pessoas_envolvidas)) %>% 
            dplyr::mutate(num_pessoas_envolvidas = stringr::str_c('pessoas_envolvidas_', num_pessoas_envolvidas)) %>% 
            dplyr::mutate(ind_fatalidade = ifelse(ind_fatalidade == 1, 'com_fatalidade', 'sem_fatalidade')) %>% 
            dplyr::mutate(ind_embriaguez = ifelse(ind_embriaguez == 1, 'com_embriaguez', 'sem_embriaguez')) %>%
            dplyr::mutate(ind_pedestre_envolvido = ifelse(ind_pedestre_envolvido == 1, 'com_pedestre', 'sem_pedestre')) %>% 
            dplyr::mutate(ind_falta_cinto_seguranca_condutor = ifelse(ind_falta_cinto_seguranca_condutor == 1, 'pelo_menos_um_condutor_sem_cinto', 'condutores_com_cinto')) %>% 
            dplyr::mutate(ind_evasao = ifelse(ind_evasao == 1, 'com_evasao', 'sem_evasao')) %>% 
            dplyr::mutate(desc_veiculos = stringr::str_replace_all(desc_veiculos, '\\,', ' ')) %>% 
            dplyr::mutate(desc_veiculos = stringr::str_squish(desc_veiculos)) %>% 
            dplyr::mutate(desc_veiculos = stringr::str_replace_all(desc_veiculos, '\\s+', '_')) %>% 
            dplyr::mutate(desc_acidente = stringr::str_replace_all(desc_acidente, '\\s+', '_')) %>% 
            tidyr::gather(key, value, -num_boletim) %>% 
            dplyr::mutate(aux = 1) %>% 
            dplyr::select(-key) %>% 
            dplyr::distinct() %>% 
            tidyr::spread(value, aux)
        
        x2 <- db_ocorrencias_transito_bh$pessoas %>% 
            dplyr::filter(ind_condutor == 1) %>% 
            dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexo_condutor_masculino', sexo)) %>% 
            dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexo_condutor_feminino', sexo)) %>% 
            dplyr::mutate(sexo = ifelse(is.na(sexo), 'sexo_condutor_sem_informacao', sexo)) %>% 
            dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                          labels = c("ate20", "de20a29", "de30a39", "de40a49", 
                                                     "de50a59", "de60a69", "de70oumais"))) %>% 
            dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'sem_informacao', as.character(cut_idade))) %>% 
            dplyr::mutate(cut_idade = stringr::str_c('faixa_etaria_condutor_', cut_idade)) %>% 
            dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem_informacao', desc_severidade_acidente)) %>%   
            dplyr::mutate(desc_severidade_acidente = stringr::str_replace_all(desc_severidade_acidente, '\\s+', '_')) %>%   
            dplyr::mutate(desc_severidade_acidente = ifelse(desc_severidade_acidente != 'sem_ferimentos', stringr::str_c('ferimento_', desc_severidade_acidente), desc_severidade_acidente)) %>% 
            dplyr::mutate(desc_severidade_acidente = stringr::str_c(desc_severidade_acidente, '_condutor')) %>%
            dplyr::mutate(ind_embriagado = ifelse(ind_embriagado == 1, 'condutor_embriagado', 'condutor_nao_embriagado')) %>% 
            dplyr::mutate(ind_embriagado = ifelse(is.na(ind_embriagado), 'condutor_sem_informacao_embriaguez', ind_embriagado)) %>% 
            dplyr::select(num_boletim, sexo, cut_idade, desc_severidade_acidente, ind_embriagado) %>% 
            tidyr::gather(key, value, -num_boletim) %>% 
            dplyr::mutate(aux = 1) %>% 
            dplyr::select(-key) %>% 
            dplyr::distinct() %>% 
            tidyr::spread(value, aux)
        
        x3 <- db_ocorrencias_transito_bh$pessoas %>% 
            dplyr::filter(ind_pedestre == 1) %>% 
            dplyr::mutate(sexo = ifelse(sexo == 'm', 'sexo_pedestre_masculino', sexo)) %>% 
            dplyr::mutate(sexo = ifelse(sexo == 'f', 'sexo_pedestre_feminino', sexo)) %>% 
            dplyr::mutate(sexo = ifelse(is.na(sexo), 'sexo_pedestre_sem_informacao', sexo)) %>% 
            dplyr::mutate(cut_idade = cut(idade, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                                          labels = c("ate20", "de20a29", "de30a39", "de40a49", 
                                                     "de50a59", "de60a69", "de70oumais"))) %>% 
            dplyr::mutate(cut_idade = ifelse(is.na(cut_idade), 'sem_informacao', as.character(cut_idade))) %>% 
            dplyr::mutate(cut_idade = stringr::str_c('faixa_etaria_pedestre_', cut_idade)) %>% 
            dplyr::mutate(desc_severidade_acidente = ifelse(is.na(desc_severidade_acidente), 'sem_informacao', desc_severidade_acidente)) %>%   
            dplyr::mutate(desc_severidade_acidente = stringr::str_replace_all(desc_severidade_acidente, '\\s+', '_')) %>%   
            dplyr::mutate(desc_severidade_acidente = ifelse(desc_severidade_acidente != 'sem_ferimentos', stringr::str_c('ferimento_', desc_severidade_acidente), desc_severidade_acidente)) %>% 
            dplyr::mutate(desc_severidade_acidente = stringr::str_c(desc_severidade_acidente, '_pedestre')) %>%
            dplyr::mutate(ind_embriagado = ifelse(ind_embriagado == 1, 'pedestre_embriagado', 'pedestre_nao_embriagado')) %>% 
            dplyr::mutate(ind_embriagado = ifelse(is.na(ind_embriagado), 'pedestre_sem_informacao_embriaguez', ind_embriagado)) %>% 
            dplyr::select(num_boletim, sexo, cut_idade, desc_severidade_acidente, ind_embriagado) %>% 
            tidyr::gather(key, value, -num_boletim) %>% 
            dplyr::mutate(aux = 1) %>% 
            dplyr::select(-key) %>% 
            dplyr::distinct() %>% 
            tidyr::spread(value, aux)
        
        x <- x1 %>% 
            dplyr::inner_join(x2, by = 'num_boletim') %>% 
            dplyr::left_join(x3, by = 'num_boletim') %>% 
            dplyr::select(-num_boletim) %>% 
            dplyr::mutate_if(.predicate = is.numeric,
                             .funs = function(x){ifelse(is.na(x), 0, x)}) %>% 
            as.matrix()
        
        colnames(x) <- c("AtropelamentoAnimal", "AtropelamentoPedestre", 
                         "Automovel(1)", "Automovel(1)Bicicleta(1)", 
                         "Automovel(1)Caminhonete(1)", "Automovel(1)Motocicleta(1)", 
                         "Automovel(1)Onibus(1)", "Automovel(2)", "Automovel(2)Motocicleta(1)", 
                         "Automovel(3)", "Caminhao(1)Motocicleta(1)", "Caminhonete(1)Motocicleta(1)", 
                         "Camioneta(1)Motocicleta(1)", "CapotamentoVeiculo", 
                         "ColisaoVeiculoComObjetoParado", "ColisaoVeiculos", "ComEmbriaguez", 
                         "ComEvasao", "ComFatalidade", "ComPedestre", "CondutoresComCinto", 
                         "Motocicleta(1)", "Motocicleta(1)Onibus(1)", "Motocicleta(2)", "Onibus(1)", 
                         "OutrasCombinacoes", "OutrosAcidentes", "PeloMenosUmCondutorSemCinto", 
                         "Madrugadas", "Manhas", "Noites", "Tardes", "PessoasEnvolvidas1", 
                         "PessoasEnvolvidas2", "PessoasEnvolvidas3", "PessoasEnvolvidas4", 
                         "PessoasEnvolvidasMaisQue4", "QuedaPessoaVeiculo", 
                         "SemEmbriaguez", "SemEvasao", "SemFatalidade", "SemPedestre", 
                         "VazamentoCargaVeiculo", "CondutorEmbriagado", "CondutorNaoEmbriagado",
                         "SemInformacaoEmbriaguezCondutor", "IdadeCondutorAte20",
                         "IdadeCondutor20a29", "IdadeCondutor30a39", "IdadeCondutor40a49", 
                         "IdadeCondutor50a59", "IdadeCondutor60a69", "IdadeCondutor70OuMais", 
                         "IdadeCondutorSemInformacao", "FerimentoFatalCondutor", 
                         "FerimentoNaoFatalCondutor", "FerimentoSemInformacaoCondutor", 
                         "SemFerimentosCondutor", "SexoCondutorFeminino", "SexoCondutorMasculino",
                         "SexoCondutorSemInformacao", "IdadePedestreAte20", "IdadePedestre20a29", 
                         "IdadePedestre30a39", "IdadePedestre40a49", "IdadePedestre50a59", 
                         "IdadePedestre60a69", "IdadePedestre70OuMais", "IdadePedestreSemInformacao", 
                         "FerimentoFatalPedestre", "FerimentoNaoFatalPedestre", 
                         "FerimentoSemInformacaoPedestre", "SemFerimentosPedestre", 
                         "PedestreEmbriagado", "PedestreNaoEmbriagado", "SemInformacaoEmbriaguezPedestre",
                         "SexoPedestreFeminino", "SexoPedestreMasculino", "SexoPedestreSemInformacao")
        
        mt_burt <- t(x)%*%x
        
        acm <- ca::ca(mt_burt)
        plot_acm <- ca::plot.ca(acm)
        
        df_acm <- plot_acm$rows %>% 
            dplyr::as_tibble() %>% 
            dplyr::mutate(variavel = rownames(plot_acm$rows)) %>% 
            dplyr::mutate(grupo = c("TipoAcidente", "TipoAcidente", 
                                    "CombinacaoVeiculos", "CombinacaoVeiculos", 
                                    "CombinacaoVeiculos", "CombinacaoVeiculos", 
                                    "CombinacaoVeiculos", "CombinacaoVeiculos", "CombinacaoVeiculos", 
                                    "CombinacaoVeiculos", "CombinacaoVeiculos", "CombinacaoVeiculos", 
                                    "CombinacaoVeiculos", "TipoAcidente", 
                                    "TipoAcidente", "TipoAcidente", "SinaisEmbriaguez", 
                                    "IndicioEvasao", "Fatalidade", "Pedestres", "UsoCinto", 
                                    "CombinacaoVeiculos", "CombinacaoVeiculos", "CombinacaoVeiculos", "CombinacaoVeiculos", 
                                    "CombinacaoVeiculos", "TipoAcidente", "UsoCinto", 
                                    "Periodo", "Periodo", "Periodo", "Periodo", "PessoasEnvolvidas", 
                                    "PessoasEnvolvidas", "PessoasEnvolvidas", "PessoasEnvolvidas", 
                                    "PessoasEnvolvidas", "TipoAcidente", 
                                    "SinaisEmbriaguez", "IndicioEvasao", "Fatalidade", "Pedestres", 
                                    "TipoAcidente", "SinaisEmbriaguez", "SinaisEmbriaguez",
                                    "SinaisEmbriaguez", "IdadeCondutor",
                                    "IdadeCondutor", "IdadeCondutor", "IdadeCondutor", 
                                    "IdadeCondutor", "IdadeCondutor", "IdadeCondutor", 
                                    "IdadeCondutor", "FerimentosCondutor", 
                                    "FerimentosCondutor", "FerimentosCondutor", 
                                    "FerimentosCondutor", "SexoCondutor", "SexoCondutor",
                                    "SexoCondutor", "IdadePedestre", "IdadePedestre", 
                                    "IdadePedestre", "IdadePedestre", "IdadePedestre", 
                                    "IdadePedestre", "IdadePedestre", "IdadePedestre", 
                                    "FerimentosPedestre", "FerimentosPedestre", 
                                    "FerimentosPedestre", "FerimentosPedestre", 
                                    "SinaisEmbriaguez", "SinaisEmbriaguez", "SinaisEmbriaguez",
                                    "SexoPedestre", "SexoPedestre", "SexoPedestre")) %>% 
            setNames(nm = c('dim1', 'dim2', 'variavel', 'grupo')) %>% 
            dplyr::mutate(postodim1 = rank(dim1)) %>% 
            dplyr::mutate(postodim2 = rank(dim2))
        
        df_acm
    }

    output$acm_grafico <- plotly::renderPlotly({
        
        p <- df_acm() %>% 
            ggplot2::ggplot(ggplot2::aes(x = dim1, y = dim2, color = grupo)) + 
            ggplot2::geom_text(ggplot2::aes(label = variavel), size = 2.5) +
            ggplot2::theme_bw() +
            ggplot2::xlab('Dimensão 1 (28,5%)') + 
            ggplot2::ylab('Dimensão 2 (14,1%)')
        
        plotly::ggplotly(p)
        
    })


}

# Run the application 
shiny::shinyApp(ui = ui, server = server)
