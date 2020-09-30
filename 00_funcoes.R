# ----- Auxiliares ------------------------------------------------------------
subst_na <-  function(x) {
  x[is.na(x)] <- 0
  x
}

# ----- POF 2003 --------------------------------------------------------------
ler_rendimentos2003 <- function() {
  nomes <- c("rendimentos", "outros_reci")
  arqs <- glue::glue(
    "dados/2003/t_{nomes}.rds"
  )
  if (any(!file.exists(arqs))) {
    stop("Os arquivos necessários para rodar esse scripts\n",
         "são criados no script '96_download_leitura_2003.R'.\n",
         "Rode ele para poder executar essa função.", 
         call. = FALSE)
  }
  
  rendimentos <- readRDS("dados/2003/t_rendimentos.rds") %>% 
    as_tibble()
  
  outros_reci <- readRDS("dados/2003/t_outros_reci.rds") %>% 
    as_tibble()
  
  # use old translator for generating same table as 2008-09 script
  tabelagregada <- "https://raw.githubusercontent.com/rodrigoesborges/pofesferas/master/tradutores/codigos-recodificacao-rendimentos.csv" %>% 
    read_csv2() %>% 
    set_names(c("cod_novo","tipoderendimento","cod_rec"))

  incomeRecodesX <- read_csv("https://raw.githubusercontent.com/rodrigoesborges/pofesferas/master/tradutores/tradutor-detalhado2003-reag.csv") %>% 
    rename(cod_novo = cod.novo, cod_rec = cod.rec)
  
  t_rendimentos <- rendimentos %>% 
    mutate(recmes = rend_def_anual / deflator / 12 ,
           cod_uc = paste0( uf , seq , dv , domcl , uc ),
           cod_rec = quadro * 1000 + pos_ocup
    )
  
  
  t_rendimentos_recoded <- t_rendimentos %>% 
    left_join(incomeRecodesX, by = "cod_rec") %>% 
    select(cod_rec, cod_uc, recmes, fator_set, fator, cod_novo, 
           pos_ocup, c_ocupacao, c_atividade)
  
  
  t_outros_reci <- outros_reci %>% 
    mutate(
      recmes = rend_def_anual / deflator / 12 ,
      cod_uc = paste0( uf , seq , dv , domcl , uc ),
      cod_rec = quadro*1000+ floor(item/100)
    )
  
  t_outros_reci_recoded <- t_outros_reci %>% 
    left_join(incomeRecodesX, by = "cod_rec") %>% 
    select(cod_rec, cod_uc, recmes, fator_set, fator, cod_novo )
  
  bind_rows(t_rendimentos_recoded, t_outros_reci_recoded)
}

classificar_rendimentos2003 <- function(df) {
  # morador <- readRDS("dados/2003/t_morador.rds") %>% 
  #   as_tibble()
  # 
  # t_morador <- morador %>% 
  #   mutate(cod_uc = paste0( uf , seq , dv , domcl , uc ) )
  # 
  # domicilio_rendas <- t_morador %>% 
  #   select(cod_uc, renda) %>% 
  #   unique()
  # 
  # componentes <- incomeRecodesX %>% 
  #   set_names(c("cod_novo","tipoderendimento","cod_rec"))
  # 
  # # Now we generate the vector that will be used as base for our aggregation criteria
  # # First we take (in fixed form) all items / subcodes
  # todos.subcodigos.tiporenda <- componentes %>% 
  #   filter(substring(cod_novo,1, 1) == 1) %>% 
  #   select(cod_novo)
  # 
  # # next we aggregate by family/household the recoded income data
  # domicilios_porcodigo <- df %>% 
  #   filter(cod_novo %in% todos.subcodigos.tiporenda$cod_novo) %>% 
  #   select(cod_novo, recmes, cod_uc)
  # 
  # domicilios_porcodigo_agregados <- domicilios_porcodigo %>% 
  #   dplyr::mutate(nivel = stringr::str_remove_all(cod_novo, "\\.") %>% 
  #                   as.numeric() ) %>% 
  #   dplyr::group_by(cod_uc, nivel) %>% 
  #   dplyr::summarise(recmes = sum(recmes))
  
  # domicilios_porcodigo_agregados %>%
  df %>% 
    dplyr::mutate(nivel = stringr::str_remove_all(cod_novo, "\\.") %>% 
                    as.numeric() ) %>% 
    dplyr::filter(!is.na(nivel)) %>% 
    dplyr::mutate(forma = dplyr::case_when(
        # rendimento de empregado baseado no vínculo
        nivel == 111 & pos_ocup == 3 ~ "cv", # doméstico
        nivel == 111 & ((c_ocupacao %/% 1000) == 0) ~ "mv", # militares
        nivel == 111 & pos_ocup == 1 ~ "cv", #privado
        # setor estatutário
        # nivel == 111 & v5302 == 4 & v5303 == 1 ~ "mv",
        # supoe não ter informalidade no setor publico
        # trabalhador de estatal
        # nivel == 111 & v5302 == 4 & v5304 == 1 ~ "cv",
        # Tem casos (2491) de servidor sem carteria assinada ou estatuto. Pq?
        nivel == 111 & pos_ocup == 2 ~ "mv", # empregado público
        nivel == 111 & pos_ocup == 6 ~ "emp", # empregador
        nivel == 111 & pos_ocup == 7 ~ "cp", # conta própria
        # trab não remunerado e outros (estagiário, etc) ?
        nivel == 111 & pos_ocup %in% c(4, 5, 8:10) ~ "cv", 
      nivel == 111 ~ "cv", # empregado mas sem informação da pos_ocup
      nivel == 112 ~ "emp", # empregador
      nivel == 121 ~ "cv", # INSS
      nivel == 122 ~ "cv", # previdencia pública
      nivel == 123 ~ "mv", # previdencia privada
      nivel == 124 ~ "cv", # programas sociais
      nivel == 13 ~ "mv", # aluguel
      nivel == 14 ~ "mv", # outras rendas (morador ausente, menor de 10,
      # indenização judicial, acoes, juros, outros)
      
      # Deixei esses casos por ultimo por sao casos para pensarmos
      nivel == 113 ~ "cp", # conta própria
      nivel == 125 ~ "cv", # pensao alimenticia, mesada, etc ?
      nivel == 126 ~ "cv", # outras transferências
      TRUE ~ NA_character_,
    ),
    # Esses 2.000 vieram da análise de Kmeans
    # das rendas de conta-própria com 2 núcleos
    # foi excluido um outlier de 400.000
    forma = ifelse(forma != "cp", forma, 
                   ifelse(recmes > 2000, "mv", "cv")),
    forma = ifelse(forma != "emp", forma, 
                   ifelse(recmes > 4500, "mv", "cv"))
    )
}

ler_despesas2003 <- function() {
  nomes <- c("caderneta_despesa", "despesa_90dias", "despesa_12meses", 
             "despesa_veiculo",  "despesa", "despesa_esp")
  arqs <- glue::glue(
    "dados/2003/t_{nomes}.rds"
  )
  if (any(!file.exists(arqs))) {
    stop("Os arquivos necessários para rodar esse scripts\n",
         "são criados no script '96_download_leitura_2003.R'.\n",
         "Rode ele para poder executar essa função.", 
         call. = FALSE)
  }
  cad_desp <- readRDS("dados/2003/t_caderneta_despesa.rds")
  
  desp_90d <- readRDS("dados/2003/t_despesa_90dias.rds")
  
  desp_12m <- readRDS("dados/2003/t_despesa_12meses.rds")
  
  desp_veic <- readRDS("dados/2003/t_despesa_veiculo.rds")
  
  despesa <- readRDS("dados/2003/t_despesa.rds")
  
  despesa_esp <- readRDS("dados/2003/t_despesa_esp.rds")
  
  # Carrega tabela que traduz itens POF --> SCN
  tf <- tempfile()
  "https://github.com/rodrigoesborges/pofesferas/blob/master/tradutores/Tradu_POF_2003xContas-rearrumado-na-mao.xls?raw=true" %>% 
    download.file(tf)
  tradutor <- readxl::read_excel(tf, skip = 1)
  
  componentes <- read_csv("https://raw.githubusercontent.com/rodrigoesborges/pofesferas/master/tradutores/cod68X20componentes-HIERARQ.csv")
  
  # Definimos função para recodificar, recalcular e selecionar apenas dados necessários para as próximas fases
  recod.despesas <- function (tabela, n.cod.qd) {
    tabela %>% 
      mutate(
        codigo = {{n.cod.qd}} * 100000 + item,
        despmes = val_def_anual / deflator / 12 ,
        cod_uc = paste0(uf , seq , dv , domcl , uc) 
      ) %>% 
      group_by(cod_uc, codigo, fator) %>% 
      summarise(despmes = sum(despmes)) %>% 
      ungroup()
  }
  
  despesas_mensais_col <- recod.despesas(cad_desp, n.cod.qd = grupo)
  
  despesas_mensais_ind <- recod.despesas(despesa, quadro)
  
  despesas_90 <- recod.despesas(desp_90d, quadro)
  
  despesas_veic <- recod.despesas(desp_veic, quadro)
  
  despesas_12m <- recod.despesas(desp_12m, quadro)
  
  despesas_esp <- recod.despesas(despesa_esp, quadro)
  
  bind_rows(
    despesas_mensais_col,
    despesas_mensais_ind,
    despesas_90,
    despesas_veic,
    despesas_12m,
    despesas_esp
  )
}

# ----- POF 2009 --------------------------------------------------------------
ler_rendimentos2009 <- function() {
  nomes <- c("rendimentos", "outros_reci")
  arqs <- glue::glue(
    "dados/2009/Dados/t_{nomes}_s.rds"
  )
  if (any(!file.exists(arqs))) {
    stop("Os arquivos necessários para rodar esse scripts\n",
         "são criados no script '98_download_leitura_2009.R'.\n",
         "Rode ele para poder executar essa função.", 
         call. = FALSE)
  }
  
  rendimentos <- readRDS("dados/2009/Dados/t_rendimentos_s.rds") %>% 
    tibble::as_tibble()
  
  outros_reci <- readRDS("dados/2009/Dados/t_outros_reci_s.rds") %>% 
    tibble::as_tibble()
  
  incomeRecodes <- "https://raw.githubusercontent.com/rodrigoesborges/pofesferas/master/tradutores/codigos-recodificacao-rendimentos.csv" %>% 
    readr::read_csv2() %>% 
    dplyr::mutate(cod.inc = gsub(".*\\+.*", "50000", cod.inc))
  
  componentes <- incomeRecodes %>% 
    purrr::set_names(c("cod_novo","tipoderendimento","cod_rec"))
  
  incomeRecodesX <- incomeRecodes$numero %>% 
    purrr::map2_df(incomeRecodes$cod.inc, ~{
      cmd <- paste("c(", .y,")")
      tibble::tibble(cod_novo = .x, 
                     cod_rec = as.character(eval(parse(text = cmd))))
    })
  
  rendimentos_recoded <- rendimentos %>% 
    dplyr::mutate(
      recmes = ( valor_anual_expandido2 / fator_expansao2 ) / 12,
      cod_uc = paste0( cod_uf, num_seq, num_dv, cod_domc, num_uc ),
      cod_rec = paste0( num_quadro, substr(cod_item,1,3))
    ) %>% 
    dplyr::left_join(incomeRecodesX, by = "cod_rec") %>% 
    dplyr::select(cod_rec, cod_uc, recmes, fator_expansao1, fator_expansao2, 
                  cod_novo, cod_posi_ocupa, cod_ocup_final)
  
  # É aqui que precisamos mexer caso queramos incluir critérios do informantes
  # setor que trabalho, profissao, etc
  outros_reci_recoded <- outros_reci %>% 
    dplyr::mutate(
      recmes = ( valor_anual_expandido2 / fator_expansao2 ) / 12,
      cod_uc = paste0( cod_uf, num_seq, num_dv, cod_domc, num_uc ),
      cod_rec = paste0( num_quadro, substr(cod_item,1, 3))
    ) %>% 
    dplyr::left_join(incomeRecodesX, by = "cod_rec") %>% 
    dplyr::select(cod_rec, cod_uc, recmes, fator_expansao1, fator_expansao2, 
                  cod_novo)
  
  rendimentos_recoded %>% 
    dplyr::bind_rows(outros_reci_recoded)
}


classificar_rendimentos2009 <- function(df) {
  
  # morador <- readRDS("dados/2009/Dados/t_morador_s.rds") %>% 
  #   tibble::as_tibble()
  # 
  # componentes <- "https://raw.githubusercontent.com/rodrigoesborges/pofesferas/master/tradutores/codigos-recodificacao-rendimentos.csv" %>% 
  #   readr::read_csv2() %>% 
  #   dplyr::mutate(cod.inc = gsub(".*\\+.*", "50000", cod.inc)) %>% 
  #   purrr::set_names(c("cod_novo","tipoderendimento","cod_rec"))
  # 
  # domicilio_rendas <- morador %>% 
  #   dplyr::mutate(cod_uc = paste0( cod_uf, num_seq, num_dv, cod_domc, num_uc )) %>% 
  #   dplyr::select(cod_uc, renda_total) %>% 
  #   unique()
  # 
  # todos_subcodigos_tiporenda <- componentes %>% 
  #   dplyr::filter(substring(cod_novo, 1, 1) == 1) %>% 
  #   dplyr::pull('cod_novo')
  # 
  # domicilios_porcodigo <- df %>% 
  #   dplyr::filter(cod_novo %in% todos_subcodigos_tiporenda) %>% 
  #   dplyr::select(cod_novo, recmes, cod_uc)
  # 
  # domicilios_porcodigo_agregados <- domicilios_porcodigo %>% 
  #   dplyr::group_by(cod_uc, nivel = cod_novo) %>% 
  #   dplyr::summarise(recmes = sum(recmes))
  # 
  # domicilios_porcodigo_agregados %>%
  # dplyr::mutate(forma = dplyr::case_when(
  df %>% 
    dplyr::rename(nivel = cod_novo) %>% 
    dplyr::filter(!is.na(nivel)) %>% 
    dplyr::mutate(
      cod_posi_ocupa = as.numeric(cod_posi_ocupa),
      cod_ocup_final = as.numeric(cod_ocup_final),
      forma = dplyr::case_when(
      # rendimento de empregado baseado no vínculo
      nivel == 111 & cod_posi_ocupa == 3 ~ "cv", # doméstico
      nivel == 111 & ((cod_ocup_final %/% 1000) == 0) ~ "mv", # militares
      nivel == 111 & cod_posi_ocupa == 1 ~ "cv", #privado
      # setor estatutário
      # nivel == 111 & v5302 == 4 & v5303 == 1 ~ "mv",
      # supoe não ter informalidade no setor publico
      # trabalhador de estatal
      # nivel == 111 & v5302 == 4 & v5304 == 1 ~ "cv",
      # Tem casos (2491) de servidor sem carteria assinada ou estatuto. Pq?
      nivel == 111 & cod_posi_ocupa == 2 ~ "mv", # empregado público
      nivel == 111 & cod_posi_ocupa == 5 ~ "emp", # empregador
      nivel == 111 & cod_posi_ocupa == 6 ~ "cp", # conta própria
      # trab não remunerado e outros (estagiário, etc) ?
      nivel == 111 & cod_posi_ocupa %in% c(4, 7:9) ~ "cv", 
      nivel == 111 ~ "cv", # empregado mas sem informação da cod_posi_ocupa
      nivel == 112 ~ "emp", # empregador
      nivel == 121 ~ "cv", # INSS
      nivel == 122 ~ "cv", # previdencia pública
      nivel == 123 ~ "mv", # previdencia privada
      nivel == 124 ~ "cv", # programas sociais
      nivel == 13 ~ "mv", # aluguel
      nivel == 14 ~ "mv", # outras rendas (morador ausente, menor de 10,
      # indenização judicial, acoes, juros, outros)
      
      # Deixei esses casos por ultimo por sao casos para pensarmos
      nivel == 113 ~ "cp", # conta própria
      nivel == 125 ~ "cv", # pensao alimenticia, mesada, etc ?
      nivel == 126 ~ "cv", # outras transferências
      TRUE ~ NA_character_,
    ),
    # Esses dois 2.500 viram da análise de Kmeans
    # das rendas de conta-própria com 2 núcleos
    forma = ifelse(forma != "cp", forma, 
                   ifelse(recmes > 2500, "mv", "cv")),
    forma = ifelse(forma != "emp", forma, 
                   ifelse(recmes > 5000, "mv", "cv"))
    )

  # renda_m_total <- domicilios_porcodigo_agregados %>% 
  #   dplyr::group_by(cod_uc) %>%
  #   dplyr::summarise(cod_novo = 1,
  #             recmes = sum(recmes, na.rm = TRUE)) %>% 
  #   dplyr::bind_rows(domicilios_porcodigo_agregados) %>% 
  #   tidyr::spread(key = cod_novo, value = recmes) %>% 
  #   purrr::map_dfc(subst_na)
  # 
  # # Aqui a gente define rendas do trabalho, etc
  # renda_m <- renda_m_total %>% 
  #   dplyr::mutate(renda_trabalho = ((`111` + `113` +
  #                               `121` + `124` + 0.01 ) / 
  #                              (`1`+ 0.01)) * 100) %>% 
  #   dplyr::select(cod_uc, renda_trabalho)
  # 
  # domicilios_trabalhadores <- domicilio_rendas %>% 
  #   dplyr::left_join(renda_m, by = "cod_uc")
  # 
  # # Aqui os NAs com baixa renda foram definidos como trabalhadores
  # # De onde a gente tirou esses 2.000 mil reais?
  # domicilios_trabalhadores[is.na(domicilios_trabalhadores$renda_trabalho) &
  #                            domicilios_trabalhadores$renda_total <= 2000, "renda_trabalho"] <- 100
  # 
  # domicilios_trabalhadores[is.na(domicilios_trabalhadores$renda_trabalho) &
  #                            domicilios_trabalhadores$renda_total > 2000, "renda_trabalho"] <- 0
  # 
  # # Aqui é estabelecido o critéiro de corte
  # domicilios_trabalhadores %>% 
  #   dplyr::mutate(
  #     trabalhador.cat = cut(renda_trabalho, c( 0, 60, Inf ),
  #                           include.lowest = TRUE, 
  #                           labels = c( "Não Trabalhador", "Trabalhador" )),
  #     control = substr(cod_uc, 1, 6 )
  #   )  
}

ler_despesas2009 <- function() {
  nomes <- c("caderneta_despesa", "despesa_individual", "despesa_90dias", 
             "despesa_12meses", "despesa_veiculo")
  arqs <- glue::glue(
    "dados/2009/Dados/t_{nomes}_s.rds"
  )
  if (any(!file.exists(arqs))) {
    stop("Os arquivos necessários para rodar esse scripts\n",
         "são criados no script '98_download_leitura_2009.R'.\n",
         "Rode ele para poder executar essa função.", 
         call. = FALSE)
  }
  cad_desp <- readRDS("dados/2009/Dados/t_caderneta_despesa_s.rds") %>% 
    as_tibble()
  
  desp_ind <- readRDS("dados/2009/Dados/t_despesa_individual_s.rds") %>% 
    as_tibble()
  
  desp_90d <- readRDS("dados/2009/Dados/t_despesa_90dias_s.rds") %>% 
    as_tibble()
  
  desp_12m <- readRDS("dados/2009/Dados/t_despesa_12meses_s.rds") %>% 
    as_tibble()
  
  desp_veic <- readRDS("dados/2009/Dados/t_despesa_veiculo_s.rds") %>% 
    as_tibble()
  
  # Carrega tabela que traduz itens POF --> SCN
  tf <- tempfile()
  "https://github.com/rodrigoesborges/pofesferas/blob/master/tradutores/Tradutor_POF2009_ContasNacionais.xls?raw=true" %>% 
    download.file(tf)
  tradutor <- readxl::read_excel(tf, sheet = 1 , skip = 1)
  
  # Tabela de componentes hierarquizada cod68 x cod 20 - dicionário de tradução agregado
  componentes <- read_csv("https://raw.githubusercontent.com/rodrigoesborges/pofesferas/master/tradutores/cod68X20componentes-HIERARQ.csv")
  
  # Carrega tabela com códigos POF que não entram inicialmente como Consumo Final das Famílias
  pofnaoconsumo <- read_csv("https://raw.githubusercontent.com/rodrigoesborges/pofesferas/master/tradutores/codigos_semtradutor.csv")
  
  # Definimos função para recodificar, recalcular e selecionar apenas dados necessários para as próximas fases
  recod.despesas <- function (tabela, n.cod.qd, n.cod.it) { 
    tabela <- tabela %>% 
      # mutate(codigo = substr(paste0(eval(parse(text = n.cod.qd)), eval(parse(text = n.cod.it))) , 1 , 5) ,
      mutate(codigo = substr(paste0({{n.cod.qd}}, {{n.cod.it}}) , 1 , 5) ,
             despmes = ( valor_anual_expandido2 / fator_expansao2 / 12) ,
             cod_uc = paste0(cod_uf , num_seq , num_dv , cod_domc , num_uc )
      ) %>% 
      select(cod_uc, codigo, despmes) %>% 
      group_by(cod_uc, codigo) %>% 
      summarise(despmes = sum(despmes)) %>% 
      ungroup()
  }
  
  despesas_mensais_col <- recod.despesas(cad_desp, n.cod.qd = prod_num_quadro_grupo_pro, n.cod.it = cod_item)
  
  despesas_mensais_ind <- recod.despesas(desp_ind, n.cod.qd = num_quadro, n.cod.it = cod_item)
  
  despesas_90 <- recod.despesas(desp_90d)
  
  despesas_veic <- recod.despesas(desp_veic)
  
  despesas_12m <- recod.despesas(desp_12m)
  
  bind_rows(
    despesas_mensais_col,
    despesas_mensais_ind,
    despesas_90,
    despesas_veic,
    despesas_12m
  )
}

# ----- POF 2018 --------------------------------------------------------------
ler_rendimentos2018 <- function() {
  morador_uc <- pof::ler_morador(2018) %>%
    select(UF, ESTRATO_POF, TIPO_SITUACAO_REG,
           COD_UPA, NUM_DOM ,NUM_UC, PESO_FINAL) %>%
    unique()
  
  soma_familia <- sum(morador_uc$PESO_FINAL)
  
  # Precisei extrair os tradutores manualmente (questao de encoding)
  dic_rendimento <- pof::ler_tradutor_rendimento(2018)
  
  dic2 <- dic_rendimento %>%
    dplyr::select(codigo, nivel = nivel_3, desc = descricao_3) %>%
    dplyr::filter(!is.na(nivel)) %>%
    dplyr::bind_rows(
      dic_rendimento %>%
        dplyr::filter(nivel_2 %in% c(13, 14)) %>%
        dplyr::select(codigo, nivel = nivel_2, desc = descricao_2)
    )
  
  rend_trabalho <- pof::ler_rend_trab(2018) %>%
    filter(!is.na(V8500_DEFLA)) %>%
    transmute(
      V9001 = V9001,
      v5302 = V5302, v5303 = V5303, v5304 = V5304,
      cod_uc = paste0(UF, ESTRATO_POF, TIPO_SITUACAO_REG,
                      COD_UPA, NUM_DOM ,NUM_UC),
      valor_mensal = (V8500_DEFLA*V9011*FATOR_ANUALIZACAO)/12,
      peso_final = PESO_FINAL
    )
  
  outros_rend <- pof::ler_rend_outros(2018) %>%
    transmute(
      V9001 = V9001,
      cod_uc = paste0(UF, ESTRATO_POF, TIPO_SITUACAO_REG,
                      COD_UPA, NUM_DOM ,NUM_UC),
      valor_mensal = ifelse( QUADRO == 54,
                             (V8500_DEFLA*V9011*FATOR_ANUALIZACAO)/12,
                             (V8500_DEFLA*FATOR_ANUALIZACAO)/12
      ),
      peso_final = PESO_FINAL
    )
  
  rend_trabalho %>%
    bind_rows(outros_rend) %>%
    mutate(codigo = str_sub(V9001, 1,5)) %>%
    left_join(dic2, by = "codigo")
}

classificar_rendimentos <- function(df) {
  junta_rendas <- df %>%
    # Os casos que caem com o filtro abaixo são movimentações 
    # financeiras que não sao renda
    # deposito de poupança, compra de ações, etc...
    filter(!is.na(nivel)) %>%
    mutate(forma = case_when(
      # rendimento de empregado baseado no vínculo
      nivel == 111 & v5302 == 1 ~ "cv",
      nivel == 111 & v5302 == 2 ~ "mv",
      nivel == 111 & v5302 == 3 ~ "cv",
      # setor estatutário
      # nivel == 111 & v5302 == 4 & v5303 == 1 ~ "mv",
      # supoe não ter informalidade no setor publico
      # trabalhador de estatal
      # nivel == 111 & v5302 == 4 & v5304 == 1 ~ "cv",
      # Tem casos (2491) de servidor sem carteria assinada ou estatuto. Pq?
      nivel == 111 & v5302 == 4 ~ "mv", # empregado público
      nivel == 111 & v5302 == 5 ~ "emp", # empregador
      nivel == 111 & v5302 == 6 ~ "cp", # conta própria
      nivel == 111 & v5302 == 7 ~ "cv", # trab não remunerado ?
      nivel == 111 ~ "cv", # empregado mas sem informação da v5302
      nivel == 112 ~ "emp", # empregador
      nivel == 121 ~ "cv", # INSS
      nivel == 122 ~ "cv", # previdencia pública
      nivel == 123 ~ "mv", # previdencia privada
      nivel == 124 ~ "cv", # programas sociais
      nivel == 13 ~ "mv", # aluguel
      nivel == 14 ~ "mv", # outras rendas (morador ausente, menor de 10,
      # indenização judicial, acoes, juros, outros)

      # Deixei esses casos por ultimo por sao casos para pensarmos
      nivel == 113 ~ "cp", # conta própria
      nivel == 125 ~ "cv", # pensao alimenticia, mesada, etc ?
      nivel == 126 ~ "cv", # outras transferências
      TRUE ~ NA_character_,
    ))
  
  # O código abaixo foi usado para definir a renda dos conta-própria
  # que deve ser usada para classificar como baixa/alta
  # O valor encontrado foi R$ 6.000
  # junta_rendas %>%
  #   filter(forma == "cp") %>%
  #   mutate(grupo = kmeans(valor_mensal, c(1e3, 20e3))$cluster) %>%
  #   ggplot(aes(valor_mensal, col = as.factor(grupo))) +
  #   geom_density() +
  #   scale_x_continuous(limits = c(0, 15)*1000, breaks = c(0:15)*1e3) +
  #   theme(axis.text.x = element_text(angle = 90), axis.text.y = element_blank())
  
  # algo semelhante pode ser feito para rendas dos empresários
  # para eliminar MEIs que contratam 1 pessoa
  junta_rendas %>%
    mutate(forma = ifelse(forma != "cp", forma,
                          # valor usado vem do gráfico acima
                          ifelse(valor_mensal > 6000, "mv", "cv")),
           forma = ifelse(forma != "emp", forma,
                          # valor usado vem do gráfico acima
                          ifelse(valor_mensal > 10000, "mv", "cv")))
  
  # df %>% 
  #   dplyr::filter(!is.na(nivel)) %>%
  #   dplyr::mutate(forma = dplyr::case_when(
  #   nivel == 111 ~ "cv", # empregado mas sem informação da v5302
  #   nivel == 112 ~ "mv", # empregador
  #   nivel == 121 ~ "cv", # INSS
  #   nivel == 122 ~ "cv", # previdencia pública
  #   nivel == 123 ~ "mv", # previdencia privada
  #   nivel == 124 ~ "cv", # programas sociais
  #   nivel == 13 ~ "mv", # aluguel
  #   nivel == 14 ~ "mv", # outras rendas (morador ausente, menor de 10,
  #   # indenização judicial, acoes, juros, outros)
  #   
  #   # Deixei esses casos por ultimo por sao casos para pensarmos
  #   nivel == 113 ~ "cp", # conta própria
  #   nivel == 125 ~ "cv", # pensao alimenticia, mesada, etc ?
  #   nivel == 126 ~ "cv", # outras transferências
  #   TRUE ~ NA_character_,
  # ),
  # # Esses dois 2.500 viram da análise de Kmeans
  # # das rendas de conta-própria com 2 núcleos
  # forma = ifelse(forma != "cp", forma, 
  #                ifelse(valor_mensal > 6000, "mv", "cv"))
  # )
}

ler_despesas2018 <- function() {
  alu_estimado <- ler_aluguel(2018) %>%
    transmute(
      V9001 = V9001,
      cod_uc = paste0(UF, ESTRATO_POF, TIPO_SITUACAO_REG,
                      COD_UPA, NUM_DOM ,NUM_UC),
      valor_mensal = (V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12
    )
  
  desp_coletiva <- ler_desp_col(2018) %>%
    transmute(V9001 = V9001,
              cod_uc = paste0(UF, ESTRATO_POF, TIPO_SITUACAO_REG,
                              COD_UPA, NUM_DOM ,NUM_UC),
              valor_mensal = ifelse( QUADRO == 10 | QUADRO == 19,
                                     (V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12,
                                     (V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12
              ) ,
              inss_mensal=(V1904_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12
    )
  
  cad_coletiva <- ler_cad_col(2018) %>%
    transmute(V9001 = V9001,
              cod_uc = paste0(UF, ESTRATO_POF, TIPO_SITUACAO_REG,
                              COD_UPA, NUM_DOM ,NUM_UC),
              valor_mensal = (V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12
    )
  
  desp_individual <- ler_desp_ind(2018) %>%
    transmute(V9001 = V9001,
              cod_uc = paste0(UF, ESTRATO_POF, TIPO_SITUACAO_REG,
                              COD_UPA, NUM_DOM ,NUM_UC),
              valor_mensal = ifelse( QUADRO %in% c(44, 47, 48, 49, 50),
                                     (V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12,
                                     (V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12)
    )
  
  rend_trabalho <- ler_rend_trab(2018) %>%
    transmute(V9001 = V9001,
              cod_uc = paste0(UF, ESTRATO_POF, TIPO_SITUACAO_REG,
                              COD_UPA, NUM_DOM ,NUM_UC),
              prev_pub_mensal=(V531112_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12,
              imp_renda_mensal=(V531122_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12,
              iss_mensal=(V531132_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12
    )
  
  outros_rend <- ler_rend_outros(2018) %>%
    transmute(V9001 = V9001,
              cod_uc = paste0(UF, ESTRATO_POF, TIPO_SITUACAO_REG,
                              COD_UPA, NUM_DOM ,NUM_UC),
              deducao_mensal = ifelse( QUADRO == 54,
                                       (V8501_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12,
                                       (V8501_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12
              )
    )
  
  desp_coletiva_n <- desp_coletiva %>%
    mutate(prev_pub_mensal = NA, imp_renda_mensal = NA,
           iss_mensal = NA , deducao_mensal = NA)
  
  cad_coletiva_n <- cad_coletiva %>%
    mutate(inss_mensal = NA, prev_pub_mensal = NA, imp_renda_mensal = NA,
           iss_mensal = NA, deducao_mensal = NA)
  
  desp_individual_n <- desp_individual %>%
    mutate(inss_mensal = NA, prev_pub_mensal = NA, imp_renda_mensal = NA,
           iss_mensal = NA, deducao_mensal = NA)
  
  alu_estimado_n <- alu_estimado %>%
    mutate(V9001 = as.numeric(V9001), inss_mensal = NA, prev_pub_mensal = NA,
           imp_renda_mensal = NA, iss_mensal = NA, deducao_mensal = NA)
  
  rend_trabalho_n <- rend_trabalho %>%
    mutate(inss_mensal = NA, prev_pub_mensal = NA, imp_renda_mensal = NA,
           iss_mensal = NA, deducao_mensal = NA)
  
  outros_rend_n <- outros_rend %>%
    mutate(valor_mensal = NA, inss_mensal = NA, prev_pub_mensal = NA,
           imp_renda_mensal = NA, iss_mensal = NA) %>%
    select(V9001, valor_mensal, inss_mensal:iss_mensal, deducao_mensal)
  
  junta_despesas <- bind_rows( desp_coletiva_n ,
                               cad_coletiva_n ,
                               desp_individual_n ,
                               alu_estimado_n,
                               rend_trabalho_n ,
                               outros_rend_n ) %>%
    mutate(codigo = round(V9001/100)) %>%
    select(-V9001)
  
  merge1 <- junta_despesas %>%
    left_join(pof:::ler_tradutor_despesa(2018) %>%
                select(codigo, variavel, starts_with("nivel")),
              "codigo") %>%
    mutate(
      valor = ifelse( variavel == 'V8000_DEFLA' ,
                      valor_mensal ,
                      ifelse( variavel == 'V1904_DEFLA' ,
                              inss_mensal ,
                              ifelse( variavel == 'V531112_DEFLA' ,
                                      prev_pub_mensal ,
                                      ifelse( variavel == 'V531122_DEFLA' ,
                                              imp_renda_mensal ,
                                              ifelse( variavel == 'V531132_DEFLA' ,
                                                      iss_mensal ,
                                                      ifelse( variavel == 'V8501_DEFLA' ,
                                                              deducao_mensal ,
                                                              NA
                                                      )
                                              )
                                      )
                              )
                      )
      )
    ) %>%
    filter(!is.na(valor))
  
  merge1
}

instrucoes_sas <- function(caminho) {
  leitura_con <- file( caminho , encoding = 'windows-1252' )
  
  z <- readLines( leitura_con ) %>% 
    stringr::str_replace_all( "\t" , " " )
  
  # remove lines containing the `if reg=__ then do;` pattern
  z <- z[ !grepl( 'if reg=.* then do;' , z ) ] %>% 
    stringr::str_replace_all( "@;" , "") %>% 
    stringr::str_replace_all( "/;" , "/")
  
  # remove lines containing solely `input`
  z <- z[ !( tolower( z ) == 'input' ) ]
  
  # remove the (SAScii-breaking) overlapping `controle` columns
  z[ !grepl( "@3 controle 6." , z , fixed = TRUE ) ]
}
 