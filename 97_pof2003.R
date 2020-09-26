library(tidyverse)

morador <- readRDS("dados/2003/t_morador.rds") %>% 
  as_tibble()

domicilio <- readRDS( "dados/2003/t_domicilio.rds" ) %>% 
  as_tibble()

rendimentos <- readRDS("dados/2003/t_rendimentos.rds") %>% 
  as_tibble()

outros_reci <- readRDS("dados/2003/t_outros_reci.rds") %>% 
  as_tibble()

# use old translator for generating same table as 2008-09 script
tabelagregada <- "https://raw.githubusercontent.com/rodrigoesborges/pofesferas/master/tradutores/codigos-recodificacao-rendimentos.csv" %>% 
  read_csv2() %>% 
  set_names(c("cod_novo","tipoderendimento","cod_rec"))

# Do some recodes
t_domicilio <- domicilio %>% 
  mutate(estrato_unico = uf*100 + estrato)

###############################
# trocado por tradutor de 2003 #
###############################

incomeRecodesX <- read_csv("https://raw.githubusercontent.com/rodrigoesborges/pofesferas/master/tradutores/tradutor-detalhado2003-reag.csv") %>% 
  rename(cod_novo = cod_novo, cod_rec = cod.rec)


t_rendimentos <- rendimentos %>% 
  mutate(recmes = rend_def_anual / deflator / 12 ,
         cod_uc = paste0( uf , seq , dv , domcl , uc ),
         cod_rec = quadro * 1000 + pos_ocup
  )


t_rendimentos_recoded <- t_rendimentos %>% 
  left_join(incomeRecodesX, by = "cod_rec") %>% 
  select(cod_rec, cod_uc, recmes, fator_set, fator, cod_novo)


t_outros_reci <- outros_reci %>% 
  mutate(
    recmes = rend_def_anual / deflator / 12 ,
    cod_uc = paste0( uf , seq , dv , domcl , uc ),
    cod_rec = quadro*1000+ floor(item/100)
  )

t_outros_reci_recoded <- t_outros_reci %>% 
  left_join(incomeRecodesX, by = "cod_rec") %>% 
  select(cod_rec, cod_uc, recmes, fator_set, fator, cod_novo )

allincomes <- rbind(t_rendimentos_recoded, t_outros_reci_recoded)

t_morador <- morador %>% 
  mutate(cod_uc = paste0( uf , seq , dv , domcl , uc ) )

domicilio_rendas <- t_morador[ , c( 'cod_uc' , 'renda' ) ] %>% 
  unique()

componentes <- incomeRecodesX
names(componentes) <- c("cod_novo","tipoderendimento","cod_rec")

# Now we generate the vector that will be used as base for our aggregation criteria
# First we take (in fixed form) all items / subcodes
todos.subcodigos.tiporenda <- componentes %>% 
  filter(substring(cod_novo,1, 1) == 1) %>% 
  select(cod_novo)

# next we aggregate by family/household the recoded income data
domicilios.porcodigo <- allincomes %>% 
  filter(cod_novo %in% todos.subcodigos.tiporenda$cod_novo) %>% 
  select(cod_novo, recmes, cod_uc)

domicilios.porcodigo.agregados <- domicilios.porcodigo %>% 
  group_by(cod_uc, cod_novo) %>% 
  summarise(recmes = sum(recmes))
  
# We've got all we need to actually generate the vector, proceed to it
renda_m_total <- domicilios.porcodigo.agregados %>% 
  group_by(cod_uc) %>%
  summarise(cod_novo = "1",
            recmes = sum(recmes, na.rm = TRUE)) %>% 
  bind_rows(domicilios.porcodigo.agregados) %>% 
  spread(key = cod_novo, value = recmes) %>% 
  map_dfc(subst_na)

renda_m <- renda_m_total %>%
  mutate(renda_trabalho = ((`1.1.1` + `1.1.3` +
                              `1.2.6` + `1.2.4` + 0.01 )/(`1`+ 0.01))*100)

domicilios_trabalhadores <- domicilio_rendas %>% 
  left_join(renda_m[, c("cod_uc", "renda_trabalho")], by = "cod_uc")

# Aqui os NAs com baixa renda foram definidos como trabalhadores
domicilios_trabalhadores[is.na(domicilios_trabalhadores$renda_trabalho) &
                           domicilios_trabalhadores$renda_total <= 2000, "renda_trabalho"] <- 100

domicilios_trabalhadores[is.na(domicilios_trabalhadores$renda_trabalho) &
                           domicilios_trabalhadores$renda_total > 2000, "renda_trabalho"] <- 0


domicilios_trabalhadores <- domicilios_trabalhadores %>% 
  mutate(
    trabalhador.cat = cut(renda_trabalho, c( 0 , 80, Inf ) ,
                          include.lowest = TRUE , 
                          labels = c( "Não Trabalhador" , "Trabalhador" )
    ),
    control = substr( cod_uc , 1 , 6 )
  )

# Essa opção é importante para permitir a existência de estratos únicos
options(survey.lonely.psu = "adjust")

tabela_2.1.1 <- function(incomeCode, 
                         domicilios_trabalhadores = domicilios_trabalhadores ,
                         allincomes = allincomes ,
                         componentes = componentes
                         #, poststr = poststr
                         ) {
  incomeCode.plus.subcodes <- componentes %>% 
    filter(substring(cod_novo, 1, nchar(incomeCode)) == incomeCode) %>%
    pull(cod_novo)
  
  family.incomes.by.code <- allincomes %>% 
    filter(cod_novo %in% incomeCode.plus.subcodes) %>% 
    select(c(cod_novo, recmes, cod_uc, fator))
  if(nrow(family.incomes.by.code) == 0) {
    family.incomes.by.code <- data.frame(cod_novo = incomeCode ,
                                         recmes = 0,
                                         cod_uc = 0,
                                         fator = 0)
  }
  
  # aggregate incomes to the one-record-per-family-level
  family.level.income.aggregated <- aggregate(recmes ~ cod_uc , 
                                              family.incomes.by.code , sum )
  
  # merge the income and familiar income tables,
  # assuming that the income table has no missings
  y <- merge( domicilios_trabalhadores , family.level.income.aggregated , all.x = TRUE )
  
  # all missing values from the left-join above
  # should be converted to zeroes
  y[ is.na( y$recmes ) , 'recmes' ] <- 0
  
  
  # merge on necessary post-stratification variables..
  z <- left_join(y, unique(allincomes[ , c( 'cod_uc' , 'fator') ]), by = "cod_uc")
  
  z <- z[is.na(z$fator) == FALSE, ]
  
  # ..and confirm no record-loss
  # stopifnot( nrow( z ) == nrow( y ) )
  
  # construct the preliminary survey object
  # (not yet post-stratified)
  sample.pof <- survey::svydesign(
      id = ~control , 
      # strata = ~estrato_unico , 
      weights = ~fator ,
      data = z , 
      nest = TRUE
    )
  
  # construct the target population table
    # uc.totals <- 
    #   data.frame(
    #     pos_estrato = unique( z$pos_estrato ) , 
    #     Freq = unique( z$tot_unidade_c )
    #   )
    
    # construct the final post-stratified survey object
    pof.design <- sample.pof
    # survey::postStratify(
    #   sample.pof , 
    #   ~pos_estrato , 
    #   uc.totals
    # )
    
    # take the overall mean..
    st <- survey::svymean( ~recmes , pof.design )
    
    # ..and the mean, broken down by income categories
    sb <- survey::svyby(
        ~recmes , 
        ~trabalhador.cat , 
        pof.design , 
        survey::svymean
      )
    
    # make a single-row data.frame for the total..
    ot <-
      data.frame( 
        trabalhador.cat = 'Total' , 
        mean = coef( st ) , 
        se = as.numeric( survey::SE( st ) ) , 
        cv = as.numeric( survey::cv( st ) )
      )
    
    # ..and a multi-row data.frame for the breakouts
    ob <-
      data.frame( 
        trabalhador.cat = sb$trabalhador.cat , 
        mean = coef( sb ) , 
        se = as.numeric( survey::SE( sb ) ) , 
        cv = as.numeric( survey::cv( sb ) )
      )
    
    # stack them
    w <- rbind( ot , ob )
    
    # throw on the current income type code
    w$cod_novo <- incomeCode
    
    # finish up with a single row of data,
    # stretched out into `wide` format
    reshape(w , 
            idvar = 'cod_novo' ,
            timevar = 'trabalhador.cat' ,
            direction = 'wide'
    )
  }

# proper table replication ---------------------------------------------------------------------
tabela <- tabelagregada %>% 
  select(tipo.de.rendimento = tipoderendimento, cod_novo) %>% 
  unique()

allRows <- map_df(tabela$cod_novo, 
       tabela_2.1.1, 
       domicilios_trabalhadores,
       allincomes , 
       componentes)

# merge on the descriptions
result_2.1.1 <- left_join( tabela, allRows, by = "cod_novo")

# Gastos
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

totais_despesas <- bind_rows(
  despesas_mensais_col,
  despesas_mensais_ind,
  despesas_90,
  despesas_veic,
  despesas_12m,
  despesas_esp
)

# algumas recodificações e enxugamento dos dicionários de tradução
#tradutor$'Cod Pof' <- str_sub(tradutor$'Cod Pof' , 1 , 5) 
tradutor <- tradutor %>% 
  rename(codigo = 1) %>% 
  filter(!duplicated(codigo)) %>% 
  mutate(codigo = as.numeric(codigo))

warning("Não entendi o que rolou aqui. Código não roda")
warning("Temos que entender melhor o que queriamos aqui, tá meio jogado")
# trad.agregado <- tradutor
trad.agregado <- componentes
# trad.agregado[trad.agregado=="", ] <- NA
trad.agregado <- trad.agregado[complete.cases(trad.agregado), c(1,4)]

#merge dos gastos
gastos_SCN <- totais_despesas %>% 
  left_join(tradutor, by = "codigo") %>% 
  filter(complete.cases(.)) %>% 
  mutate(cod68 = substr(CodAdapt , 1 , 4)) %>% 
  left_join(trad.agregado, by = "cod68") 

###################### Função que gera as estimativas para cada item - semi adaptado ------------
cesta_esferas <- function(curCode ,
                          family.level.income = domicilios_trabalhadores ,
                          gastos_SCN = gastos_SCN ,
                          componentes = componentes 
                          #poststr = poststr
                          ) {
    curCode.plus.subcodes <- componentes %>% 
      filter(substring(item68x20, 1, nchar(curCode)) == curCode) %>% 
      pull(item68x20) 
    
    family.expenditures.by.code <- gastos_SCN %>% 
      filter(item68x20 %in% curCode.plus.subcodes) %>% 
      select(item68x20, despmes, cod_uc)
    
    family.level.spending <- family.expenditures.by.code %>% 
      group_by(cod_uc) %>% 
      summarise(despmes = sum(despmes))
    
    y <- family.level.income %>% 
      left_join(family.level.spending)
    
    y[ is.na( y$despmes ) , 'despmes' ] <- 0
    
    z <- y %>% 
      left_join(
        unique(select(totais_despesas, cod_uc, fator)), 
        by = "cod_uc"
      ) %>% 
      filter(!is.na(fator))
    
    sample.pof <- survey::svydesign(
        id = ~control , 
        #strata = ~estrato_unico , 
        weights = ~fator ,
        data = z , 
        nest = TRUE
      )
    
    #  uc.totals <- 
    #    data.frame(
    #      pos_estrato = unique( z$pos_estrato ) , 
    #      Freq = unique( z$tot_unidade_c )
    #    )
    
    warning("pensar por trocamos `postStratify` por `sample.pof`.")
    pof.design <- sample.pof
    # postStratify(
    #   sample.pof , 
    #   ~pos_estrato , 
    #   uc.totals
    # )
    # 
    st <- survey::svymean( ~despmes , pof.design )
    
    sb <- survey::svyby(
        ~despmes , 
        ~trabalhador.cat , 
        pof.design , 
        survey::svymean
      )
    
    ot <-
      data.frame( 
        trabalhador.cat = 'Total' , 
        mean = coef( st ) , 
        se = as.numeric( survey::SE( st ) ) , 
        cv = as.numeric( survey::cv( st ) )
      )
    
    ob <-
      data.frame( 
        trabalhador.cat = sb$trabalhador.cat , 
        mean = coef( sb ) , 
        se = as.numeric( survey::SE( sb ) ) , 
        cv = as.numeric( survey::cv( sb ) )
      )
    
    w <- rbind( ot , ob )
    
    w$item68x20 <- curCode
    
    reshape( 
      w , 
      idvar = 'item68x20' ,
      timevar = 'trabalhador.cat' ,
      direction = 'wide'
    )
  }

#### Gera a tabela final efetiva --------
# Ver Itens SCN sem registro de Gasto
tabelar <- componentes %>% 
  filter(!item68x20 %in% gastos_SCN$item68x20)

tabela <- tibble(
  num.despesa = ifelse(!componentes$item68x20 %in% tabelar$item68x20, 
         componentes$item68x20, NA),
  setor = ifelse(!componentes$item68x20 %in% tabelar$item68x20, 
         componentes$descrição, NA)
) %>% filter(!is.na(setor))

allRows <- map_df(tabela$num.despesa, 
                  cesta_esferas,
                  domicilios_trabalhadores ,
                  gastos_SCN , componentes #, poststr 
)

res_cesta_esferas <- left_join( componentes , allRows)
