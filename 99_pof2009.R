library(tidyverse)

# Receitas para classificar moradias
morador <- readRDS("dados/2009/Dados/t_morador_s.rds") %>% 
  as_tibble()

domicilio <- readRDS("dados/2009/Dados/t_domicilio_s.rds") %>% 
  as_tibble()

rendimentos <- readRDS("dados/2009/Dados/t_rendimentos_s.rds") %>% 
  as_tibble()

outros_reci <- readRDS("dados/2009/Dados/t_outros_reci_s.rds") %>% 
  as_tibble()

poststr <- readxl::read_excel(
  "dados/2009/documentacao/Pos_estratos_totais.xls"
)


incomeRecodes <- read_csv2("codigos-recodificacao-rendimentos.csv") %>% 
  mutate(cod.inc = gsub(".*\\+.*", "50000", cod.inc))

componentes <- incomeRecodes %>% 
  set_names(c("cod_novo","tipoderendimento","cod_rec"))

incomeRecodesX <- incomeRecodes$numero %>% 
  map2_df(incomeRecodes$cod.inc, ~{
    cmd <- paste("c(", .y,")")
    tibble(cod_novo = .x, 
           cod_rec = as.character(eval(parse(text = cmd))))
  })

rendimentos_recoded <- rendimentos %>% 
  mutate(
    recmes = ( valor_anual_expandido2 / fator_expansao2 ) / 12,
    cod_uc = paste0( cod_uf, num_seq, num_dv, cod_domc, num_uc ),
    cod_rec = paste0( num_quadro, substr(cod_item,1,3))
  ) %>% 
  left_join(incomeRecodesX, by = "cod_rec") %>% 
  select(cod_rec, cod_uc, recmes, fator_expansao1, fator_expansao2, cod_novo)


outros_reci_recoded <- outros_reci %>% 
  mutate(
    recmes = ( valor_anual_expandido2 / fator_expansao2 ) / 12,
    cod_uc = paste0( cod_uf, num_seq, num_dv, cod_domc, num_uc ),
    cod_rec = paste0( num_quadro, substr(cod_item,1,3))
  ) %>% 
  left_join(incomeRecodesX, by = "cod_rec") %>% 
  select(cod_rec, cod_uc, recmes, fator_expansao1, fator_expansao2, cod_novo)

allincomes <- rbind(rendimentos_recoded, outros_reci_recoded)

domicilio_rendas <- morador %>% 
  mutate(cod_uc = paste0( cod_uf, num_seq, num_dv, cod_domc, num_uc )) %>% 
  select(cod_uc, renda_total) %>% 
  unique()

todos_subcodigos_tiporenda <- componentes %>% 
  filter(substring(cod_novo, 1, 1) == 1) %>% 
  pull('cod_novo')

domicilios_porcodigo <- allincomes %>% 
  filter(cod_novo %in% todos_subcodigos_tiporenda) %>% 
  select(cod_novo, recmes, cod_uc)

domicilios_porcodigo_agregados <- domicilios_porcodigo %>% 
  group_by(cod_uc, cod_novo) %>% 
  summarise(recmes = sum(recmes))

subst_na <-  function(x) {
  x[is.na(x)] <- 0
  x
}

renda_m_total <- domicilios_porcodigo_agregados %>% 
  group_by(cod_uc) %>%
  summarise(cod_novo = 1,
            recmes = sum(recmes, na.rm = TRUE)) %>% 
  bind_rows(domicilios_porcodigo_agregados) %>% 
  spread(key = cod_novo, value = recmes) %>% 
  map_dfc(subst_na)

# Aqui a gente define rendas do trabalho, etc
renda_m <- renda_m_total %>% 
  mutate(renda_trabalho = ((`111` + `113` +
                              `121` + `124` + 0.01 ) / 
                             (`1`+ 0.01)) * 100) %>% 
  select(cod_uc, renda_trabalho)

domicilios_trabalhadores <- domicilio_rendas %>% 
  left_join(renda_m, by = "cod_uc")

# Aqui os NAs com baixa renda foram definidos como trabalhadores
# De onde a gente tirou esses 2.000 mil reais?
domicilios_trabalhadores[is.na(domicilios_trabalhadores$renda_trabalho) &
                           domicilios_trabalhadores$renda_total <= 2000, "renda_trabalho"] <- 100

domicilios_trabalhadores[is.na(domicilios_trabalhadores$renda_trabalho) &
                           domicilios_trabalhadores$renda_total > 2000, "renda_trabalho"] <- 0

# Aqui é estabelecido o critéiro de corte
domicilios_trabalhadores <- domicilios_trabalhadores %>% 
  mutate(trabalhador.cat = cut(
        renda_trabalho, c( 0, 60, Inf ),
        include.lowest = TRUE, 
        labels = c( "Não Trabalhador", "Trabalhador" )
      ),
    
    control = substr(cod_uc, 1, 6 )
  )

# Gastos
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

totais_despesas <- bind_rows(
  despesas_mensais_col,
  despesas_mensais_ind,
  despesas_90,
  despesas_veic,
  despesas_12m
)

despesas_trabalhadores <- totais_despesas %>% 
  left_join(domicilios_trabalhadores %>% select(cod_uc, trabalhador.cat))

despesas_trabalhadores %>% 
  group_by(grupo = trabalhador.cat, cod_uc) %>% 
  summarise(soma = sum(despmes)) %>% 
  ggplot(aes(grupo , soma, col = grupo)) +
  # geom_density() +
  geom_violin() + 
  scale_y_continuous(limits = c(0, 15e3))
  group_by(grupo) %>% 
  summarise(massa = sum(soma), 
            media = mean(soma),
            mediana = median(soma))

# Todo resto abaixo é detalhamento

# # algumas recodificações e enxugamento dos dicionários de tradução
# tradutor <- tradutor %>% 
#   mutate(codigo = str_sub(`Produto POF` , 1 , 5) ) %>% 
#   filter(!duplicated(codigo))
# 
# #adiciona código fictício para códigos POF que não são traduzidos como itens de consumo final
# # Código "98000" como Imposto
# # Código "99000" como FBKF
# imposto <- grepl(pattern = "IMPOSTO|TAXA|LICENÇA", x = pofnaoconsumo$desc)
# pofnaoconsumo$cod685[imposto] <- "98000"
# pofnaoconsumo$cod685[!imposto] <- "99000"
# pofnaoconsumo$scn[imposto] <- "IMPOSTOS"
# pofnaoconsumo$scn[!imposto] <- "FBKF"
# 
# names(pofnaoconsumo) <- c("codigo","Descrição POF", "Produto Contas Nacionais", 
#                           "Descrição Contas Nacionais")
# 
# #adiciona códigos ao tradutor geral
# tradutor <- rbind(tradutor, pofnaoconsumo, stringsAsFactors = FALSE)
# 
# #junta códigos ficticios para FBKX e Imposto na tabela componentes
# fbkf.tax <- data.frame(c("9800","9900"),c("U","V"),c("Impostos","FBKF"),c("4","5"), stringsAsFactors = FALSE)
# names(fbkf.tax) <- names(componentes)
# componentes <- rbind(componentes, fbkf.tax)
# 
# #tradutor para nível hierarquizado compatível com nível 68 SCN e nível 20 (ISIC v4)
# trad.agregado <- componentes
# trad.agregado[trad.agregado==""] <- NA
# trad.agregado <- trad.agregado[complete.cases(trad.agregado),c(1,4)]
# 
# #merge dos gastos
# gastos_SCN <- totais_despesas %>% 
#   left_join(tradutor) %>% 
#   filter(!is.na(`Produto POF`)) %>% 
#   mutate(cod68 = substr(`Produto Contas Nacionais` , 1 , 4)) %>% 
#   left_join(trad.agregado)
# 
# # Se quiser exportar o objeto com "microdados por família e tipo de despesa compatível com SCN"
# # saveRDS(gastos_SCN,"RDS/microdados_despesas.rds")
# 
# 
# ###################### Função que gera as estimativas para cada item - semi adaptado ------------
# curCode.plus.subcodes <- componentes[
# 		  substring(componentes$item68x20,1,nchar(curCode)) == curCode,'item68x20'
# 		] 
# 
# 		family.expenditures.by.code <- gastos_SCN[
# 		  gastos_SCN$item68x20 %in% curCode.plus.subcodes , c( 'item68x20' , 'despmes' , 'cod_uc' ) 
# 		]
# 
# 		family.level.spending <- family.expenditures.by.code %>% 
# 		  group_by(cod_uc) %>% 
# 		  summarise(despmes = sum(despmes))
# 
# 		y <- left_join(family.level.income, family.level.spending)
# 
# 		y[ is.na( y$despmes ) , 'despmes' ] <- 0
# 
# cesta_esferas <- function(curCode, family.level.income = domicilios_trabalhadores,
#                           gastos_SCN = gastos_SCN, componentes = componentes, 
#                           poststr = poststr){
# 
# 		curCode.plus.subcodes <- componentes[
# 		  substring(componentes$item68x20,1,nchar(curCode)) == curCode,'item68x20'
# 		] 
# 
# 		family.expenditures.by.code <- gastos_SCN[
# 		  gastos_SCN$item68x20 %in% curCode.plus.subcodes , c( 'item68x20' , 'despmes' , 'cod_uc' ) 
# 		]
# 
# 		family.level.spending <- family.expenditures.by.code %>% 
# 		  group_by(cod_uc) %>% 
# 		  summarise(despmes = sum(despmes))
# 
# 		y <- left_join(family.level.income, family.level.spending)
# 
# 		y[ is.na( y$despmes ) , 'despmes' ] <- 0
# 		
# 		z <- y %>% 
# 			left_join( 
# 				poststr %>% 
# 				  select(control, estrato_unico, fator_des, pos_estrato, tot_unidade_c) %>% 
# 				  mutate(control = as.character(control))
# 			)
# 
# 		stopifnot( nrow( z ) == nrow( y ) )
# 
# 		sample.pof <- survey::svydesign(
# 				id = ~control , 
# 				strata = ~estrato_unico , 
# 				weights = ~fator_des ,
# 				data = z , 
# 				nest = TRUE
# 			)
# 			
# 		uc.totals <- data.frame(
# 			  pos_estrato = unique( z$pos_estrato ) , 
# 				Freq = unique( z$tot_unidade_c )
# 			)
# 		
# 		pof.design <- survey::postStratify(sample.pof , ~pos_estrato , uc.totals)
# 
# 		st <- survey::svymean( ~despmes , pof.design )
# 		
# 		sb <- survey::svyby(
# 				~despmes , 
# 				~trabalhador.cat , 
# 				pof.design , 
# 				survey::svymean
# 			)
# 			
# 		ot <- data.frame( 
# 				trabalhador.cat = 'Total' , 
# 				mean = coef( st ) , 
# 				se = as.numeric( SE( st ) ) , 
# 				cv = as.numeric( cv( st ) )
# 			)
# 		
# 		ob <- data.frame( 
# 				trabalhador.cat = sb$trabalhador.cat , 
# 				mean = coef( sb ) , 
# 				se = as.numeric( SE( sb ) ) , 
# 				cv = as.numeric( cv( sb ) )
# 			)
# 		
# 		w <- rbind( ot , ob )
# 		
# 		w$item68x20 <- curCode
# 		
# 		reshape( 
# 			w , 
# 			idvar = 'item68x20' ,
# 			timevar = 'trabalhador.cat' ,
# 			direction = 'wide'
# 		)
# 	}
# 
# 	
# #### Gera a tabela final efetiva --------
# tabela <- data.frame( num.despesa = NULL )
# # Ver Itens SCN sem registro de Gasto
# tabelar <- componentes[!(componentes$item68x20 %in% gastos_SCN$item68x20),]
# 
# for ( i in seq( nrow( componentes ) ) ){
# 
#   if ( !(componentes[i , 'item68x20'] %in% tabelar$item68x20)) {
#     if ( !( componentes[ i , 'item68x20' ] %in% tabela$tipo.de.despesa) ){
# 		
# 			tabela[ nrow( tabela ) + 1 , 'num.despesa' ] <- 
# 				componentes[ i , 'item68x20' ]
# 				
# 			tabela[ nrow( tabela ) , 'setor' ] <- 
# 				componentes[ i , 'descrição' ]
# 		
#     }
#   }
# }
# 
# for ( i in seq( nrow( tabela ) ) ) {
# 	
# 	print( tabela[ i , 'num.despesa' ] )
# 	
# 	curRow <- cesta_esferas( 
# 			tabela[ i , 'num.despesa' ] , 
# 			domicilios_trabalhadores ,
# 			gastos_SCN , componentes , poststr 
# 		)
# 		
# 	if ( i == 1 ) allRows <- curRow else allRows <- rbind( allRows , curRow )
# 	
# }
# 
# res_cesta_esferas <- merge( componentes , allRows, all.x = TRUE )
# 
# 
