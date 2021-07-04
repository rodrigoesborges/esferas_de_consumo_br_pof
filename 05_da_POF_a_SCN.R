##Dos dados de 04 com tradução para SCN 
#- baseado nos scripts gastos-esferas do repositório pofesferas

# Carrega tabelas que traduzem itens POF --> SCN

#acerta codificação para o restante do script
options( encoding = "utf8" )	
# Tabela de componentes hierarquizada cod68 x cod 20 - dicionário de tradução agregado
componentes <- read.csv("sourcedata/tradutores/cod68X20componentes-HIERARQ-e-isic-paraimportacao.csv", 
                        colClasses = c("item68x20" = "character",
                        "cod68" = "character","isic_prop_M"="character","isic_prop_M2"="character",
                        "isic_prop_M3"="character"), fileEncoding = "utf-8")

# Carrega tabela com códigos POF que não entram inicialmente como Consumo Final das Famílias
# Nomeadamente para 2009 (e 2018)
pofnaoconsumo <- read.csv("sourcedata/tradutores/codigos_semtradutor.csv", stringsAsFactors = FALSE, colClasses = c("x" = "character"))
#adiciona código fictício para códigos POF que não são traduzidos como itens de consumo final
# Código "98000" como Imposto
# Código "99000" como FBKF
imposto <- grepl(pattern = "IMPOSTO|TAXA|LICENÇA", x = pofnaoconsumo$desc)

pofnaoconsumo$cod685[imposto] <- "98000"
pofnaoconsumo$cod685[!imposto] <- "99000"
pofnaoconsumo$scn[imposto] <- "IMPOSTOS"
pofnaoconsumo$scn[!imposto] <- "FBKF"

names(pofnaoconsumo) <- c("codigo","Descrição POF",
                          "Produto Contas Nacionais", "Descrição Contas Nacionais")

#junta códigos ficticios para FBKX e Imposto na tabela componentes
fbkf.tax <- data.frame(c("9800","9900"),c("U","V"),c("Impostos","FBKF"),c("4","5"),c("999","999"),c("",""),c("",""), stringsAsFactors = FALSE)
names(fbkf.tax) <- names(componentes)
componentes <- rbind(componentes, fbkf.tax)



#2003
tradutor2003 <- read_excel(dir(recursive = TRUE)[grep(pattern = "Tradu_POF",
                                                      x = dir(recursive = TRUE))],
                           sheet = 1 , skip = 1, col_types = rep("text", 5))
# algumas recodificações e enxugamento dos dicionários de tradução
#tradutor$'Cod Pof' <- str_sub(tradutor$'Cod Pof' , 1 , 5) 
names(tradutor2003)[1] <- "codigo"
tradutor2003 <- tradutor2003[!duplicated(tradutor2003$codigo),]
#tradutor2003$codigo <- as.numeric(tradutor2003$codigo)
################# Em princípio na tabela tradutora POF 2003 não há itens sem tradutores ##

##################
#tradutor para nível hierarquizado compatível com nível 68 SCN e nível 20 (ISIC v4)
trad.agregado <- componentes%>%select(-"isic_prop_M2",-"isic_prop_M3")

trad.agregado[trad.agregado==""] <- NA
trad.agregado <- trad.agregado[complete.cases(trad.agregado),c(1,5)]

#merge dos gastos
gastos_SCN2003 <- left_join(gastos_expandidos%>%filter(ano==2003), tradutor2003)

gastos_SCN2003 <- gastos_SCN2003[complete.cases(gastos_SCN2003),]

gastos_SCN2003 <- gastos_SCN2003 %>% mutate(cod68 = substr(gastos_SCN2003$`CodAdapt` , 1 , 4))

gastos_SCN2003 <- left_join(gastos_SCN2003,trad.agregado)


esferas_setores2003 <- gastos_SCN2003%>%group_by(esfera,`Cod Contas 110`)%>%summarize(consumo=sum(valor*peso_final))

esferas_setores2003$ano <- 2003



tradutor2009 <- read_excel(dir(recursive = TRUE)[grep(pattern = "Tradutor_POF",x = dir(recursive = TRUE))],
                                            sheet = 1 , skip = 1)


# algumas recodificações e enxugamento dos dicionários de tradução
tradutor2009$'Produto POF' <- str_sub(tradutor2009$'Produto POF' , 1 , 5) 
names(tradutor2009)[1] <- "codigo"
tradutor2009 <- tradutor2009[!duplicated(tradutor2009$codigo),]



#adiciona códigos ao tradutor geral
tradutor2009 <- rbind(tradutor2009,pofnaoconsumo, stringsAsFactors = FALSE)


#merge dos gastos
gastos_SCN2009 <- left_join(gastos_expandidos%>%filter(ano==2009), tradutor2009)

gastos_SCN2009 <- gastos_SCN2009[complete.cases(gastos_SCN2009),]

gastos_SCN2009 <- gastos_SCN2009 %>% mutate(cod68 = substr(gastos_SCN2009$`Produto Contas Nacionais` , 1 , 4))

gastos_SCN2009 <- left_join(gastos_SCN2009,trad.agregado)

gastos_SCN2009$ano <- 2009

#Tradutor para 2018 identico ao de 2009
tradutor2018 <- tradutor2009

#merge dos gastos
gastos_SCN2018 <- left_join(gastos_expandidos%>%filter(ano==2018), tradutor2018)

gastos_SCN2018 <- gastos_SCN2018[complete.cases(gastos_SCN2018),]

gastos_SCN2018 <- gastos_SCN2018 %>% mutate(cod68 = substr(gastos_SCN2018$`Produto Contas Nacionais` , 1 , 4))

gastos_SCN2018 <- left_join(gastos_SCN2018,trad.agregado)

gastos_SCN2018$ano <- 2018



gastos_SCN <- bind_rows(gastos_SCN2003,gastos_SCN2009,gastos_SCN2018)


