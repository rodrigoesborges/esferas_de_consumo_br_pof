#Análises de Gastos Setoriais e de Importação por Esfera
library(readODS)

gastos_SCN_m <- gastos_SCN%>%filter(isic_prop_M!=999)%>%
  left_join(consumos, by = c("ano","isic_prop_M" = "IndustryCode"))



###Vejamos a tese de Laura Carvalho

esferas_nos_n_traded <- gastos_SCN_m%>%group_by(ano,traded,esfera)%>%summarize(importado=sum(valor*peso_final*importado),
                                                       interno=sum(valor*peso_final*interno))


enntm <- esferas_nos_n_traded%>%select(-interno)%>%pivot_wider(names_from=esfera,values_from=importado)

enntm[3:4] <- prop.table(as.matrix(enntm[3:4]),1)

names(enntm)[2] <- "Setor comercializado"

e_n_tot <- gastos_SCN_m%>%group_by(ano,esfera)%>%summarize(importado=sum(valor*peso_final*importado),
                                                                  interno=sum(valor*peso_final*interno))

zzz <- e_n_tot %>% mutate(prop_m = importado/(importado+interno)) 
  


ennti <- esferas_nos_n_traded%>%select(-importado)%>%pivot_wider(names_from=esfera,values_from=interno)

ennti[3:4] <- prop.table(as.matrix(ennti[3:4]),1)

names(ennti)[2] <- "Setor comercializado"


gastos_setoriais <- gastos_SCN %>%
  group_by(ano, esfera, cod68) %>%
  summarise(valor = sum(valor * peso_final)/10^6, contasa = first(`Descrição Contas`), contasn = first(`Descrição Contas Nacionais`)) %>%
  pivot_wider(names_from = esfera, values_from = valor) %>%
  mutate(prop = alta / (alta + baixa), valor = alta+baixa) %>%
  split(.$ano) %>%
  map(arrange, desc(valor,prop))

gastos_setoriais <- rbindlist(gastos_setoriais)


write_ods(gastos_setoriais, "resultados/gastos_setoriais.ods")

gastos_setoriais <- read_ods("resultados/gastos_setoriais.ods")

setores68 <- gastos_setoriais%>%select(cod68,contas)%>% group_by(cod68)%>%summarize(cod68=first(cod68),contas=first(contas))


gastos_setoriais_t <- gastos_setoriais%>%
  select(-3)%>%left_join(setores68)%>%
  mutate(contas=paste0(cod68,"-",contas))%>%
  pivot_longer(-c(ano,contas,cod68),names_to = "indicador",values_to = "valor")


gastos_setoriais_alta <- gastos_setoriais_t%>%filter(indicador== "alta" & !(cod68 %in% c(9800,9900)))%>%
  pivot_wider(names_from=ano,values_from=valor,values_fn = {first})%>%arrange(contas)


gastos_setoriais_alta[is.na(gastos_setoriais_alta)] <- 0

gastos_setoriais_alta_p <- gastos_setoriais_alta[,2]%>%bind_cols(as_tibble(prop.table(as.matrix(gastos_setoriais_alta[,4:6]),2)))

write_ods(gastos_setoriais_alta,"resultados/gastos_setoriais_alta.ods")

write_ods(gastos_setoriais_alta_p,"resultados/gastos_setoriais_alta_p.ods")


prop_gastos_set <- gastos_setoriais_t%>%filter(indicador=="prop"& !(cod68 %in% c(9800,9900)))%>%
  pivot_wider(names_from=ano, values_from=valor,values_fn = {first})%>%arrange(contas)

write_ods(prop_gastos_set,"resultados/proporcoes_setoriais.ods")
