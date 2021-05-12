library(pof)
library(tidyverse)

# O script está montado para ter 4 partes: cada uma uma função
# As funções usadas estão no script 00_funcoes.R
# 1) Ler os dados de acordo com as intruções do IBGE
# 2) Classificar os rendimentos nas esferas alta/baixa
# A ideia é que só precisemos mexer nessa etapa
# 3) Identificar as UC nas esferas 
# baseado em corte vindo da análise dos dados (0.63)
# 4) Vincular os gastos das UCs com as esferas


# Etapa 0 ------------------ preparar repoditório -------------------
# downaload_pof(2018)
# unzip_pof(2018)
# 
## Tive que usar essa linha de baixo pq o IBGE mudou
## a forma de extrair os arquivos
# dir(glue::glue("dados/2018/Arquivos de dados"), 
#     full.names = TRUE) %>% 
#   file.rename(str_remove(., "Arquivos de dados/"))

# Etapa 1 -----------------------------------------------------------
rendas2009 <- ler_rendimentos2009()

# Etapa 2 -----------------------------------------------------------
rendas_classificadas2009 <- classificar_rendimentos2009(rendas2009,T)

rendas_classificadas2009$grupo_cod <- as.numeric(rendas_classificadas2009$grupo_cod)

rendas_classificadas2009 %<>% mutate(
  forma = case_when(
    is.na(grupo_cod) ~ forma,
    !is.na(grupo_cod) & forma %in% c(5,6,7) ~ forma,
    !is.na(grupo_cod) & !(forma %in% c(5,6,7)) ~ grupo_cod)
  )
  

rendas_ucs2009 <- rendas_classificadas2009 %>%
#  filter(recmes>0)%>%
  dplyr::group_by(cod_uc, forma) %>%
  dplyr::summarise(renda = sum(recmes)) %>%
  tidyr::pivot_wider(names_from = forma, values_from = renda, 
              values_fill = list(renda = 0)) %>%
  select(1:4,12,5,7,13,6,9,10,14,11,8,15)%>%
  mutate(cv = `1`+`2`+`3`+`4`+`9`+`12`,
         `5` = ifelse(`5`<0 ,ifelse(abs(`5`)>2*cv,abs(`5`),0),`5`),
         baixa = `1`+`2`+`3`+`8`+`9`+`10`+`12`,
         mv = `5`+`6`+`7`+`8`+`10`+`11`+`13`+`14`) %>%
  dplyr::mutate(total = cv + mv,
         p_cv = (cv + 0.00001) / (total + 0.00001),
         p_baixa = min(1,(baixa+0.00001)/(total+0.00001))
         ) %>%
  select(sort(tidyselect::peek_vars()))%>%
  select(c(1,7:14,2:6,15:21))%>%
  dplyr::ungroup()

rendas_ucs2009$max_baixa <- apply(rendas_ucs2009[baixa],1,max)

rendas_ucs2009$max_alta <- apply(rendas_ucs2009[alta],1,max)

#rendas_ucs2009$fracao <- sapply(max.col(rendas_ucs2009[1:14],ties.method = "first"),
 #                           function (x) {as.numeric(names(rendas_ucs2009[1:14])[x])})

rendas_ucs2009$fracao <- ifelse(rendas_ucs2009$p_baixa >= corte,
                            as.numeric(names(rendas_ucs2009[baixa])[max.col(rendas_ucs2009[baixa],ties.method = "first")]),
                            as.numeric(names(rendas_ucs2009[alta])[max.col(rendas_ucs2009[alta],ties.method = "first")])
)


rendas_ucs2009%<>%mutate(fracao = ifelse(total<=0.75*465,9,fracao),
                         fracao = ifelse(fracao == 3, 2, fracao))

rendas_ucs2009 <- separalbx(tabela=rendas_ucs2009,gr=10)

#para 10 e 11
rendas_ucs2009 <- separalbx(tabela=rendas_ucs2009,baixa=10,alta=11)
#para 12 e 13

rendas_ucs2009 <- separalbx(tabela=rendas_ucs2009,baixa=12,alta=13)

#para 4 e 1
rendas_ucs2009 <- separalbx(tabela=rendas_ucs2009,baixa=1,alta=4)

#para 4 e 2
rendas_ucs2009 <- separalbx(tabela=rendas_ucs2009,baixa=2,alta=4)

#para 14 e 6
rendas_ucs2009 <- separalbx(tabela=rendas_ucs2009,baixa=6,alta=14,cl=2)


rendas_ucs2009$esfera <- ifelse(rendas_ucs2009$fracao %in% baixa,"baixa","alta")

rendas_esferas2009 <- rendas_ucs2009
##Correção 0,2% dos mais ricos
rendas_esferas2009 %<>%mutate(maisrico = ntile(total,1000),
       total = ifelse(maisrico>998,2*total,total))%>%
  select(-maisrico)


esferas_ucs2009 <- rendas_esferas2009 %>%
  select(cod_uc, esfera, p_cv,fracao,total)




# Etapa 4 -----------------------------------------------------------
# Esse ano está sem os níveis, apenas código
despesas_esferas2009 <- ler_despesas2009() %>%
  left_join(esferas_ucs2009, by = "cod_uc") %>%
  select(cod_uc,codigo,esfera,fracao,p_cv,despmes,total)%>%
  # select(cod_uc, codigo, nivel_0:esfera)
  rename(valor = despmes)


despesas_esferas2009 %>% 
  mutate(ano = 2009) %>%
  write_csv("gastos_esferas_fracoes_2009.csv")

###Vamos adicionar "poupança" por UC

poupanca_ucs2009 <- rendas2009%>%filter(is.na(cod_novo))%>%
  group_by(cod_uc)%>%
  summarize(cod_uc=first(cod_uc),
            poupanca=sum(-1*recmes*as.numeric(recmes<0)))

poupanca_ucs2009 <- poupanca_ucs2009%>%
  full_join(despesas_esferas2009%>%left_join(pesos2009)%>%filter(grepl("^47",codigo))%>%
              group_by(cod_uc)%>%summarize(imovel=sum(valor/peso_final)))



poupanca_ucs2009$poupanca <- poupanca_ucs2009$poupanca+poupanca_ucs2009$imovel

poupanca_ucs2009 <- poupanca_ucs2009 %>% select(-imovel)

poupanca_ucs2009[is.na(poupanca_ucs2009)] <-  0

esferas_ucs2009 <- esferas_ucs2009%>%left_join(poupanca_ucs2009)

esferas_ucs2009[is.na(esferas_ucs2009$poupanca),"poupanca"] <- 0

ep2009 <- esferas_ucs2009%>% mutate(poupa=poupanca/(total+0.00001))%>%
  left_join(dicfracoes)%>%left_join(pesos2009)

ep2009%>%group_by(esfera,fracao)%>%summarize(poupanca_media=weighted.mean(poupa,peso_final))%>%left_join(dicfracoes)%>%mutate(poupanca_media = ifelse(fracao == 9,0,percent(poupanca_media,accuracy = 0.1)))%>%select(esfera,nomefracao,poupanca_media)



rm2009 <- ggplot(esferas_ucs2009%>%left_join(dicfracoes),aes(factor(gsub(" ","\n",nomefracao)),total,fill=esfera))+
  geom_violin(draw_quantiles = 0.5)+
  scale_y_log10(labels = comma_format(big.mark = ".",
                                      decimal.mark = ","))+
  xlim(l[8],l[1],l[2],l[3],l[10],l[9],l[4],l[14],l[11],l[6],l[13],l[5],l[12])+
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2)+
  ggtitle("Distribuição de renda nas frações - 2009")+
  theme_minimal()+
  theme(axis.title.x=element_blank())

