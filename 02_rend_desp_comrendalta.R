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
  )%>%
  mutate(forma = ifelse(recmes>=4000 & forma == 1, 5, forma))
  

rendas_ucs2009 <- rendas_classificadas2009 %>%
#  filter(recmes>0)%>%
  dplyr::group_by(cod_uc, forma) %>%
  dplyr::summarise(renda = sum(recmes)) %>%
  tidyr::pivot_wider(names_from = forma, values_from = renda, 
              values_fill = list(renda = 0)) %>%
  select(1:4,13,5,7,12,6,9,10,14,11,8)%>%
  mutate(cv = `1`+`2`+`3`+`4`+`9`+`12`,
         `5` = ifelse(`5`<0 & abs(`5`)>5*cv,abs(`5`),0),
         baixa = `1`+`2`+`3`+`8`+`9`+`10`+`12`,
         mv = `5`+`6`+`7`+`8`+`10`+`11`+`13`) %>%
  dplyr::mutate(total = cv + mv,
         p_cv = (cv + 0.00001) / (total + 0.00001),
         p_baixa = min(1,(baixa+0.00001)/(total+0.00001))
         ) %>%
  dplyr::ungroup()


rendas_ucs2009$fracao <- sapply(max.col(rendas_ucs2009[2:14],ties.method = "last"),
                            function (x) {as.numeric(names(rendas_ucs[2:14])[x])})

# rendas_ucs2009 <- rendas_ucs2009 %>%
#   mutate(rendalta = total > quantile(total,0.8, names = F))


# # K-Means usadas para estabelecer corte alta/baixa
# k_rendas <- kmeans(rendas_ucs2009$p_cv, c(0.2, 0.5))
# k_rendas$centers
# 
# ggplot(rendas_ucs2009, aes(p_cv, total, col = factor(k_rendas$cluster))) +
#   geom_point(shape = ".", alpha = 0.5) +
#   geom_vline(xintercept = 0.6, lty = 2) +
#   scale_y_continuous(limits = c(0, 20e3)) +
#   theme_classic() +
#   theme(legend.position = "none")

# A análise acima propor o corte de 63% para esfera alta/baixa
corte <- 0.5

# Etapa 3 -----------------------------------------------------------
rendas_esferas2009 <- rendas_ucs2009 %>%
  mutate(esfera = ifelse(p_baixa > corte, "baixa", "alta"))

#para renda alta
#rendas_esferas2009 <- rendas_ucs2009 %>%
#  mutate(esfera = ifelse(!rendalta, "baixa", "alta"))

##Correção 0,2% dos mais ricos
rendas_esferas2009 %<>%mutate(maisrico = ntile(total,1000),
       total = ifelse(maisrico>998,2*total,total))%>%
  select(-maisrico)

##Correção de 5 + esfera baixa
rendas_esferas2009 %<>% 
  mutate(fracao = ifelse(esfera == "baixa" & fracao == 5,
                         8,fracao),
         ##Correção de outros "corner cases"
         fracao = ifelse(esfera == "baixa" & total == 0 & fracao ==11,
                         9,fracao),
         fracao = ifelse(total <= 232, 9, fracao),
         esfera = ifelse(total <= 232, "baixa", esfera),
         fracao = ifelse(fracao == 9 & total >=465, 2,fracao),
         fracao = ifelse(esfera == "alta" & fracao == 12, 13,fracao),
         esfera = ifelse(esfera == "alta" & fracao %in% c(1,2,3,8),"baixa",esfera),
         esfera = ifelse(esfera == "baixa" & fracao %in% c(11,6,7,13,4), "alta",
                         esfera),
         fracao = ifelse(fracao %in% c(3,8) & total > 6800, 5,fracao),
         fracao = ifelse(fracao %in% c(1,2) & total > 4000, 4,fracao),
         fracao = ifelse(fracao == 4 & total < 2500, 1, fracao ),
         esfera = ifelse(fracao == 1, "baixa",esfera),
         esfera = ifelse(fracao %in% c(4,5), "alta",esfera),
         fracao = ifelse(fracao == 11 & total < 1000, 10,fracao),
         fracao = ifelse(fracao == 10 & total > 6500, 11, fracao),
         esfera = ifelse(fracao == 11, "alta", esfera),
         esfera = ifelse(fracao == 10, "baixa", esfera),
         fracao = ifelse(fracao == 5 & total < 2500, 8,fracao),
         esfera = ifelse(fracao == 8,  "baixa",esfera),
         fracao = ifelse(fracao == 12 & total > 5000,13,fracao),
         esfera = ifelse(fracao == 13, "alta",esfera),
         ##Juntar "proletário" e "assalariados comércio / finanças"
         fracao = ifelse(fracao %in% c(2,3), 1, fracao)
  )


esferas_ucs2009 <- rendas_esferas2009 %>%
  select(cod_uc, esfera, p_cv,fracao,total)


rm2009 <- ggplot(esferas_ucs2009%>%left_join(dicfracoes),aes(factor(gsub(" ","\n",nomefracao)),total,fill=esfera))+
  geom_violin(draw_quantiles = 0.5)+
  scale_y_log10()+
  ggtitle("Distribuição de renda nas frações - 2009")+
  theme_minimal()+
  theme(axis.title.x=element_blank())


###Vamos adicionar "poupança" por UC

poupanca_ucs2009 <- rendas2009%>%filter(is.na(cod_novo))%>%
  group_by(cod_uc)%>%
  summarize(cod_uc=first(cod_uc),
            poupanca=sum(-1*recmes*as.numeric(recmes<0)))

poupanca_ucs2009 <- poupanca_ucs2009%>%
  full_join(despesas_esferas2009%>%left_join(pesos2009)%>%filter(grepl("^47",codigo))%>%group_by(cod_uc)%>%summarize(imovel=sum(valor/peso_final)))

poupanca_ucs2009[is.na(poupanca_ucs2009)] <-  0

poupanca_ucs2009$poupanca <- poupanca_ucs2009$poupanca+poupanca_ucs2009$imovel

poupanca_ucs2009 <- poupanca_ucs2009 %>% select(-imovel)



esferas_ucs2009 <- esferas_ucs2009%>%left_join(poupanca_ucs2009)

esferas_ucs2009[is.na(esferas_ucs2009$poupanca),"poupanca"] <- 0

ep2009 <- esferas_ucs2009%>% mutate(poupa=poupanca/(total+0.00001))%>%
  left_join(dicfracoes)%>%left_join(pesos2009)

ep2009%>%group_by(esfera,fracao)%>%summarize(poupanca_media=weighted.mean(poupa,peso_final))%>%left_join(dicfracoes)%>%mutate(poupanca_media = ifelse(fracao == 9,0,percent(poupanca_media,accuracy = 0.1)))%>%select(esfera,nomefracao,poupanca_media)





# Etapa 4 -----------------------------------------------------------
# Esse ano está sem os níveis, apenas código
despesas_esferas2009 <- ler_despesas2009() %>%
  left_join(esferas_ucs2009, by = "cod_uc") %>%
  select(cod_uc,codigo,esfera,fracao,p_cv,despmes,total)%>%
  # select(cod_uc, codigo, nivel_0:esfera)
  rename(valor = despmes)

# Estimativa geral esferas
t09 <- despesas_esferas2009 %>%
  group_by(esfera,fracao) %>%
  summarise(soma = sum(valor) * 12 / 1e6) %>% # Em bilhões
  filter(!is.na(esfera)) %>% 
  mutate(partic = soma / sum(soma))

t09$partic_total <- t09$soma/sum(t09$soma)
t09  <- t09 %>% left_join(dicfracoes) %>% select(-fracao,-partic)

# 
# despesas_esferas2009 %>% 
#   mutate(ano = 2009) %>% 
#   write_csv("gastos_esferas_2009.csv")

despesas_esferas2009 %>% 
  mutate(ano = 2009) %>%
  write_csv("gastos_esferas_2009_fracoes.csv")

gastos2009 <- despesas_esferas2009%>%
  filter(!is.na(cod_uc))%>%left_join(pesos2009)

g09 <- gastos2009%>%
  mutate(esfera = ifelse(is.na(esfera),"baixa",esfera))%>%
  group_by(cod_uc)%>%
  summarise(massa=sum(valor)*12/1e9,
            peso_final = first(peso_final),
            esfera = first(esfera),
            fracao = first(fracao),
            renda = first(total))%>%
  mutate(maisrico = ntile(massa,1000),
         massa = ifelse(maisrico>998,massa*2,massa))%>%
  mutate(fracao=ifelse(is.na(fracao),1,fracao))%>%
  group_by(esfera,fracao)%>%
  summarize(domicilios=sum(peso_final),
            massa_valor = sum(massa*peso_final),
            renda_total = sum(renda*peso_final)*12/1e9,
            renda_media = weighted.mean(renda,peso_final),
            media_gastos = weighted.mean(massa,peso_final)*1e9/12)%>%
  left_join(dicfracoes)

g09$participacao <- g09$massa_valor/sum(g09$massa_valor)
g09$prenda <- g09$renda_total/sum(g09$renda_total,na.rm = T)

g09 %>% group_by(esfera)%>% summarize(participacao=sum(participacao))

gh09 <- gastos2009%>%
  mutate(esfera=ifelse(is.na(esfera),"baixa",esfera))%>%
  group_by(cod_uc)%>%
  summarize(massa=sum(valor),
            peso_final=first(peso_final),
            esfera= first(esfera),
            fracao= first(fracao)) %>%
  mutate(maisrico = ntile(massa,1000),
         massa = ifelse(maisrico>998,massa*2,massa))%>%
  mutate(fracao=ifelse(is.na(fracao),1,fracao))%>%
  left_join(dicfracoes)