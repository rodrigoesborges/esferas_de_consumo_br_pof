library(pof)
library(tidyverse)
library(readxl)

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
rendas2003 <- ler_rendimentos2003()

# Etapa 2 -----------------------------------------------------------
rendas_classificadas2003 <- classificar_rendimentos2003(rendas2003,T)

rendas_classificadas2003$grupo_cod <- as.numeric(rendas_classificadas2003$grupo_cod)

rendas_classificadas2003 %<>% mutate(
  forma = case_when(
    is.na(grupo_cod) ~ forma,
    !is.na(grupo_cod) & forma %in% c(5,6,7) ~ forma,
    !is.na(grupo_cod) & !(forma %in% c(5,6,7)) ~ grupo_cod)
  )%>%
  mutate(forma = ifelse(recmes>=5000 & forma == 1, 5,forma))



rendas_ucs2003 <- rendas_classificadas2003 %>%
  group_by(cod_uc, forma) %>%
  summarise(renda = sum(recmes)) %>%
  pivot_wider(names_from = forma, values_from = renda, 
              values_fill = list(renda = 0))  %>%
  select(cod_uc,sort(current_vars()))%>%
  select(1:2,7:14,3:6)%>%
  mutate(cv = `1`+`3`+`4`+`9`+`12`, 
         `5` = ifelse(`5`<0 & abs(`5`)>5*cv,abs(`5`),0),
         baixa = `1`+`2`+`3`+`8`+`9`+`10`+`12`,
         mv = `2`+`5`+`6`+`7`+`8`+`10`+`11`+`13`)%>%
  mutate(total = cv + mv,
         p_cv = (cv+0.00001)/ (total+0.00001),
         p_baixa = min(1,(baixa+0.00001)/(total+0.00001))) %>%
  ungroup()

rendas_ucs2003$fracao <- sapply(max.col(rendas_ucs2003[2:14],ties.method = "last"),
                                function (x) {as.numeric(names(rendas_ucs2003[2:14])[x])})



# rendas_ucs2003 <- rendas_ucs2003 %>%
#   mutate(rendalta = total > quantile(total,0.8, names = F))

# K-Means usadas para estabelecer corte alta/baixa
k_rendas <- kmeans(rendas_ucs2003$p_cv, c(0.2, 0.5))
k_rendas$centers

ggplot(rendas_ucs2003, aes(p_cv, total, col = factor(k_rendas$cluster))) +
  geom_point(shape = ".", alpha = 0.5) +
  geom_vline(xintercept = 0.54, lty = 2) +
  scale_y_continuous(limits = c(0, 20e3)) +
  theme_classic() +
  theme(legend.position = "none")

# A análise acima propor o corte de 66% para esfera alta/baixa
corte <- 0.50

# Etapa 3 -----------------------------------------------------------
rendas_esferas2003 <- rendas_ucs2003 %>%
  mutate(esfera = ifelse(p_baixa >= corte, "baixa", "alta"))


##Correção 0,2% mais rico
rendas_esferas2003 %<>% mutate(maisrico = ntile(total,1000),
       total = ifelse(maisrico>998,2*total,total))

##Correção de 5 + esfera baixa
rendas_esferas2003 %<>% 
  mutate(fracao = ifelse(esfera == "baixa" & fracao == 5,
                                              8,fracao),
         ##Correção de outros "corner cases"
         fracao = ifelse(esfera == "baixa" & total == 0 & fracao ==11,
                         9,fracao),
         fracao = ifelse(total <= 100, 9, fracao),
         esfera = ifelse(total <= 100, "baixa", esfera),
         fracao = ifelse(fracao == 9 & total >=200, 2,fracao),
         fracao = ifelse(esfera == "alta" & fracao == 12, 13,fracao),
         esfera = ifelse(esfera == "alta" & fracao %in% c(1,2,3,8),"baixa",esfera),
         esfera = ifelse(esfera == "baixa" & fracao %in% c(11,6,7,13,4), "alta",
                         esfera),
         fracao = ifelse(fracao %in% c(3,8) & total > 5000, 5,fracao),
         fracao = ifelse(fracao %in% c(1,2) & total > 3000, 4,fracao),
         fracao = ifelse(fracao == 4 & total < 1000, 1, fracao ),
         esfera = ifelse(fracao == 1, "baixa",esfera),
         esfera = ifelse(fracao %in% c(4,5), "alta",esfera),
         fracao = ifelse(fracao == 11 & total < 600, 10,fracao),
         fracao = ifelse(fracao == 10 & total > 2500, 11, fracao),
         esfera = ifelse(fracao == 11, "alta", esfera),        
         esfera = ifelse(fracao == 10, "baixa", esfera),
         esfera = ifelse(fracao == 10, "baixa", esfera),
         fracao = ifelse(fracao == 5 & total < 1000, 8,fracao),
         esfera = ifelse(fracao == 8,  "baixa",esfera),
         fracao = ifelse(fracao == 12 & total > 3000,13,fracao),
         esfera = ifelse(fracao == 13, "alta",esfera),
         ##Juntar "proletário" e "assalariados comércio / finanças"
         fracao = ifelse(fracao %in% c(2,3), 1, fracao)
         
         
  )






# #para renda alta
# rendas_esferas2003 <- rendas_ucs2003 %>%
#   mutate(esfera = ifelse(!rendalta, "baixa", "alta"))



esferas_ucs2003 <- rendas_esferas2003 %>%
  select(cod_uc, esfera,p_cv,fracao,total)

rm2003 <- ggplot(esferas_ucs2003%>%left_join(dicfracoes),aes(factor(gsub(" ","\n",nomefracao)),total,fill=esfera))+geom_violin(draw_quantiles = 0.5)+scale_y_log10()+
  ggtitle("Distribuição de renda nas frações - 2003")+
  theme_minimal()+
  theme(axis.title.x=element_blank())




###Vamos adicionar "poupança" por UC

poupanca_ucs2003 <- rendas2003%>%filter(is.na(cod_novo))%>%
  group_by(cod_uc)%>%
  summarize(cod_uc=first(cod_uc),
            poupanca=sum(-1*recmes*as.numeric(recmes<0)))

poupanca_ucs2003 <- poupanca_ucs2003%>%
  full_join(despesas_esferas2003%>%left_join(pesos2003)%>%filter(grepl("^47",codigo))%>%group_by(cod_uc)%>%summarize(imovel=sum(valor/peso_final)))

poupanca_ucs2003[is.na(poupanca_ucs2003)] <-  0

poupanca_ucs2003$poupanca <- poupanca_ucs2003$poupanca+poupanca_ucs2003$imovel

poupanca_ucs2003 <- poupanca_ucs2003 %>% select(-imovel)



esferas_ucs2003 <- esferas_ucs2003%>%left_join(poupanca_ucs2003)

esferas_ucs2003[is.na(esferas_ucs2003$poupanca),"poupanca"] <- 0

ep2003 <- esferas_ucs2003%>% mutate(poupa=poupanca/(total+0.00001))%>%
  left_join(dicfracoes)%>%left_join(pesos2003)

ep2003%>%group_by(esfera,fracao)%>%summarize(poupanca_media=weighted.mean(poupa,peso_final))%>%left_join(dicfracoes)%>%mutate(poupanca_media = ifelse(fracao == 9,0,percent(poupanca_media,accuracy = 0.1)))%>%select(esfera,nomefracao,poupanca_media)

ep2003%>%filter(fracao !=9)%>%group_by(esfera)%>%summarize(poupanca_media=weighted.mean(poupa,peso_final))


# Etapa 4 -----------------------------------------------------------
despesas_esferas2003 <- ler_despesas2003() %>%
  left_join(esferas_ucs2003, by = "cod_uc") %>% 
  select(cod_uc, codigo, esfera, fracao,p_cv,despmes,total)%>%
  # sem níveis nessa versão
  # select(cod_uc, codigo, nivel_0:esfera)
rename(valor=despmes)

# Estimativa geral esferas
t <- despesas_esferas2003 %>%
  group_by(esfera,fracao) %>%
  summarise(soma = sum(despmes) * 12 / 1e6) %>% # Em milhões
  filter(!is.na(esfera)) %>% 
  mutate(partic = soma / sum(soma))

t$partic_total <- t$soma/sum(t$soma)
t <- t %>% left_join(dicfracoes) %>% select(-fracao,-partic)


despesas_esferas2003 %>% 
  mutate(ano = 2003) %>%
  write_csv("gastos_esferas_fracoes_2003.csv")

# despesas_esferas2003 %>% 
#   mutate(ano = 2003) %>%
#   write_csv("gastos_esferas_2003_renda.csv")


gastos_2003 <- despesas_esferas2003%>%
  filter(!is.na(cod_uc)) %>% left_join(pesos2003)



g <- gastos_2003 %>% 
  mutate(esfera=ifelse(is.na(esfera),"baixa",esfera))%>%
  group_by(cod_uc)%>% 
  summarize(massa=sum(despmes)*12/1e9,
            peso_final = first(peso_final),
            esfera=first(esfera),fracao=first(fracao))%>% 
  mutate(maisrico = ntile(massa,1000),
         massa = ifelse(maisrico>998,massa*2,massa))%>%
  mutate(fracao=ifelse(is.na(fracao),1,fracao))%>%
  group_by(esfera,fracao)%>%
  summarize(domicilios=sum(peso_final),
            massa_valor = sum(massa*peso_final),
            media_gastos = weighted.mean(massa, peso_final)*1e9/12)%>% 
  left_join(dicfracoes)


g$participacao <- g$massa_valor/sum(g$massa_valor)

g%>%group_by(esfera)%>%summarize(participacao=sum(participacao))


gh <- gastos_2003 %>% 
  mutate(esfera=ifelse(is.na(esfera),"baixa",esfera))%>%
  group_by(cod_uc)%>% 
  summarize(massa=sum(despmes),
            peso_final = first(peso_final),
            esfera=first(esfera),fracao=first(fracao))%>% 
  mutate(maisrico = ntile(massa,1000),
         massa = ifelse(maisrico>998,massa*2,massa))%>%
  mutate(fracao=ifelse(is.na(fracao),1,fracao))

ggplot(gh,aes(factor(fracao),massa,fill=esfera))+
geom_violin(draw_quantiles = 0.5) +
  scale_y_log10()
