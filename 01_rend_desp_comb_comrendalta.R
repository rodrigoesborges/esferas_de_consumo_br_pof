library(pof)
library(tidyverse)
library(scales)
source("00_funcoes_patrcod.R")
# O script está montado para ter 4 partes: cada uma uma função
# As funções usadas estão no script 00_funcoes.R
# 1) Ler os dados de acordo com as intruções do IBGE
# 2) Classificar os rendimentos nas esferas alta/baixa
# A ideia é que só precisemos mexer nessa etapa
# 3) Identificar as UC nas esferas 
# baseado em corte vindo da análise dos dados (0.63)
# 4) Vincular os gastos das UCs com as esferas


# Etapa 0 ------------------ preparar repositório -------------------
# download_pof(2018)
# unzip_pof(2018)
# 
## Tive que usar essa linha de baixo pq o IBGE mudou
## a forma de extrair os arquivos
# dir(glue::glue("dados/2018/Arquivos de dados"), 
#     full.names = TRUE) %>% 
#   file.rename(str_remove(., "Arquivos de dados/"))

# Etapa 1 -----------------------------------------------------------
rendas2018 <- ler_rendimentos2018()

# Etapa 2 -----------------------------------------------------------
rendas_classificadas <- classificar_rendimentos(rendas2018,T)

rendas_classificadas$grupo_cod <- as.numeric(rendas_classificadas$grupo_cod)

rendas_classificadas %<>% mutate(
  forma = case_when(
    is.na(grupo_cod) ~ forma,
    !is.na(grupo_cod) & forma %in% c(5,6,7) ~ forma,
    !is.na(grupo_cod) & !(forma %in% c(5,6,7)) ~ grupo_cod)
)


rendas_ucs <- rendas_classificadas %>%
  group_by(cod_uc, forma) %>%
  summarise(renda = sum(valor_mensal)) %>%
  pivot_wider(names_from = forma, values_from = renda, 
              values_fill = list(renda = 0)) %>%
  mutate(cv = `1`+`3`+`4`+`9`+`12`,
         `5` = ifelse(`5`<0 ,ifelse(abs(`5`)>2*cv,abs(`5`),0),`5`),
         baixa = `1`+`2`+`3`+`8`+`9`+`10`+`12`,
         mv = `2`+`5`+`6`+`7`+`8`+`10`+`11`+`13`+`14`)%>%
  mutate(total = cv + mv,
         p_cv = (cv+0.00001)/(total++0.00001),
         p_baixa = min(1,(baixa+0.00001)/(total+0.00001)))%>%
  select(sort(tidyselect::peek_vars()))%>%
  select(c(1,7:14,2:6,15:21))
  

rendas_ucs$max_baixa <- apply(rendas_ucs[baixa],1,max)

rendas_ucs$max_alta <- apply(rendas_ucs[alta],1,max)

rendas_ucs$fracao <- ifelse(rendas_ucs$p_baixa >= corte,
                            as.numeric(names(rendas_ucs[baixa])[max.col(rendas_ucs[baixa],ties.method = "first")]),
                                    as.numeric(names(rendas_ucs[alta])[max.col(rendas_ucs[alta],ties.method = "first")])
                                    )
                     

##Função de correção via k-means
separalbx <- function(tabela = rendas_ucs,baixa=8,alta=5,cl=1,gr=3) {
    tf <- tabela[tabela$fracao %in% c(baixa,alta),]$total
  centros <- kmeans(tf,gr)$centers
  centros <- sort(centros)
  print(centros)
  k <- kmeans(tf,centros)
  tabela[tabela$fracao %in% c(baixa,alta),]$fracao <- 
    ifelse(k$cluster <= cl, baixa, alta)
  tabela
}

##Correção população marginalizada
rendas_ucs %<>%
  mutate(fracao = ifelse(total < 750,9,fracao),
         fracao = ifelse(fracao == 3, 2, fracao))

# K-Means usadas para 5 e 8
rendas_ucs <- separalbx(gr=10)


#para 10 e 11
rendas_ucs <- separalbx(baixa=10,alta=11)
#para 12 e 13

rendas_ucs <- separalbx(baixa=12,alta=13)

#para 4 e 1
rendas_ucs <- separalbx(baixa=1,alta=4)

#para 4 e 2
rendas_ucs <- separalbx(baixa=2,alta=4)

#para 14 e 6
rendas_ucs <- separalbx(baixa=6,alta=14,cl=2)

# corte <- 0.5
# Etapa 3 -----------------------------------------------------------
# rendas_esferas <- rendas_ucs %>%
#     mutate(esfera = ifelse(p_baixa > corte, "baixa", "alta"))

rendas_ucs$esfera <- ifelse(rendas_ucs$fracao %in% baixa,"baixa","alta")

rendas_esferas <- rendas_ucs

##Correção de 0,2% mais ricos (Artigo IBGE sobre PNAD,POF e Censo)
rendas_esferas %<>%
  mutate(maisrico = ntile(total,1000),
         total = ifelse(maisrico>998,2*total,total))  



##Correção de 5 + esfera baixa
# rendas_esferas %<>%
#   mutate(
    #fracao = ifelse(esfera == "baixa" & fracao == 5,
     #                      8,fracao),
           ##Correção de outros "corner cases"
         #fracao = ifelse(esfera == "baixa" & total == 0 & fracao ==11,
#                         9,fracao),
#         fracao = ifelse(total <= 500, 9, fracao),
         #esfera = ifelse(total <= 500, "baixa", esfera),
#         fracao = ifelse(fracao == 9 & total >=1000, 2,fracao),
#         fracao = ifelse(esfera == "alta" & fracao == 12, 13,fracao),
          # fracao = ifelse(total<1000,9,fracao),
         # esfera = ifelse(esfera == "alta" & fracao %in% c(1,2,3,8),"baixa",esfera),
         # esfera = ifelse(esfera == "baixa" & fracao %in% c(11,6,7,13,4), "alta",
                         # esfera),
#         fracao = ifelse(fracao %in% c(3,8) & total > 13000, 5,fracao),
#         fracao = ifelse(fracao %in% c(1,2) & total > 9000, 4,fracao),
#         fracao = ifelse(fracao == 4 & total < 5000, 1, fracao ),
         # esfera = ifelse(fracao == 1, "baixa",esfera),
         # esfera = ifelse(fracao %in% c(4,5), "alta",esfera),
#         fracao = ifelse(fracao == 11 & total < 3000, 10,fracao),
#         fracao = ifelse(fracao == 10 & total > 7000, 11, fracao),
         # esfera = ifelse(fracao == 11, "alta", esfera),
         # esfera = ifelse(fracao == 10, "baixa", esfera),
#         fracao = ifelse(fracao == 5 & total < 5000, 8,fracao),
         # esfera = ifelse(fracao == 8,  "baixa",esfera),
#         fracao = ifelse(fracao == 12 & total > 8000,13,fracao),
         # esfera = ifelse(fracao == 12, "baixa",esfera),
         # esfera = ifelse(fracao == 13, "alta",esfera)
#         fracao = ifelse(fracao %in% c(2,3), 1, fracao)

    # )



esferas_ucs <- rendas_esferas %>%
  select(cod_uc, esfera,p_cv,fracao,total)



####Continua após definir objeto despesas_esferas

# Etapa 4 -----------------------------------------------------------
despesas_esferas <- ler_despesas2018() %>%
  left_join(esferas_ucs, by = "cod_uc") %>% 
  select(cod_uc, codigo, fracao,p_cv,total,nivel_0:esfera)

###

despesas_esferas %>% 
  mutate(ano = 2018) %>% 
  select(-starts_with("nivel")) %>% 
  select(cod_uc, codigo, esfera, fracao, p_cv,valor,total,  ano)%>%
  write_csv("gastos_esferas_fracoes_2018.csv")



###Vamos adicionar "poupança" por UC

poupanca_ucs <- rendas2018%>%filter(is.na(nivel))%>%
  group_by(cod_uc)%>%
  summarize(cod_uc=first(cod_uc),
            poupanca=sum(-1*valor_mensal*as.numeric(valor_mensal<0)))

poupanca_ucs <- poupanca_ucs%>%
  full_join(despesas_esferas%>%left_join(pesos2018)%>%filter(grepl("^47",codigo))%>%group_by(cod_uc)%>%summarize(imovel=sum(valor/peso_final)))

poupanca_ucs[is.na(poupanca_ucs)] <-  0

poupanca_ucs$poupanca <- poupanca_ucs$poupanca+poupanca_ucs$imovel

poupanca_ucs <- poupanca_ucs %>% select(-imovel)



esferas_ucs <- esferas_ucs%>%left_join(poupanca_ucs)

esferas_ucs[is.na(esferas_ucs$poupanca),"poupanca"] <- 0

ep <- esferas_ucs%>% mutate(poupa=poupanca/(total+0.00001))%>%
  left_join(dicfracoes)%>%left_join(pesos2018)

ep%>%group_by(esfera,fracao)%>%summarize(poupanca_media=weighted.mean(poupa,peso_final))%>%left_join(dicfracoes)%>%mutate(poupanca_media = ifelse(fracao == 9,0,percent(poupanca_media,accuracy = 0.1)))%>%select(esfera,nomefracao,poupanca_media)




rm2018 <-
  ggplot(esferas_ucs%>%left_join(dicfracoes),aes(factor(gsub(" ","\n",nomefracao)),total,fill=esfera))+geom_violin(draw_quantiles = 0.5)+
  scale_y_log10(labels = comma_format(big.mark = ".",
                                      decimal.mark = ","))+
  xlim(l[8],l[1],l[2],l[3],l[10],l[9],l[4],l[14],l[11],l[6],l[13],l[5],l[12])+
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2)+
  ggtitle("Distribuição de renda nas frações - 2018")+
  theme_minimal()+
  theme(axis.title.x=element_blank())
