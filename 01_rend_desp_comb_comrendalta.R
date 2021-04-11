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

# km <- rendas2018 %>% 
#   group_by(cod_uc) %>% 
#   summarise(peso_final = first(peso_final),
#             valor = sum(valor_mensal)) %>% 
#   pull(valor) %>% 
#   kmeans(3)

rendas2018 %>% 
  group_by(cod_uc) %>% 
  summarise(peso_final = first(peso_final),
            valor = sum(valor_mensal)) %>% 
  ggplot(aes(seq_along(valor), valor, col = factor(km$cluster))) + 
  geom_point(alpha = 0.3) + 
  # scale_y_log10() +
  scale_y_continuous(limits = c(0, 50e3)) +
  geom_hline(yintercept = 20e3)

rendas2018 %>% 
  group_by(cod_uc) %>% 
  summarise(peso = first(peso_final),
            valor = sum(valor_mensal)) %>% 
  mutate(grupo = kmeans(valor, 3)$cluster, 
         centros = list(kmeans(valor, 3)$centers %>% as.vector()),
         grupo_tipo = centros[[]]) %>% 
  group_by(grupo) %>% 
  summarise(soma = sum(valor * 12 * peso / 1e9),
            media = weighted.mean(valor, peso)) %>% 
  arrange(desc(media))

teste2 <- teste %>% 
  mutate(teste = map2_dbl(centros, grupo, ~.x[[.y]] == min(.x)))

teste2 %>% 
  ggplot(aes(valor, seq_along(valor), col = factor(grupo))) + 
  geom_point(alpha = 0.3) + 
  geom_vline(xintercept = 20e3) + 
  scale_x_log10()

# Etapa 2 -----------------------------------------------------------
rendas_classificadas <- classificar_rendimentos(rendas2018,T)

rendas_classificadas$grupo_cod <- as.numeric(rendas_classificadas$grupo_cod)

rendas_classificadas %<>% mutate(
  forma = case_when(
    is.na(grupo_cod) ~ forma,
    !is.na(grupo_cod) & forma %in% c(5,6,7) ~ forma,
    !is.na(grupo_cod) & !(forma %in% c(5,6,7)) ~ grupo_cod)
)%>%
  mutate(forma = ifelse(valor_mensal>=8000 & forma == 1, 5,forma))
  


rendas_ucs <- rendas_classificadas %>%
  group_by(cod_uc, forma) %>%
  summarise(renda = sum(valor_mensal)) %>%
  pivot_wider(names_from = forma, values_from = renda, 
              values_fill = list(renda = 0)) %>%
    select(cod_uc,sort(current_vars()))%>%
    select(1:2,7:14,3:6)%>%
  mutate(cv = `1`+`3`+`4`+`9`+`12`,
         `5` = ifelse(`5`<0 & abs(`5`)>5*cv,abs(`5`),0),
         baixa = `1`+`2`+`6`+`8`+`9`+`10`+`12`,
         mv = `2`+`5`+`6`+`7`+`8`+`10`+`11`+`13`)%>%
  mutate(total = cv + mv,
         p_cv = (cv+0.00001)/(total++0.00001),
         p_baixa = min(1,(baixa+0.00001)/(total+0.00001))) %>%
  ungroup()
  
rendas_ucs$fracao <- sapply(max.col(rendas_ucs[2:14],ties.method = "last"),
                                  function (x) {as.numeric(names(rendas_ucs[2:14])[x])})
  # rendas_ucs <- rendas_ucs %>%
  #   mutate(rendalta = total > quantile(total,0.8, names = F))
  
# K-Means usadas para estabelecer corte alta/baixa
# k_rendas <- kmeans(rendas_ucs$p_cv, c(0.2, 0.5))
# k_rendas$centers
# 
# ggplot(rendas_ucs, aes(p_cv, total, col = factor(k_rendas$cluster))) +
#   geom_point(shape = ".", alpha = 0.5) +
#   geom_vline(xintercept = 0.63, lty = 2) +
#   scale_y_continuous(limits = c(0, 20e3)) +
#   theme_classic() +
#   theme(legend.position = "none")

# A análise acima propor o corte de 63% para esfera alta/baixa
#corte <- 0.99
#cortemedia <- 0.6
# Etapa 3 -----------------------------------------------------------
rendas_esferas <- rendas_ucs %>%
    mutate(esfera = ifelse(p_baixa > corte, "baixa", "alta"))

# rendas_esferas <- rendas_ucs %>%
#   mutate(esfera = ifelse(p_cv > corte, "baixa", ifelse(p_cv>cortemedia,"média","alta")))


#para renda alta
# rendas_esferas <- rendas_ucs %>%
#   mutate(esfera = ifelse(!rendalta, "baixa", "alta"))

# 
# 
# rendas_esferas %>% 
#   group_by(esfera) %>% 
#   summarise(v= sum(total), n =  n()) %>% 
#   mutate(tx = v/last(v), p = n / sum(n))

##Correção de 0,2% mais ricos (Artigo IBGE sobre PNAD,POF e Censo)
rendas_esferas %<>%
  mutate(maisrico = ntile(total,1000),
         total = ifelse(maisrico>998,2*total,total))  
  
##Correção de 5 + esfera baixa
rendas_esferas %<>% 
  mutate(fracao = ifelse(esfera == "baixa" & fracao == 5,
                           8,fracao),
           ##Correção de outros "corner cases"
         fracao = ifelse(esfera == "baixa" & total == 0 & fracao ==11,
                         9,fracao),
         fracao = ifelse(total <= 500, 9, fracao),
         esfera = ifelse(total <= 500, "baixa", esfera),
         fracao = ifelse(fracao == 9 & total >=1000, 2,fracao),
         fracao = ifelse(esfera == "alta" & fracao == 12, 13,fracao),
         esfera = ifelse(esfera == "alta" & fracao %in% c(1,2,3,8),"baixa",esfera),
         esfera = ifelse(esfera == "baixa" & fracao %in% c(11,6,7,13,4), "alta",
                         esfera),
         fracao = ifelse(fracao %in% c(3,8) & total > 13000, 5,fracao),
         fracao = ifelse(fracao %in% c(1,2) & total > 9000, 4,fracao),
         fracao = ifelse(fracao == 4 & total < 5000, 1, fracao ),
         esfera = ifelse(fracao == 1, "baixa",esfera),
         esfera = ifelse(fracao %in% c(4,5), "alta",esfera),
         fracao = ifelse(fracao == 11 & total < 3000, 10,fracao),
         fracao = ifelse(fracao == 10 & total > 7000, 11, fracao),
         esfera = ifelse(fracao == 11, "alta", esfera),
         esfera = ifelse(fracao == 10, "baixa", esfera),
         fracao = ifelse(fracao == 5 & total < 5000, 8,fracao),
         esfera = ifelse(fracao == 8,  "baixa",esfera),
         fracao = ifelse(fracao == 12 & total > 8000,13,fracao),
         esfera = ifelse(fracao == 13, "alta",esfera),
         fracao = ifelse(fracao %in% c(2,3), 1, fracao)
         
    )



esferas_ucs <- rendas_esferas %>%
  select(cod_uc, esfera,p_cv,fracao,total)

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


rm2018 <- ggplot(esferas_ucs%>%left_join(dicfracoes),aes(factor(gsub(" ","\n",nomefracao)),total,fill=esfera))+geom_violin(draw_quantiles = 0.5)+
  scale_y_log10()+
  ggtitle("Distribuição de renda nas frações - 2018")+
  theme_minimal()+
  theme(axis.title.x=element_blank())

# Etapa 4 -----------------------------------------------------------
despesas_esferas <- ler_despesas2018() %>%
  left_join(esferas_ucs, by = "cod_uc") %>% 
  select(cod_uc, codigo, fracao,p_cv,total,nivel_0:esfera)

# Estimativa geral esferas
t18 <- despesas_esferas %>%
  group_by(esfera,fracao) %>%
  summarise(soma = sum(valor) * 12 / 1e6) %>% # Em milhões
  mutate(partic = soma / sum(soma))

t18$partic_total <- t18$soma/sum(t18$soma)
t18  <- t18 %>% left_join(dicfracoes) %>% select(-fracao,-partic)

# Estimativa um produto: livros (cod: 110803)
despesas_esferas %>% 
  group_by(nivel = nivel_4, esfera) %>%
  summarise(soma = sum(valor) * 12 / 1e6)  %>%
  filter(nivel == 110803) %>%
  mutate(partic = soma / sum(soma))

despesas_esferas %>% 
  mutate(ano = 2018) %>% 
  select(-starts_with("nivel")) %>% 
  select(cod_uc, codigo, esfera, fracao, p_cv,valor,total,  ano)%>%
  write_csv("gastos_esferas_fracoes_2018.csv")

# despesas_esferas %>% 
#   mutate(ano = 2018) %>% 
#   select(-starts_with("nivel")) %>% 
#   write_csv("gastos_esferas_2018_renda.csv")

gastos2018 <- despesas_esferas%>%
  filter(!is.na(cod_uc))%>%left_join(pesos2018)


g18 <- gastos2018 %>%
  mutate(esfera = ifelse(is.na(esfera),"baixa",esfera))%>%
  # 2018 já estava multuplicadao pelo peso 
  mutate(valor = ifelse(ano == 2018, valor / peso_final, valor)) %>% 
  group_by(cod_uc)%>%
  summarize(massa=sum(valor)*12/1e9,
            peso_final = first(peso_final),
            esfera=first(esfera),
            fracao=first(fracao))%>%
  mutate(maisrico = ntile(massa,1000),
         massa = ifelse(maisrico>998,massa*2,massa))%>%
  mutate(fracao=ifelse(is.na(fracao),1,fracao))%>%
  group_by(esfera,fracao)%>%
  summarize(domicilios=sum(peso_final),
            massa_valor = sum(massa*peso_final),
            media_gastos = weighted.mean(massa,peso_final)*1e9/12)%>%
  left_join(dicfracoes)

g18$participacao <- g18$massa_valor/sum(g18$massa_valor)

g18 %>%group_by(esfera)%>% summarize(participaco=sum(participacao))

gh18 <- gastos2018 %>%
  mutate(esfera=ifelse(is.na(esfera),"baixa",esfera))%>%
  # 2018 já estava multuplicadao pelo peso 
  mutate(valor = ifelse(ano == 2018, valor / peso_final, valor)) %>% 
  group_by(cod_uc)%>%
  summarize(massa=sum(valor),
            peso_final=first(peso_final),
            esfera=first(esfera),
            fracao=first(fracao))%>%
  mutate(maisrico = ntile(massa,1000),
         massa = ifelse(maisrico>998,massa*2,massa))%>%
  mutate(fracao=ifelse(is.na(fracao),1,fracao))%>%
  left_join(dicfracoes)


ggplot(gh18,aes(factor(gsub(" ","\n",nomefracao)),massa,fill=esfera))+
  geom_violin(draw_quantiles = 0.5) +
  scale_y_log10()
