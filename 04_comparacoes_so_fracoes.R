library(tidyverse)
library(ggpubr)
library(hrbrthemes)
#devtools::install_github("hrbrmstr/waffle")
library(waffle)
library(scales)
library(magrittr)
library(extrafont)

# Os arquivos lidos abaixo são gerados nos scripts 01, 02 e 03
gastos2003 <- readr::read_csv("gastos_esferas_fracoes_2003.csv", 
                              col_types = "cccccddd")
gastos2009 <- readr::read_csv("gastos_esferas_fracoes_2009.csv", 
                              col_types = "cccccddd")
gastos2018 <- readr::read_csv("gastos_esferas_fracoes_2018.csv", 
                              col_types = "cccccddd")

gastos <- gastos2003 %>% 
#  rename(valor = despmes) %>% 
  bind_rows(gastos2009, gastos2018)



estats <- gastos %>% 
  group_by(ano, esfera) %>%
  summarise(soma = sum(valor) * 12 / 1e9) %>% # Em bilhões
  mutate(partic = soma / sum(soma))

estats %>% 
  ggplot(aes(ano, partic * 100, col = esfera)) + 
  geom_line(size = 1) + 
  geom_point(size = 2, col = "black") + 
  geom_text(aes(label = round(partic * 100)), 
            # filter(estats, ano == 2018), 
            # nudge_y = 7) + 
            nudge_y = c(rep(c(-7, 7, 7), times = 2), rep(7, 3))) + 
  scale_x_continuous(breaks = c(2003, 2009, 2018)) + 
  theme_classic() +
  labs(x = "", y = "Participação no total (%)",
       title = "Esferas de consumo", 
       subtitle = "POFs de 2003, 2009 e 2018")

ggsave("imagens/relativo.png")

pesos2003 <- readRDS("dados/2003/t_morador.rds") %>% 
  as_tibble() %>% 
  mutate(cod_uc = paste0(uf, seq, dv, domcl, uc ),
         ano = 2003) %>% 
  select(ano, cod_uc, peso = fator, peso_final = fator_set) %>% 
  unique()

pesos2009 <- readRDS("dados/2009/Dados/t_morador_s.rds") %>% 
  as_tibble() %>% 
  mutate(cod_uc = paste0(cod_uf, num_seq, num_dv, cod_domc, num_uc ),
         ano = 2009) %>% 
  select(ano, cod_uc, peso = fator_expansao1, 
         peso_final = fator_expansao2) %>% 
  unique()

pesos2018 <- pof::ler_morador(2018) %>% 
  mutate(cod_uc = paste0(UF, ESTRATO_POF, TIPO_SITUACAO_REG,
                      COD_UPA, NUM_DOM, NUM_UC),
         ano = 2018) %>% 
  janitor::clean_names() %>% 
  select(ano, cod_uc, peso, peso_final) %>% 
  unique()

pesos <- bind_rows(
  pesos2003, 
  pesos2009,
  pesos2018
) 

gastos_expandidos <- gastos %>% 
  filter(!is.na(cod_uc)) %>%
  left_join(pesos, by = c("ano", "cod_uc"))



estat_ponderadas <- gastos_expandidos %>% 
  # 2018 já estava multuplicadao pelo peso 
  mutate(valor = ifelse(ano == 2018, valor / peso_final, valor)) %>% 
  group_by(ano, esfera, cod_uc) %>% 
  summarise(massa = sum(valor),
            peso_final = first(peso_final),
            renda = max(total)) %>% 
  summarise(massa_valor = sum(massa * peso_final),
            media_pond = weighted.mean(massa, peso_final), 
            unidades = sum(peso_final),
            renda_total = sum(renda,peso_final,na.rm = T),
            renda_media = weighted.mean(renda,peso_final, na.rm = T))

estat_ponderadas %>% ungroup()%>%group_by(ano)%>%summarize(esfera = esfera,partic = massa_valor/sum(massa_valor))%>%
  ggplot(aes(ano, partic * 100, col = esfera)) + 
  geom_line(size = 1) + 
  geom_point(size = 2, col = "black") + 
  geom_text(aes(label = round(partic * 100)), 
            # filter(estats, ano == 2018), 
            # nudge_y = 7) + 
            nudge_y = c(rep(c(-7, 7, 7), times = 2), rep(7, 3))) + 
  scale_x_continuous(breaks = c(2003, 2009, 2018)) + 
  theme_classic() +
  labs(x = "", y = "Participação no total de gastos(%)",
       title = "Esferas de consumo", 
       subtitle = "POFs de 2003, 2009 e 2018")


estat_ponderadas %>% ungroup()%>%group_by(ano)%>%summarize(esfera = esfera,partic = renda_total/sum(renda_total))%>%
  ggplot(aes(ano, partic * 100, col = esfera)) + 
  geom_line(size = 1) + 
  geom_point(size = 2, col = "black") + 
  geom_text(aes(label = round(partic * 100)), 
            # filter(estats, ano == 2018), 
            # nudge_y = 7) + 
            nudge_y = c(rep(c(-7, 7, 7), times = 2), rep(7, 3))) + 
  scale_x_continuous(breaks = c(2003, 2009, 2018)) + 
  theme_classic() +
  labs(x = "", y = "Participação no total da renda(%)",
       title = "Esferas de consumo", 
       subtitle = "POFs de 2003, 2009 e 2018")






ipca <- tribble(
  ~ano, ~ipca,
  2003, 9.30,
  2004, 7.60,
  2005, 5.69,
  2006, 3.14,
  2007, 4.46,
  2008, 5.90,
  2009, 4.31,
  2010, 5.91,
  2011, 6.50,
  2012, 5.84,
  2013, 5.91,
  2014, 6.41,
  2015, 10.67,
  2016, 6.29,
  2017, 2.95,
  2018, 3.75
) %>% 
  mutate(
    indice = 1 + ipca/100,
    indice_acum = cumprod(indice),
    deflator = indice_acum / last(indice_acum)
  )

estat_defla <- estat_ponderadas %>% 
  left_join(select(ipca, ano, deflator), by = "ano") %>% 
  mutate(bi = massa_valor * 12 / 1e9 / deflator,
         media_pond = media_pond / deflator) %>% 
  filter(!is.na(esfera)) 

ggplot(estat_defla, aes(ano, bi, col = esfera)) + 
  geom_line(size = 1) + 
  geom_point(size = 2, col = "black") + 
  geom_text(aes(label = round(bi)),
            nudge_y = 100) + 
  scale_x_continuous(breaks = c(2003, 2009, 2018)) + 
  theme_classic() +
  labs(x = "", y = "Estimativa (R$ 2018, bilhões)",
       title = "Esferas de consumo", 
       subtitle = "POFs de 2003, 2009 e 2018")

ggsave("imagens/massas.png")

ggplot(estat_defla, aes(ano, media_pond, col = esfera)) + 
  geom_line(size = 1) + 
  geom_point(size = 2, col = "black") + 
  geom_text(aes(label = round(media_pond) %>% prettyNum(".", decimal.mark = ",")),
            nudge_y = rep(c(-500, 500), times = 3),
            nudge_x = rep(c(0.3, 0, -0.3), each = 2)) + 
  scale_x_continuous(breaks = c(2003, 2009, 2018)) + 
  theme_classic() +
  labs(x = "", y = "Gastos médios (R$ 2018)",
       title = "Esferas de consumo", 
       subtitle = "POFs de 2003, 2009 e 2018")

ggsave("imagens/media.png")

ggplot(estat_defla, aes(ano, unidades / 1e6, col = esfera)) + 
  geom_line(size = 1) + 
  geom_point(size = 2, col = "black") + 
  geom_text(aes(label = round(unidades /1e6, 1) %>% 
                  prettyNum(".", decimal.mark = ",")),
            nudge_y = rep(c(5, -5), times = 3)) + 
  scale_x_continuous(breaks = c(2003, 2009, 2018)) + 
  theme_classic() +
  labs(x = "", y = "Milhões de unidades",
       title = "Esferas de consumo", 
       subtitle = "POFs de 2003, 2009 e 2018")

ggsave("imagens/unidades.png")

gastos2 <- gastos %>% 
  filter(!is.na(cod_uc)) %>%
  left_join(pesos, by = c("ano", "cod_uc")) %>% 
  mutate(valor = ifelse(ano == 2018, valor / peso_final, valor)) %>% 
  group_by(ano, cod_uc,esfera) %>% 
  summarise(valor = sum(valor))
  # summarise(valor = sum(valor)) %>% 
  # mutate(clusters = list(kmeans(valor, 3)),
  #        grupo = clusters[[1]]$cluster,
  #        centro_cl = map(clusters, list("centers", as.vector)),
  #        esfera = map2_int(centro_cl, grupo, ~.x[[.y]] == min(.x)),
  #        esfera = c("alta", "baixa")[esfera + 1])

estats_esf <- gastos2 %>% 
  left_join(pesos) %>% 
  group_by(ano, esfera) %>% 
  summarise(massa = sum(valor * peso_final, na.rm = TRUE),
            media = weighted.mean(valor, peso_final, na.rm = TRUE),
            unidades = sum(peso_final)) %>% 
  left_join(ipca) %>% 
  mutate(p = massa * 100 / sum(massa),
         massa_defla = massa / deflator / 1e9,
         media_defla = media / deflator) %>% 
  select(-ipca, -indice, -indice_acum, -deflator)

estats_esf  %>% 
  ggplot(aes(ano, media_defla, col = esfera)) + 
  geom_line(size = 1) + 
  geom_point(size = 2, col = "black") + 
  geom_text(aes(label = round(media_defla) %>% prettyNum(".", decimal.mark = ",")),
            nudge_y = rep(c(-1000, 1000), times = 3),
            nudge_x = rep(c(0.3, 0, -0.3), each = 2)) + 
  scale_x_continuous(breaks = c(2003, 2009, 2018)) + 
  theme_classic() +
  labs(x = "", y = "Gastos médios (R$ 2018)",
       title = "Esferas de consumo", 
       subtitle = "POFs de 2003, 2009 e 2018")

estats_esf  %>% 
  ggplot(aes(ano, media_defla, col = esfera)) + 
  geom_line(size = 1) + 
  geom_point(size = 2, col = "black") + 
  geom_text(aes(label = round(media_defla) %>% prettyNum(".", decimal.mark = ",")),
            nudge_y = rep(c(-1000, 1000), times = 3),
            nudge_x = rep(c(0.3, 0, -0.3), each = 2)) + 
  scale_x_continuous(breaks = c(2003, 2009, 2018)) + 
  theme_classic() +
  labs(x = "", y = "Gastos médios (R$ 2018)",
       title = "Esferas de consumo", 
       subtitle = "POFs de 2003, 2009 e 2018")
  
gastos_raiz <- gastos %>% 
  group_by(ano, cod_uc, esfera) %>% 
  summarise(valor = sum(valor))

ggplot(gastos_raiz %>% filter(!is.na(esfera)), 
       aes(factor(ano), valor, fill = esfera)) + 
  geom_violin(draw_quantiles = 0.5) +
  scale_y_log10() + 
  facet_wrap(~factor(ano))
  
ggplot(gastos2 %>% filter(!is.na(esfera)), 
       aes(factor(ano), valor, fill = esfera)) + 
  geom_violin(draw_quantiles = 0.5) +
  scale_y_log10() +
  scale_y_continuous(limits = c(0, 20e3)) + 
  facet_wrap(~factor(ano))+
  theme_minimal()



#### Colocando as frações nas comparações

##grafico de pizza
estatsf <-  gastos %>% 
  group_by(ano, esfera,fracao) %>%
  summarise(soma = sum(valor) * 12 / 1e9) %>% # Em bilhões
  mutate(partic = soma / sum(soma))

grafp <-function(qano,qesf,mostraleg) {
  p <- estatsf %>%
    mutate(fracao=as.numeric(fracao))%>%
    left_join(dicfracoes)%>%
    filter(esfera == qesf & ano == qano)%>%
    arrange(desc(nomefracao))%>%
    mutate(ypos = cumsum(partic)-0.5*partic)%>%
    rename(`Fração de Classe` = nomefracao)
  p <- ggplot(p,aes(x="",y=partic,fill=`Fração de Classe`))+
    geom_bar(width=1,stat="identity")+
    coord_polar("y")+
    scale_fill_brewer(palette="Dark2")+
    geom_text(aes(y = ypos, label = percent(partic,accuracy = 0.1)), size = 3)+
    theme_void()+
    theme(legend.title = element_blank(),
          legend.position = ifelse(mostraleg == 1,"bottom","none"))+
    guides(fill=guide_legend(nrow=4,byrow=TRUE))+
    ggtitle(ifelse(mostraleg == 1, 
                   paste("Participação de frações na renda total da Esfera",
                         str_to_title(qesf),"\n\n                        ",
                         qano),
                   paste("                                   ",qano)))
} 

anos <- c(2003,2009,2018)

esfs <- c("baixa","alta")

anosesf <- expand.grid(anos,esfs)

names(anosesf) <- c("qano","qesf")
anosesf$mostraleg <- rep(c(0,1,0),2)
#anosesf %<>% arrange(qano)

plotagens <- mapply(grafp,anosesf$qano,anosesf$qesf,anosesf$mostraleg,SIMPLIFY = F)



ggarrange(plotlist = plotagens, ncol = 3, nrow = 2)

estatesf_p <- gastos_expandidos %>% 
  mutate(fracao = ifelse(is.na(fracao),9,fracao))%>%
  # 2018 já estava multuplicadao pelo peso 
  mutate(valor = ifelse(ano == 2018, valor / peso_final, valor)) %>% 
  group_by(ano, fracao,cod_uc) %>% 
  summarise(massa = sum(valor),
            peso_final = first(peso_final),
            renda = max(total),
            esfera = first(esfera)) %>% 
  summarise(massa_valor = sum(massa * peso_final),
            media_pond = weighted.mean(massa, peso_final), 
            unidades = sum(peso_final),
            renda_total = sum(renda,peso_final,na.rm = T),
            renda_media = weighted.mean(renda,peso_final, na.rm = T),
            esfera = first(esfera))

estatesf_pano <- estatesf_p%>%
  group_by(ano)%>%
  mutate(partic = massa_valor/sum(massa_valor),
         fracao = as.numeric(fracao))%>%
  ungroup()


estatesf_p %>% 
  filter(!is.na(esfera))%>%
  mutate(fracao=as.numeric(fracao))%>%
  ungroup()%>%left_join(dicfracoes)%>%
  group_by(ano)%>%
  summarize(fracao = nomefracao,
            partic = massa_valor/sum(massa_valor),
            esfera = esfera)%>%
  arrange(esfera,fracao)%>%
  ggplot(aes(ano, partic, fill = fracao)) + 
  geom_bar(stat = "identity") + 
  #geom_point(size = 2, col = "black") + 
  geom_text(aes(label = round(partic * 100)), 
            # filter(estats, ano == 2018), 
            # nudge_y = 7) + 
            position = position_fill(vjust= 0.5)
) + 
  scale_fill_brewer(palette="RdBu")+
  scale_x_continuous(breaks = c(2003, 2009, 2018)) + 
  theme_classic() +
  labs(x = "", y = "Participação no total de gastos(%)",
       title = "Esferas de consumo", 
       subtitle = "POFs de 2003, 2009 e 2018")


grafpp <-function(qano,qesf,mostraleg, 
                  tabela = estatesf_pano, 
                  titulo = "nos gastos totais") {
  p <- tabela %>%
    mutate(fracao=as.numeric(fracao))%>%
    left_join(dicfracoes)%>%
    filter(esfera == qesf & ano == qano)%>%
    arrange(desc(nomefracao))%>%
    mutate(ypos = cumsum(partic)-0.5*partic)%>%
    rename(`Fração de Classe` = nomefracao)
  partic_p <- percent(sum(p$partic),accuracy = 0.1)
  p <- ggplot(p,aes(x="",y=partic,fill=`Fração de Classe`))+
    geom_bar(width=1,stat="identity")+
    coord_polar("y")+
    scale_fill_brewer(palette="Dark2")+
    geom_text(aes(y = ypos, x = 1.4, label = 
                    percent(partic/sum(partic),accuracy = 0.1)),
              size = 4)+
    geom_text(aes(y = ypos, x = 1.1, label = paste0(
                           "(",
                           percent(partic,accuracy = 0.1),
                           ")")),size = 3)+
    theme_void()+
    theme(legend.title = element_blank(),
          legend.position = ifelse(mostraleg == 1,"bottom","none"))+
    guides(fill=guide_legend(nrow=4,byrow=TRUE))+
    ggtitle(ifelse(mostraleg == 1, 
                   paste("Participação de frações",
                         titulo,"da Esfera",
                         str_to_title(qesf),"\n\n                        ",
                         qano,"\n                        ",
                         partic_p),
                   paste("                                   ",qano,
                         "\n                                   ",
                         partic_p)))
} 



plotagens <- mapply(grafpp,anosesf$qano,
                    anosesf$qesf,
                    anosesf$mostraleg,
                    rep(list(estatesf_pano),6),
                    rep("nos gastos totais",6),SIMPLIFY = F)

ggarrange(plotlist = plotagens, ncol = 3, nrow = 2)

estatesf_puni <- estatesf_p%>%
  group_by(ano)%>%
  mutate(partic = unidades/sum(unidades),
         fracao = as.numeric(fracao))%>%
  ungroup()

plot_uni <- mapply(grafpp,anosesf$qano,
                    anosesf$qesf,
                    anosesf$mostraleg,
                    rep(list(estatesf_puni),6),
                    rep("no total de domicílios",6),SIMPLIFY = F)


ggarrange(plotlist = plot_uni, ncol = 3, nrow = 2)


ggarrange(plotlist = list(rm2003,rm2009,rm2018), nrow = 3)


wafp <- function(qano,qesf,mostraleg, 
                    tabela = estatesf_pano, 
                    titulo = "nos gastos totais") {
  p <- tabela %>%
    mutate(fracao=as.numeric(fracao))%>%
    left_join(dicfracoes)%>%
    filter(esfera == qesf & ano == qano)%>%
    arrange(desc(nomefracao))%>%
    mutate(partic=partic)%>%
    rename(`Fração de Classe` = nomefracao)%>%
    arrange(desc(partic))
  print(summary(p$partic))
  partic_p <- percent(sum(p$partic),accuracy = 0.1)
  p <- ggplot(p,aes(values=partic,fill=factor(`Fração de Classe`)))+
  geom_waffle(n_rows = 20, make_proportional = T,size = 2, colour = "white", flip = TRUE)+
    coord_equal() +
    theme_ipsum_rc(grid="") +
    theme_enhance_waffle()+
    scale_fill_brewer(palette="Dark2")+
    theme_void()+
    theme(legend.title = element_blank(),
          legend.position = ifelse(mostraleg == 1,"bottom","none"))+
    ggtitle(ifelse(mostraleg == 1, 
                   paste("Participação de frações",
                         titulo,"da esfera",
                         qesf,"\n\n",
                         qano,"\n",
                         partic_p),
                   paste(qano,
                         "\n",
                         partic_p)))+
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(family="Bitstream Charter"))
    
  p
}
# Pensar como adicionar novamente mais info no waffle    
#     geom_text(aes(y = ypos, x = 1.4, label = 
#                     percent(partic/sum(partic),accuracy = 0.1)),
#               size = 4)+
#     geom_text(aes(y = ypos, x = 1.1, label = paste0(
#       "(",
#       percent(partic,accuracy = 0.1),
#       ")")),size = 3)+
#     guides(fill=guide_legend(nrow=4,byrow=TRUE))+
# } 

plotagens_gw <- mapply(wafp,anosesf$qano,
                    anosesf$qesf,
                    anosesf$mostraleg,
                    rep(list(estatesf_pano),6),
                    rep("nos gastos totais",6),SIMPLIFY = F)

ggarrange(plotlist = plotagens_gw, ncol = 3, nrow = 2)




plotagens_uw <- mapply(wafp,anosesf$qano,
                       anosesf$qesf,
                       anosesf$mostraleg,
                       rep(list(estatesf_puni),6),
                       rep("no total de domicílios",6),SIMPLIFY = F)

ggarrange(plotlist = plotagens_uw, ncol = 3, nrow = 2)
