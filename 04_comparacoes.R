library(tidyverse)
# Os arquivos lidos abaixo são gerados nos scripts 01, 02 e 03
gastos2003 <- readr::read_csv("gastos_esferas_2003.csv", 
                              col_types = "cccdd")
gastos2009 <- readr::read_csv("gastos_esferas_2009.csv", 
                              col_types = "ccdcd")
gastos2018 <- readr::read_csv("gastos_esferas_2018.csv", 
                              col_types = "ccdcd")

gastos <- gastos2003 %>% 
  rename(valor = despmes) %>% 
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
            nudge_y = c(rep(c(7, -7, 7), times = 2), rep(7, 3))) + 
  scale_x_continuous(breaks = c(2003, 2009, 2018)) + 
  theme_classic() +
  labs(x = "", y = "Participação no total (%)",
       title = "Esferas de consumo", 
       subtitle = "POFs de 2003, 2009 e 2018")

ggsave("relativo.png")

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
            peso_final = first(peso_final)) %>% 
  summarise(massa_valor = sum(massa * peso_final),
            media_pond = weighted.mean(massa, peso_final), 
            unidades = sum(peso_final))

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
            nudge_y = rep(c(200, -200, 200), times = 2)) + 
  scale_x_continuous(breaks = c(2003, 2009, 2018)) + 
  theme_classic() +
  labs(x = "", y = "Estimativa (R$ 2018, bilhões)",
       title = "Esferas de consumo", 
       subtitle = "POFs de 2003, 2009 e 2018")

ggsave("massas.png")

ggplot(estat_defla, aes(ano, media_pond, col = esfera)) + 
  geom_line(size = 1) + 
  geom_point(size = 2, col = "black") + 
  geom_text(aes(label = round(media_pond) %>% prettyNum(".", decimal.mark = ",")),
            nudge_y = rep(c(-500, 500), times = 3),
            nudge_x = rep(c(0.3, 0, -0.3), each = 2)) + 
  scale_x_continuous(breaks = c(2003, 2009, 2018)) + 
  theme_classic() +
  labs(x = "", y = "Renda média (R$ 2018)",
       title = "Esferas de consumo", 
       subtitle = "POFs de 2003, 2009 e 2018")

ggsave("media.png")

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

ggsave("unidades.png")
