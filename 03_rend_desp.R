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
rendas2003 <- ler_rendimentos2003()

# Etapa 2 -----------------------------------------------------------
rendas_classificadas2003 <- classificar_rendimentos2003(rendas2003)

rendas_ucs2003 <- rendas_classificadas2003 %>%
  group_by(cod_uc, forma) %>%
  summarise(renda = sum(recmes)) %>%
  pivot_wider(names_from = forma, values_from = renda, 
              values_fill = list(renda = 0)) %>%
  mutate(total = cv + mv,
         p_cv = (cv + 0.01)/ (total + 0.01)) %>%
  ungroup()

# K-Means usadas para estabelecer corte alta/baixa
k_rendas <- kmeans(rendas_ucs2003$p_cv, c(0.2, 0.5))
k_rendas$centers

ggplot(rendas_ucs2003, aes(p_cv, total, col = factor(k_rendas$cluster))) +
  geom_point(shape = ".", alpha = 0.5) +
  geom_vline(xintercept = 0.61, lty = 2) +
  scale_y_continuous(limits = c(0, 20e3)) +
  theme_classic() +
  theme(legend.position = "none")

# A análise acima propor o corte de 61% para esfera alta/baixa
corte <- 0.95

# Etapa 3 -----------------------------------------------------------
rendas_esferas2003 <- rendas_ucs2003 %>%
  mutate(esfera = ifelse(p_cv > corte, "baixa", "alta"))

esferas_ucs2003 <- rendas_esferas2003 %>%
  select(cod_uc, esfera)

# Etapa 4 -----------------------------------------------------------
despesas_esferas2003 <- ler_despesas2003() %>%
  left_join(esferas_ucs2003, by = "cod_uc") %>% 
  select(cod_uc, codigo, esfera, despmes)
  # sem níveis nessa versão
  # select(cod_uc, codigo, nivel_0:esfera)

# Estimativa geral esferas
despesas_esferas2003 %>%
  group_by(esfera) %>%
  summarise(soma = sum(despmes) * 12 / 1e9) %>% # Em bilhões
  filter(!is.na(esfera)) %>% 
  mutate(partic = soma / sum(soma), tx_mv = soma / last(soma))

despesas_esferas2003 %>% 
  mutate(ano = 2003) %>%
  write_csv("gastos_esferas_2003.csv")
