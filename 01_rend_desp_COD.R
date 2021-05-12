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
source("00_funcoes.R")
# Etapa 1 -----------------------------------------------------------
rendas2018 <- ler_rendimentos2018()

km <- rendas2018 %>% 
  group_by(cod_uc) %>% 
  summarise(peso_final = first(peso_final),
            valor = sum(valor_mensal)) %>% 
  pull(valor) %>% 
  kmeans(3)

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
rendas_classificadas <- classificar_rendimentos(rendas2018,cod = T)
names(rendas_classificadas)[18] <- "forma"

rendas_ucs <- rendas_classificadas %>%
  group_by(cod_uc, forma) %>%
  summarise(renda = sum(valor_mensal)) %>%
  pivot_wider(names_from = forma, values_from = renda, 
              values_fill = list(renda = 0)) %>%
  mutate(total = alta + baixa,
         p_cv = case_when(total == 0 ~ 1,total > 0 ~ baixa/total)) %>%
  ungroup()

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
corte <- 0.95

# Etapa 3 -----------------------------------------------------------
rendas_esferas <- rendas_ucs %>%
  mutate(esfera = ifelse(p_cv > corte, "baixa", "alta"))
  
rendas_esferas %>% 
  group_by(esfera) %>% 
  summarise(v= sum(total), n =  n()) %>% 
  mutate(tx = v/last(v), p = n / sum(n))

esferas_ucs <- rendas_esferas %>%
  select(cod_uc, esfera)

# Etapa 4 -----------------------------------------------------------
despesas_esferas <- ler_despesas2018() %>%
  left_join(esferas_ucs, by = "cod_uc") %>% 
  select(cod_uc, codigo, nivel_0:esfera)

# Estimativa geral esferas
despesas_esferas %>%
  group_by(esfera) %>%
  summarise(soma = sum(valor) * 12 / 1e9) %>% # Em bilhões
  mutate(partic = soma / sum(soma))

# Estimativa um produto: livros (cod: 110803)
despesas_esferas %>% 
  group_by(nivel = nivel_4, esfera) %>%
  summarise(soma = sum(valor) * 12 / 1e6)  %>%
  filter(nivel == 110803) %>%
  mutate(partic = soma / sum(soma))

despesas_esferas %>% 
  mutate(ano = 2018) %>% 
  select(-starts_with("nivel")) %>% 
  write_csv("gastos_esferas_2018.csv")
