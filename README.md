
<!-- README.md is generated from README.Rmd. Please edit that file -->

> \!\!Trabalho em progresso\!\!

# 2020-09-esferas

<!-- badges: start -->

<!-- badges: end -->

Repositório destinado e servir de memória de cálculo para capítulo sobre
o desenvolvimento das esferas alta e baixa durante esse século, paseado
nas POFs de 2002/3, 2008/9 e 2017/8

Até o momento o projeto está organizado assim:

> ./  
> |  
> |— 00\_funcoes.R  
> |— 01\_rend\_resp.R  
> |— 02\_rend\_resp.R  
> |— 03\_rend\_resp.R  
> |— 04\_comparacoes.R  
> |— …  
> |— 96\_download\_leitura\_2003.R  
> |— 97\_pof2003.R  
> |— 98\_download\_leitura\_2009.R  
> |— 99\_pof2009.R

Os scripts estão numerados na ordem em que devem ser rodados na verdade
o `96` e o `98` tem que ser rodados antes do `02` e `03`.

``` r
# Isso não é boa prática, depois transformamos em pacote
source("00_funcoes.R")
# Funções carregadas
ls()
#> [1] "classificar_rendimentos" "instrucoes_sas"         
#> [3] "ler_despesas2018"        "ler_rendimentos2018"    
#> [5] "subst_na"

t0 <- Sys.time()
source("01_rend_desp.R")
Sys.time() - t0
#> Time difference of 1.864457 mins
# tabelas carregadas
ls()
#>  [1] "classificar_rendimentos" "corte"                  
#>  [3] "despesas_esferas"        "esferas_ucs"            
#>  [5] "instrucoes_sas"          "ler_despesas2018"       
#>  [7] "ler_rendimentos2018"     "rendas_classificadas"   
#>  [9] "rendas_esferas"          "rendas_ucs"             
#> [11] "rendas2018"              "subst_na"               
#> [13] "t0"
```

Ainda tem alguns arquivos ao final (98 e 99) em caráter preliminar (até
por isso essa numeração). O que eles fazem é reproduzir o trabalho de
2016.

# Resultados preliminares

## Participação relativa das esferas

![](relativo.png)

## Massa dos valores das esferas (em bilhões de R$ de 2018)

![](massas.png)

## Renda média das unidades (em R$ de 2018)

![](media.png)

## Quantidade de famílias (em milhões de unidades)

![](unidades.png)
