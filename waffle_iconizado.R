##
library(tidyverse)
library(ggpubr)
library(hrbrthemes)
#detools::install_github("hrbrmstr/waffle")
library(waffle)
library(scales)
library(magrittr)
library(extrafont)

#dicfracoes

dicfracoes <- tribble(
  ~fracao,~nomefracao,
  1, "proletários privados",
  2, "assalariado comércio/finanças, subproletários e demais",
  3, "demais assalariado privados e subproletários",
  4, "aristocracia operária",
  5, "média e grande burguesia",
  6, "pequeno burguesia n. proprietária (capas médias/burocracia)",
  7, "pequeno burguesia proprietária (liberais?médicos etc)",
  8, "pequeno-média burguesia proprietária de K",
  9, "população marginalizada",
  10, "base das forças repressivas",
  11, "alto escalão das forças repressivas",
  12, "trabalhador aposentado",
  13, "aristocracia operária ou pequeno burguesia aposentada",
  14, "altíssimo escalão burocrático"
)


fracoes_no_total <- read_csv2("resultados/resultados_fracoes.csv")

names(fracoes_no_total)[2:3] <- c("fracao_sigla","partic") 


esferas_no_total <- read_csv2("resultados/resultados_esferas.csv")

esferas_no_total <- bind_rows(esferas_no_total,esferas_no_total%>%mutate(esfera = "alta",part_gasto = 100-part_gasto))

dicfracsigla <- data.frame(fracao_sigla = unique(fracoes_no_total$fracao_sigla),
                           fracao = dicfracoes$fracao[c(2,1,12,9,8,10,5,4,6,13,7,14,11)],
                           sigla = c("shopping-cart","hard-hat","blind",
                                     "users","biking","rocket",
                                     "dollar-sign","user-tie","id-badge","blind",
                                     "chalkboard-teacher","landmark","fighter-jet"),
                           esfera = c(rep("baixa",6),rep("alta",7)))


dicfracsigla <- dicfracoes%>%left_join(dicfracsigla)

dicfracsigla[8,2] <- "micro-pequeno burguesia proprietária de K"

wafpfa <- function(qano,qesf,mostraleg, 
                 tabela = fracoes_no_total, 
                 titulo = "nos gastos totais") {
  p <- tabela %>%
    left_join(dicfracsigla)%>%
    filter(esfera == qesf & ano == qano)%>%
    arrange(sigla)%>%
    mutate(partic=partic)%>%
    mutate(`Fração de Classe` = factor(nomefracao,levels = nomefracao, ordered = F))
  print(p$sigla)
  #print(p)
  siglas <- factor(p$sigla,levels = p$sigla,ordered = F)
  partic_p <- (esferas_no_total%>%filter(esfera == qesf,ano == qano))$part_gasto
  p%<>%select(`Fração de Classe`,partic,sigla,fracao)
  p$partic <- round(p$partic/2,0)
  p$`Fração de Classe` <- gsub("([^ ]* [^ ]*) ","\\1\n",p$`Fração de Classe`)
  #  siglas <- p$sigla
  #  nompe <- p$`Fração de Classe`
  # p <- p$partic
  #names(p$partic) <- p$`Fração de Classe`
  # p <- ggplot(p,aes(values=partic,fill=factor(`Fração de Classe`)))+
  p <- waffle(p,rows = 5, size = 2,pad = 1,glyph_size = 5, colors = gray.colors(ifelse(qesf == "baixa",6,7),0.1,0.9),
              flip = F, use_glyph = siglas, glyph_font = "FontAwesome",keep = F,
              glyph_font_family = "FontAwesome5Free-Solid",
              legend_pos = "bottom")+
    #  theme_enhance_waffle()+
    #  theme_void()+
    theme(legend.title = element_blank(),
          legend.position = ifelse(mostraleg == 1,"bottom","none"),
          legend.text = element_text(size=12))+
    ggtitle(ifelse(mostraleg == 1, 
                   paste("Participação de frações",
                         titulo,"da esfera",
                         qesf,"\n\n\n",
                         qano,"\n",
                         partic_p,"%"),
                   paste(qano,
                         "\n",
                         partic_p,"%")))+
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(family="Lato Thin"))
  
  p
}

anos <- c(2003,2009,2018)

esfs <- c("baixa","alta")

anosesf <- expand.grid(anos,esfs)

names(anosesf) <- c("qano","qesf")
anosesf$mostraleg <- rep(c(0,1,0),2)




plotagens_gwfa <- mapply(wafpfa,anosesf$qano,
                       anosesf$qesf,
                       anosesf$mostraleg,
                       rep(list(fracoes_no_total),6),
                       rep("nos gastos totais",6),SIMPLIFY = F)

plotagemw <- ggarrange(plotlist = plotagens_gwfa, ncol = 3, nrow = 2)

plotagemw

ggsave("resultados/fracoes_no_gasto.jpg",plotagemw,dpi=300,width=17,units="cm")
