#Consumo das Famílias Interno e Importado - a partir da WIOD
library(data.table)


consumobr <- function(ano = 2003, pasta = "sourcedata/wiots/") {
  if (ano > 2014) {ano <- 2014}
  fonte <- paste0(pasta,"WIOT",ano,"_October16_ROW.rds")

consumofam2003 <- readRDS(fonte)

consumobrasfam2003 <- consumofam2003%>%select(IndustryCode,Country,BRA57)

consumointernobr2003 <- consumobrasfam2003%>%filter(Country == "BRA")%>%select(-Country)
names(consumointernobr2003)[2] <- "interno"

consumoimportadobr2003 <- consumobrasfam2003%>%
  filter(Country != "BRA")%>%
  group_by(IndustryCode)%>%
  summarize(importado = sum(BRA57))%>%
  filter(IndustryCode %in% consumointernobr2003$IndustryCode)

ciM <- consumointernobr2003%>%left_join(consumoimportadobr2003)

ciM[ciM == 0] <- 0.0000000000000001

ciM[,2:3] <- as.data.frame(prop.table(as.array(as.matrix(ciM[,-1])),1))

ciM$ano <- ano

ciM
}


consumos <- rbindlist(lapply(c(2003,2009,2018),consumobr))


consumos$IndustryCode <- substr(consumos$IndustryCode,1,3)



tradisic <- read_xls("sourcedata/tradutores/CNAE20_Correspondencia_Cnae20xIsic4.xls",skip=3)[-1,-4]
names(tradisic) <- c("cnae2","desc_cnae","isic4","desc_isic","obs")

tradisic <- tradisic %>%mutate(cnaeag = ifelse(is.na(as.numeric(cnae2)),cnae2,""),
  isicsec = ifelse(is.na(as.numeric(isic4)),isic4,""),
                               )


for (i in 1:nrow(tradisic)) {
  c <- length(tradisic)
  if(tradisic[i,c]==""){
    tradisic[i,c] <- tradisic[i-1,c]
  }
  if(tradisic[i,"isic4"] != tradisic[i,c]) {
  tradisic[i,"isic4"] <- paste0(tradisic[i,c],tradisic[i,"isic4"])
  tradisic[i,"cnaeag"] <- paste0(tradisic[i,c],tradisic[i,"cnae2"])
  }
}

tradisicrs <- tradisic[nchar(tradisic$isic4)< 4,]




consumos <- consumos%>%left_join(tradisicrs,by = c("IndustryCode"= "isic4"))


unique(consumos$cnae2)





gastos_SCN %>% 
  group_by(ano, esfera, cod68, item68x20) %>% 
  summarise(valor = sum(valor * peso)/10^6) %>% 
  pivot_wider(names_from = esfera, values_from = valor) %>% 
  mutate(prop = alta / (alta + baixa)) %>% 
  split(.$ano) %>% 
  map(arrange, desc(prop))
