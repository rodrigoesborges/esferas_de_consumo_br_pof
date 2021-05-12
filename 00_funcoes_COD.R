#Função auxiliar para preparar dicionário a partir da COD 
# fornecido como arquivo XLS separado
require(magrittr)
require(dplyr)
require(readxl)

preparadicod <- function(arq = "sourcedata/Ocupação COD-esferas.xlsx"){
  require(magrittr)
  require(dplyr)
  
  dictionary <- readxl::read_excel(arq)
  colnames(dictionary) <- paste0("X__",1:dim(dictionary)[2])
  dictionary %<>% subset(!is.na(X__4)) %>% select(-1:-3, -6)
  names(dictionary) <- dictionary[1,]
  dictionary <- dictionary[-1,]
  dictionary[1] <- transform(dictionary[1],`Grupo de base` = as.numeric(`Grupo de base`))
  for (i in 3:7) {
    dictionary[[i]] <- as.factor(dictionary[[i]])
  }
  
  dictionary
}


