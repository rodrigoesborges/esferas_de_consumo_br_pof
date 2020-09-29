# pof::download_pof(2003)
# pof::unzip_pof(2003)

# library(lodown)
library(tidyverse)

# Leituras baseadas no lodown
# extract the leitura file containing the sas importation instructions
files <- dir(path = "dados/2003/", recursive = T, full.names = T)
leitura <- files[ grep( 'leitura' , tolower( files ) ) ]

# funcao em 00_funcaoes.R
source("00_funcoes.R")
z <- instrucoes_sas(leitura)

# write the file back to your second temporary file
tf2 <- tempfile()
writeLines( z , tf2 )

# find each line containing the string `INFILE` or `infile`
all.beginlines <- grep( 'INFILE|infile' , z )

# find line start positions
start.pos <- z[ all.beginlines ] %>% 
  gregexpr(pattern = "\\" , fixed = TRUE) %>% 
  lapply(max) %>% 
  unlist()  %>% 
  magrittr::add(1)

# find line end positions
end.pos <- z[ all.beginlines ] %>% 
  gregexpr(pattern = ".txt") %>% 
  unlist() %>% 
  magrittr::subtract(1)

# isolate the names of all data files to be imported..
data.files.to.import <- substr(z[ all.beginlines ],
                               start.pos, end.pos)

all.file.basenames <- basename(files) %>% 
  strsplit('.' , fixed = TRUE) %>% 
  map_chr(`[[` , 1)

# Depois tirar desse loop e separar cada arquivo em uma função
for ( dfn in data.files.to.import ){
  
  if ( tolower( dfn ) == 't_rendimentos' ) {
    data.file <- files[ which( 't_rendimentos1' == tolower( all.file.basenames ) ) ]
  } else {
    data.file <- files[ which( tolower( dfn ) == tolower( all.file.basenames ) ) ]
  }
  
  curfile <- grep(data.file, pattern = "\\.(txt|TXT)$", value = TRUE)
  
  # figure out which beginline position to use
  cur.beginline <- which( tolower( dfn ) == tolower( data.files.to.import ) )
  
  # import the data file into R
  x <- lodown:::read_SAScii(
    curfile, tf2 ,
    beginline = all.beginlines[ cur.beginline ] ,
    skip_decimal_division = TRUE ,
    sas_encoding = "latin1"
  )
  
  names( x ) <- tolower( names( x ) )
  
  # E ai tem que tirar isso e devolver x
  saveRDS( x , glue::glue("dados/2003/{tolower(dfn)}.rds"))
}
