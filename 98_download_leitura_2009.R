pof::download_pof(2009)
pof::unzip_pof(2009)

# unzip_warn_fail <- function( ... ) tryCatch( { unzip( ... ) } , warning = function( w ) stop( conditionMessage( w ) ) )
library(lodown)

# Leituras baseadas no lodown
# extract the leitura file containing the sas importation instructions
files <- dir(path = "dados/2009/", recursive = T, full.names = T)
leitura <- files[ grep( 'leitura' , tolower( files ) ) ]

leitura_con <- file( leitura , encoding = 'windows-1252' )

z <- readLines( leitura_con ) %>% 
  str_replace_all( "\t" , " " )

# remove lines containing the `if reg=__ then do;` pattern
z <- z[ !grepl( 'if reg=.* then do;' , z ) ] %>% 
  str_replace_all( "@;" , "") %>% 
  str_replace_all( "/;" , "/")

# remove lines containing solely `input`
z <- z[ !( tolower( z ) == 'input' ) ]

# remove the (SAScii-breaking) overlapping `controle` columns
z <- z[ !grepl( "@3 controle 6." , z , fixed = TRUE ) ]

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
  lapply('[[' , 1) %>% 
  unlist()

# Depois tirar desse loop e separar cada arquivo em uma função
for ( dfn in data.files.to.import ){
  
  if ( tolower( dfn ) == 't_rendimentos' ) {
    data.file <- files[ which( 't_rendimentos1' == tolower( all.file.basenames ) ) ]
  } else {
    data.file <- files[ which( tolower( dfn ) == tolower( all.file.basenames ) ) ]
  }
  
  curfile <- grep(data.file, pattern = "\\.txt$", value = TRUE)
  
  # figure out which beginline position to use
  cur.beginline <- which( tolower( dfn ) == tolower( data.files.to.import ) )
  
  # import the data file into R
  x <- read_SAScii(
    curfile,
    tf2 ,
    beginline = all.beginlines[ cur.beginline ] ,
    skip_decimal_division = TRUE ,
    sas_encoding = "latin1"
  )
  
  names( x ) <- tolower( names( x ) )
  
  # E ai tem que tirar isso e devolver x
  saveRDS( x , glue::glue("dados/2009/Dados/{tolower(dfn)}.rds"))
}
