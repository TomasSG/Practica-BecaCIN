# --------------------------------------Bibliotecas------------------------------------

library(purrr)

# --------------------------------------Funciones------------------------------------

ver_variables_con_na <- function(df){
  map_lgl(df, anyNA) %>% 
    which() %>% 
    names()
}



