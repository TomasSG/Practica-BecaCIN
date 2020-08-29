# --------------------------------------Bibliotecas------------------------------------

library(purrr)

# --------------------------------------Funciones------------------------------------

ver_variables_con_na <- function(df){
  map_lgl(df, anyNA) %>% 
    which() %>% 
    names()
}

calcular_cant_bins <- function(n) 1 + 3.333 * log10(n)


