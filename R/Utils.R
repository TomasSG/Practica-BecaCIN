# --------------------------------------Bibliotecas------------------------------------

library(purrr)
library(rlang)

# --------------------------------------Funciones------------------------------------

ver_variables_con_na <- function(df){
  map_lgl(df, anyNA) %>% 
    which() %>% 
    names()
}

calcular_cant_bins <- function(n) 1 + 3.333 * log10(n)

hacer_barplot_con_dos_cuantitativas <- function(datos, var1, var2){
  
  datos %>% 
    group_by(!!sym(var2), !!sym(var1)) %>% 
    summarise(n = n()) %>% 
    mutate(freq_rel = n / sum(n)) %>% 
    ggplot(aes_string(var1, "freq_rel", group = var2, fill = var2)) +
    geom_bar(alpha = .6, stat = "identity", position =  "dodge2")
    
}

