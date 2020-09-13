# --------------------------------------Bibliotecas------------------------------------

library(purrr)
library(rlang)
library(caret) #confusionMatrix

# --------------------------------------Funciones------------------------------------

ver_variables_con_na <- function(df){
  map_lgl(df, anyNA) %>% 
    which() %>% 
    names()
}

calcular_cant_bins <- function(n) 1 + 3.333 * log10(n)

hacer_barplot_con_dos_cuantitativas <- function(datos, var1, var2, min = 0, max = 1, paso = 0.1){
  
  datos %>% 
    group_by(!!sym(var2), !!sym(var1)) %>% 
    summarise(n = n()) %>% 
    mutate(freq_rel = n / sum(n)) %>% 
    ggplot(aes_string(var1, "freq_rel", group = var2, fill = var2)) +
    geom_bar(alpha = .6, stat = "identity", position =  "dodge2") +
    ylab("") +
    scale_y_continuous(labels = label_percent(), breaks = seq(min, max, paso))
    
}


obtener_resultados_todos_posibles_valores_criticos<- function(valores_reales,
                                                              probabilidades_estimadas,
                                                              signo_negativo = 0, 
                                                              signo_positivo = 1,
                                                              separacion = .01) {
  # Creo los valores críticos
  
  valores_criticos <- seq(0, 1, separacion)
  
  # Llamo iterativamente a obtener_resultados_matriz_confusion con distintos
  # valores críticos
  
  df_resultado <- map_df(valores_criticos, 
                      obtener_resultados_matriz_confusion,
                      valores_reales = valores_reales,
                      probabilidades_estimadas = probabilidades_estimadas,
                      signo_negativo = signo_negativo,
                      signo_positivo = signo_positivo)
  return(df_resultado)
}


obtener_resultados_matriz_confusion <- function(valor_corte = .5,
                                                valores_reales, 
                                                probabilidades_estimadas, 
                                                signo_negativo = 0, 
                                                signo_positivo = 1) {
  
  # Realizo las predicciones con el valor de corte
  
  predicciones <- ifelse(probabilidades_estimadas > valor_corte, signo_positivo, signo_negativo)
  predicciones_factor <- factor(predicciones, levels = levels(valores_reales))
  
  # Obtengo la matriz de confusión
  
  matriz <- confusionMatrix(predicciones_factor, valores_reales)
  
  # Extraigo la información que me interesa
  
  sensibilidad <- matriz$byClass["Sensitivity"]
  especificidad <- matriz$byClass["Specificity"]
  accuracy <- matriz$overall["Accuracy"]
  
  df_resultado <- data.frame("valor_corte" = valor_corte,
                             "sensibilidad" = sensibilidad,
                             "especificidad" = especificidad,
                             "accuracy" = accuracy,
                             row.names = NULL)
  return(df_resultado)
}


calcular_pseudo_R2 <- function(modelo) {
  # Usamos el McFadden pseudo R2
  
  pseudo_R2 <- 1 -(modelo$deviance / modelo$null.deviance)
  p_valor <- pchisq(2*(modelo$deviance - modelo$null.deviance), df = length(modelo$coefficients) - 1)
  return(c("pseudo_R2" = pseudo_R2, "p_valor" = p_valor))
}

medidas_resumen <- function(data, nombre_var, remover_na = FALSE) {
  data_frame_resultado <- data.frame("Variable" = nombre_var,
                                     "Media" = mean(data, na.rm = remover_na),
                                     "Mediana" = median(data, na.rm = remover_na),
                                     "Primer_Cuartil" = quantile(data, probs = .25, na.rm = remover_na),
                                     "Tercer_Cuartil" = quantile(data, probs = .75, na.rm = remover_na),
                                     "Desvio" = sd(data, na.rm = remover_na),
                                     "NAs" = sum(is.na(data)),
                                     row.names = NULL)
  return(data_frame_resultado)
}

