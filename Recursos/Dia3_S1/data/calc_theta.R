library(dplyr)
library(purrr)
library(rlang)
library(vctrs)

calc_theta <- function(result_list, levels = NULL, ci_level = 0.95) {
  
  # Validación de la lista
  if (!is.list(result_list)) {
    stop("El argumento 'result_list' debe ser una lista.")
  }
  
  # Extraer primer elemento no nulo
  example_df <- result_list %>% compact() %>% pluck(1)
  if (is.null(example_df)) stop("La lista está vacía o todos los elementos son NULL.")
  
  # Validar niveles de agregación
  if (!is.null(levels)) {
    missing_vars <- setdiff(levels, names(example_df))
    if (length(missing_vars) > 0) {
      stop("Las siguientes variables no existen en los datos: ",
           paste(missing_vars, collapse = ", "))
    }
  }
  
  # Función auxiliar: calcula theta para cada iteración
  summarise_iter <- function(df) {
    if (is.null(df)) return(NULL)
    if (is.null(levels)) {
      df %>% summarise(theta = weighted.mean(yk, n), .groups = "drop")
    } else {
      df %>%
        group_by(across(all_of(levels))) %>%
        summarise(theta = weighted.mean(yk, n), .groups = "drop")
    }
  }
  
  # Procesar todas las iteraciones (más rápido con vec_rbind)
  res <- map(result_list, summarise_iter) %>% bind_rows()
  
  # Calcular estadísticos resumen
  if (is.null(levels)) {
    res_summary <- res %>%
      summarise(
        estimate = mean(theta),
        sd = sd(theta),
        lci = quantile(theta, probs = (1 - ci_level)/2),
        uci = quantile(theta, probs = 1 - (1 - ci_level)/2)
      )
  } else {
    res_summary <- res %>%
      group_by(across(all_of(levels))) %>%
      summarise(
        estimate = mean(theta),
        sd = sd(theta),
        lci = quantile(theta, probs = (1 - ci_level)/2),
        uci = quantile(theta, probs = 1 - (1 - ci_level)/2),
        .groups = "drop"
      )
  }
  
  return(list(
    estimates = res_summary,
    draws = res
  ))
}


