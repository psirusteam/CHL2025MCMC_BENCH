#' Resumen posterior del benchmarking multinomial de empleo por DAM
#'
#' Calcula el resumen posterior de los resultados obtenidos a partir del
#' benchmarking multinomial iteración-a-iteración en cadenas MCMC, agregando
#' contribuciones por dominio agregado (DAM) y derivando indicadores de empleo.
#'
#' La función resume la distribución posterior de cada indicador mediante:
#' media, desviación estándar e intervalos creíbles simétricos al nivel
#' especificado.
#'
#' @details
#'
#' ## Entrada esperada
#'
#' El objeto `res_bench` debe ser el resultado de la función
#' `benchmark_mcmc_multinomial_bench_dam()` y contener el elemento
#' `resultados_iter`, que es una lista de data frames (uno por iteración MCMC
#' válida). Cada data frame debe incluir, al menos, las siguientes columnas:
#'
#' - `w_bench`: pesos benchmarked por subdominio (dam2),
#' - `O_bench`, `D_bench`, `I_bench`: contribuciones benchmarked por categoría,
#' - una variable que identifique el DAM (por defecto `dam`).
#'
#' ## Agregación por iteración
#'
#' Para cada iteración MCMC s y cada DAM d se calculan:
#'
#'   N_d^(s)  = sum_k w_{k,d}^(s)
#'   O_d^(s)  = sum_k O_{k,d}^(s)
#'   D_d^(s)  = sum_k D_{k,d}^(s)
#'   I_d^(s)  = sum_k I_{k,d}^(s)
#'
#' A partir de estos totales se derivan los indicadores:
#'
#'   TO_d^(s) = O_d^(s) / N_d^(s)
#'   TP_d^(s) = (O_d^(s) + D_d^(s)) / N_d^(s)
#'   TD_d^(s) = D_d^(s) / (O_d^(s) + D_d^(s))
#'
#' Estas cantidades conservan la dependencia posterior inducida por el modelo
#' multinomial y el proceso de benchmarking.
#'
#' ## Resumen posterior
#'
#' Para cada DAM d y cada indicador θ se resume la distribución posterior
#' {θ_d^(s)} mediante:
#'
#' - Media posterior: mean(θ_d)
#' - Desviación estándar posterior: sd(θ_d)
#' - Intervalo creíble simétrico al nivel `ci_level`, definido por los cuantiles:
#'
#'   lci = quantile(θ_d, (1 - ci_level) / 2)
#'   uci = quantile(θ_d, 1 - (1 - ci_level) / 2)
#'
#' Estos intervalos son **intervalos creíbles del modelo**, no intervalos de
#' confianza de diseño.
#'
#' @param res_bench Lista resultante de
#' `benchmark_mcmc_multinomial_bench_dam()`. Debe contener el elemento
#' `resultados_iter`.
#'
#' @param dam_var Nombre de la variable que identifica el dominio agregado (DAM).
#' Por defecto `"dam"`.
#'
#' @param ci_level Nivel del intervalo creíble posterior. Por defecto `0.95`.
#'
#' @return Una lista con dos elementos:
#' \itemize{
#'   \item iter_dam: data frame con resultados agregados por iteración MCMC y DAM,
#'         incluyendo N, O, D, I, TP, TD y TO.
#'   \item resumen: data frame en formato largo con el resumen posterior por DAM
#'         e indicador, que contiene las columnas `mean`, `sd`, `lci` y `uci`.
#' }
#'
#' @note
#' Los intervalos calculados por esta función corresponden a incertidumbre
#' **posterior del modelo**, no a incertidumbre de diseño. Para comparaciones
#' con estimaciones directas, se recomienda utilizar únicamente la media
#' posterior y contrastarla con los intervalos de confianza de diseño.
#'
#' @seealso
#' \code{\link{benchmark_mcmc_multinomial_bench_dam}},
#' \code{\link{quantile}}
#'
#' @export


resumir_bench_empleo_dam <- function(
    res_bench,
    dam_var = "dam",
    ci_level = 0.95
){
  
  library(dplyr)
  library(tidyr)
  
  # Validación de estructura esperada
  stopifnot("resultados_iter" %in% names(res_bench))
  
  alpha <- 1 - ci_level
  
  # ----------------------------------------------------------
  # (1) Agregados por iteración y DAM
  # ----------------------------------------------------------
  # Se agregan contribuciones benchmarked:
  #   N = sum(w_bench)
  #   O = sum(O_bench), D = sum(D_bench), I = sum(I_bench)
  # y se derivan indicadores:
  #   TP = (O + D) / N
  #   TD = D / (O + D)
  #   TO = O / N
  iter_dam <- bind_rows(res_bench$resultados_iter, .id = "iter") %>%
    group_by(iter, .data[[dam_var]]) %>%
    summarise(
      N  = sum(w_bench),
      O  = sum(O_bench),
      D  = sum(D_bench),
      I  = sum(I_bench),
      TP = (O + D) / N,
      TD = D / (O + D),
      TO = O / N,
      .groups = "drop"
    )
  
  # ----------------------------------------------------------
  # (2) Resumen posterior por DAM e indicador
  # ----------------------------------------------------------
  # Se pasa a formato largo y se calculan estadísticos posteriores:
  #   mean, sd, lci, uci (cuantiles simétricos al nivel ci_level)
  resumen <- iter_dam %>%
    pivot_longer(
      cols = c(O, D, I, TP, TD, TO),
      names_to = "variable",
      values_to = "value"
    ) %>%
    group_by(.data[[dam_var]], variable) %>%
    summarise(
      mean = mean(value, na.rm = TRUE),
      sd   = sd(value, na.rm = TRUE),
      lci  = quantile(value, probs = alpha / 2, na.rm = TRUE),
      uci  = quantile(value, probs = 1 - alpha / 2, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(list(
    iter_dam = iter_dam,
    resumen  = resumen
  ))
}
