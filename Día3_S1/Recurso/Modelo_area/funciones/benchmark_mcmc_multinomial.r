#' @title Benchmarking Multinomial por Iteración MCMC
#'
#' @description
#' Aplica benchmarking multinomial para las categorías **Ocupado–Desocupado–Inactivo**
#' sobre cada iteración MCMC generada por un modelo bayesiano multinomial ajustado
#' con `rstan`.  
#' 
#' En cada iteración se:
#' \enumerate{
#'   \item Extraen predicciones de \eqn{\theta^{(l)}} para áreas observadas y no observadas.
#'   \item Construyen matrices de calibración \eqn{X_s} por dominio para Ocupado y Desocupado.
#'   \item Obtienen los factores de calibración \eqn{g_k^{(l)}} mediante el método `sampling::calib()`.
#'   \item Calculan las tasas ajustadas (benchmarked) \eqn{O^{(l)}, D^{(l)}, I^{(l)}} y derivan
#'         tasas laborales coherentes: TD, TO, TP.
#' }
#'
#' Las iteraciones que no logran convergencia en la calibración se descartan automáticamente.
#'
#' @param fit Objeto `stanfit` con parámetros `theta` (áreas observadas) y
#'   `theta_pred` (áreas no observadas).
#' @param theta_obs_ordenado Data frame con predicciones ordenadas de \eqn{\theta}
#'   para áreas observadas (categorías Ocupado, Desocupado, Inactivo y dam2).
#' @param theta_pred_ordenado Data frame con predicciones ordenadas de
#'   \eqn{\theta_{\text{pred}}} para áreas no observadas.
#' @param conteo_pp_dam Data frame con población por dominio `dam2`
#'   incluyendo `N_mpio`.
#' @param Razon_empleo Vector con totales directos por dominio y categoría
#'   (denominadores y numeradores) necesarios para benchmarking.
#' @param method Método de calibración usado en `sampling::calib()`.  
#'   Valores típicos: `"linear"` o `"logit"` (recomendado).
#' @param max_iter Número máximo de iteraciones permitidas para la calibración.
#'
#' @details
#' La función implementa benchmarking resolviendo en cada iteración MCMC:
#' \deqn{
#' \sum_k g_k^{(l)} N_k X_{ks} = T_s
#' }
#' donde:
#' \itemize{
#'   \item \eqn{g_k^{(l)}} son los factores de calibración.
#'   \item \eqn{X_{ks}} es la matriz de restricciones por dominio y categoría.
#'   \item \eqn{T_s} son los totales directos provenientes de encuesta.
#' }
#'
#' Se ajustan de forma independiente las categorías:
#' \itemize{
#'   \item Ocupado
#'   \item Desocupado
#' }
#'
#' La categoría **Inactivo** se obtiene como residuo para garantizar que:
#' \deqn{
#' O^{(l)} + D^{(l)} + I^{(l)} = 1.
#' }
#'
#' @return
#' Una lista con:
#' \itemize{
#'   \item `resultados_iter`: lista de data frames, uno por iteración MCMC válida
#'         (incluye variables benchmarked).
#'   \item `id_dam`: vector con la identificación de cada dominio.
#'   \item `S_total`: número total de iteraciones procesadas.
#'   \item `S_validas`: número de iteraciones que lograron calibración.
#'   \item `method`: método de calibración empleado.
#' }
#'
#' @examples
#' \dontrun{
#' res <- benchmark_mcmc_multinomial(
#'   fit,
#'   theta_obs_ordenado,
#'   theta_pred_ordenado,
#'   conteo_pp_dam,
#'   Razon_empleo,
#'   method = "logit"
#' )
#' }
#'
#' @export


benchmark_mcmc_multinomial <- function(
    fit,
    theta_obs_ordenado,
    theta_pred_ordenado,
    conteo_pp_dam,
    Razon_empleo,
    method = "logit",
    max_iter = 5000
){
  
  library(dplyr)
  library(purrr)
  library(fastDummies)
  library(sampling)
  
  message("Extrayendo cadenas MCMC...")
  
  # Identificaciones de dominios
  id_dam <- bind_rows(theta_obs_ordenado, theta_pred_ordenado) %>% pull(dam2)
  
  # Número de categorías y DAM observados
  P <- 3
  D <- nrow(theta_obs_ordenado)
  
  # Extraer draws
  theta_draws      <- rstan::extract(fit, pars = "theta")$theta
  theta_pred_draws <- rstan::extract(fit, pars = "theta_pred")$theta_pred
  
  S <- nrow(theta_draws)
  # S  <- 500
  message("Número de iteraciones MCMC: ", S)
  
  # Lista de resultados
  resultados <- vector("list", S)
  
  # Loop por iteración MCMC
  for(iter in 1:S){
    
    # 1. Reorganizar θ_obs y θ_pred
    theta_obs_i  <- theta_draws[iter, , ] %>% as.data.frame()
    theta_pred_i <- theta_pred_draws[iter, , ] %>% as.data.frame()
    
    est_i <- bind_rows(theta_obs_i, theta_pred_i)
    colnames(est_i) <- c("Ocupado_pred", "Desocupado_pred", "Inactivo_pred")
    
    est_i$dam2 <- id_dam
    
    # Unir población
    est_i <- est_i %>% 
      left_join(conteo_pp_dam, by = "dam2") %>%
      mutate(dam = substr(dam2, 1, 2))
    
    # Crear dummies
    X_dam <- dummy_cols(est_i$dam,
                        remove_selected_columns = TRUE) %>% as.matrix()
    
    # ------------------------------------------------------------------
    # MATRIZ PARA DESOCUPADOS
    # ------------------------------------------------------------------
    Xs_D <- cbind(
      X_dam,
      X_dam * est_i$Desocupado_pred
    )
    
    colnames(Xs_D) <- grep(
      x = names(Razon_empleo),
      pattern = "(Den$|Desocupado)",
      value = TRUE
    )
    dir_Desocupado <- Razon_empleo[colnames(Xs_D)]
    
    # ------------------------------------------------------------------
    # MATRIZ PARA OCUPADOS
    # ------------------------------------------------------------------
    Xs_O <- cbind(
      X_dam,
      X_dam * est_i$Ocupado_pred
    )
    
    colnames(Xs_O) <- grep(
      x = names(Razon_empleo),
      pattern = "(Den$|tot_Ocupado)",
      value = TRUE
    )
    dir_Ocupado <- Razon_empleo[colnames(Xs_O)]
    
    
    # ------------------------------------------------------------------
    # CALIBRACIÓN POR ITERACIÓN
    # ------------------------------------------------------------------
    
    gk_desocupado <- tryCatch(
      calib(
        Xs = Xs_D,
        d = est_i$N_mpio,
        total = dir_Desocupado,
        method = method,
        max_iter = max_iter
      ),
      error = function(e) return(NA)
    )
    
    gk_ocupado <- tryCatch(
      calib(
        Xs = Xs_O,
        d = est_i$N_mpio,
        total = dir_Ocupado,
        method = method,
        max_iter = max_iter
      ),
      error = function(e) return(NA)
    )
    
    # Si falla alguna calibración, saltar la iteración
    if(any(is.na(gk_desocupado)) | any(is.na(gk_ocupado))|
       any(is.null(gk_desocupado)) | any(is.null(gk_ocupado))){
      resultados[[iter]] <- NULL
      next
    }
    
    # ------------------------------------------------------------------
    # Tasas ajustadas
    # ------------------------------------------------------------------
    est_i <- est_i %>%
      mutate(
        gk_ocupado    = gk_ocupado,
        gk_desocupado = gk_desocupado,
        Ocupado_Bench    = Ocupado_pred    * gk_ocupado,
        Desocupado_Bench = Desocupado_pred * gk_desocupado,
        Inactivo_Bench   = 1 - (Ocupado_Bench + Desocupado_Bench), 
        TP_pred = (Ocupado_pred  + Desocupado_pred )/(Ocupado_pred  + Desocupado_pred + Inactivo_pred ),
        TD_pred = Desocupado_pred /(Ocupado_pred  + Desocupado_pred ),
        TO_pred =  TP_pred*(1-TD_pred),
        TP_Bench = (Ocupado_Bench  + Desocupado_Bench )/(Ocupado_Bench  + Desocupado_Bench + Inactivo_Bench ),
        TD_Bench = Desocupado_Bench /(Ocupado_Bench  + Desocupado_Bench ),
        TO_Bench = TP_Bench*(1-TD_Bench)
      )
 
    resultados[[iter]] <- est_i
  }
  
  # Mantener solo iteraciones válidas
  resultados_final <- resultados[!sapply(resultados, is.null)]
  
  message("Iteraciones válidas: ", length(resultados_final), " de ", S)
  
  return(list(
    resultados_iter = resultados_final,
    id_dam = id_dam,
    S_total = S,
    S_validas = length(resultados_final),
    method = method
  ))
}

#' @title Resumen Posterior del Benchmarking Multinomial
#'
#' @description
#' Resume las distribuciones posteriores generadas por la función
#' `benchmark_mcmc_multinomial()`, calculando:
#' \itemize{
#'   \item estimador puntual posterior (media)
#'   \item desviación estándar
#'   \item coeficiente de variación porcentual (CVE)
#'   \item intervalo creíble al nivel deseado
#' }
#'
#' @param res_bench Objeto retornado por `benchmark_mcmc_multinomial()`.
#' @param group_vars Variables por las cuales agregar resultados
#'   (por defecto `dam`).
#' @param vars Nombres de las variables benchmarked a resumir
#'   (por defecto tasas multinomiales ajustadas).
#' @param ci_level Nivel del intervalo creíble (default = 0.95).
#'
#' @details
#' La función transforma la lista de resultados MCMC (una por iteración)
#' a formato largo y calcula los estadísticos posteriores por dominio
#' y categoría.  
#'
#' La salida es equivalente al resumen típico:
#' \deqn{
#' \hat{\theta}_d = E(\theta_d \mid \text{datos})
#' }
#' con su correspondiente incertidumbre:
#' \itemize{
#'   \item SD posterior
#'   \item Intervalos creíbles percentiles 2.5%–97.5%.
#' }
#'
#' @return
#' Un data frame con:
#' \itemize{
#'   \item dominio (`dam`)
#'   \item categoría (Ocupado, Desocupado, Inactivo)
#'   \item estimate: media posterior
#'   \item sd: desviación estándar
#'   \item cve: coeficiente de variación
#'   \item lci, uci: intervalo creíble
#' }
#'
#' @examples
#' \dontrun{
#' resumen <- resumir_bench_multinomial(res, group_vars = "dam")
#' }
#'
#' @export




resumir_bench_multinomial <- function(
    res_bench,
    group_vars = c("dam"),
    vars = "Ocupado_pred",
    n = "N_mpio",
    gk = "gk_ocupado",
    ci_level = 0.95
){
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(rlang)
  
  resultados_iter <- res_bench$resultados_iter
  
  if (length(resultados_iter) == 0) {
    stop("No hay iteraciones válidas en 'res_bench$resultados_iter'.")
  }
  
  alpha <- 1 - ci_level
  
  # Si la columna gk no existe en las iteraciones, crearla = 1
  resultados_iter <- map(resultados_iter, function(df){
    if (!(gk %in% names(df))) {
      df <- df %>% mutate(!!gk := 1)
    }
    df
  })
  
  # Convertir a formato largo: agregar ID de iteración
  draws_long <- map2_df(
    resultados_iter,
    .y = seq_along(resultados_iter),
    ~ .x %>%
      mutate(iter = .y) %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(
        value = weighted.mean(
          .data[[vars]],
          .data[[n]] * .data[[gk]],
          na.rm = TRUE
        ),
        categoria = vars,
        iter = unique(iter),
        .groups = "drop"
      )
  )
  
  # Resumen final por dominio y categoría multivariada
  resumen <- 
    draws_long %>%
    group_by(across(all_of(c(group_vars, "categoria")))) %>%
    summarise(
      estimate = mean(value, na.rm = TRUE),
      sd       = sd(value,   na.rm = TRUE),
      cve      = 100 * sd / estimate,
      lci      = quantile(value, probs = alpha/2,     na.rm = TRUE),
      uci      = quantile(value, probs = 1 - alpha/2, na.rm = TRUE),
      .groups  = "drop"
    )
  
  return(resumen)
}


