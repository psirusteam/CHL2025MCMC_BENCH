library(dplyr)
library(purrr)
library(sampling)   # calib()
library(parallel)

#-------------------------------------------------------------
# f_bench_por_dam
#-------------------------------------------------------------
# Benchmarking iterativo por dominio (DAM2) sobre draws MCMC.
#
# Args:
#   temp_draws     : data.frame / tibble con columnas:
#                    - una columna inicial de identificador (p.ej. "dam2" o "draw")
#                    - columnas de iteraciones: iter_1, iter_2, ..., iter_M
#   X_dam          : matriz de indicadores por dominio (one-hot por DAM),
#                    de dimensión (n_unidades x D)
#   totales        : vector nombrado de totales (denominadores + numeradores),
#                    en el mismo orden que las columnas de Xs
#   estimacionesPre: data.frame con:
#                    - dam2: identificador del dominio
#                    - estimacion_normal: predictor base (sin benchmarking)
#                    - total_pp: total poblacional por unidad / dominio
#   n_iter         : número de iteraciones MCMC a usar (por defecto 50).
#                    Se truncará a la cantidad de columnas disponibles.
#   calib_method   : método de calibración para calib(), por defecto "linear"
#                    (puede ser "linear", "raking", "logit", etc.)
#   parallel       : lógico. Si TRUE, paraleliza el loop sobre iteraciones.
#   n_cores        : número de núcleos a usar si parallel = TRUE.
#                    Si es NULL, usa detectCores() - 1.
#
# Returns:
#   Lista con:
#     - draws   : data.frame largo con y_bench por iter y dam2
#     - resumen : resumen por dam2 (mean, sd, lci, uci, cve)
#
# Notas:
#   - Las iteraciones con errores en calib() se reportan por warning
#     y se omiten del resumen.
#   - Se generan advertencias si:
#       * hay draws negativos en alguna iteración;
#       * hay valores calibrados negativos (y_bench < 0).
#-------------------------------------------------------------
Benchmarking_area <- function(temp_draws,
                            X_dam,
                            totales,
                            estimacionesPre,
                            n_iter       = 50,
                            calib_method = "linear",
                            parallel     = FALSE,
                            n_cores      = NULL) {
  #-----------------------------------------------------------
  # 0. Validaciones básicas
  #-----------------------------------------------------------
  if (!is.data.frame(temp_draws)) {
    stop("temp_draws debe ser un data.frame o tibble.")
  }
  if (ncol(temp_draws) < 2) {
    stop("temp_draws debe tener al menos una columna de iteraciones además del identificador.")
  }
  if (!all(c("dam2", "estimacion_normal", "total_pp") %in% names(estimacionesPre))) {
    stop("estimacionesPre debe contener las columnas: 'dam2', 'estimacion_normal', 'total_pp'.")
  }
  
  # Columnas de iteraciones (se asume que la primera no es un draw)
  iter_cols <- colnames(temp_draws)[-1]
  n_iter_max <- length(iter_cols)
  
  if (n_iter > n_iter_max) {
    warning(
      "n_iter (", n_iter, 
      ") es mayor que el número de columnas de iteraciones (", n_iter_max, 
      "). Se usará n_iter = ", n_iter_max, "."
    )
    n_iter <- n_iter_max
  }
  
  # Índices de iteraciones efectivas
  iter_idx <- seq_len(n_iter)
  
  #-----------------------------------------------------------
  # 1. Función interna para procesar UNA iteración
  #-----------------------------------------------------------
  procesar_iter <- function(i) {
    iter_name <- iter_cols[i]
    
    # Verificar draws negativos en esta iteración
    if (any(temp_draws[[iter_name]] < 0, na.rm = TRUE)) {
      warning("Se detectaron valores negativos en 'temp_draws[[", iter_name, "]]'. Iteración: ", i)
    }
    
    # Construir matriz Xs por iteración
    Xs <- cbind(
      Den_DAM = X_dam,
      Num_DAM = X_dam * temp_draws[[iter_name]]
    )
    colnames(Xs) <- names(totales)
    
    # Intentar calibración, capturando posibles errores
    gk <- try(
      calib(
        Xs    = Xs,
        total = totales,
        d     = estimacionesPre$total_pp,
        method = calib_method
      ),
      silent = TRUE
    )
    
    if (inherits(gk, "try-error")) {
      warning("Fallo calib() en la iteración ", i, 
              " (", iter_name, "). Se omite esta iteración.")
      return(NULL)
    }
    
    if(round(sum(colSums(gk * estimacionesPre$total_pp * Xs) - totales), 5) < 0.000001) {
      cat(iter_name, "\n")
    }
    
   
    
    data.frame(
      iter    = i,
      dam2    = estimacionesPre$dam2,
      n    = estimacionesPre$total_pp,
      yk = temp_draws[[iter_name]], 
      gk = gk
    ) %>% mutate(y_bench =gk*yk)
  }
  
  #-----------------------------------------------------------
  # 2. Aplicar función a todas las iteraciones (paralelo / secuencial)
  #-----------------------------------------------------------
  if (parallel) {
    if (is.null(n_cores)) {
      n_cores <- max(1, parallel::detectCores() - 1)
    }
    
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    
    # Exportar objetos necesarios al cluster
    parallel::clusterExport(
      cl,
      varlist = c("iter_cols", "temp_draws", "X_dam", "totales", 
                  "estimacionesPre", "calib_method", "procesar_iter"),
      envir = environment()
    )
    
    lista_res <- parallel::parLapply(cl, iter_idx, procesar_iter)
    
  } else {
    lista_res <- lapply(iter_idx, procesar_iter)
  }
  
  # Filtrar iteraciones fallidas (NULL)
  lista_res <- lista_res[!vapply(lista_res, is.null, logical(1))]
  
  if (length(lista_res) == 0) {
    stop("Todas las iteraciones fallaron en la calibración. No hay resultados para resumir.")
  }

  #-----------------------------------------------------------
  # 3. Combinar iteraciones
  #-----------------------------------------------------------
  df_all <- bind_rows(lista_res) 
  
  #-----------------------------------------------------------
  # 4. Resumen por DAM
  #-----------------------------------------------------------
  resumen <- df_all %>%
    group_by(dam2) %>%
    summarise(
      mean = mean(y_bench, na.rm = TRUE),
      sd   = sd(y_bench, na.rm = TRUE),
      lci  = quantile(y_bench, 0.025, na.rm = TRUE),
      uci  = quantile(y_bench, 0.975, na.rm = TRUE),
      cve  = (sd / mean)*100,
      .groups = "drop"
    )
  
  list(
    draws   = df_all,
    resumen = resumen
  )
}
