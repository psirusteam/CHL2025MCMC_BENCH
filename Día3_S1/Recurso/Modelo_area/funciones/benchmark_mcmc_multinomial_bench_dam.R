#' Benchmarking multinomial iteración-a-iteración en cadenas MCMC por DAM
#'
#' Aplica benchmarking exacto a probabilidades multinomiales (Ocupado–Desocupado–
#' Inactivo) generadas por un modelo bayesiano estimado vía MCMC, forzando que los
#' agregados por dominio (DAM) coincidan exactamente con los totales directos de
#' encuesta para población total (N), ocupados (O) y desocupados (D).
#'
#' El procedimiento se ejecuta iteración por iteración sobre la cadena MCMC,
#' preservando la incertidumbre posterior del modelo. Las iteraciones que no
#' cumplen condiciones de coherencia o estabilidad numérica son descartadas.
#'
#' @details
#' ## Notación
#'
#' Sea:
#' \itemize{
#'   \item d = 1, ..., D: dominios agregados (DAM).
#'   \item k = 1, ..., K: subdominios o post-estratos (por ejemplo, `dam2`).
#'   \item c ∈ {O, D, I}: categorías multinomiales (ocupado, desocupado, inactivo).
#'   \item N_k: tamaño poblacional del subdominio k (`N_mpio`).
#'   \item p_{k,c}^{(s)}: probabilidad multinomial estimada por el modelo en la
#'         iteración MCMC s.
#'   \item T_{d,c}^{DIR}: total directo de encuesta para la categoría c en el DAM d.
#' }
#'
#' ## Objetivo del benchmarking
#'
#' Para cada iteración MCMC s, se buscan factores de calibración g_k^{(s)} tales que
#' los pesos ajustados
#'
#'   w_k^{(s)} = N_k * g_k^{(s)}
#'
#' satisfagan, para todo DAM d:
#'
#'   sum_{k ∈ d} w_k^{(s)}                 = T_{d,N}^{DIR}
#'   sum_{k ∈ d} w_k^{(s)} * p_{k,O}^{(s)} = T_{d,O}^{DIR}
#'   sum_{k ∈ d} w_k^{(s)} * p_{k,D}^{(s)} = T_{d,D}^{DIR}
#'
#' Este sistema se resuelve mediante calibración lineal (Deville–Särndal) usando
#' `sampling::calib()`.
#'
#' ## Construcción de la matriz de calibración
#'
#' Para cada DAM d se construyen tres bloques de restricciones:
#'
#'   - Bloque N: 1(k ∈ d)
#'   - Bloque O: 1(k ∈ d) * p_{k,O}^{(s)}
#'   - Bloque D: 1(k ∈ d) * p_{k,D}^{(s)}
#'
#' Apilando estos bloques se obtiene la matriz Xs, y el vector de totales reales
#' t_DIR = (N_1, ..., N_D, O_1, ..., O_D, D_1, ..., D_D).
#'
#' ## Validaciones
#'
#' En cada iteración se verifica:
#' \itemize{
#'   \item Coherencia aritmética de los totales directos (O ≥ 0, D ≥ 0, O + D ≤ N).
#'   \item Variabilidad interna dentro de cada DAM (varianza positiva de
#'         p_{k,O} y p_{k,D}).
#'   \item Cierre exacto posterior al benchmarking dentro de una tolerancia `tol`.
#' }
#'
#' Las iteraciones que no cumplen estas condiciones son descartadas.
#'
#' @param fit Objeto `stanfit` que contiene las cadenas MCMC del modelo multinomial.
#' Debe incluir los parámetros `theta` (dominios observados) y `theta_pred`
#' (dominios predichos).
#'
#' @param theta_obs_ordenado Data frame con la identificación (`dam2`) de los
#' dominios observados, en el mismo orden que `theta` en el objeto `fit`.
#'
#' @param theta_pred_ordenado Data frame con la identificación (`dam2`) de los
#' dominios predichos, en el mismo orden que `theta_pred` en el objeto `fit`.
#'
#' @param conteo_pp_dam Data frame con columnas `dam2`, `dam` y `N_mpio`, donde
#' `N_mpio` representa el conteo poblacional del subdominio.
#'
#' @param totales_reales_dam Data frame con totales directos por DAM. Debe contener
#' las columnas `dam`, `N_tot`, `O_tot` y `D_tot`.
#'
#' @param dam_var Nombre de la variable que identifica el DAM (por defecto `"dam"`).
#'
#' @param method Etiqueta del método de benchmarking (informativa). La calibración
#' se ejecuta con método lineal dentro de `sampling::calib()`.
#'
#' @param max_iter Número máximo de iteraciones permitidas en el algoritmo de
#' calibración.
#'
#' @param tol Tolerancia numérica para validar el cierre exacto de los totales
#' benchmarked.
#'
#' @return Una lista con los siguientes elementos:
#' \itemize{
#'   \item resultados_iter: lista de data frames, uno por iteración MCMC válida,
#'         que contienen pesos benchmarked y contribuciones por categoría.
#'   \item dams: vector con los DAM incluidos.
#'   \item S_total: número total de iteraciones MCMC.
#'   \item S_validas: número de iteraciones que pasaron todas las validaciones.
#'   \item method: método de benchmarking reportado.
#' }
#'
#' @seealso \code{\link[sampling]{calib}}, \code{\link[srvyr]{survey_total}}
#'
#' @export



benchmark_mcmc_multinomial_bench_dam <- function(
    fit,
    theta_obs_ordenado,
    theta_pred_ordenado,
    conteo_pp_dam,        # dam2, dam, N_mpio
    totales_reales_dam,   # dam, N_tot, O_tot, D_tot
    dam_var = "dam",
    method = "logit",     # Se devuelve en output; en calib() se usa "linear" abajo
    max_iter = 5000,
    tol = 1e-6
){
  library(dplyr)
  library(sampling)
  library(fastDummies)
  
  # ============================================================
  # 3.1) VALIDADORES INTERNOS
  # ============================================================
  
  # Verificación aritmética de totales por DAM
  validar_totales <- function(df){
    all(with(df,
             N_tot > 0 &
               O_tot >= 0 & D_tot >= 0 &
               O_tot <= N_tot &
               D_tot <= N_tot &
               (O_tot + D_tot) <= N_tot))
  }
  
  # Evita calibraciones degeneradas donde dentro de un DAM no hay variación
  # (por ejemplo, todas las probabilidades iguales => var = 0)
  validar_variabilidad <- function(df){
    df %>%
      group_by(.data[[dam_var]]) %>%
      summarise(
        var_O = var(Ocupado_pred),
        var_D = var(Desocupado_pred),
        .groups = "drop"
      ) %>%
      summarise(ok = all(var_O > 0 & var_D > 0)) %>%
      pull(ok)
  }
  
  # ============================================================
  # 3.2) EXTRACCIÓN DE DRAWS MCMC
  # ============================================================
  # Se asume que fit contiene dos arreglos:
  #   theta      : [S x n_obs x 3]
  #   theta_pred : [S x n_pred x 3]
  # donde 3 corresponde a (O, D, I) o el orden que uses en Stan.
  
  message("Extrayendo cadenas MCMC...")
  
  # Vector de dam2 en el orden correcto para concatenar obs + pred
  id_dam2 <- bind_rows(theta_obs_ordenado, theta_pred_ordenado) %>% pull(dam2)
  
  theta_draws      <- rstan::extract(fit, pars = "theta")$theta
  theta_pred_draws <- rstan::extract(fit, pars = "theta_pred")$theta_pred
  
  S <- nrow(theta_draws)
  message("Número de iteraciones MCMC: ", S)
  
  # Contenedor para resultados por iteración
  resultados <- vector("list", S)
  
  # ============================================================
  # 3.3) LOOP PRINCIPAL MCMC
  # ============================================================
  for(iter in seq_len(S)){
    
    # ----------------------------------------------------------
    # (1) Construir data.frame de probabilidades multinomiales
    # ----------------------------------------------------------
    # Se concatenan dominios observados y predichos para esta iteración.
    est_i <- bind_rows(
      as.data.frame(theta_draws[iter, , ]),
      as.data.frame(theta_pred_draws[iter, , ])
    )
    
    # Estandarizar nombres de columnas: O, D, I
    colnames(est_i) <- c("Ocupado_pred", "Desocupado_pred", "Inactivo_pred")
    est_i$dam2 <- id_dam2
    
    # ----------------------------------------------------------
    # (2) Adjuntar dominios DAM y población base (N_mpio)
    # ----------------------------------------------------------
    # conteo_pp_dam debe aportar: dam2 -> (dam, N_mpio)
    est_i <- est_i %>%
      left_join(conteo_pp_dam, by = "dam2")
    
    # ----------------------------------------------------------
    # (3) Validaciones previas
    # ----------------------------------------------------------
    # - Totales reales coherentes
    # - Variabilidad suficiente dentro de cada DAM
    if (!validar_totales(totales_reales_dam) ||
        !validar_variabilidad(est_i)) {
      resultados[[iter]] <- NULL
      next
    }
    
    # ----------------------------------------------------------
    # (4) Matriz Xs para calibración (restricciones por DAM)
    # ----------------------------------------------------------
    # Idea: calibrar pesos gk (multiplicadores) sobre d=N_mpio
    # de modo que:
    #   sum(w_bench)                 = N_tot  por DAM
    #   sum(w_bench * p_O)           = O_tot  por DAM
    #   sum(w_bench * p_D)           = D_tot  por DAM
    #
    # Donde w_bench = d * gk.
    
    # Alinear factor de DAM con el orden presente en totales_reales_dam
    est_i[[dam_var]] <- factor(
      est_i[[dam_var]],
      levels = totales_reales_dam[[dam_var]]
    )
    
    # Dummy matrix de DAM: columnas = DAMs, filas = dam2
    X_dam <- model.matrix(
      as.formula(paste0("~ ", dam_var, " - 1")),
      data = est_i
    )
    
    # Nombres determinísticos de dominios
    dams <- colnames(X_dam)
    
    # Xs apila: [N | O | D] por DAM (cada bloque tiene una columna por DAM)
    Xs <- cbind(
      X_dam,                          # Restricción de N
      X_dam * est_i$Ocupado_pred,     # Restricción de O
      X_dam * est_i$Desocupado_pred   # Restricción de D
    )
    
    colnames(Xs) <- c(
      paste0(dams, "_N"),
      paste0(dams, "_O"),
      paste0(dams, "_D")
    )
    
    # ----------------------------------------------------------
    # (5) Vector de totales reales alineado con columnas de Xs
    # ----------------------------------------------------------
    # Se crea un vector (en el mismo orden de columnas de Xs):
    #   c(DAM1_N, ..., DAMK_N, DAM1_O, ..., DAMK_O, DAM1_D, ..., DAMK_D)
    
    totales_reales_dam[[dam_var]] <- factor(
      totales_reales_dam[[dam_var]],
      levels = totales_reales_dam[[dam_var]]
    )
    
    X_tot <- model.matrix(
      as.formula(paste0("~ ", dam_var, " - 1")),
      data = totales_reales_dam
    )
    
    totales_mat <- cbind(
      X_tot * totales_reales_dam$N_tot,  # N por DAM
      X_tot * totales_reales_dam$O_tot,  # O por DAM
      X_tot * totales_reales_dam$D_tot   # D por DAM
    ) %>% colSums()
    
    names(totales_mat) <- c(
      paste0(dams, "_N"),
      paste0(dams, "_O"),
      paste0(dams, "_D")
    )
    
    # ----------------------------------------------------------
    # (6) Calibración: obtener factores gk
    # ----------------------------------------------------------
    # IMPORTANTÍSIMO:
    #   En tu código original había un bug: total = totales (no existe).
    #   Debe ser total = totales_mat.
    #
    # method = "linear" en calib() produce ajuste lineal; si quieres "logit"
    # de Deville-Särndal, el string típico en sampling::calib es "logit".
    # (Dejo tu comportamiento original: calib usa "linear" como estaba.)
    #
    # Si calib falla (matriz singular, etc.), se descarta la iteración.
    
    gk <- tryCatch(
      calib(
        Xs       = Xs,
        d        = est_i$N_mpio,
        total    = totales_mat,     # <- FIX coherente
        method   = "linear",
        max_iter = max_iter
      ),
      error = function(e) return(NULL)
    )
    
    # Si la calibración falla, se descarta la iteración
    if (is.null(gk) || any(!is.finite(gk))) {
      resultados[[iter]] <- NULL
      next
    }
    
    # Pesos benchmarked por unidad (dam2)
    w_bench <- est_i$N_mpio * gk
    
    # ----------------------------------------------------------
    # (7) Validación posterior: cierre exacto por DAM
    # ----------------------------------------------------------
    # Se verifica que los totales ajustados cierran dentro de tolerancia.
    cierre_ok <- est_i %>%
      mutate(w = w_bench) %>%
      group_by(.data[[dam_var]]) %>%
      summarise(
        N_chk = sum(w),
        O_chk = sum(w * Ocupado_pred),
        D_chk = sum(w * Desocupado_pred),
        .groups = "drop"
      ) %>%
      left_join(totales_reales_dam, by = dam_var) %>%
      summarise(
        ok = all(
          abs(N_chk - N_tot) < tol &
            abs(O_chk - O_tot) < tol &
            abs(D_chk - D_tot) < tol
        )
      ) %>%
      pull(ok)
    
    if (!cierre_ok){
      resultados[[iter]] <- NULL
      next
    }
    
    # ----------------------------------------------------------
    # (8) Guardar resultados válidos para la iteración
    # ----------------------------------------------------------
    resultados[[iter]] <- est_i %>%
      mutate(
        gk      = gk,
        w_bench = w_bench,
        # Contribuciones benchmarked por categoría
        O_bench = w_bench * Ocupado_pred,
        D_bench = w_bench * Desocupado_pred,
        I_bench = w_bench * Inactivo_pred
      )
  }
  
  # Filtrar iteraciones válidas (no NULL)
  resultados_final <- resultados[!sapply(resultados, is.null)]
  
  message("Iteraciones válidas: ",
          length(resultados_final), " de ", S)
  
  return(list(
    resultados_iter = resultados_final,
    dams            = unique(conteo_pp_dam[[dam_var]]),
    S_total         = S,
    S_validas       = length(resultados_final),
    method          = method
  ))
}