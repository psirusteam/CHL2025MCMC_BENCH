###############################################################################
# SCRIPT: Benchmarking multinomial iteración-a-iteración (MCMC) por DAM
# TEMA: Empleo O-D-I (Ocupado, Desocupado, Inactivo)
#
# OBJETIVO:
#   - Obtener totales directos por DAM desde el diseño muestral (srvyr).
#   - Ajustar (bench) las probabilidades multinomiales generadas por MCMC,
#     para que los agregados ponderados por DAM cierren exactamente con:
#       N_tot, O_tot, D_tot  (y opcionalmente I_tot si se incluye).
#   - Resumir posterior (media, sd, IC) por DAM e indicador.
#   - Comparar con estimación de diseño y graficar.
#
# SUPUESTOS / INSUMOS:
#   diseno: objeto srvyr (encuesta) con variables dam y empleo.
#   fit: objeto stanfit que contiene parámetros:
#        - theta      (para dominios observados)
#        - theta_pred (para dominios predichos)
#   theta_obs_ordenado y theta_pred_ordenado: dataframes con dam2 en el orden
#        que corresponde a las matrices theta / theta_pred.
#   conteo_pp_dam: data.frame con columnas:
#        - dam2: id dominio sub-DAM
#        - dam : dominio agregado
#        - N_mpio: "peso base" o conteo poblacional por dam2
###############################################################################

library(dplyr)
library(srvyr)

###############################################################################
# 1) TOTALES DIRECTOS POR DAM (BENCHMARKS)
###############################################################################
# Se calcula N_tot, O_tot, D_tot por DAM a partir del diseño muestral.
# NOTA:
#   - survey_total() devuelve un objeto de clase "survey_stat"; se castea a numeric
#     para quedarnos sólo con el estimador (sin varianzas/SE).
#   - empleo se restringe a {1,2,3} para mantener coherencia multinomial.

indicador_agregado <- diseno %>% 
  filter(empleo %in% 1:3) %>% 
  group_by(dam) %>% 
  summarise(
    # Total poblacional (N) por DAM, según diseño
    N_tot = as.numeric(survey_total(vartype = NULL)),
    # Total Ocupados (O) por DAM
    O_tot = as.numeric(survey_total(empleo == 1, vartype = NULL)),
    # Total Desocupados (D) por DAM
    D_tot = as.numeric(survey_total(empleo == 2, vartype = NULL)),
    .groups = "drop"
  )

###############################################################################
# 2) VALIDACIONES BÁSICAS DE COHERENCIA EN TOTALES
###############################################################################
# Estas validaciones son mínimas para que la calibración tenga sentido:
#   - N_tot > 0
#   - O_tot, D_tot >= 0
#   - O_tot + D_tot <= N_tot

totales_reales_dam <- indicador_agregado %>%
  mutate(
    check_N   = N_tot > 0,
    check_OD  = O_tot >= 0 & D_tot >= 0,
    check_sum = (O_tot + D_tot) <= N_tot
  )

stopifnot(
  all(totales_reales_dam$check_N),
  all(totales_reales_dam$check_OD),
  all(totales_reales_dam$check_sum)
)

# Limpiar dataset: dejar sólo lo relevante para benchmarking
totales_reales_dam <- totales_reales_dam %>%
  select(dam, N_tot, O_tot, D_tot)

###############################################################################
# 3) FUNCIÓN: BENCHMARKING ITERACIÓN A ITERACIÓN (MCMC)
###############################################################################
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

###############################################################################
# 4) FUNCIÓN: RESUMEN POSTERIOR (MEDIA + SD + IC) POR DAM E INDICADOR
###############################################################################
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

###############################################################################
# 5) EJECUCIÓN: BENCHMARKING + RESUMEN
###############################################################################
# Aquí se ejecuta el benchmarking y se genera la tabla long con mean/lci/uci.

reusul_res_bench <- benchmark_mcmc_multinomial_bench_dam(
  fit,
  theta_obs_ordenado,
  theta_pred_ordenado,
  conteo_pp_dam,        # dam2, dam, N_mpio
  totales_reales_dam,   # dam, N_tot, O_tot, D_tot
  dam_var = "dam",
  method = "logit",
  max_iter = 5000,
  tol = 1e-6
)

resumen_bench <- resumir_bench_empleo_dam(reusul_res_bench)

bench_long <- resumen_bench$resumen %>%
  select(dam, variable, mean, lci, uci)

###############################################################################
# 6) FUNCIÓN: AGREGADOS DIRECTOS COMPLETOS (N, O, D, I + TP, TD, TO)
###############################################################################
# Se calcula un set completo de indicadores por DAM desde el diseño muestral.
# Esto se usa para comparar vs posterior benchmarked.

make_agregados_empleo_dam <- function(
    diseno,
    dam_var    = "dam",
    empleo_var = "empleo",
    categorias = c(1, 2, 3),
    ci_level   = 0.95
){
  
  library(dplyr)
  library(srvyr)
  
  # ------------------------------------------------------------------
  # Agregados directos con IC (totales + proporciones)
  # ------------------------------------------------------------------
  agg <- diseno %>%
    filter(.data[[empleo_var]] %in% categorias) %>%
    group_by(.data[[dam_var]]) %>%
    summarise(
      
      # ==============================================================
      # TOTALES CON INTERVALO DE CONFIANZA
      # ==============================================================
      N_tot = survey_total(vartype = "ci", level = ci_level),
      O_tot = survey_total(.data[[empleo_var]] == 1,
                           vartype = "ci", level = ci_level),
      D_tot = survey_total(.data[[empleo_var]] == 2,
                           vartype = "ci", level = ci_level),
      I_tot = survey_total(.data[[empleo_var]] == 3,
                           vartype = "ci", level = ci_level),
      
      # ==============================================================
      # PROPORCIONES CON INTERVALO DE CONFIANZA
      # ==============================================================
      # TO = O / N
      TO = survey_mean(.data[[empleo_var]] == 1,
                       vartype = "ci", level = ci_level),
      
      # TP = (O + D) / N
      TP = survey_mean(.data[[empleo_var]] %in% c(1, 2),
                       vartype = "ci", level = ci_level),
      
      # TD = D / (O + D)
      TD = survey_mean(.data[[empleo_var]] == 2,
                       vartype = "ci", level = ci_level,
                       subset = .data[[empleo_var]] %in% c(1, 2)),
      
      .groups = "drop"
    )
  
  # ------------------------------------------------------------------
  # Reorganizar nombres (srvyr devuelve columnas anidadas)
  # ------------------------------------------------------------------
  agg <- agg %>%
    mutate(
      # Totales (extraer estimate, low, upp)
      N     = N_tot,
      N_lci = N_tot_low,
      N_uci = N_tot_upp,
      
      O     = O_tot,
      O_lci = O_tot_low,
      O_uci = O_tot_upp,
      
      D     = D_tot,
      D_lci = D_tot_low,
      D_uci = D_tot_upp,
      
      I     = I_tot,
      I_lci = I_tot_low,
      I_uci = I_tot_upp,
      
      # Proporciones
      TO     = TO,
      TO_lci = TO_low,
      TO_uci = TO_upp,
      
      TP     = TP,
      TP_lci = TP_low,
      TP_uci = TP_upp,
      
      TD     = TD,
      TD_lci = TD_low,
      TD_uci = TD_upp
    ) %>%
    select(
      !!dam_var,
      N, N_lci, N_uci,
      O, O_lci, O_uci,
      D, D_lci, D_uci,
      I, I_lci, I_uci,
      TO, TO_lci, TO_uci,
      TP, TP_lci, TP_uci,
      TD, TD_lci, TD_uci
    )
  
  # ------------------------------------------------------------------
  # Validaciones mínimas de coherencia
  # ------------------------------------------------------------------
  stopifnot(
    all(agg$N > 0),
    all(agg$O >= 0),
    all(agg$D >= 0),
    all(agg$I >= 0),
    all(agg$O + agg$D + agg$I <= agg$N + 1e-6)
  )
  
  return(agg)
}

###############################################################################
# 7) PREPARAR COMPARACIÓN: POSTERIOR BENCH vs DISEÑO (FORMATO LONG)
###############################################################################
agregados_dam <- make_agregados_empleo_dam(diseno)

design_long <- agregados_dam %>%
  pivot_longer(
    cols = -dam,
    names_to = "name",
    values_to = "value"
  ) %>%
  mutate(
    variable = gsub("_(lci|uci)$", "", name),
    tipo     = case_when(
      grepl("_lci$", name) ~ "lci",
      grepl("_uci$", name) ~ "uci",
      TRUE                 ~ "mean"
    )
  ) %>%
  select(dam, variable, tipo, value)

design_long <- design_long %>%
  pivot_wider(
    names_from  = tipo,
    values_from = value
  ) %>%
  rename(
    design     = mean,
    design_lci = lci,
    design_uci = uci
  )

bench_mean <- bench_long %>%
  select(dam, variable, mean) %>%
  rename(bench_mean = mean)

# Unir resultados benchmark (posterior) con estimación directa de diseño
comp <- bench_mean %>%
  left_join(
    design_long,
    by = c("dam", "variable")
  )


###############################################################################
# 8) FUNCIÓN: GRÁFICO DE COMPARACIÓN POR INDICADOR
###############################################################################
# - Puntos (jitter) para la media posterior benchmarked
# - Barras = intervalo creíble (lci, uci)
# - Punto (shape 18) = estimación directa por diseño

plot_compare_indicator <- function(df, indicador){
  
  df_i <- df %>% 
    filter(variable == indicador) %>%
    mutate(fuente = "Posterior benchmarked")
  
  # Diseño: una observación por DAM con IC
  df_design <- df_i %>%
    select(dam, design, design_lci, design_uci) %>%
    distinct() %>%
    mutate(fuente = "Diseño muestral")
  
  ggplot() +
    
    # ----------------------------------------------------------
  # Diseño muestral: IC
  # ----------------------------------------------------------
  geom_errorbar(
    data = df_design,
    aes(
      x = dam,
      ymin = design_lci,
      ymax = design_uci,
      color = fuente
    ),
    width = 0.35,
    linewidth = 1.2
  ) +
    geom_point(
      data = df_design,
      aes(
        x = dam,
        y = design,
        color = fuente
      ),
      position = position_jitter(width = 0.15),
      size = 2.6,
      shape = 16
    ) +  
    # ----------------------------------------------------------
  # Media posterior benchmarked
  # ----------------------------------------------------------
  geom_point(
    data = df_i,
    aes(
      x = dam,
      y = bench_mean,
      color = fuente
    ),
    position = position_jitter(width = 0.15),
    size = 2.6,
    shape = 16
  ) +
    
    scale_color_manual(
      values = c(
        "Posterior benchmarked" = "#1F78B4",
        "Diseño muestral"       = "#B22222"
      )
    ) +
    
    labs(
      title = paste("Benchmarking MCMC vs Diseño –", indicador),
      x = "DAM",
      y = indicador,
      color = NULL
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x  = element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.position = "top",
      panel.grid.minor = element_blank()
    )
}

###############################################################################
# 9) GENERAR LISTA DE GRÁFICOS PARA TODOS LOS INDICADORES
###############################################################################
indicadores <- unique(comp$variable)

plots <- lapply(indicadores, function(v){
  plot_compare_indicator(comp, v)
})

names(plots) <- indicadores

# Ejemplos de impresión individual:
plots$TP
plots$TD
plots$TO
plots$O
plots$D
plots$I

###############################################################################
# FIN
###############################################################################
