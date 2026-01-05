#' Agregados directos de empleo por DAM con intervalos de confianza de diseño
#'
#' Calcula estimaciones directas por dominio agregado (DAM) para indicadores de
#' empleo, incluyendo totales y proporciones, junto con sus respectivos
#' intervalos de confianza de diseño, a partir de un objeto de encuesta complejo
#' compatible con `srvyr`.
#'
#' La función produce estimaciones para las categorías:
#' Ocupado (O), Desocupado (D) e Inactivo (I), así como los indicadores derivados:
#' Tasa de Ocupación (TO), Tasa de Participación (TP) y Tasa de Desocupación (TD).
#'
#' @details
#'
#' ## Definición de indicadores
#'
#' Para cada DAM d, se calculan los siguientes totales directos:
#'
#'   N_d = sum_{i ∈ s_d} w_i
#'   O_d = sum_{i ∈ s_d} w_i * 1(y_i = Ocupado)
#'   D_d = sum_{i ∈ s_d} w_i * 1(y_i = Desocupado)
#'   I_d = sum_{i ∈ s_d} w_i * 1(y_i = Inactivo)
#'
#' A partir de estos totales se definen las proporciones:
#'
#'   TO_d = O_d / N_d
#'   TP_d = (O_d + D_d) / N_d
#'   TD_d = D_d / (O_d + D_d)
#'
#' donde la TD se calcula condicional a la Población Económicamente Activa (PEA),
#' es decir, restringiendo el denominador a las categorías Ocupado y Desocupado.
#'
#' ## Intervalos de confianza
#'
#' Los intervalos de confianza se calculan utilizando métodos de linealización
#' bajo el diseño muestral complejo, mediante las funciones:
#'
#' - `survey_total(vartype = "ci")` para totales,
#' - `survey_mean(vartype = "ci")` para proporciones.
#'
#' Estos intervalos corresponden a **incertidumbre de diseño** y no deben
#' interpretarse como intervalos creíbles bayesianos.
#'
#' @param diseno Objeto de encuesta complejo compatible con `srvyr`, típicamente
#' creado con `as_survey_design()` o `as_survey_rep()`.
#'
#' @param dam_var Nombre de la variable que identifica el dominio agregado (DAM).
#' Por defecto `"dam"`.
#'
#' @param empleo_var Nombre de la variable categórica de empleo. Debe codificar
#' al menos las categorías indicadas en `categorias`.
#'
#' @param categorias Vector con los códigos válidos de la variable de empleo.
#' Por defecto `c(1, 2, 3)` (Ocupado, Desocupado, Inactivo).
#'
#' @param ci_level Nivel del intervalo de confianza de diseño. Por defecto `0.95`.
#'
#' @return Un data frame con una fila por DAM que contiene:
#' \itemize{
#'   \item Totales: `N`, `O`, `D`, `I` y sus IC (`*_lci`, `*_uci`).
#'   \item Proporciones: `TO`, `TP`, `TD` y sus IC (`*_lci`, `*_uci`).
#' }
#'
#' @note
#' Esta función asume que la variable de empleo es mutuamente excluyente y
#' exhaustiva dentro de las categorías especificadas. Se valida que:
#' O + D + I ≤ N (dentro de tolerancia numérica).
#'
#' Para análisis comparativos con resultados bayesianos benchmarked, se
#' recomienda utilizar estas estimaciones directas como referencia externa,
#' manteniendo separados los intervalos de confianza de diseño y los intervalos
#' creíbles del modelo.
#'
#' @seealso
#' \code{\link[srvyr]{survey_total}},
#' \code{\link[srvyr]{survey_mean}}
#'
#' @export


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
