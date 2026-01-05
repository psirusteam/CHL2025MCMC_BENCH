#' Gráfico de comparación entre benchmark MCMC y diseño muestral por indicador
#'
#' Genera un gráfico comparativo por dominio agregado (DAM) entre:
#' \itemize{
#'   \item la media posterior del estimador benchmarked obtenido vía MCMC, y
#'   \item la estimación directa de diseño con su intervalo de confianza.
#' }
#'
#' El gráfico está diseñado para **no mezclar fuentes de incertidumbre**:
#' los intervalos corresponden exclusivamente al diseño muestral, mientras que
#' el resultado bayesiano se resume únicamente mediante su media posterior.
#'
#' @details
#'
#' ## Estructura esperada de los datos
#'
#' El data frame `df` debe contener, al menos, las siguientes columnas:
#'
#' \itemize{
#'   \item `dam`: identificador del dominio agregado.
#'   \item `variable`: nombre del indicador (por ejemplo `"TO"`, `"TD"`, `"TP"`,
#'         `"O"`, `"D"`, `"I"`).
#'   \item `bench_mean`: media posterior del indicador benchmarked.
#'   \item `design`: estimación puntual directa de diseño.
#'   \item `design_lci`: límite inferior del intervalo de confianza de diseño.
#'   \item `design_uci`: límite superior del intervalo de confianza de diseño.
#' }
#'
#' Cada DAM debe tener **una única observación de diseño**, mientras que el
#' benchmark puede provenir de múltiples iteraciones agregadas previamente.
#'
#' ## Convención gráfica
#'
#' El gráfico utiliza los siguientes elementos:
#'
#' \itemize{
#'   \item Barra vertical (errorbar, color rojo oscuro): intervalo de confianza
#'         del diseño muestral.
#'   \item Punto rojo: estimación puntual directa de diseño.
#'   \item Punto azul: media posterior del benchmark MCMC.
#' }
#'
#' La ligera dispersión horizontal (`jitter`) se utiliza únicamente para mejorar
#' la legibilidad cuando existen múltiples DAM.
#'
#' ## Interpretación
#'
#' El gráfico permite evaluar:
#' \itemize{
#'   \item la coherencia entre el benchmark bayesiano y la estimación directa,
#'   \item la magnitud del ajuste inducido por el benchmarking,
#'   \item la posición del estimador modelado respecto al intervalo de diseño.
#' }
#'
#' @param df Data frame que contiene los resultados combinados de diseño y
#' benchmark, típicamente producido tras unir `bench_mean` con `design_long`.
#'
#' @param indicador Cadena de caracteres que identifica el indicador a graficar
#' (por ejemplo `"TO"`, `"TD"`, `"TP"`).
#'
#' @return Un objeto `ggplot` que puede ser impreso directamente o modificado
#' posteriormente (por ejemplo, para facetas, escalas o exportación).
#'
#' @note
#' Este gráfico es **comunicacional y analítico**, no inferencial. Los intervalos
#' corresponden únicamente a incertidumbre de diseño. La incertidumbre posterior
#' del modelo debe analizarse por separado mediante diagnósticos MCMC.
#'
#' @seealso
#' \code{\link{resumir_bench_empleo_dam}},
#' \code{\link{make_agregados_empleo_dam}},
#' \code{\link[ggplot2]{ggplot}}
#'
#' @export

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
