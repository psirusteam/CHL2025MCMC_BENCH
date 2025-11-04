#' Benchmarking de factores de calibración
#'
#' Implementa el procedimiento de calibración propuesto por Gutiérrez y Guerrero (2022) 
#' para ajustar los pesos de post-estratificación y garantizar que los totales 
#' estimados a partir de un modelo sean consistentes con los totales observados 
#' en la encuesta.
#'
#' @param poststrat_df Data frame de post-estratificación que contiene las 
#'   combinaciones de covariables de interés, el número de unidades en cada celda 
#'   (\code{n}) y el valor esperado de la variable de estudio (\code{yk}) 
#'   proveniente de las predicciones del modelo.
#' @param encuesta_sta Data frame con la información de la encuesta, que incluye 
#'   las mismas covariables en \code{names_cov}, el valor observado de la variable 
#'   de estudio (\code{yk}) y el factor de expansión de la encuesta (\code{fep}).
#' @param names_cov Vector de caracteres con los nombres de las covariables 
#'   utilizadas en la calibración (por ejemplo, \code{c("area", "sexo")}).
#' @param metodo Método de calibración a emplear. Se pasa directamente al 
#'   argumento \code{method} de la función \code{calib} del paquete 
#'   \code{sampling} (por ejemplo, \code{"linear"} o \code{"logit"}).
#' @param show_plot Lógico. Si \code{TRUE}, suprime la generación de resúmenes 
#'   y gráficos de los factores de calibración \eqn{g_k}. Por defecto es \code{FALSE}.
#'
#' @details 
#' El procedimiento consiste en:
#' \enumerate{
#'   \item Construir la matriz de calibración \eqn{\mathbf{X}_k} a partir de los 
#'   post-estratos y la variable de interés \eqn{y_k}.
#'   \item Calcular los totales poblacionales a partir de la encuesta 
#'   (\eqn{\mathbf{t}_X}).
#'   \item Resolver el sistema de calibración mediante \code{calib()}, obteniendo 
#'   los factores \eqn{g_k}.
#'   \item Ajustar los conteos de post-estratos (\eqn{n_k^* = n_k g_k}) para 
#'   asegurar la coherencia entre los totales del modelo y los de la encuesta.
#' }
#'
#' @return Un data frame equivalente a \code{poststrat_df} con una columna 
#'   adicional \code{n2}, que contiene los tamaños de celda ajustados 
#'   (\eqn{n_k g_k}). Además, opcionalmente muestra resúmenes y gráficos 
#'   (histograma y boxplot) de los factores de calibración.
#'
#' @examples
#' \dontrun{
#' # Supongamos que poststrat_df y encuesta_sta están definidos
#' bench_df <- benchmarking(
#'   poststrat_df = poststrat_df,
#'   encuesta_sta = encuesta_sta,
#'   names_cov = c("area", "sexo"),
#'   metodo = "linear"
#' )
#' }
#'
#' @references
#' Gutiérrez, A., & Guerrero, S. (2022). Benchmarking en Cadenas MCMC. 
#'
#' @export


benchmarking <- function(poststrat_df, encuesta_sta,  names_cov, metodo, 
                         show_plot = FALSE){

    Xk <- poststrat_df %>% select(all_of(names_cov)) %>% 
      fastDummies::dummy_cols(select_columns = names_cov,
                              remove_selected_columns = TRUE) %>% 
      mutate_at(vars(matches("\\d$")) ,~.*poststrat_df$yk) %>% as.matrix()
    
    ### total
    
    encuesta_temp <- encuesta_sta %>% select(all_of(names_cov)) %>% 
      fastDummies::dummy_cols(select_columns = names_cov,
                              remove_selected_columns = TRUE)
    Total_Xk <- encuesta_temp %>% 
      mutate_at(vars(matches("\\d$")) ,~.*encuesta_sta$yk*encuesta_sta$fep) %>% 
      colSums()
    
    
    gk_calib <- calib(
      Xs = Xk,
      d = poststrat_df$n,
      total = Total_Xk,
      method = metodo
    ) 
    
    check_calib <- checkcalibration(Xs = Xk, 
                                    d = poststrat_df$n,
                                    total = Total_Xk,
                                    g = gk_calib)$result
    
    if(!check_calib){cat("La calibración NO hizo convergencia \n")}
    if(min(gk_calib) <= 0){cat("Los gks tienen valores negativos \n")}
    
    if(show_plot){
    cat("Resumen de los gks \n")
  
    print(summary(gk_calib))
    
    plot1 <- ggplot(data = data.frame(gk_calib), aes(x = gk_calib))  
    plot1 <- plot1 + geom_histogram(bins = 100) +  labs(y = "",  x = "gks") |
      plot1 + geom_boxplot() +  labs(y = "", x = "gks") +  theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
    
    X11()
    print(plot1) 
    }
  
  return(poststrat_df %>% mutate(n2 = n*gk_calib))
}

