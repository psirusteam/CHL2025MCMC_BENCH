#' Graficar Densidad y Rastro para Muestras Bayesianas
#'
#' Esta función grafica la densidad y el rastro para los parámetros especificados
#' de un modelo bayesiano utilizando muestras MCMC.
#'
#' @param modelo El objeto del modelo bayesiano.
#' @param pars Vector de caracteres que especifica los parámetros a graficar.
#' @return Un gráfico que muestra la densidad y el rastro para los parámetros especificados.
#' @importFrom patchwork / operator /
#' @importFrom bayesplot as.array as_draws_matrix mcmc_dens_chains mcmc_areas traceplot
#' @export
Plot_dens_draws <- function(modelo, pars = "beta[1,1]") {
  require(patchwork)
  
  draws <- as.array(modelo) %>% as_draws_matrix(pars = pars)
  
  dens_chains <- mcmc_dens_chains(draws[, pars])
  areas <- mcmc_areas(draws[, pars])
  trace <- traceplot(modelo, pars = pars, inc_warmup = TRUE)
  
  dens_chains / areas / trace
}
