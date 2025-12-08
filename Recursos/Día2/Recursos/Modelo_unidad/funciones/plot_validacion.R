plot_validacion <- function(tabla, aggregado, titulo = NULL) {
  
  
  # Validación de columnas requeridas
  req_vars <- c(aggregado, "directo", "directo_low", "directo_upp", 
                "n_sample", "modelo", "bench")
  faltantes <- setdiff(req_vars, names(tabla))
  if (length(faltantes) > 0) {
    stop(paste("Faltan las siguientes variables en la tabla:", 
               paste(faltantes, collapse = ", ")))
  }
  
  # Convertir a formato largo
  tabla_long <- tabla %>%
    select(all_of(aggregado), n_sample, directo, modelo, bench) %>%
    melt(id.vars = c(aggregado, "n_sample"),
         variable.name = "estimador",
         value.name = "valor")
  
  lims_IC <- tabla %>%
    select(all_of(aggregado), directo_low, directo_upp, valor = directo)
  
  # Gráfico
  gg_plot <- ggplot(tabla_long, aes(
    x = .data[[aggregado]],
    y = valor,
    color = estimador
  )) +
    geom_point(position = position_jitter(width = 0.15),
               alpha = 0.9) +
    geom_errorbar(
      data = lims_IC,
      aes(x = .data[[aggregado]], ymin = directo_low, ymax = directo_upp),
      width = 0.2,
      color = "black"
    ) +
    scale_color_manual(
      values = c("directo" = "red", "modelo" = "blue", "bench" = "green"),
      labels = c("Directo", "Modelo", "Benchmark")
    ) +
    scale_size_continuous(name = "Tamaño muestral") +
    labs(
      x = toupper(aggregado),
      y = "Valor del indicador",
      color = "Estimador",
      title = titulo
    ) +
    theme_bw(base_size = 13) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 90, vjust = 0.3, size = 9),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
    )
  
  
  return(gg_plot)
}
